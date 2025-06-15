#' Launch the ShinyPLUS App
#'
#' Starts the Shiny application for weekly grocery planning, including features for meal selection, fixed and extra products, basket management, and integration with PLUS.nl.
#' @details
#' The `shinyplus()` function launches a multi-tab Shiny application with the following core components:
#'
#' - **Weekmenu**: Allows users to select dishes for each day of the week. Each dish is linked to ingredients, which can be automatically added to the grocery basket.
#' - **Vaste boodschappen**: Users can maintain and select from a list of fixed (recurring) products. Selected quantities are added to the basket.
#' - **Extra artikelen**: Users can manually search and add additional products not linked to meals or fixed lists.
#' - **Mandje**: A complete overview of all selected products is shown, grouped by label. Users can adjust quantities or remove items. The basket can then be sent to the online PLUS.nl shopping cart.
#' - **PLUS Winkelwagen**: Displays the current contents of the user's PLUS.nl online cart, including quantities, prices, and product images. Includes a checkout button.
#' - **Gerechten beheren**: Users can create and manage custom dishes, define their preparation details, and assign ingredients from the product list.
#' - **Inloggen**: Supports login to PLUS.nl using [`Chromote`][chromote::Chromote] automation. This is required for sending the basket to the online cart or retrieving cart contents.
#'
#' ### Getting Started:
#' 1. Create an account at <https://www.plus.nl> and select your preferred local store.
#' 2. Save your PLUS.nl login credentials in a `.yaml` file with `email` and `password` fields.
#' 3. Set the file path in your R session using:
#'
#'    ```r
#'    options(plus_credentials = "path/to/your_file.yaml")
#'    ```
#' 4. Generate a local product list using `shinyplus::update_product_list_from_html()` (see its documentation for detailed steps).
#' 5. Launch the app with:
#'
#'    ```r
#'    shinyplus::shinyplus()
#'    ```
#'
#' All user-specific data (e.g. dishes, baskets, fixed products) is saved locally as `.rds` files per user.
#' @importFrom shiny a actionButton br checkboxGroupInput checkboxInput column conditionalPanel div fluidPage fluidRow h3 h4 h5 hr HTML icon img isolate modalButton modalDialog navbarPage numericInput observe observeEvent p passwordInput radioButtons reactive reactiveVal reactiveValues removeModal renderUI req selectInput selectizeInput shinyApp showModal showNotification span strong tabPanel tagList tags textInput uiOutput updateActionButton updateCheckboxGroupInput updateCheckboxInput updateNumericInput updateRadioButtons updateSelectInput updateSelectizeInput updateTextInput wellPanel withProgress incProgress
#' @importFrom bslib bs_theme font_google card
#' @importFrom dplyr filter pull mutate select arrange desc inner_join bind_rows distinct if_else left_join count row_number
#' @importFrom tibble tibble as_tibble
#' @importFrom DT datatable DTOutput renderDT formatCurrency
#' @importFrom cli symbol
#' @importFrom shinyjs hide show useShinyjs runjs
#' @encoding UTF-8
#' @inheritSection shinyplus-package Disclaimer
#' @export
shinyplus <- function() {

  weekdays_list <- c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")
  weekdays_list_full <- c(weekdays_list, paste0("extra", 1:5))

  addResourcePath("shinyplus-assets", system.file(package = "shinyplus"))

  ui <- fluidPage(
    useShinyjs(),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('openCheckout', function(url) {
        window.open(url, '_blank');
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateSelectizeDishList', function(message) {
        var input = $('#' + message.inputId)[0];
        if (input && input.selectize) {
          input.selectize.clearOptions();
          message.choices.forEach(function(opt) {
            input.selectize.addOption(opt);
          });
          input.selectize.refreshOptions(false);
        }
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateSelectizeProductList', function(message) {
        var input = $('#' + message.inputId)[0];
        if (input && input.selectize) {
          input.selectize.clearOptions();
          message.choices.forEach(function(opt) {
            input.selectize.addOption(opt);
          });
          input.selectize.refreshOptions(false);
        }
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateBasketCount', function(count) {
        document.getElementById('basket-count').textContent = count;
      });
    ")),
    tags$script(HTML("
      (function waitForImagePreview() {
        const preview = document.getElementById('imagePreview');
        if (!preview) {
          setTimeout(waitForImagePreview, 100); // wait and retry
          return;
        }

        document.addEventListener('mousemove', function (e) {
          if (preview.style.display === 'block') {
            preview.style.left = (e.pageX + 10) + 'px';
            preview.style.top = (e.pageY + 10) + 'px';
          }
        });

        document.addEventListener('mouseover', function (e) {
          const target = e.target.closest('img.hover-preview');
          if (!target) return;

          preview.src = target.src;
          preview.style.display = 'block';
        });

        document.addEventListener('mouseout', function (e) {
          const target = e.target.closest('img.hover-preview');
          if (!target) return;

          preview.style.display = 'none';
        });
      })();
    ")),
    tags$style(HTML("
      :root {
        --plus-red: rgb(227, 19, 29);
        --plus-green-light: rgb(128, 189, 29);
        --plus-green-dark: rgb(34, 118, 71);
        --plus-purple: rgb(85, 77, 167);
        --plus-purple-bg: rgb(255, 231, 244);
      }

      #imagePreview {
        position: absolute;
        max-width: 600px;
        max-height: 400px;
        pointer-events: none;
        display: none;
        padding: 8px;
        border-radius: 12px;
        background: rgba(255, 255, 255, 0.3); /* translucent */
        backdrop-filter: blur(12px); /* macOS-style blur */
        -webkit-backdrop-filter: blur(12px); /* for Safari */
        border: 1px solid rgba(255, 255, 255, 0.5);
        z-index: 9999;
      }

      .navbar {
        background: none;
      }
      hr {
        margin: 1rem 0;
      }

      #basket-icon-wrapper {
        position: absolute;
        top: 10px;
        right: 20px;
        font-size: 2rem;
        z-index: 999;
      }

      .btn-danger {
        background: var(--plus-red);
        border-color: var(--plus-red);
      }
      .btn-danger:hover, .btn-danger:active, .btn-danger:focus {
        background: color-mix(in srgb, var(--plus-red) 70%, black);
        border-color: var(--plus-red);
      }
      .btn-success {
        background: var(--plus-green-light);
        border-color: var(--plus-green-light);
      }
      .btn-success:hover, .btn-success:active, .btn-success:focus {
        background: color-mix(in srgb, var(--plus-green-light) 70%, black);
        border-color: var(--plus-green-light);
      }
      .btn-primary {
        background: var(--plus-purple);
        border-color: var(--plus-purple);
      }
      .btn-primary:hover, .btn-primary:active, .btn-primary:focus {
        background: color-mix(in srgb, var(--plus-purple) 70%, black);
        border-color: var(--plus-purple);
      }
      a {
        color: var(--plus-purple);
      }
      a:hover, a:active, a:focus {
        color: color-mix(in srgb, var(--plus-purple) 70%, black);
      }

      .text-danger {
        color: var(--plus-red) !important;
      }

      .shiny-input-container .checkbox input:checked,
      .shiny-input-container .radio input:checked {
        background-color: var(--plus-purple);
        border-color: var(--plus-purple);
      }
      .shiny-input-container input:focus {
        box-shadow: none !important;
      }
      .selectize-dropdown .selected {
        background-color: var(--plus-purple);
      }

      #column-weekmenu .bslib-card {
        background: color-mix(in srgb, var(--plus-green-light) 3%, white);
      }
      #column-weekmenu .bslib-card h3,
      #column-weekmenu .bslib-card h5 {
        color: var(--plus-green-light);
      }
      #column-weekmenu .btn:hover {
        background-color: color-mix(in srgb, var(--plus-green-light) 25%, white);
        border-color: inherit;
        color: inherit;
      }
      #column-sale .bslib-card {
        background: color-mix(in srgb, var(--plus-red) 3%, white);
      }
      #column-sale .bslib-card h3,
      #column-sale .bslib-card h5 {
        color: var(--plus-red);
      }
      #column-sale .btn:hover {
        background-color: color-mix(in srgb, var(--plus-red) 25%, white);
        border-color: inherit;
        color: inherit;
      }
      #column-fixed .bslib-card {
        background: color-mix(in srgb, var(--plus-purple) 3%, white);
      }
      #column-fixed .bslib-card h3,
      #column-fixed .bslib-card h5 {
        color: var(--plus-purple);
      }
      #column-fixed .btn:hover {
        background-color: color-mix(in srgb, var(--plus-purple) 25%, white);
        border-color: inherit;
        color: inherit;
      }
      #column-extra .bslib-card {
        background: color-mix(in srgb, var(--plus-green-dark) 3%, white);
      }
      #column-extra .bslib-card h3,
      #column-extra .bslib-card h5 {
        color: var(--plus-green-dark);
      }
      #column-extra .btn:not(.btn-danger):not(.btn-success):hover {
        background-color: color-mix(in srgb, var(--plus-green-dark) 25%, white);
        border-color: inherit;
        color: inherit;
      }

      .well {
        background: rgba(255, 255, 255, 0.7);
        color: RGB(var(--bs-emphasis-color-rgb, 0, 0, 0));
        border-radius: 10px;
      }

      .row.products-list-row {
        height: 75px;
        display: flex;
        align-items: center;
        margin-bottom: 5px;
      }

      .products-list-img img {
        max-height: 70px;
        max-width: 100%;
        object-fit: contain;
      }

      .products-list-p p {
        margin: 0;
        font-size: 0.95rem;
        display: inline-block;
        line-height: 1.2;
      }

      @media (max-width: 575.98px) {
        .row.row.products-list-row {
          display: inline;
        }
      }

      .basket-label {
        font-size: 0.7em;
        margin-left: 5px;
        padding-left: 5px;
        padding-right: 5px;
        border-radius: 5px;
      }

      .products-list-p .basket-label.weekmenu {
        color: white;
        background: var(--plus-green-light);
        border: 1px solid var(--plus-green-light);
      }
      .products-list-p .basket-label.sale {
        color: white;
        background: var(--plus-red);
        border: 1px solid var(--plus-red);
      }
      .products-list-p .basket-label.fixed {
        color: white;
        background: var(--plus-purple);
        border: 1px solid var(--plus-purple);
      }
      .products-list-p .basket-label.extra {
        color: white;
        background: var(--plus-green-dark);
        border: 1px solid var(--plus-green-dark);
      }

      .products-list-qty {
        padding-top: 10px;
      }

      .products-list-qty .form-control {
        height: 35px;
        padding: 5px;
      }

      .dish-selector .selectize-dropdown .option {
        padding: 6px 12px !important;  /* More left/right padding */
        font-weight: normal !important;  /* Not bold */
      }

      .dish-selector .selectize-dropdown .option:hover,
      .dish-selector .selectize-dropdown .option.active {
        background-color: #f0f0f0 !important;  /* Minimalist grey */
        color: #000 !important;
      }

      .dish-selector .selectize-input, .selectize-dropdown {
        font-size: 0.9rem;
      }

      .extra-input-row .selectize-control,
      .extra-input-row .form-control {
        font-size: 0.85rem;
      }

      .card.stretch-with-margin {
        margin-bottom: 300px;
      }

      .selectize-dropdown-content {
        max-height: 500px !important;  /* same as .card.stretch-with-margin */
      }

      #column-sale {
        display: flex;
        flex-direction: column;
        height: calc(100vh - 120px);
        overflow: hidden;
      }
      #sale-list {
        flex-grow: 1;
        overflow-y: auto;
        min-height: 0; /* required for flex child to shrink correctly */
        margin-bottom: 1rem;
      }
      #column-sale > .card:first-child {
        flex-shrink: 0;
      }

      .sale-card {
        border: 1px solid #eee;
        border-radius: 12px;
        padding: 12px;
        background: rgba(255, 255, 255, 0.8);
        text-align: center;
        height: 100%;
      }
      .sale-img {
        height: 120px;
        max-width: 100px;
        margin-bottom: 10px;
      }
      .sale-txt {
        color: var(--plus-red);
        font-size: 0.85rem;
        margin-bottom: 5px;
      }
      .sale-name {
        font-size: 0.85rem;
      }
      .sale-unit {
        color: #888;
        font-size: 0.8rem;
        margin-bottom: 8px;
      }
      .sale-qty * {
        text-align: center;
      }
      .price-line {
        margin-top: 5px;
      }
      .price-current {
        font-size: 1.1rem;
        color: var(--plus-red);
        margin-right: 4px;
      }
      .price-previous {
        text-decoration: line-through;
        color: #888;
        font-size: 0.9rem;
      }

      .product-qty {
        color: grey;
        font-size: 0.9em;
      }

      #online_cart_table td:nth-child(6) {
        font-weight: bold;
      }
    ")),
    tags$img(id = "imagePreview"), # preview image element
    theme = bs_theme(version = 5, base_font = font_google("Open Sans")),
    navbarPage(
      title = div(
        img(src = "shinyplus-assets/shinylogo.png", height = "40px", style = "margin-right: 10px;"),
        span(
          id = "basket-icon-wrapper",
          icon("basket-shopping"),
          span(id = "basket-count", class = "badge badge-danger", style = "position: absolute; top: 5px; right: -4px; background: var(--plus-green-light); color: white; border-radius: 50%; padding: 4px 7px; font-size: 0.75rem;", "0")
        )
      ),
      tabPanel("Boodschappen doen", # UI: Boodschappen ----
               fluidPage(
                 fluidRow(
                   column(2, id = "column-weekmenu",
                          card(class = "basket-card-1",
                               h3("1. Weekmenu"), ## 1. Weekmenu ----
                               actionButton("add_weekmenu_products_to_basket", "Toevoegen aan mandje", icon = icon("basket-shopping")),
                               div(class = "dish-selector",
                                   h5("Avondeten"),
                                   lapply(weekdays_list, function(day) {
                                     selectizeInput(
                                       inputId = paste0("dish_day_", day),
                                       label = day,
                                       width = "100%",
                                       choices = NULL,
                                       options = list(
                                         render = I("
                                          {
                                            option: function(item, escape) {
                                              return '<div style=\"padding-left: 5px;\">' +
                                                       '' + escape(item.label) + '<br>' +
                                                       '<small style=\"opacity: 0.8; padding-left: 20px;\">' +
                                                       item.subtext + '</small>' +
                                                     '</div>';
                                            },
                                            item: function(item, escape) {
                                              return '<div>' + escape(item.label) + '</div>';
                                            }
                                          }
                                        ")
                                       )
                                     )
                                   })
                               ),
                               div(class = "dish-selector",
                                   h5("Lunch / Extra"),
                                   lapply(paste0("extra", 1:5), function(day) {
                                     selectizeInput(
                                       inputId = paste0("dish_day_", day),
                                       label = NULL,
                                       width = "100%",
                                       choices = NULL,
                                       options = list(
                                         render = I("
                                          {
                                            option: function(item, escape) {
                                              return '<div style=\"padding-left: 5px;\">' +
                                                       '' + escape(item.label) + '<br>' +
                                                       '<small style=\"opacity: 0.8; padding-left: 20px;\">' +
                                                       item.subtext + '</small>' +
                                                     '</div>';
                                            },
                                            item: function(item, escape) {
                                              return '<div>' + escape(item.label) + '</div>';
                                            }
                                          }
                                        ")
                                       )
                                     )
                                   })
                               ),
                               # actionButton("save_weekplan", "Weekmenu opslaan", icon = icon("save")),
                               hr(),
                               radioButtons("sort_dishes", "Gerechten sorteren op", choices = c("Bereidingstijd", "Naam", "Hoeveelheid groenten", "Type vlees"), selected = "Bereidingstijd", width = "100%"),
                          ),
                   ),
                   column(5, id = "column-sale",
                          card(class = "basket-card-2",
                               h3("2. Aanbiedingen"), ## 2. Aanbiedingen ----
                               div(id = "loading_spinner", style = "display:none;", p("Bezig met ophalen van aanbiedingen...")),
                               uiOutput("sale_header_ui"),
                          ),
                          card(class = "basket-card-2", id = "sale-list",
                               uiOutput("sale_items_ui"),
                          ),
                   ),
                   column(5, id = "column-fixed",
                          card(class = "basket-card-3",
                               h3("3. Vaste boodschappen"), ## 3. Vast ----
                               fluidRow(
                                 column(6, actionButton("add_fixed_to_basket", "Toevoegen aan mandje", icon = icon("basket-shopping"), width = "100%")),
                                 column(6, actionButton("fixed_to_zero", "Alles op nul zetten", icon = icon("rotate-left"), width = "100%")),
                               ),
                               h5("Selecteer uit vaste producten"),
                               uiOutput("fixed_items_ui"),
                               hr(),
                               h5("Beheer vaste producten"),
                               selectizeInput('add_fixed_product', NULL,
                                              choices = NULL,
                                              width = "100%",
                                              options = list(
                                                placeholder = 'Type om te zoeken...',
                                                dropdownParent = 'body',
                                                onInitialize = I('function() { this.setValue(""); }'),
                                                inputAttr = list(
                                                  autocomplete = "off",
                                                  autocorrect = "off",
                                                  autocapitalize = "off",
                                                  spellcheck = "false"
                                                ),
                                                render = I("{
                                                  option: function(item, escape) {
                                                    return '<div class=\"product-option\" style=\"display: flex; align-items: center; height: 50px;\">' +
                                                             '<img src=\"' + escape(item.img) + '\" style=\"height: 40px; width: 40px; object-fit: contain; margin-right: 10px;\" />' +
                                                             '<div style=\"flex: 1; min-width: 0;\">' +
                                                               '<div style=\"font-weight: normal; text-align: left;\">' + escape(item.label) + '</div>' +
                                                               '<div style=\"color: grey; font-size: 0.8em; text-align: left;\">' + escape(item.subtext) + '</div>' +
                                                             '</div>' +
                                                           '</div>';
                                                  },
                                                  item: function(item, escape) {
                                                    return '<div>' + escape(item.label) + '</div>';
                                                  }
                                                }")
                                              )),
                               fluidRow(
                                 column(6, actionButton("add_fixed_product_button", "Toevoegen aan vaste producten", icon = icon("plus"), width = "100%")),
                                 column(6, actionButton("remove_fixed_product_button", "Verwijderen uit vaste producten", icon = icon("trash"), width = "100%"))
                               )
                          )
                   )
                 )
               )
      ),

      tabPanel("Mandje en PLUS Winkelwagen", # UI: Mandje & PLUS Cart ----
               fluidRow(
                 column(6, id = "column-extra",
                        card(class = "basket-card-4 stretch-with-margin",
                             h3("4. Mandje"),
                             selectizeInput('add_extra_product', "Extra artikel toevoegen:",
                                            choices = NULL,
                                            width = "100%",
                                            options = list(
                                              placeholder = 'Type om te zoeken...',
                                              dropdownParent = 'body',
                                              onInitialize = I('function() { this.setValue(""); }'),
                                              inputAttr = list(
                                                autocomplete = "off",
                                                autocorrect = "off",
                                                autocapitalize = "off",
                                                spellcheck = "false"
                                              ),
                                              render = I("{
                                                  option: function(item, escape) {
                                                    return '<div class=\"product-option\" style=\"display: flex; align-items: center; height: 50px;\">' +
                                                             '<img src=\"' + escape(item.img) + '\" style=\"height: 40px; width: 40px; object-fit: contain; margin-right: 10px;\" />' +
                                                             '<div style=\"flex: 1; min-width: 0;\">' +
                                                               '<div style=\"font-weight: normal; text-align: left;\">' + escape(item.label) + '</div>' +
                                                               '<div style=\"color: grey; font-size: 0.8em; text-align: left;\">' + escape(item.subtext) + '</div>' +
                                                             '</div>' +
                                                           '</div>';
                                                  },
                                                  item: function(item, escape) {
                                                    return '<div>' + escape(item.label) + '</div>';
                                                  }
                                                }")
                                            )),
                             uiOutput("basket_overview_table"),
                             hr(),
                             fluidRow(
                               column(4, actionButton("sort_basket_name", "Op naam", icon = icon("arrow-down-a-z"), width = "100%")),
                               column(4, actionButton("sort_basket_label", "Op label", icon = icon("arrow-down-short-wide"), width = "100%")),
                               column(4, actionButton("sort_basket_quantity", "Op aantal", icon = icon("arrow-down-9-1"), width = "100%"))
                             ),
                             fluidRow(
                               column(6, actionButton("send_basket_to_cart", "In PLUS Winkelwagen plaatsen", icon = icon("cart-arrow-down"), class = "btn-success", width = "100%")),
                               column(6, actionButton("clear_basket", "Mandje leegmaken", icon = icon("trash"),  class = "btn-danger", width = "100%")),
                             )
                        ),
                 ),
                 column(6,
                        uiOutput("online_cart_summary"),
                        br(),
                        br(),
                        DTOutput("online_cart_table"),
                 )
               )
      ),
      tabPanel("Gerechten beheren", # UI: Manage dishes ----
               fluidRow(
                 column(3,
                        wellPanel(
                          h4("Gerecht selecteren"),
                          selectInput("selected_dish", NULL, choices = NULL),
                          actionButton("delete_dish", "Gerecht verwijderen", icon = icon("trash"), class = "btn-danger"),
                          hr(),
                          h5("Nieuw gerecht"),
                          textInput("new_dish_name", NULL, placeholder = "Naam van het gerecht"),
                          actionButton("new_dish", "Nieuw gerecht", icon = icon("plus")),
                        )
                 ),
                 column(3,
                        wellPanel(
                          h4("Gerecht bewerken"),
                          div(style = "display: none;", numericInput("dish_id", NULL, value = 0)),
                          textInput("dish_name", "Naam gerecht:"),
                          checkboxGroupInput("dish_days", "Geschikt voor:", choices = c("Doordeweeks", "Weekend", "Avondeten", "Lunch / Extra" = "Lunch"), selected = NULL),
                          radioButtons("dish_preptime", "Bereidingstijd:",
                                       choices = c(20, 40, 60, 120) |>
                                         # stats::setNames(c("\U0001F552\U0001F642 = tot 20 minuten",
                                         #                   "\U0001F552\U0001F610 = 20-40 minuten",
                                         #                   "\U0001F552\U0001F641 = 40-60 minuten",
                                         #                   "\U0001F552\U0001F975 = 60+ minuten")),
                                         stats::setNames(c("0-20 minuten",
                                                           "20-40 minuten",
                                                           "40-60 minuten",
                                                           "60+ minuten")),
                                       inline = FALSE, selected = 20),
                          fluidRow(
                            column(6,
                                   radioButtons("dish_vegetables", "Hoeveelheid groenten:",
                                                choices = c(0:3) |>
                                                  stats::setNames(c("\U0001F6AB",                       # red round sign emoji
                                                                    "\U0001F966",                       # 2 brocolli emojis
                                                                    "\U0001F966\U0001F966",             # 2 brocolli emojis
                                                                    "\U0001F966\U0001F966\U0001F966")), # 3 brocolli emojis
                                                inline = FALSE,
                                                selected = 0)
                            ),
                            column(6,
                                   radioButtons("dish_meat", "Vlees:",
                                                choices = c("Vegetarisch", "Kip", "Rund", "Varken", "Gecombineerd") |>
                                                  stats::setNames(c("\U0001F331", # leaf emoji
                                                                    "\U0001F413", # chicken emoji
                                                                    "\U0001F404", # cow emoji
                                                                    "\U0001F416", # pig emoji
                                                                    "Combi")),
                                                inline = FALSE,
                                                selected = "Vegetarisch")
                            )
                          )
                        )
                 ),
                 column(6,
                        wellPanel(
                          h5("Ingredi\u00EBnten bewerken"),
                          selectizeInput('ingredient_url', "Ingredi\u00EBnt",
                                         choices = NULL,
                                         width = "100%",
                                         options = list(
                                           placeholder = 'Type om te zoeken...',
                                           dropdownParent = 'body',
                                           onInitialize = I('function() { this.setValue(""); }'),
                                           inputAttr = list(
                                             autocomplete = "off",
                                             autocorrect = "off",
                                             autocapitalize = "off",
                                             spellcheck = "false"
                                           ),
                                           render = I("{
                                                  option: function(item, escape) {
                                                    return '<div class=\"product-option\" style=\"display: flex; align-items: center; height: 50px;\">' +
                                                             '<img src=\"' + escape(item.img) + '\" style=\"height: 40px; width: 40px; object-fit: contain; margin-right: 10px;\" />' +
                                                             '<div style=\"flex: 1; min-width: 0;\">' +
                                                               '<div style=\"font-weight: normal; text-align: left;\">' + escape(item.label) + '</div>' +
                                                               '<div style=\"color: grey; font-size: 0.8em; text-align: left;\">' + escape(item.subtext) + '</div>' +
                                                             '</div>' +
                                                           '</div>';
                                                  },
                                                  item: function(item, escape) {
                                                    return '<div>' + escape(item.label) + '</div>';
                                                  }
                                                }")
                                         )),
                          numericInput("ingredient_quantity", "Aantal", 1),
                          actionButton("add_ingredient", "Toevoegen", icon = icon("plus")),

                          hr(),
                          uiOutput("dish_ingredients_table")
                        )
                 )
               )
      ),
      tabPanel(title = uiOutput("account_tab_title"), value = "account",  # UI: Login ----
               uiOutput("login_ui")
      )
    )
  )

  server <- function(input, output, session) {

    hide("sale-list")

    observe({
      product_choices <- lapply(seq_len(nrow(plus_env$product_list)), function(i) {
        row <- plus_env$product_list[i, ]
        list(
          value = row$url,
          label = row$name,
          subtext = row$unit,
          img = paste0(row$img, "?w=80&h=80")
        )
      })
      session$sendCustomMessage("updateSelectizeProductList", list(
        inputId = "add_fixed_product",
        choices = product_choices))

      session$sendCustomMessage("updateSelectizeProductList", list(
        inputId = "add_extra_product",
        choices = product_choices))

      session$sendCustomMessage("updateSelectizeProductList", list(
        inputId = "ingredient_url",
        choices = product_choices))
    })

    # Server: Setup ----
    values <- reactiveValues(
      sale_items = NULL,
      dishes = tibble(dish_id = numeric(), name = character(), days = character(), preptime = integer(), vegetables = integer(), meat = character()),
      dish_ingredients = tibble(dish_id = numeric(), product_url = character(), quantity = numeric()),
      weekplan = tibble(day = character(), dish = character()),
      fixed_products = character(),
      fixed_items = character(),
      extra_items = character(),
      basket = tibble(product_url = character(), quantity = integer(), label = character()),
      new_dish_name = NULL,
      logged_in = FALSE
    )

    # RDS files for saving
    dishes_file <- reactive({
      req(selected_email())
      file.path(plus_env$data_dir, paste0("dishes-", make.names(selected_email()), ".rds"))
    })
    dish_ingredients_file <- reactive({
      req(selected_email())
      file.path(plus_env$data_dir, paste0("dish_ingredients-", make.names(selected_email()), ".rds"))
    })
    weekplan_file <- reactive({
      req(selected_email())
      file.path(plus_env$data_dir, paste0("weekplan-", make.names(selected_email()), ".rds"))
    })
    fixed_products_file <- reactive({
      req(selected_email())
      file.path(plus_env$data_dir, paste0("fixed_products-", make.names(selected_email()), ".rds"))
    })
    basket_file <- reactive({
      req(selected_email())
      file.path(plus_env$data_dir, paste0("basket-", make.names(selected_email()), ".rds"))
    })

    selected_email <- reactiveVal(NULL)
    extra_input_count <- reactiveVal(1)
    extra_inputs <- reactiveValues(data = list(), expanded = list())

    # fill in values from RDS files
    observeEvent(selected_email(), {
      if (file.exists(dishes_file())) {
        values$dishes <- readRDS(dishes_file())
      }
      if (file.exists(dish_ingredients_file())) {
        values$dish_ingredients <- readRDS(dish_ingredients_file())
      }
      if (file.exists(weekplan_file())) {
        values$weekplan <- readRDS(weekplan_file())
      }
      if (file.exists(fixed_products_file())) {
        values$fixed_products <- readRDS(fixed_products_file())
      }
      if (file.exists(basket_file())) {
        values$basket <- readRDS(basket_file())
      }
    })

    add_to_basket <- function(product_url, quantity = 1, label = "", on_top = FALSE) {
      if (length(product_url) > 1 && length(quantity) == 1) {
        quantity <- rep(quantity, length(product_url))
      }
      if (length(product_url) != length(quantity)) stop("Mismatched product_url and quantity lengths")

      df <- tibble(product_url, quantity, label) |>
        filter(product_url != "", quantity > 0)

      if (nrow(df) == 0) return(invisible())

      if (on_top == TRUE) {
        new_basket <- bind_rows(df, values$basket)
      } else {
        new_basket <- bind_rows(values$basket, df)
      }

      new_basket <- new_basket |>
        # prevent sorting by using factors
        mutate(product_url = factor(as.character(product_url), levels = unique(product_url), ordered = TRUE)) |>
        group_by(product_url, label) |>
        summarise(quantity = sum(quantity), .groups = "drop")

      # Only update if different
      if (!identical(new_basket, values$basket)) {
        values$basket <- new_basket
      }

      showNotification(paste0(sum(df$quantity, na.rm = TRUE),
                              " artikel", ifelse(sum(df$quantity, na.rm = TRUE) > 1, "en", ""),
                              " aan mandje toegevoegd."))
    }


    # Server: Login ----

    user_selection_modal <- function() {
      modalDialog(
        title = "Selecteer een gebruiker",
        selectInput("selected_user_email", "Kies je e-mailadres",
                    choices = c(getOption("plus_credentials")$email, "Zonder inloggen"),
                    selected = NULL),
        easyClose = FALSE,
        footer = actionButton("confirm_user_email", "Doorgaan", class = "btn-primary")
      )
    }

    observe({
      showModal(user_selection_modal())
    })
    observeEvent(input$confirm_user_email, {
      req(input$selected_user_email)
      plus_env$email <- trimws(tolower(input$selected_user_email))
      selected_email(plus_env$email)
      removeModal()
    })

    output$account_tab_title <- renderUI({
      logged_in <- values$logged_in
      email <- plus_env$email
      if (logged_in && !is.null(email)) {
        HTML(paste0("Ingelogd als <strong>", email, "</strong>"))
      } else {
        "Inloggen"
      }
    })

    output$login_ui <- renderUI({
      creds <- getOption("plus_credentials", default = list(email = "", password = ""))
      # keep only the first
      creds <- lapply(creds, function(x) x[1])
      if (!values$logged_in) {
        tagList(
          actionButton("change_user", "Selecteer andere gebruiker", class = "btn-primary", icon = icon("user-xmark")),
          hr(),
          p("Gebruik je PLUS inloggegevens om in te loggen."),
          if (all(unlist(creds) == ""))
            p(HTML("<small>Maak een account aan op "),
              a("plus.nl",
                href = "https://aanmelden.plus.nl/plus/registration",
                target = "_blank",
                .noWS = "outside"),
              HTML(".</small>"))
          else
            p(HTML("<small>Inloggegevens vooraf ingevuld op basis van instelling <code>plus_credentials</code>.</small>")),
          p(HTML(paste0("Email: <strong>", plus_env$email, "</strong>"))),
          passwordInput("login_password", "Wachtwoord", value = creds$password),
          actionButton("do_login", "Inloggen bij PLUS.nl", class = "btn-success", icon = icon("right-to-bracket"))
        )
      } else {
        tagList(
          strong("Ingelogd als: "), plus_env$email,
          br(),
          br(),
          actionButton("do_logout", "Uitloggen", class = "btn-danger", icon = icon("right-from-bracket"))
        )
      }
    })

    observeEvent(input$change_user, {
      selected_email(NULL)
      showModal(user_selection_modal())
    })

    observeEvent(values$logged_in, {
      if (values$logged_in) {
        values$online_cart <- plus_current_cart(credentials = values$credentials, info = FALSE)
      }
    })

    observeEvent(input$do_login, {
      hide("login_password")
      hide("do_login")
      creds <- list(email = plus_env$email, password = input$login_password)
      tryCatch({
        showNotification("Inloggen bij PLUS...")
        plus_login(credentials = creds, info = FALSE)
        values$logged_in <- TRUE
        values$credentials <- creds
        showNotification("Succesvol ingelogd.", type = "message")
      }, error = function(e) {
        show("login_password")
        show("do_login")
        values$logged_in <- FALSE
        showNotification(
          paste("Inloggen mislukt. Probeer het over een paar seconden opnieuw via de knop 'Inloggen'.\nFoutmelding:", e$message),
          type = "error"
        )
      })
    })

    observeEvent(input$do_logout, {
      plus_logout(info = FALSE)
      values$logged_in <- FALSE
    })


    # Server: Boodschappen ----

    ## 1. Weekmenu ----
    get_current_weekmenu_selections <- function() {
      isolate(stats::setNames(lapply(weekdays_list_full, function(day) input[[paste0("dish_day_", day)]]), weekdays_list_full))
    }
    rebuild_weekmenu_inputs <- function(sorted_dishes) {
      current_selections <- get_current_weekmenu_selections()

      for (day in weekdays_list_full) {
        # Filter dishes by day-type
        if (day %in% c("Zondag", "Maandag", "Dinsdag", "Woensdag", "Donderdag") || grepl("extra", day)) {
          dishes <- sorted_dishes |> filter(grepl("Doordeweeks", days))
        } else {
          dishes <- sorted_dishes |> filter(grepl("Weekend", days))
        }
        # Dinner / Lunch
        if (grepl("extra", day)) {
          dishes <- sorted_dishes |> filter(grepl("Lunch", days))
        } else if (day %in% weekdays_list) {
          dishes <- dishes |> filter(grepl("Avond", days))
        }


        if (nrow(dishes) == 0) next

        # Build list of choices with label, value, and subtext
        choices_list <- lapply(seq_len(nrow(dishes)), function(i) {
          dish <- dishes[i, ]
          list(
            label = dish$name,
            value = dish$name,
            subtext = paste0(meat_icon(dish$meat), "&nbsp;&nbsp;", symbol$bullet, "&nbsp;&nbsp;",
                             preptime_icon(dish$preptime), "&nbsp;&nbsp;", symbol$bullet, "&nbsp;&nbsp;",
                             vegetables_icon(dish$vegetables))
          )
        })

        session$sendCustomMessage("updateSelectizeDishList", list(
          inputId = paste0("dish_day_", day),
          choices = choices_list
        ))

        updateSelectInput(session, paste0("dish_day_", day), selected = current_selections[[day]])
      }
    }

    observeEvent(input$sort_dishes, {
      sorted_dishes <- sort_dish_df(values$dishes, input$sort_dishes)
      rebuild_weekmenu_inputs(sorted_dishes)
    })
    observeEvent(values$dishes, {
      sorted_dishes <- sort_dish_df(values$dishes, input$sort_dishes %||% "Bereidingstijd")
      rebuild_weekmenu_inputs(sorted_dishes)
    })

    # observeEvent(input$save_weekplan, {
    #   values$weekplan <- bind_rows(lapply(weekdays_list, function(day) {
    #     dish <- input[[paste0("dish_day_", day)]]
    #     tibble(day = day, dish = ifelse(is.null(dish) || dish == "", NA_character_, dish))
    #   }))
    #   saveRDS(values$weekplan, weekplan_file())
    # })

    observeEvent(input$add_weekmenu_products_to_basket, {
      selected_dishes <- unlist(lapply(weekdays_list_full, function(day) input[[paste0("dish_day_", day)]]))
      selected_dishes <- selected_dishes[selected_dishes != ""]
      dish_ingredients <- values$dish_ingredients |>
        inner_join(values$dishes |> filter(name %in% selected_dishes), by = "dish_id")
      ingredients <- rep(dish_ingredients$product_url, dish_ingredients$quantity)

      if (length(ingredients) == 0) {
        showNotification("Geen producten om toe te voegen.", type = "error")
      } else {
        add_to_basket(product_url = ingredients, quantity = 1, label = "Weekmenu")
      }
    })

    ## 2. Aanbiedingen ----
    output$sale_header_ui <- renderUI({
      if (is.null(values$sale_items)) {
        tagList(
          div(id = "div_sale_retrieve",
              actionButton("sale_retrieve", "Aanbiedingen ophalen van PLUS.nl", icon = icon("cloud-arrow-down"), width = "100%"),
              br(),
              br()),
          p("Dit haalt de aanbiedingen op van ", a(href = plus_url("aanbiedingen"), "www.plus.nl/aanbiedingen", .noWS = "outside"), ".")
        )
      } else {
        df <- values$sale_items

        tagList(
          h5(paste0("Geldig van ", trimws(tolower(attributes(df)$promo_period)), ".")),
          p(paste("Er zijn", NROW(df), "aanbiedingen. Klik op de afbeelding om naar de aanbieding te gaan.")),
          br(),
          actionButton("add_sale_to_basket", "Toevoegen aan mandje", icon = icon("basket-shopping"), width = "100%"),
        )
      }
    })
    output$sale_items_ui <- renderUI({
      if (!is.null(values$sale_items)) {
        df <- values$sale_items
        tagList(
          div(class = "sale-rows",
              lapply(seq(1, NROW(df), by = 3), function(i) {
                fluidRow(
                  lapply(i:min(i + 2, NROW(df)), function(j) {
                    row <- df[j, ]
                    column(
                      width = 4,
                      div(class = "sale-card",
                          a(href = plus_url(row$url), target = "_blank",
                            img(src = row$img, class = "sale-img hover-preview")
                          ),
                          div(class = "sale-txt", row$sale_txt),
                          div(class = "sale-name", row$name),
                          div(class = "sale-unit", row$unit),
                          div(class = "price-line",
                              span(class = "price-current", as_euro(row$price_current)),
                              span(class = "price-previous", row$price_previous)
                          ),
                          if (isTRUE(row$is_product)) {
                            div(class = "sale-qty",
                                numericInput(
                                  inputId = paste0("qty_sale_", make.names(row$url)),
                                  label = NULL,
                                  value = 0,
                                  min = 0,
                                  step = 1,
                                  width = "100%"
                                )
                            )
                          }
                      )
                    )
                  })
                )
              })
          )
        )
      }
    })
    observeEvent(input$sale_retrieve, {
      hide("div_sale_retrieve")
      show("loading_spinner")
      df <- get_sales(replace_img = TRUE)  # this might take up to 5 seconds
      values$sale_items <- df
      hide("loading_spinner")
      show("sale-list")
    })
    observeEvent(input$add_sale_to_basket, {
      df <- values$sale_items
      qtys <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        if (!isTRUE(row$is_product)) return(NULL)
        input_id <- paste0("qty_sale_", make.names(row$url))
        qty <- input[[input_id]]
        if (is.null(qty) || qty <= 0) return(NULL)
        tibble(product_url = row$url, quantity = qty)
      })
      qtys <- bind_rows(qtys)
      if (nrow(qtys) > 0) {
        add_to_basket(product_url = qtys$product_url, quantity = qtys$quantity, label = "Aanbieding")
      }
    })

    ## 3. Vast ----

    output$fixed_items_ui <- renderUI({
      if (length(values$fixed_products) == 0) return(p("Nog geen vaste producten."))

      values$fixed_products <- sort(values$fixed_products)

      tagList(
        lapply(values$fixed_products, function(prod) {
          fluidRow(
            class = "row products-list-row",
            column(2, div(class = "products-list-img", height = "100%", a(href = plus_url(prod), target = "_blank", img(src = get_product_image(prod), width = "100%", class = "hover-preview")))),
            column(8, div(class = "products-list-p", height = "100%", p(HTML(paste0(get_product_name(prod), " ", span(class = "product-qty", paste0(symbol$bullet, " ", get_product_unit(prod)))))))),
            column(2, div(class = "products-list-qty", height = "100%", numericInput(paste0("qty_fixed_", make.names(prod)), NULL, value = 0, min = 0, step = 1, width = "100%")))
          )
        })
      )
    })

    # Manage fixed_products (add/remove)
    observeEvent(input$add_fixed_product_button, {
      req(input$add_fixed_product)
      prod <- input$add_fixed_product
      if (!is.null(prod) && !(prod %in% values$fixed_products)) {
        values$fixed_products <- unique(c(values$fixed_products, prod))
        saveRDS(values$fixed_products, fixed_products_file())
      }
      updateSelectInput(session, "add_fixed_product", selected = "")
    })

    observeEvent(input$remove_fixed_product_button, {
      req(input$add_fixed_product)
      prod <- input$add_fixed_product
      if (!is.null(prod) && prod %in% values$fixed_products) {
        values$fixed_products <- setdiff(values$fixed_products, prod)
        saveRDS(values$fixed_products, fixed_products_file())
      }
      updateSelectInput(session, "add_fixed_product", selected = "")
    })

    # Add selected fixed items + quantities to planning basket
    observeEvent(input$add_fixed_to_basket, {
      qtys <- lapply(values$fixed_products, function(prod) {
        qty <- input[[paste0("qty_fixed_", make.names(prod))]]
        if (is.null(qty) || qty <= 0) return(NULL)
        tibble(product_url = prod, quantity = qty)
      })

      qtys <- bind_rows(qtys)
      if (nrow(qtys) > 0) {
        add_to_basket(product_url = qtys$product_url, quantity = qtys$quantity, label = "Vast")
      }
    })

    observeEvent(input$fixed_to_zero, {
      for (prod in values$fixed_products) {
        input_id <- paste0("qty_fixed_", make.names(prod))
        updateNumericInput(session, inputId = input_id, value = 0)
      }
    })


    ## 4. Mandje ----

    # save to basket RDS if anything is changed
    observeEvent(values$basket, {
      req(selected_email())
      saveRDS(values$basket, basket_file())

      # update number on basket icon
      count <- sum(values$basket$quantity, na.rm = TRUE)
      session$sendCustomMessage("updateBasketCount", count)
    }, ignoreInit = TRUE)

    # basket overview table
    output$basket_overview_table <- renderUI({
      if (nrow(values$basket) == 0) return(p("Mandje is leeg."))

      tagList(
        lapply(seq_len(nrow(values$basket)), function(i) {
          row <- values$basket[i, ]
          prod <- row$product_url
          qty <- row$quantity
          src <- row$label
          src_label <- tolower(row$label)
          if (src_label == "aanbieding") src_label <- "sale"
          if (src_label == "vast") src_label <- "fixed"
          input_id <- paste0("basket_qty_", make.names(prod))
          remove_id <- paste0("basket_remove_", make.names(prod))

          fluidRow(
            class = "row products-list-row",
            column(2, div(class = "products-list-img", a(href = plus_url(prod), target = "_blank", img(src = get_product_image(prod), width = "100%", class = "hover-preview")))),
            column(6, div(class = "products-list-p", p(HTML(paste0(get_product_name(prod), " ",
                                                                   span(class = "product-qty", paste0(symbol$bullet, " ", get_product_unit(prod), " ", symbol$bullet)),
                                                                   "<span class='basket-label ", src_label, "'>", src, "</span>"))))),
            column(2, div(class = "products-list-qty", numericInput(input_id, NULL, value = qty, min = 1, step = 1, width = "100%"))),
            column(2, actionButton(remove_id, "", icon = icon("trash"), class = "btn-danger btn-sm", style = "margin-top: -8px;"))
          )
        })
      )
    })

    # Make quantity and remove buttons work
    observe({
      req(nrow(values$basket) > 0)
      lapply(values$basket$product_url, function(prod) {
        input_id <- paste0("basket_qty_", make.names(prod))
        observeEvent(input[[input_id]], {
          new_qty <- input[[input_id]]
          values$basket <- values$basket |>
            mutate(quantity = if_else(as.character(product_url) == as.character(prod), new_qty, quantity))
        }, ignoreInit = TRUE)
      })
    })
    observe({
      req(nrow(values$basket) > 0)
      lapply(values$basket$product_url, function(prod) {
        observeEvent(input[[paste0("basket_remove_", make.names(prod))]], {
          values$basket <- values$basket |> filter(as.character(product_url) != as.character(prod))
        }, ignoreInit = TRUE)
      })
    })

    observeEvent(input$add_extra_product, {
      req(input$add_extra_product)
      product_url <- input$add_extra_product

      add_to_basket(product_url = product_url, quantity = 1, label = "Extra", on_top = TRUE)
      updateSelectInput(session, "add_extra_product", selected = "")
    })

    # Sorting buttons
    observeEvent(input$sort_basket_name, {
      req(nrow(values$basket) > 0)
      current <- values$basket
      # sort ascending
      values$basket <- values$basket |>
        left_join(plus_env$product_list |> select(url, name),
                  by = c("product_url" = "url")) |>
        arrange(name) |>
        select(-name)
      if (identical(current, values$basket)) {
        # sort descending
        values$basket <- values$basket |>
          left_join(plus_env$product_list |> select(url, name),
                    by = c("product_url" = "url")) |>
          arrange(desc(name)) |>
          select(-name)
      }
    })
    observeEvent(input$sort_basket_label, {
      req(nrow(values$basket) > 0)
      current <- values$basket

      base_levels <- c("Weekmenu", "Aanbieding", "Vast", "Extra")
      other_levels <- setdiff(sort(unique(values$basket$label)), base_levels)
      all_levels <- c(base_levels, other_levels)

      # sort ascending
      values$basket <- values$basket |>
        mutate(label = factor(label, levels = all_levels, ordered = TRUE)) |>
        arrange(label) |>
        mutate(label = as.character(label))
      if (identical(current, values$basket)) {
        # sort descending
        values$basket <- values$basket |>
          mutate(label = factor(label, levels = all_levels, ordered = TRUE)) |>
          arrange(desc(label)) |>
          mutate(label = as.character(label))
      }
    })
    observeEvent(input$sort_basket_quantity, {
      req(nrow(values$basket) > 0)
      current <- values$basket

      # sort descending
      values$basket <- values$basket |> arrange(desc(quantity))
      if (identical(current, values$basket)) {
        # sort ascending
        values$basket <- values$basket |> arrange(quantity)
      }
    })

    # Send basket to cart
    observeEvent(input$send_basket_to_cart, {
      if (!values$logged_in) {
        showNotification("Log eerst in om te verzenden naar PLUS Winkelwagen.", type = "error")
        return()
      }

      req(nrow(values$basket) > 0)

      showModal(modalDialog(
        title = "In PLUS Winkelwagen plaatsen",
        p(paste("Weet je zeker dat je",
                ifelse(NROW(values$basket) == 1,
                       "dit artikel",
                       paste("deze", NROW(values$basket), "artikelen")),
                "in de PLUS Winkelwagen wilt plaatsen? Dit kan even duren.")),
        footer = tagList(
          modalButton("Annuleren"),
          actionButton("confirm_send_basket", "OK", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$confirm_send_basket, {
      removeModal()

      showModal(modalDialog(
        title = "In PLUS Winkelwagen plaatsen...",
        tagList(
          div(
            class = "progress",
            div(
              id = "progress_bar",
              class = "progress-bar progress-bar-striped progress-bar-animated",
              role = "progressbar",
              style = "width: 0%; background-color: rgb(85, 77, 167);",
              "0%"
            )
          ),
          br(),
          div(id = "progress_name", style = "text-align:center; font-weight:bold;")
        ),
        footer = NULL,
        easyClose = FALSE
      ))

      n <- nrow(values$basket)

      for (i in seq_len(n)) {
        url <- values$basket$product_url[i]
        quantity <- values$basket$quantity[i]
        name <- escape_js_string(get_product_name_unit(url))
        name_qty <- paste0(name, " x", quantity)
        pct <- round(i / n * 100)

        runjs(sprintf("
          $('#progress_bar').css({
            'width': '%d%%',
            'background-color': 'rgb(85, 77, 167)'
          });
          $('#progress_bar').text('%d%%');
          $('#progress_name').html('%s');
        ", pct, pct, name_qty))

        plus_add_products(url, quantity = quantity, credentials = values$credentials, info = FALSE)
      }

      removeModal()
      showNotification("Artikelen in PLUS Winkelwagen geplaatst.", type = "message")

      # Refresh cart summary + table
      values$online_cart <- plus_current_cart(credentials = values$credentials, info = FALSE)
      if (is.null(values$online_cart)) {
        showNotification("PLUS Winkelwagen niet up-to-date, vernieuwen...", type = "message")
        Sys.sleep(2)
        values$online_cart <- plus_current_cart(credentials = values$credentials, info = FALSE)
      }
    })

    observeEvent(input$clear_basket, {
      values$basket <- tibble(product_url = character(), quantity = integer(), label = character())
    })


    # Server: PLUS Cart ----

    output$online_cart_summary <- renderUI({
      if (!values$logged_in) {
        return(tagList(
          h3("PLUS Winkelwagen"),
          p("Om artikelen in de online PLUS Winkelwagen te zien, log eerst in via het menu 'Inloggen'.", class = "text-danger")
        ))
      }
      req(values$logged_in)
      req(values$online_cart)

      total_items <- sum(values$online_cart$quantity, na.rm = TRUE)
      unique_items <- nrow(values$online_cart)
      original_total <- sum(values$online_cart$price_total, na.rm = TRUE)
      actual_total <- attributes(values$online_cart)$final_total
      saving_pct <- round(((original_total - actual_total) / original_total) * 100, 1)

      tagList(
        h3("PLUS Winkelwagen"),
        p("De inhoud van deze winkelwagen is opgehaald van de ",
          a(href = plus_url("winkelwagen"),
            target= "_blank",
            "online PLUS Winkelwagen", .noWS = "outside"),
          ". Wijzigingen kunnen alleen daar worden aangebracht."),
        br(),
        h4(HTML(paste0("<strong>Totale prijs:</strong> ",
                       as_euro(sum(actual_total, na.rm = TRUE)),
                       ifelse(original_total != actual_total, paste0(" <span class='price-previous'>", as_euro(original_total), "</span>"))))),
        p(HTML(paste0("<strong>Korting:</strong> ", as_euro(original_total - actual_total), " (", format(saving_pct, nsmall = 1, big.interval = ".", decimal.mark = ","), "%)"))),
        p(HTML(paste0("<strong>Totaal artikelen:</strong> ", total_items, " (uniek: ", unique_items, ")"))),
        actionButton("checkout", "Afrekenen bij PLUS.nl", class = "btn-success", icon = icon("right-from-bracket")),
        actionButton("refresh_online_cart", "Vernieuwen", icon = icon("refresh")),
      )
    })
    output$online_cart_table <- renderDT({
      req(values$logged_in)
      req(values$online_cart)

      cart <- values$online_cart |>
        arrange(product) |>
        left_join(plus_env$product_list, by = c("product" = "name", "unit")) |>
        mutate(img = if_else(is.na(img), img,
                             paste0(
                               "<div style='height: 70px; max-width: 100px;'>",
                               "<img src='", img, "' style='max-height: 70px; max-width: 100px; margin-left: auto; margin-right: auto;'>",
                               "</div>")),
               url = if_else(is.na(url), url,
                             paste0("<a href='https://www.plus.nl", url, "' target='_blank'>",
                                    "<i class='fas fa-right-from-bracket'></i></a>")),
               name_unit = paste0(product, " (", unit, ")")) |>
        as_tibble()


      display_cart <- cart |>
        select(" " = img,
               Artikel = name_unit,
               Prijs = price,
               "Per kg|L|st" = per_kg_l_st,
               Aantal = quantity,
               Totaal = price_total,
               Artikelinfo = url)

      datatable(
        display_cart,
        escape = FALSE, # allow HTML for image and icon
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = 't',
          columnDefs = list(
            list(className = 'dt-center', targets = 0),
            list(className = 'dt-left', targets = 1),
            list(className = 'dt-center', targets = 2:6)
          ),
          stripeClasses = NULL # remove row striping
        )
      ) |>
        formatCurrency(columns = c("Prijs", "Per kg|L|st", "Totaal"),
                       currency = "\u20ac ", mark = ".", dec.mark = ",", digits = 2)
    })

    observeEvent(input$checkout, {
      session$sendCustomMessage("openCheckout", plus_url("checkout"))
    })

    observeEvent(input$refresh_online_cart, {
      req(values$logged_in)
      values$online_cart <- plus_current_cart(credentials = values$credentials, info = FALSE)
      if (is.null(values$online_cart)) {
        showNotification("PLUS Winkelwagen niet up-to-date, vernieuwen...", type = "message")
        Sys.sleep(3)
        values$online_cart <- plus_current_cart(credentials = values$credentials, info = FALSE)
      }
    })


    # Server: Manage dishes ----

    # helper: generate unique dish_id
    generate_dish_id <- function() {
      if (nrow(values$dishes) == 0) return(1L)
      max(values$dishes$dish_id, na.rm = TRUE) + 1L
    }

    # update dish selector dropdown
    observe({
      current_id <- input$selected_dish
      choices <- values$dishes$dish_id |> stats::setNames(values$dishes$name)
      updateSelectInput(
        session,
        "selected_dish",
        choices = choices, # critical
        selected = if (current_id %in% values$dishes$dish_id) current_id else NULL
      )
    })

    # new dish = clear form
    observeEvent(input$new_dish, {
      if (input$new_dish_name == "") {
        showNotification("Vul naam van gerecht in.", type = "error")
        return(invisible())
      }
      new_id <- generate_dish_id()
      values$dishes <- bind_rows(values$dishes, tibble(
        dish_id = new_id,
        name = input$new_dish_name,
        days = "Doordeweeks,Weekend,Avondeten",
        preptime = 20,
        vegetables = 0,
        meat = "Vegetarisch"
      )) |>
        arrange(name)
      updateTextInput(session, "new_dish_name", value = "")
      values$new_dish_name <- input$new_dish_name
      updateSelectInput(session, "selected_dish", selected = new_id)
      saveRDS(values$dishes, dishes_file())
    })
    observeEvent(values$new_dish_name, {
      # required to update the select list when clicking new_dish
      req(values$new_dish_name %in% values$dishes$name)
      updateSelectInput(session, "selected_dish", selected = max(values$dishes$dish_id))
      values$new_dish_name <- NULL  # reset
    })
    # updating existing fields
    observeEvent(input$dish_name, {
      req(input$dish_id)
      values$dishes <- values$dishes |>
        mutate(name = if_else(dish_id == input$dish_id, input$dish_name, name)) |>
        arrange(name)
      saveRDS(values$dishes, dishes_file())
    })
    observeEvent(input$dish_days, {
      req(input$dish_id)
      values$dishes <- values$dishes |>
        mutate(days = if_else(dish_id == input$dish_id, paste(input$dish_days, collapse = ","), days))
      saveRDS(values$dishes, dishes_file())
    })
    observeEvent(input$dish_preptime, {
      req(input$dish_id)
      values$dishes <- values$dishes |>
        mutate(preptime = if_else(dish_id == input$dish_id, as.integer(input$dish_preptime), preptime))
      saveRDS(values$dishes, dishes_file())
    })
    observeEvent(input$dish_vegetables, {
      req(input$dish_id)
      values$dishes <- values$dishes |>
        mutate(vegetables = if_else(dish_id == input$dish_id, as.integer(input$dish_vegetables), vegetables))
      saveRDS(values$dishes, dishes_file())
    })
    observeEvent(input$dish_meat, {
      req(input$dish_id)
      values$dishes <- values$dishes |>
        mutate(meat = if_else(dish_id == input$dish_id, input$dish_meat, meat))
      saveRDS(values$dishes, dishes_file())
    })

    # select existing dish
    observeEvent(input$selected_dish, {
      sel <- values$dishes |> filter(dish_id == input$selected_dish)
      if (nrow(sel) != 1) return()
      updateNumericInput(session, "dish_id", value = input$selected_dish)
      updateTextInput(session, "dish_name", value = sel$name)
      updateCheckboxGroupInput(session, "dish_days", selected = unlist(strsplit(sel$days, ",")))
      updateRadioButtons(session, "dish_preptime", selected = sel$preptime)
      updateRadioButtons(session, "dish_vegetables", selected = sel$vegetables)
      updateRadioButtons(session, "dish_meat", selected = sel$meat)
    })

    # delete dish
    observeEvent(input$delete_dish, {
      req(input$selected_dish)

      showModal(modalDialog(
        title = "Weet je het zeker?",
        paste0("Gerecht '", values$dishes$name[values$dishes$dish_id == input$selected_dish], "' verwijderen?"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Annuleren"),
          actionButton("confirm_delete_dish", "Verwijderen", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_delete_dish, {
      req(input$selected_dish)
      sel_id <- values$dishes |> filter(dish_id == input$selected_dish) |> pull(dish_id)
      values$dishes <- values$dishes |> filter(dish_id != input$selected_dish)
      values$dish_ingredients <- values$dish_ingredients |> filter(dish_id != sel_id)

      saveRDS(values$dishes, dishes_file())
      saveRDS(values$dish_ingredients, dish_ingredients_file())

      removeModal()
      updateSelectInput(session, "selected_dish", choices = sort(values$dishes$name), selected = "")
    })

    # save to dish ingredients RDS if anything is changed
    observeEvent(values$dish_ingredients, {
      req(selected_email())
      saveRDS(values$dish_ingredients, dish_ingredients_file())
    }, ignoreInit = TRUE)

    # add ingredient
    observeEvent(input$add_ingredient, {
      req(input$ingredient_url)
      values$dish_ingredients <- bind_rows(values$dish_ingredients, tibble(
        dish_id = as.numeric(input$selected_dish),
        product_url = input$ingredient_url,
        quantity = input$ingredient_quantity
      )) |>
        arrange(product_url) |>
        group_by(dish_id, product_url) |>
        summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop")
      updateSelectInput(session, "ingredient_url", selected = "")
      updateNumericInput(session, "ingredient_quantity", value = 1)
    })

    # render ingredient table with remove buttons
    output$dish_ingredients_table <- renderUI({
      req(input$selected_dish)
      df <- values$dish_ingredients |> filter(dish_id == input$selected_dish)
      if (nrow(df) == 0) return(p("Nog geen ingredi\u00EBnten toegevoegd."))

      tagList(
        lapply(seq_len(nrow(df)), function(i) {
          row <- df[i, ]
          remove_id <- paste0("remove_ingr_", row$dish_id, "_", make.names(row$product_url))

          fluidRow(
            class = "row products-list-row",
            column(2, div(class = "products-list-img", a(href = plus_url(row$product_url), target = "_blank", img(src = get_product_image(row$product_url), width = "100%", class = "hover-preview")))),
            column(9, div(class = "products-list-p", p(HTML(paste0("<strong>", row$quantity, "x</strong> ",
                                                                   get_product_name(row$product_url), " ",
                                                                   span(class = "product-qty", paste0(symbol$bullet, " ", get_product_unit(row$product_url)))))))),
            column(1, actionButton(remove_id, "", icon = icon("trash"), class = "btn-danger btn-sm", style = "margin-top: -8px;"))
          )
        })
      )
    })

    # Make remove buttons work
    observe({
      req(NROW(values$dish_ingredients) > 0)
      lapply(seq_len(nrow(values$dish_ingredients)), function(i) {
        row <- values$dish_ingredients[i, ]
        remove_id <- paste0("remove_ingr_", row$dish_id, "_", make.names(row$product_url))
        observeEvent(input[[remove_id]], {
          values$dish_ingredients <- values$dish_ingredients |> filter(!(dish_id == input$selected_dish & product_url == row$product_url))
        }, ignoreInit = TRUE)
      })
    })
  }

  shinyApp(ui, server)
}

sort_dish_df <- function(dishes, method) {
  sort_field <- switch(method,
                       "Naam" = "name",
                       "Bereidingstijd" = "preptime",
                       "Hoeveelheid groenten" = "vegetables",
                       "Type vlees" = "meat",
                       "preptime")

  if (sort_field == "name") {
    arrange(dishes, name)
  } else if (sort_field == "preptime") {
    arrange(dishes, preptime, desc(vegetables))
  } else if (sort_field == "vegetables") {
    arrange(dishes, desc(vegetables), preptime)
  } else if (sort_field == "meat") {
    arrange(dishes, meat, desc(vegetables), preptime)
  } else {
    dishes
  }
}

preptime_icon <- function(x) {
  # switch(as.character(x),
  #        "20" = "\U0001F552\U0001F642",
  #        "40" = "\U0001F552\U0001F610",
  #        "60" = "\U0001F552\U0001F641",
  #        "120" = "\U0001F552\U0001F975",
  #        paste0(x, "+"))
  switch(as.character(x),
         "20" = "0-20 min",
         "40" = "20-40 min",
         "60" = "40-60 min",
         "120" = "60+ min",
         paste0(x, "+"))
}

meat_icon <- function(type) {
  switch(tolower(type),
         "vegetarisch" = "\U0001F331",
         "kip" = "\U0001F413",
         "rund" = "\U0001F404",
         "varken" = "\U0001F416",
         "gecombineerd" = "Combi",
         type)
}

vegetables_icon <- function(type) {
  switch(as.character(type),
         "0" = "\U0001F6AB",
         "1" = "\U0001F966",
         "2" = "\U0001F966\U0001F966",
         "3" = "\U0001F966\U0001F966\U0001F966",
         type)
}
