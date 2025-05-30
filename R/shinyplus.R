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
#' @importFrom shiny a actionButton br checkboxGroupInput checkboxInput column conditionalPanel div fluidPage fluidRow h3 h4 h5 hr HTML icon img isolate modalButton modalDialog navbarPage numericInput observe observeEvent p passwordInput radioButtons reactive reactiveVal reactiveValues removeModal renderUI req selectInput selectizeInput shinyApp showModal showNotification span strong tabPanel tagList tags textInput uiOutput updateActionButton updateCheckboxGroupInput updateCheckboxInput updateNumericInput updateRadioButtons updateSelectInput updateSelectizeInput updateTextInput wellPanel
#' @importFrom bslib bs_theme font_google card
#' @importFrom dplyr filter pull mutate select arrange desc inner_join bind_rows distinct if_else left_join count row_number
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_detect
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom cli symbol
#' @importFrom shinyjs hide show useShinyjs
#' @encoding UTF-8
#' @inheritSection shinyplus-package Disclaimer
#' @export
shinyplus <- function() {

  weekdays_list <- c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")
  weekdays_short <- substr(weekdays_list, 1, 2)

  ui <- fluidPage(
    useShinyjs(),
    tags$head(tags$script(HTML("
      Shiny.addCustomMessageHandler('openCheckout', function(url) {
        window.open(url, '_blank');
      });
    "))),
    tags$head(
      tags$style(HTML("
        #background-image {
          position: fixed;
          top: 0; left: 0;
          width: 100vw;
          height: 100vh;
          background-image: url('https://upload.wikimedia.org/wikipedia/commons/2/2f/Plus_supermarkt_Delft.jpg');
          /* background-image: url('https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Truck_Spotting_on_the_A58_E312_Direction_Kruiningen-Netherlands_16_04_2020._%2849781332681%29.jpg/1280px-Truck_Spotting_on_the_A58_E312_Direction_Kruiningen-Netherlands_16_04_2020._%2849781332681%29.jpg'); */
          background-size: cover;
          background-position: center;
          opacity: 0.05;
          z-index: -1;
        }
      "))
    ),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateSelectizeChoices', function(message) {
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
    tags$style(HTML("
      .navbar {
        background: none;
      }
      hr {
        margin: 1rem 0;
      }

      .btn-danger {
        background: rgb(208, 50, 44);
        border-color: rgb(208, 50, 44);
      }
      .btn-danger:hover {
        background: rgba(208, 50, 44, 0.8);
        border-color: rgb(208, 50, 44);
      }
      .btn-success {
        background: rgb(127, 187, 29);
        border-color: rgb(127, 187, 29);
      }
      .btn-success:hover {
        background: rgba(127, 187, 29, 0.8);
        border-color: rgb(127, 187, 29);
      }
      .btn-primary {
        background: rgb(85, 77, 167);
        border-color: rgb(85, 77, 167);
      }
      .btn-primary:hover {
        background: rgba(85, 77, 167, 0.8);
        border-color: rgb(85, 77, 167);
      }
      .a {
        color: rgb(85, 77, 167);
      }
      .a:hover {
        color: rgba(85, 77, 167, 0.8);
      }

      .basket-card-1, .products-list-p .basket-label.weekmenu {
        background: rgba(229, 240, 196, 0.45);
      }
      .basket-card-2, .products-list-p .basket-label.vast {
        background: rgba(210, 232, 253, 0.25);
      }
      .basket-card-3 {
        background: rgba(246, 227, 208, 0.70);
      }
      .basket-card-4, .products-list-p .basket-label.extra  {
        background: rgba(244, 240, 237, 0.80);
      }
      .basket-card-1 h3, .products-list-p .basket-label.weekmenu {
        color: #5a7c00; /* dark olive green to match light green bg */
      }
      .basket-card-2 h3, .products-list-p .basket-label.sale {
        color: #2b73af; /* medium blue to match light blue bg */
      }
      .basket-card-3 h3, .products-list-p .basket-label.vast {
        color: #8c5a40; /* warm brown to match light taupe bg */
      }
      .basket-card-4 h3, .products-list-p .basket-label.extra {
        color: #cc5c00; /* burnt orange to match peachy bg */
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
        font-size: 0.6rem;
        margin-left: 5px;
        padding-left: 5px;
        padding-right: 5px;
        border-radius: 5px;
      }
      .basket-label.weekmenu {
        border: 1px solid rgb(90, 124, 0, 0.5);
        color: rgb(90, 124, 0); /* #5a7c00 */
      }
      .basket-label.sale {
        border: 1px solid rgb(43, 115, 175, 0.5);
        color: rgb(43, 115, 175); /* #2b73af */
      }
      .basket-label.vast {
        border: 1px solid rgb(140, 90, 64, 0.5);
        color: rgb(140, 90, 64); /* #8c5a40 */
      }
      .basket-label.extra {
        border: 1px solid rgb(204, 92, 0, 0.5);
        color: rgb(204, 92, 0); /* #8c5a40 */
      }

      .products-list-qty {
        padding-top: 10px;
      }

      .products-list-qty .form-control {
        height: 35px;
        padding: 5px;
      }

      .selectize-dropdown .option {
        padding: 6px 12px !important;  /* More left/right padding */
        font-weight: normal !important;  /* Not bold */
      }

      .selectize-dropdown .option:hover,
      .selectize-dropdown .option.active {
        background-color: #f0f0f0 !important;  /* Minimalist grey */
        color: #000 !important;
      }

      .selectize-input, .selectize-dropdown {
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

      #sale-list {
        height: 65vh;
      }
      .sale-card {
        border: 1px solid #eee;
        border-radius: 12px;
        padding: 12px;
        background: rgba(255,255,255, 0.25);
        text-align: center;
        height: 100%;
      }
      .sale-img {
        height: 120px;
        max-width: 100px;
        margin-bottom: 10px;
      }
      .sale-txt {
        font-weight: bold;
        color: #c00;
        font-size: 0.85em;
        margin-bottom: 5px;
      }
      .sale-name {
        font-weight: 600;
        font-size: 1em;
      }
      .sale-unit {
        color: #666;
        font-size: 0.9em;
        margin-bottom: 8px;
      }
      .sale-qty * {
        text-align: center;
      }
      .price-line {
        margin-top: 5px;
      }
      .price-current {
        font-weight: bold;
        font-size: 1.4em;
        color: #c00;
        margin-right: 8px;
      }
      .price-previous {
        text-decoration: line-through;
        color: #999;
        font-size: 1em;
      }

    ")),
    div(id = "background-image"),
    theme = bs_theme(version = 5, base_font = font_google("Open Sans")),
    navbarPage(
      title = div(
        img(src = "https://upload.wikimedia.org/wikipedia/commons/9/92/PLUS_supermarket_logo.svg", height = "40px", style = "margin-right: 10px;"),
        span("ShinyPLUS", style = "font-weight: bold; font-size: 1.2rem; vertical-align: middle;")
      ),
      tabPanel("Boodschappen doen", # UI: Boodschappen ----
               fluidPage(
                 fluidRow(
                   column(2,
                          card(class = "basket-card-1",
                               h3("1. Weekmenu"), ## 1. Weekmenu ----
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
                               }),
                               # actionButton("save_weekplan", "Weekmenu opslaan", icon = icon("save")),
                               actionButton("add_weekplan_products_to_basket", "Toevoegen aan mandje", icon = icon("basket-shopping")),
                               radioButtons("sort_dishes", "Gerechten sorteren op", choices = c("Bereidingstijd", "Naam", "Hoeveelheid groenten", "Type vlees"), selected = "Bereidingstijd", width = "100%"),
                          ),
                   ),
                   column(5,
                          card(class = "basket-card-2",
                               h3("2. Aanbiedingen"), ## 2. Aanbiedingen ----
                               div(id = "loading_spinner", style = "display:none;", p("Bezig met ophalen van aanbiedingen...")),
                               uiOutput("sale_items_ui1"),
                          ),
                          card(class = "basket-card-2", id = "sale-list",
                               uiOutput("sale_items_ui2"),
                          ),
                   ),
                   column(5,
                          card(class = "basket-card-3",
                               h3("3. Vaste boodschappen"), ## 3. Vast ----

                               h5("Selecteer uit je vaste producten:"),
                               uiOutput("fixed_items_ui"),
                               actionButton("add_fixed_to_basket", "Toevoegen aan mandje", icon = icon("basket-shopping")),
                               actionButton("fixed_to_zero", "Alles op nul zetten", icon = icon("rotate-left")),
                               hr(),
                               h5("Beheer vaste producten:"),
                               selectizeInput('add_fixed_product', NULL, choices = NULL, width = "100%"),
                               actionButton("add_fixed_product_button", "Toevoegen aan vaste producten", icon = icon("plus")),
                               actionButton("remove_fixed_product_button", "Verwijderen uit vaste producten", icon = icon("trash"))
                          )
                   )
                 )
               )
      ),

      tabPanel("Mandje en PLUS Winkelwagen", # UI: PLUS Cart ----
               fluidRow(
                 column(5,
                        card(class = "basket-card-4 stretch-with-margin",
                             h3("4. Mandje"),
                             uiOutput("basket_overview_table"),
                             selectizeInput('add_extra_product', "Extra artikel toevoegen:", choices = NULL, width = "100%"),
                             hr(),
                             fluidRow(
                               column(4, actionButton("sort_basket_name", "Op naam", icon = icon("arrow-down-a-z"), width = "100%")),
                               column(4, actionButton("sort_basket_label", "Op label", icon = icon("arrow-down-short-wide"), width = "100%")),
                               column(4, actionButton("sort_basket_quantity", "Op aantal", icon = icon("arrow-down-9-1"), width = "100%"))
                             ),
                             actionButton("send_basket_to_cart", "In PLUS Winkelwagen plaatsen", icon = icon("cart-arrow-down"), class = "btn-success", width = "100%"),
                             actionButton("clear_basket", "Mandje leegmaken", icon = icon("trash"),  class = "btn-danger", width = "100%"),
                        ),
                 ),
                 column(5,
                        uiOutput("online_cart_summary"),
                        br(), br(),
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
                          checkboxGroupInput("dish_days", "Geschikt voor:", choices = c("Doordeweeks", "Weekend"), selected = NULL),
                          radioButtons("dish_preptime", "Bereidingstijd (minuten):",
                                       choices = c(20, 40, 60, 120) |>
                                         # stats::setNames(c("\U0001F552\U0001F642 = tot 20 minuten",
                                         #                   "\U0001F552\U0001F610 = 20-40 minuten",
                                         #                   "\U0001F552\U0001F641 = 40-60 minuten",
                                         #                   "\U0001F552\U0001F975 = 60+ minuten")),
                                         stats::setNames(c("0+20 minuten",
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
                          selectizeInput('ingredient_url', "Ingredi\u00EBnt", choices = NULL, width = "100%"),
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

    # set product lists in select fields according to https://shiny.posit.co/r/articles/build/selectize/ to save time
    updateSelectizeInput(session, 'add_fixed_product', server = TRUE,
                         choices = plus_env$product_list$url |> stats::setNames(get_product_name_unit(plus_env$product_list$url)),
                         selected = "",
                         options = list(placeholder = 'Type om te zoeken...',
                                        dropdownParent = 'body',
                                        onInitialize = I('function() { this.setValue(""); }'),
                                        inputAttr = list(
                                          autocomplete = "off",
                                          autocorrect = "off",
                                          autocapitalize = "off",
                                          spellcheck = "false")))
    updateSelectizeInput(session, 'add_extra_product', server = TRUE,
                         choices = plus_env$product_list$url |> stats::setNames(get_product_name_unit(plus_env$product_list$url)),
                         selected = "",
                         options = list(placeholder = 'Type om te zoeken...',
                                        dropdownParent = 'body',
                                        onInitialize = I("
                                                            function() {
                                                              this.setValue('');
                                                            }
                                                          "),
                                        inputAttr = list(
                                          autocomplete = "off",
                                          autocorrect = "off",
                                          autocapitalize = "off",
                                          spellcheck = "false"),
                                        onChange = I("
                                                            function(value) {
                                                              if (value !== '') {
                                                                Shiny.setInputValue('add_extra_product', value, {priority: 'event'});
                                                                this.setValue('');
                                                                this.focus(); // Refocus after selection
                                                              }
                                                            }
                                                          ")))
    updateSelectizeInput(session, 'ingredient_url', server = TRUE,
                         choices = plus_env$product_list$url |> stats::setNames(get_product_name_unit(plus_env$product_list$url)),
                         selected = "",
                         options = list(placeholder = 'Type om te zoeken...',
                                        dropdownParent = 'body',
                                        onInitialize = I('function() { this.setValue(""); }'),
                                        inputAttr = list(
                                          autocomplete = "off",
                                          autocorrect = "off",
                                          autocapitalize = "off",
                                          spellcheck = "false")))


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


    extra_input_count <- reactiveVal(1)
    extra_inputs <- reactiveValues(data = list(), expanded = list())

    add_to_basket <- function(product_url, quantity = 1, label = "Extra") {
      if (length(product_url) > 1 && length(quantity) == 1) {
        quantity <- rep(quantity, length(product_url))
      }
      if (length(product_url) != length(quantity)) stop("Mismatched product_url and quantity lengths")

      df <- tibble(product_url, quantity, label) |>
        filter(product_url != "", quantity > 0)

      if (nrow(df) == 0) return(invisible())

      new_basket <- bind_rows(values$basket, df) |>
        # prevent sorting by using factors
        mutate(product_url = factor(product_url, levels = unique(product_url), ordered = TRUE)) |>
        group_by(product_url, label) |>
        summarise(quantity = sum(quantity), .groups = "drop")

      # Only update if different
      if (!identical(new_basket, values$basket)) {
        values$basket <- new_basket
      }
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
          actionButton("change_user", "Selecteer andere gebruiker", class = "btn-primary"),
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
            p(HTML("<small>Inloggegevens ingevuld op basis van instelling <code>plus_credentials</code>.</small>"), class = "text-danger"),
          # textInput("login_email", "E-mail", value = creds$email),
          p(HTML(paste0("Email: <strong>", creds$email, "</strong>"))),
          passwordInput("login_password", "Wachtwoord", value = creds$password),
          actionButton("do_login", "Inloggen bij PLUS.nl", class = "btn-success")
        )
      } else {
        tagList(
          actionButton("change_user", "Selecteer andere gebruiker", class = "btn-primary"),
          hr(),
          strong("Ingelogd als: "), plus_env$email,
          br(),
          actionButton("do_logout", "Uitloggen bij PLUS.nl", class = "btn-danger")
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
      creds <- list(email = plus_env$email, password = input$login_password)
      tryCatch({
        plus_login(credentials = creds)
        values$logged_in <- TRUE
        values$credentials <- creds
        showNotification("Succesvol ingelogd.", type = "message")
      }, error = function(e) {
        values$logged_in <- FALSE
        showNotification(
          paste("Inloggen mislukt. Probeer het over een paar seconden opnieuw via de knop 'Inloggen'.\nFoutmelding:", e$message),
          type = "error"
        )
      })
    })

    observeEvent(input$do_logout, {
      plus_logout()
      values$logged_in <- FALSE
    })


    # Server: Boodschappen ----

    ## 1. Weekmenu ----
    get_current_weekmenu_selections <- function() {
      isolate(stats::setNames(lapply(weekdays_list, function(day) input[[paste0("dish_day_", day)]]), weekdays_list))
    }
    rebuild_weekmenu_inputs <- function(sorted_dishes) {
      current_selections <- get_current_weekmenu_selections()

      for (day in weekdays_list) {
        # Filter dishes by day-type
        day_type <- if (day %in% c("Zondag", "Maandag", "Dinsdag", "Woensdag", "Donderdag")) "Doordeweeks" else "Weekend"
        dishes <- sorted_dishes |> filter(grepl(day_type, days))

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

        session$sendCustomMessage("updateSelectizeChoices", list(
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

    observeEvent(input$add_weekplan_products_to_basket, {
      selected_dishes <- unlist(lapply(weekdays_list, function(day) input[[paste0("dish_day_", day)]]))
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
    output$sale_items_ui1 <- renderUI({
      if (is.null(values$sale_items)) {
        tagList(
          div(id = "div_sale_retrieve",
              actionButton("sale_retrieve", "Aanbiedingen ophalen van PLUS.nl", icon = icon("cloud-arrow-down"), width = "100%"),
              br(),
              br()),
          p("Dit haalt de aanbiedingen op van ", a(href = "https://www.plus.nl/aanbiedingen", "www.plus.nl/aanbiedingen", .noWS = "outside"), ".")
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
    output$sale_items_ui2 <- renderUI({
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
                          a(href = paste0("https://www.plus.nl", row$url), target = "_blank",
                            img(src = row$img, class = "sale-img")
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
      df <- get_sales()  # this might take up to 10 seconds
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
            column(2, div(class = "products-list-img", height = "100%", a(href = get_product_image(prod), target = "_blank", img(src = get_product_image(prod), width = "100%")))),
            column(8, div(class = "products-list-p", height = "100%", p(get_product_name_unit(prod)))),
            column(2, div(class = "products-list-qty", height = "100%", numericInput(paste0("qty_fixed_", make.names(prod)), NULL, value = 0, min = 0, step = 1, width = "100%")))
          )
        })
      )
    })

    # Manage fixed_products (add/remove)
    observeEvent(input$add_fixed_product_button, {
      prod <- input$add_fixed_product
      if (!is.null(prod) && !(prod %in% values$fixed_products)) {
        values$fixed_products <- unique(c(values$fixed_products, prod))
        saveRDS(values$fixed_products, fixed_products_file())
      }
      updateSelectInput(session, "add_fixed_product", selected = "")
    })

    observeEvent(input$remove_fixed_product_button, {
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
          input_id <- paste0("basket_qty_", make.names(prod))
          remove_id <- paste0("basket_remove_", make.names(prod))

          fluidRow(
            class = "row products-list-row",
            column(2, div(class = "products-list-img", a(href = get_product_image(prod), target = "_blank", img(src = get_product_image(prod), width = "100%")))),
            column(6, div(class = "products-list-p", p(HTML(paste0(get_product_name_unit(prod), "<span class='basket-label ", tolower(src), "'>", src, "</span>"))))),
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

      add_to_basket(product_url = product_url, quantity = 1, label = "Extra")
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
        updateActionButton(session, "sort_basket_name", icon = icon("arrow-down-a-z"))
      } else {
        updateActionButton(session, "sort_basket_name", icon = icon("arrow-down-z-a"))
      }
    })
    observeEvent(input$sort_basket_label, {
      req(nrow(values$basket) > 0)
      current <- values$basket

      base_levels <- c("Weekmenu", "Vast", "Extra")
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
        updateActionButton(session, "sort_basket_label", icon = icon("arrow-down-short-wide"))
      } else {
        updateActionButton(session, "sort_basket_label", icon = icon("arrow-down-wide-short"))
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
        updateActionButton(session, "sort_basket_quantity", icon = icon("arrow-down-9-1"))
      } else {
        updateActionButton(session, "sort_basket_quantity", icon = icon("arrow-down-1-9"))
      }
    })

    # Send basket to cart
    observeEvent(input$send_basket_to_cart, {
      if (!values$logged_in) {
        showNotification("Log eerst in om te verzenden naar winkelwagen.", type = "error")
        return()
      }

      req(nrow(values$basket) > 0)

      for (i in seq_len(nrow(values$basket))) {
        url <- values$basket$product_url[i]
        quantity <- values$basket$quantity[i]

        plus_add_products(url, quantity = quantity, credentials = values$credentials, info = FALSE)
      }

      showNotification("In PLUS Winkelwagen geplaatst.", type = "message")
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
      total_price <- sum(values$online_cart$price_total, na.rm = TRUE)

      tagList(
        h3("PLUS Winkelwagen"),
        p("De inhoud van deze winkelwagen is opgehaald van ",
          a(href = "https://www.plus.nl/winkelwagen",
            target= "_blank",
            "www.plus.nl/winkelwagen", .noWS = "outside"), "."),
        p("Wijzigingen kunnen hier niet aangebracht worden. Ook zijn kortingen niet zichtbaar. Ga daarvoor naar de ",
          a(href = "https://www.plus.nl/winkelwagen",
            target= "_blank",
            "online winkelwagen", .noWS = "outside"), "."),
        br(),
        h4(HTML(paste0("<strong>Totale prijs:</strong> ", as_euro(sum(total_price, na.rm = TRUE))))),
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
               name_unit = paste0(product, " (", unit, ")"),
               price = as_euro(price),
               price_total = as_euro(price_total)) |>
        as_tibble()

      display_cart <- cart |>
        select(" " = img, Artikel = name_unit, Prijs = price, Aantal = quantity, Totaal = price_total, Artikelinfo = url)

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
            list(className = 'dt-center', targets = 2:5)
          ),
          stripeClasses = NULL # remove row striping
        )
      )
    })

    observeEvent(input$checkout, {
      session$sendCustomMessage("openCheckout", "https://www.plus.nl/checkout")
    })

    observeEvent(input$refresh_online_cart, {
      req(values$logged_in)
      values$online_cart <- plus_current_cart(credentials = values$credentials, info = FALSE)
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
        choices = sort(choices),
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
        days = "Doordeweeks,Weekend",
        preptime = 20,
        vegetables = 0,
        meat = "Vegetarisch"
      ))
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
        mutate(name = if_else(dish_id == input$dish_id, input$dish_name, name))
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
      updateNumericInput(session, "dish_id", value = sel$dish_id)
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
            column(2, div(class = "products-list-img", a(href = get_product_image(row$product_url), target = "_blank", img(src = get_product_image(row$product_url), width = "100%")))),
            column(9, div(class = "products-list-p", p(HTML(paste0("<strong>", row$quantity, "x</strong> ", get_product_name_unit(row$product_url)))))),
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

    # # remove ingredient listener
    # observe({
    #   if (is.null(input$selected_dish) || input$selected_dish == 0 || input$selected_dish == "") return()
    #   sel_id <- input$selected_dish
    #   if (length(sel_id) != 1) return()
    #
    #   df <- values$dish_ingredients |> filter(dish_id == sel_id)
    #
    #   lapply(seq_len(nrow(df)), function(i) {
    #     observeEvent(input[[paste0("remove_ingr_", i)]], {
    #       values$dish_ingredients <- values$dish_ingredients[-i, ]
    #       saveRDS(values$dish_ingredients, dish_ingredients_file())
    #     }, ignoreInit = TRUE)
    #   })
    # })

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
