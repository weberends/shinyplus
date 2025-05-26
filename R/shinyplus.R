#' Launch PLUS Weekly Grocery App
#' @importFrom shiny fluidPage HTML div img p icon h5 h3 h4 hr br fluidRow wellPanel textInput a strong tagList reactiveVal passwordInput checkboxInput column navbarPage tabPanel selectInput selectizeInput numericInput actionButton uiOutput renderUI shinyApp observe observeEvent updateSelectInput updateNumericInput updateCheckboxInput updateTextInput updateSelectizeInput reactiveValues reactive req isolate tags modalDialog showModal removeModal modalButton showNotification conditionalPanel checkboxGroupInput updateCheckboxGroupInput radioButtons updateRadioButtons
#' @importFrom bslib bs_theme font_google card
#' @importFrom dplyr filter pull mutate select arrange desc inner_join bind_rows distinct if_else left_join count row_number
#' @importFrom tibble tibble
#' @importFrom stringr str_detect
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom cli symbol
#' @encoding UTF-8
#' @export
shinyplus <- function() {
  data_dir <- system.file("plus_data", package = "shinyplus")
  if (!dir.exists(data_dir)) dir.create(data_dir)

  weekdays_list <- c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")
  weekdays_short <- substr(weekdays_list, 1, 2)

  ui <- fluidPage(
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
    tags$style(".navbar { background: none; }"),
    tags$style(HTML("
      .btn-danger {
        background: rgb(208, 50, 44);
        border-color: rgb(208, 50, 44);
      }
      .btn-danger:hover {
        background: rgba(208, 50, 44, 0.8);
        border-color: rgb(208, 50, 44);
      }
      .btn-success {
        background: rgb(140, 185, 64);
        border-color: rgb(140, 185, 64);
      }
      .btn-success:hover {
        background: rgba(140, 185, 64, 0.8);
        border-color: rgb(140, 185, 64);
      }

      .basket-card-1, .products-list-p .basket-source.weekmenu {
        background: rgba(229, 240, 196, 0.45);
      }
      .basket-card-2, .products-list-p .basket-source.vast {
        background: rgba(210, 232, 253, 0.25);
      }
      .basket-card-3, .products-list-p .basket-source.extra  {
        background: rgba(244, 240, 237, 0.80);
      }
      .basket-card-4 {
        background: rgba(246, 227, 208, 0.70);
      }
      .basket-card-1 h3, .products-list-p .basket-source.weekmenu {
        color: #5a7c00; /* dark olive green to match light green bg */
      }
      .basket-card-2 h3, .products-list-p .basket-source.vast {
        color: #2b73af; /* medium blue to match light blue bg */
      }
      .basket-card-3 h3, .products-list-p .basket-source.extra {
        color: #8c5a40; /* warm brown to match light taupe bg */
      }
      .basket-card-4 h3 {
        color: #cc5c00; /* burnt orange to match peachy bg */
      }

      .well {
        background: rgba(255, 255, 255, 0.7);
        color: RGB(var(--bs-emphasis-color-rgb, 0, 0, 0));
        border-radius: 10px;
      }

      .row.products-list-row {
        height: 50px;
        display: flex;
        align-items: center;
        margin-bottom: 5px;
      }

      .products-list-img img {
        max-height: 40px;
        max-width: 100%;
        object-fit: contain;
      }

      .products-list-p p {
        margin: 0;
        font-size: 0.8rem;
        display: inline-block;
        line-height: 1.2;
      }

      .products-list-p .basket-source {
        font-size: 0.6rem;
        margin-left: 5px;
        padding-left: 5px;
        padding-right: 5px;
        border-radius: 5px;
      }
      .products-list-p .basket-source.weekmenu {
        border: 1px solid rgb(90, 124, 0, 0.5);
      }
      .products-list-p .basket-source.vast {
        border: 1px solid rgb(43, 115, 175, 0.5);
      }
      .products-list-p .basket-source.extra {
        border: 1px solid rgb(140, 90, 64, 0.5);
      }

      .products-list-p .basket-source.weekmenu {
        color: rgb(90, 124, 0); /* #5a7c00 */
      }

      .products-list-p .basket-source.vast {
        color: rgb(43, 115, 175); /* #2b73af */
      }

      .products-list-p .basket-source.extra {
        color: rgb(140, 90, 64); /* #8c5a40 */
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
    ")),
    div(id = "background-image"),
    theme = bs_theme(version = 5, base_font = font_google("Open Sans")),
    navbarPage(
      title = div(
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/9/92/PLUS_supermarket_logo.svg", height = "40px", style = "margin-right: 10px;"),
        tags$span("ShinyPLUS", style = "font-weight: bold; font-size: 1.2rem; vertical-align: middle;")
      ),
      tabPanel("Mandje", # UI: Basket ----
               fluidPage(
                 fluidRow(
                   column(3,
                          card(class = "basket-card-1",
                            h3("1. Weekmenu"), ## Step 1 ----
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
                                                 '<small style=\"opacity: 0.8;\">' +
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
                            p(HTML("<small>Producten worden toegevoegd met een apart label 'Weekmenu'.</small>")),
                          ),
                   ),
                   column(3,
                          card(class = "basket-card-2",
                            h3("2. Vaste boodschappen"), ## Step 2 ----

                            h5("Selecteer uit je vaste producten:"),
                            uiOutput("fixed_items_ui"),
                            actionButton("add_fixed_to_basket", "Toevoegen aan mandje", icon = icon("basket-shopping")),
                            actionButton("fixed_to_zero", "Alles op nul zetten", icon = icon("rotate-left")),
                            hr(),
                            h5("Beheer vaste producten:"),
                            selectizeInput("add_fixed_product", NULL,
                                           choices = recently_bought$name |> stats::setNames(get_product_name_unit(recently_bought$name)),
                                           options = list(placeholder = 'Type om te zoeken...',
                                                          onInitialize = I('function() { this.setValue(""); }')),
                                           width = "100%"),
                            actionButton("add_fixed_product_button", "Toevoegen aan vaste producten", icon = icon("plus")),
                            actionButton("remove_fixed_product_button", "Verwijderen uit vaste producten", icon = icon("trash"))
                          )
                   ),
                   column(3,
                          card(class = "basket-card-3",
                            h3("3. Extra artikelen"), ## Step 3 ----
                            h5("Kies extra artikelen:"),
                            uiOutput("extra_inputs_ui"),
                            actionButton("add_all_extras_to_basket", "Toevoegen aan mandje", icon = icon("basket-shopping")),
                            hr(),
                            h5("Reeds toegevoegde extra artikelen:"),
                            uiOutput("extra_items_list")
                          )
                   ),
                   column(3,
                          card(class = "basket-card-4",
                            h3("4. Mandje"), ## Step 4 ----
                            uiOutput("basket_overview_table"),
                            br(),
                            actionButton("send_basket_to_cart", "Mandje in PLUS Winkelwagen plaatsen", class = "btn-success"),
                            actionButton("clear_basket", "Mandje leegmaken", class = "btn-danger"),
                          ),
                          card(
                            uiOutput("img_preview"),
                          )
                   )
                 )
               )
      ),

      tabPanel("PLUS Winkelwagen", # UI: PLUS Cart ----
               fluidRow(
                 column(8,
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
                          actionButton("delete_dish", "Gerecht verwijderen", icon = icon("trash")),
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
                          radioButtons("dish_preptime", "Bereidingstijd (minuten):", choices = c(20, 40, 60, 120) |> stats::setNames(c("0-20", "20-40", "40-60", "60-120")), inline = TRUE, selected = 20),
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
                          selectizeInput("ingredient_name", "Ingredi\u00EBnt",
                                         choices = recently_bought$name,
                                         options = list(placeholder = 'Type om te zoeken...',
                                                        onInitialize = I('function() { this.setValue(""); }'))),
                          uiOutput("ingredient_unit_text"),
                          numericInput("ingredient_amount", "Aantal", 1),
                          actionButton("add_ingredient", "Toevoegen en opslaan"),

                          tags$hr(),
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
    # Server: Setup ----
    values <- reactiveValues(
      dishes = tibble(dish_id = numeric(), name = character(), days = character(), preptime = integer(), vegetables = integer(), meat = character()),
      dish_ingredients = tibble(dish_id = numeric(), product = character(), amount = numeric(), unit = character()),
      weekplan = tibble(day = character(), dish = character()),
      fixed_products = character(),
      fixed_items = character(),
      extra_items = character(),
      basket = tibble(product = character(), quantity = integer(), source = character()),
      new_dish_name = NULL,
      logged_in = FALSE
    )

    # RDS files for saving
    dishes_file <- reactive({
      req(selected_email())
      file.path(data_dir, paste0("dishes-", make.names(selected_email()), ".rds"))
    })
    dish_ingredients_file <- reactive({
      req(selected_email())
      file.path(data_dir, paste0("dish_ingredients-", make.names(selected_email()), ".rds"))
    })
    weekplan_file <- reactive({
      req(selected_email())
      file.path(data_dir, paste0("weekplan-", make.names(selected_email()), ".rds"))
    })
    fixed_products_file <- reactive({
      req(selected_email())
      file.path(data_dir, paste0("fixed_products-", make.names(selected_email()), ".rds"))
    })
    basket_file <- reactive({
      req(selected_email())
      file.path(data_dir, paste0("basket-", make.names(selected_email()), ".rds"))
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

    add_to_basket <- function(product, quantity = 1, source = "Extra") {
      if (length(product) > 1 && length(quantity) == 1) {
        quantity <- rep(quantity, length(product))
      }
      if (length(product) != length(quantity)) stop("Mismatched product and quantity lengths")

      df <- tibble(product, quantity, source) |>
        filter(product != "", quantity > 0)

      if (nrow(df) == 0) return(invisible())

      new_basket <- bind_rows(values$basket, df) |>
        # prevent sorting by using factors
        mutate(product = factor(product, levels = unique(product), ordered = TRUE)) |>
        group_by(product, source) |>
        summarise(quantity = sum(quantity), .groups = "drop")

      # Only update if different
      if (!identical(new_basket, values$basket)) {
        values$basket <- new_basket
      }
    }


    # Server: Login ----

    observe({
      showModal(modalDialog(
        title = "Selecteer een gebruiker",
        selectInput("selected_user_email", "Kies je e-mailadres",
                    choices = c(getOption("plus_credentials")$email, "Zonder inloggen"),
                    selected = NULL),
        easyClose = FALSE,
        footer = tagList(
          # modalButton("Annuleren"),
          actionButton("confirm_user_email", "Doorgaan", class = "btn-primary")
        )
      ))
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
          p("Gebruik je PLUS inloggegevens om in te loggen."),
          if (all(unlist(creds) == ""))
            p(HTML("<small>Maak een account aan op "),
              a("plus.nl",
                href = "https://aanmelden.plus.nl/plus/registration",
                target = "_blank"),
              HTML("</small>"))
          else
            p(HTML("<small>Inloggegevens ingevuld op basis van instelling <code>plus_credentials</code>.</small>"), class = "text-danger"),
          textInput("login_email", "E-mail", value = creds$email),
          passwordInput("login_password", "Wachtwoord", value = creds$password),
          actionButton("do_login", "Inloggen")
        )
      } else {
        tagList(
          strong("Ingelogd als: "), plus_env$email,
          br(),
          actionButton("do_logout", "Uitloggen")
        )
      }
    })

    observeEvent(values$logged_in, {
      if (values$logged_in) {
        values$online_cart <- plus_current_cart(credentials = values$credentials, info = FALSE)
      }
    })

    observeEvent(input$do_login, {
      creds <- list(email = input$login_email, password = input$login_password)
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


    # Server: Basket ----

    ## Step 1 ----
    get_current_weekmenu_selections <- function() {
      isolate(setNames(lapply(weekdays_list, function(day) input[[paste0("dish_day_", day)]]), weekdays_list))
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
            subtext = paste0(meat_icon(dish$meat), " ", symbol$bullet, " ",
                             preptime_display(dish$preptime), " min ", symbol$bullet, " ",
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
      # print("CLICK SORT")
      sorted_dishes <- sort_dish_df(values$dishes, input$sort_dishes)
      rebuild_weekmenu_inputs(sorted_dishes)
    })
    observeEvent(values$dishes, {
      # print("CHANGE DISHES")
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
      ingredients <- rep(dish_ingredients$product, dish_ingredients$amount)

      if (length(ingredients) == 0) {
        showNotification("Geen producten om toe te voegen.", type = "error")
      } else {
        add_to_basket(product = ingredients, quantity = 1, source = "Weekmenu")
      }
    })



    ## Step 2 ----

    output$fixed_items_ui <- renderUI({
      if (length(values$fixed_products) == 0) return(p("Nog geen vaste producten."))

      values$fixed_products <- sort(values$fixed_products)

      tagList(
        lapply(values$fixed_products, function(prod) {
          fluidRow(
            class = "row products-list-row",
            column(2, div(class = "products-list-img", height = "100%", img(src = get_product_image(prod), width = "100%"))),
            column(7, div(class = "products-list-p", height = "100%", p(get_product_name_unit(prod)))),
            column(3, div(class = "products-list-qty", height = "100%", numericInput(paste0("qty_fixed_", make.names(prod)), NULL, value = 0, min = 0, step = 1, width = "100%")))
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
    })

    observeEvent(input$remove_fixed_product_button, {
      prod <- input$add_fixed_product
      values$fixed_products <- setdiff(values$fixed_products, prod)
      saveRDS(values$fixed_products, fixed_products_file())
    })

    # Add selected fixed items + quantities to planning basket
    observeEvent(input$add_fixed_to_basket, {
      qtys <- lapply(values$fixed_products, function(prod) {
        qty <- input[[paste0("qty_fixed_", make.names(prod))]]
        if (is.null(qty) || qty <= 0) return(NULL)
        tibble(product = prod, quantity = qty)
      })

      qtys <- bind_rows(qtys)
      if (nrow(qtys) > 0) {
        add_to_basket(product = qtys$product, quantity = qtys$quantity, source = "Vast")
      }
    })

    observeEvent(input$fixed_to_zero, {
      for (prod in values$fixed_products) {
        input_id <- paste0("qty_fixed_", make.names(prod))
        updateNumericInput(session, inputId = input_id, value = 0)
      }
    })

    ## Step 3 ----

    observe({
      n <- extra_input_count()

      for (i in seq_len(n)) {
        id_prod <- paste0("extra_item_", i)
        id_qty  <- paste0("extra_qty_", i)

        prod_val <- input[[id_prod]]
        qty_val  <- input[[id_qty]]

        if (!is.null(prod_val) && nzchar(prod_val)) {
          prev <- tryCatch(extra_inputs$data[[i]], error = function(e) NULL)

          if (is.null(prev) || !identical(prev$product, prod_val) || !identical(prev$qty, qty_val)) {
            extra_inputs$data[[i]] <- list(product = prod_val, qty = qty_val)
          }

          # Expand only ONCE per row
          already_expanded <- tryCatch(extra_inputs$expanded[[i]], error = function(e) FALSE)
          if (i == n && !already_expanded) {
            extra_inputs$expanded[[i]] <- TRUE
            extra_input_count(n + 1)
          }
        }
      }
    })

    observe({
      n <- extra_input_count()

      for (i in seq_len(n)) {
        id_prod <- paste0("extra_item_", i)
        id_qty  <- paste0("extra_qty_", i)

        prod_val <- input[[id_prod]]
        qty_val  <- input[[id_qty]]

        # Store valid product + quantity
        if (!is.null(prod_val) && nzchar(prod_val)) {
          existing <- tryCatch(extra_inputs$data[[i]], error = function(e) NULL)

          # Only update if it changed
          if (is.null(existing) || !identical(existing$product, prod_val) || !identical(existing$qty, qty_val)) {
            extra_inputs$data[[i]] <- list(product = prod_val, qty = qty_val)

            # If it's the last row, add a new blank one
            if (i == n) {
              extra_input_count(n + 1)
            }
          }
        }
      }
    })

    observeEvent(input$add_all_extras_to_basket, {
      items <- lapply(extra_inputs$data, function(x) {
        if (!is.null(x$product) && nzchar(x$product) && x$qty > 0) {
          tibble(product = x$product, quantity = x$qty)
        }
      }) |> bind_rows()

      if (nrow(items) > 0) {
        add_to_basket(product = items$product, quantity = items$quantity, source = "Extra")
      }

      # Reset dynamic inputs
      extra_input_count(1)
      extra_inputs$data <- list()
      extra_inputs$expanded <- list()
    })

    # Grouped summary
    output$extra_items_list <- renderUI({
      if (length(values$extra_items) == 0) return(p("Nog geen extra artikelen toegevoegd."))

      grouped <- as.data.frame(table(values$extra_items))
      names(grouped) <- c("product", "qty")

      tags$ul(
        lapply(seq_len(nrow(grouped)), function(i) {
          tags$li(HTML(paste0("<strong>", grouped$product[i], "</strong>: ", grouped$qty[i], " st.")))
        })
      )
    })

    ## Step 4 ----

    # save to basket if anything is changed
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
          prod <- row$product
          qty <- row$quantity
          src <- row$source
          input_id <- paste0("basket_qty_", make.names(prod))
          remove_id <- paste0("remove_", make.names(prod))

          fluidRow(
            class = "row products-list-row",
            column(2, div(class = "products-list-img", img(src = get_product_image(prod), width = "100%"))),
            column(7, div(class = "products-list-p", p(HTML(paste0(get_product_name_unit(prod), "<span class='basket-source ", tolower(src), "'>", src, "</span>"))))),
            column(2, div(class = "products-list-qty", numericInput(input_id, NULL, value = qty, min = 0, step = 1, width = "100%"))),
            column(1, actionButton(remove_id, "", icon = icon("trash"), class = "btn-danger btn-sm", style = "margin-top: -8px;"))
          )
        })
      )
    })

    # Make Remove button work
    observe({
      req(nrow(values$basket) > 0)
      lapply(values$basket$product, function(prod) {
        observeEvent(input[[paste0("remove_", make.names(prod))]], {
          values$basket <- values$basket |> filter(product != prod)
        }, ignoreInit = TRUE)
      })
    })

    # Send basket to cart
    observeEvent(input$send_basket_to_cart, {
      if (!values$logged_in) {
        showNotification("Log eerst in om te verzenden naar winkelwagen.", type = "error")
        return()
      }

      req(nrow(values$basket) > 0)

      for (i in seq_len(nrow(values$basket))) {
        product <- values$basket$product[i]
        quantity <- values$basket$quantity[i]

        url <- tryCatch(get_product_url(product), error = function(e) NULL)
        if (!is.null(url)) {
          plus_add_products(url, quantity = quantity, credentials = values$credentials, info = FALSE)
        }
      }

      showNotification("Mandje in PLUS Winkelwagen geplaatst.", type = "message")
    })

    observeEvent(input$clear_basket, {
      values$basket <- tibble(product = character(), quantity = integer(), source = character())
    })


    # Server: PLUS Cart ----

    output$online_cart_summary <- renderUI({
      if (!values$logged_in) {
        return(p("Om artikelen in de PLUS Winkelwagen te zien, log eerst in via het menu 'Inloggen'.", class = "text-danger"))
      }
      req(values$logged_in)
      req(values$online_cart)

      total_items <- sum(values$online_cart$quantity, na.rm = TRUE)
      unique_items <- nrow(values$online_cart)
      total_price <- sum(values$online_cart$price_total, na.rm = TRUE)

      tagList(
        h3("Inhoud PLUS Winkelwagen (online)"),
        h4(HTML(paste0("<strong>Totale prijs:</strong> ", as_euro(sum(total_price, na.rm = TRUE))))),
        p(HTML(paste0("<strong>Totaal artikelen:</strong> ", total_items, " (uniek: ", unique_items, ")"))),
        actionButton("checkout", "Afrekenen bij PLUS", class = "btn-success", icon = icon("right-from-bracket")),
        actionButton("refresh_online_cart", "Vernieuwen", icon = icon("refresh")),
      )
    })
    output$online_cart_table <- renderDT({
      req(values$logged_in)
      req(values$online_cart)

      cart <- values$online_cart |>
        arrange(product) |>
        left_join(recently_bought, by = c("product" = "name")) |>
        mutate(img = paste0(
                 "<div style='height: 70px; max-width: 100px;'>",
                 "<img src='", img, "' style='max-height: 70px; max-width: 100px; margin-left: auto; margin-right: auto;'>",
                 "</div>"),
               url = paste0("<a href='https://www.plus.nl", url, "' target='_blank'>",
                            "<i class='fas fa-right-from-bracket'></i></a>"),
               price = as_euro(price),
               price_total = as_euro(price_total))

      display_cart <- cart |>
        select(" " = img, Artikel = product, Prijs = price, Aantal = quantity, Totaal = price_total, Artikelinfo = url)

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
        choices = choices,
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
      updateSelectInput(session, "selected_dish", choices = values$dishes$name, selected = "")
    })

    # add ingredient
    observeEvent(input$add_ingredient, {
      req(input$selected_dish)
      sel_id <- values$dishes |> filter(dish_id == input$selected_dish) |> pull(dish_id)
      unit <- recently_bought |> filter(name == input$ingredient_name) |> pull(unit)

      values$dish_ingredients <- bind_rows(values$dish_ingredients, tibble(
        dish_id = sel_id,
        product = input$ingredient_name,
        amount = input$ingredient_amount,
        unit = unit
      ))

      saveRDS(values$dish_ingredients, dish_ingredients_file())
    })

    # show unit info
    output$ingredient_unit_text <- renderUI({
      req(input$ingredient_name)
      unit <- recently_bought |> filter(name == input$ingredient_name) |> pull(unit)
      p(HTML(paste("<small>Eenheid:", unit, "</small>")))
    })

    # render ingredient table with remove buttons
    output$dish_ingredients_table <- renderUI({
      req(input$selected_dish)
      sel_id <- values$dishes |> filter(dish_id == input$selected_dish) |> pull(dish_id)
      if (length(sel_id) != 1) return(p(""))

      df <- values$dish_ingredients |> filter(dish_id == sel_id)
      if (nrow(df) == 0) return(p("Nog geen ingredi\u00EBnten toegevoegd."))

      tagList(
        lapply(seq_len(nrow(df)), function(i) {
          row <- df[i, ]
          remove_id <- paste0("remove_ingr_", i)

          fluidRow(
            class = "row products-list-row",
            column(2, div(class = "products-list-img", img(src = get_product_image(row$product), width = "100%"))),
            column(9, div(class = "products-list-p", p(HTML(paste0("<strong>", row$amount, "x</strong> ", get_product_name_unit(row$product)))))),
            column(1, actionButton(remove_id, "", icon = icon("trash"), class = "btn-danger btn-sm", style = "margin-top: -8px;"))
          )
        })
      )
    })

    # remove ingredient listener
    observe({
      if (is.null(input$selected_dish) || input$selected_dish == 0 || input$selected_dish == "") return()
      sel_id <- values$dishes |> filter(dish_id == input$selected_dish) |> pull(dish_id)
      if (length(sel_id) != 1) return()

      df <- values$dish_ingredients |> filter(dish_id == sel_id)

      lapply(seq_len(nrow(df)), function(i) {
        observeEvent(input[[paste0("remove_ingr_", i)]], {
          values$dish_ingredients <- values$dish_ingredients[-i, ]
          saveRDS(values$dish_ingredients, dish_ingredients_file())
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

preptime_display <- function(x) {
  switch(as.character(x),
         "20" = "0-20",
         "40" = "20-40",
         "60" = "40-60",
         "120" = "60-120",
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
