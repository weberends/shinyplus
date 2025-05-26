#' Launch PLUS Weekly Grocery App
#' @importFrom shiny fluidPage HTML div img p icon h5 h3 h4 hr br fluidRow wellPanel textInput a strong tagList reactiveVal passwordInput checkboxInput column navbarPage tabPanel selectInput selectizeInput numericInput actionButton uiOutput renderUI shinyApp observe observeEvent updateSelectInput updateNumericInput updateCheckboxInput updateTextInput updateSelectizeInput reactiveValues reactive req isolate tags modalDialog showModal removeModal modalButton showNotification conditionalPanel
#' @importFrom bslib bs_theme font_google card
#' @importFrom dplyr filter pull mutate select arrange inner_join bind_rows distinct if_else left_join count
#' @importFrom tibble tibble
#' @importFrom stringr str_detect
#' @importFrom DT datatable DTOutput renderDT
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
    ")),
    tags$style(HTML("
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
      tabPanel("Mandje", # Mandje ----
               fluidPage(
                 fluidRow(
                   column(2,
                          card(class = "basket-card-1",
                            h3("1. Weekmenu"), # Step 1 ----
                            lapply(weekdays_list, function(day) {
                              selectInput(paste0("dish_day_", day), label = day, choices = NULL, width = "100%")
                            }),
                            actionButton("save_weekplan", "Weekmenu opslaan", icon = icon("save")),
                            actionButton("add_weekplan_products_to_basket", "Toevoegen aan mandje", icon = icon("basket-shopping")),
                            p("Producten worden toegevoegd met apart label 'Weekmenu'."),
                          ),
                   ),
                   column(3,
                          card(class = "basket-card-2",
                            h3("2. Vaste boodschappen"), # Step 2 ----

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
                            h3("3. Extra artikelen"), # Step 3 ----
                            h5("Kies extra artikelen:"),
                            uiOutput("extra_inputs_ui"),
                            actionButton("add_all_extras_to_basket", "Toevoegen aan mandje", icon = icon("basket-shopping")),
                            hr(),
                            h5("Reeds toegevoegde extra artikelen:"),
                            uiOutput("extra_items_list")
                          )
                   ),
                   column(4,
                          card(class = "basket-card-4",
                            h3("4. Mandje"), # Step 4 ----
                            uiOutput("basket_overview_table"),
                            br(),
                            actionButton("send_basket_to_cart", "Mandje in PLUS Winkelwagen plaatsen", class = "btn-success"),
                            actionButton("clear_basket", "Mandje leegmaken", class = "btn-danger"),
                          )
                   )
                 )
               )
      ),

      tabPanel("PLUS Winkelwagen",
               fluidRow(
                 column(8,
                        uiOutput("online_cart_summary"),
                        br(), br(),
                        DTOutput("online_cart_table"),
                 )
               )
      ),
      tabPanel("Gerechten beheren",
               fluidRow(
                 column(4,
                        wellPanel(
                          h4("Gerecht selecteren"),
                          selectInput("selected_dish", NULL, choices = NULL),
                          actionButton("new_dish", "Nieuw gerecht", icon = icon("plus")),
                          actionButton("delete_dish", "Gerecht verwijderen", icon = icon("trash")),
                        )
                 ),
                 column(8,
                        wellPanel(
                          h4("Gerecht bewerken"),
                          textInput("dish_name_edit", "Naam gerecht"),
                          numericInput("dish_people_edit", "Personen", value = 2, min = 1),
                          div(
                            tags$label("Geschikt voor dagen:"),
                            div(
                              style = "display: flex; gap: 6px;",
                              lapply(c("Alle", "Ma", "Di", "Wo", "Do", "Vr", "Za", "Zo", "Vr-Za"), function(day) {
                                div(
                                  style = "display: flex; align-items: center;",
                                  checkboxInput(inputId = paste0("day_", day), label = day, value = FALSE, width = "40px")
                                )
                              })
                            )
                          ),
                          actionButton("save_dish", "Gerecht opslaan"),

                          tags$hr(),
                          h5("Ingredi\u00EBnten bewerken"),
                          selectizeInput("ingredient_name_edit", "Ingredi\u00EBnt",
                                         choices = recently_bought$name,
                                         options = list(placeholder = 'Type om te zoeken...',
                                                        onInitialize = I('function() { this.setValue(""); }'))),
                          uiOutput("ingredient_unit_text"),
                          numericInput("ingredient_amount_edit", "Aantal", 1),
                          actionButton("add_ingredient_edit", "Toevoegen en opslaan"),

                          tags$hr(),
                          DTOutput("dish_ingredients_table_edit")
                        )
                 )
               )
      ),
      tabPanel(title = uiOutput("account_tab_title"), value = "account",
               uiOutput("login_ui")
      )
    )
  )

  server <- function(input, output, session) {
    values <- reactiveValues(
      dishes = tibble(dish_id = numeric(), name = character(), people = numeric(), days = character()),
      dish_ingredients = tibble(dish_id = numeric(), product = character(), amount = numeric(), unit = character()),
      weekplan = tibble(day = character(), dish = character()),
      fixed_products = character(),
      fixed_items = character(),
      extra_items = character(),
      basket = tibble(product = character(), quantity = integer(), source = character()),
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


    # Login ----

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


    # TAB 1: Basket ----

    ## Step 1 ----
    # Populate weekmenu dish choices
    observe({
      for (day in weekdays_list) {
        dishes <- values$dishes |>
          filter(grepl(day, days))
        if (nrow(dishes) == 0) next
        updateSelectInput(session, paste0("dish_day_", day),
                          choices = c("", dishes$name) |> stats::setNames(c("", paste0(dishes$name, " (", dishes$people, "p)"))),
                          selected = NULL)
      }
    })

    observeEvent(input$save_weekplan, {
      values$weekplan <- bind_rows(lapply(weekdays_list, function(day) {
        dish <- input[[paste0("dish_day_", day)]]
        tibble(day = day, dish = ifelse(is.null(dish) || dish == "", NA_character_, dish))
      }))
      saveRDS(values$weekplan, weekplan_file())
    })

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


    # TAB 2: PLUS Cart ----

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

    observeEvent(input$refresh_online_cart, {
      req(values$logged_in)
      values$online_cart <- plus_current_cart(credentials = values$credentials, info = FALSE)
    })


    # TAB 3: Manage dishes ----

    observe({
      updateSelectInput(session, "selected_dish", choices = values$dishes$name)
    })

    observeEvent(input$new_dish, {
      updateTextInput(session, "dish_name_edit", value = "")
      updateNumericInput(session, "dish_people_edit", value = 2)

      # Uncheck all weekday toggles
      for (d in c("Alle", weekdays_short, "Vr-Za")) {
        updateCheckboxInput(session, paste0("day_", d), value = FALSE)
      }
      updateSelectInput(session, "selected_dish", selected = "")
    })

    observeEvent(input$delete_dish, {
      req(input$selected_dish)

      showModal(modalDialog(
        title = "Weet je het zeker?",
        paste("Verwijder gerecht:", input$selected_dish),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Annuleren"),
          actionButton("confirm_delete", "Verwijderen", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_delete, {
      removeModal()
      dish_current <- values$dishes |> filter(name == input$selected_dish) |> pull(dish_id)

      values$dishes <- values$dishes |> filter(name != input$selected_dish)
      values$dish_ingredients <- values$dish_ingredients |> filter(dish_id != dish_current)

      saveRDS(values$dishes, dishes_file())
      saveRDS(values$dish_ingredients, dish_ingredients_file())

      updateSelectInput(session, "selected_dish", choices = values$dishes$name, selected = "")
      updateTextInput(session, "dish_name_edit", value = "")
      updateNumericInput(session, "dish_people_edit", value = 2)
    })

    observeEvent(input$selected_dish, {
      selected <- values$dishes |> filter(name == input$selected_dish)
      if (nrow(selected) == 1) {
        updateTextInput(session, "dish_name_edit", value = selected$name)
        updateNumericInput(session, "dish_people_edit", value = selected$people)
      }
    })

    observeEvent(input$save_dish, {
      selected_days <- weekdays_short[vapply(weekdays_short, function(d) input[[paste0("day_", d)]], logical(1))]
      if (input$day_Alle) selected_days <- weekdays_short
      if (input$`day_Vr-Za`) selected_days <- unique(c(selected_days, "Vr", "Za"))
      selected_days <- weekdays_list[match(selected_days, weekdays_short)]

      values$dishes <- values$dishes |>
        mutate(
          name = if_else(name == input$selected_dish, input$dish_name_edit, name),
          people = if_else(name == input$selected_dish, input$dish_people_edit, people),
          days = if_else(name == input$selected_dish, paste(selected_days, collapse = ","), days)
        )
      saveRDS(values$dishes, dishes_file())
      updateSelectInput(session, "selected_dish", choices = values$dishes$name, selected = input$dish_name_edit)
    })

    output$ingredient_unit_text <- renderUI({
      req(input$ingredient_name_edit)
      unit <- recently_bought |>
        filter(name == input$ingredient_name_edit) |>
        pull(unit) |>
        unique()
      p(HTML(paste("<small>Eenheid:", unit[1], "</small>")))
    })

    observeEvent(input$add_ingredient_edit, {
      selected_id <- values$dishes |> filter(name == input$selected_dish) |> pull(dish_id)
      unit <- recently_bought |> filter(name == input$ingredient_name_edit) |> pull(unit)
      values$dish_ingredients <- bind_rows(values$dish_ingredients, tibble(
        dish_id = selected_id,
        product = input$ingredient_name_edit,
        amount = input$ingredient_amount_edit,
        unit = unit[1]
      ))
      saveRDS(values$dish_ingredients, dish_ingredients_file())
    })

    output$dish_ingredients_table_edit <- renderDT({
      req(input$selected_dish)
      selected_id <- values$dishes |> filter(name == input$selected_dish) |> pull(dish_id)
      values$dish_ingredients |>
        filter(dish_id == selected_id) |>
        select("Ingredi\u00EBnt" = product, Aantal = amount, Eenheid = unit) |>
        datatable(options = list(dom = 't'))
    })

    observeEvent(input$send_to_cart, {
      if (!values$logged_in) return()
      selected_dishes <- values$weekplan$dish[!is.na(values$weekplan$dish)]
      dish_ingredients_to_add <- values$dish_ingredients |>
        inner_join(values$dishes |> filter(name %in% selected_dishes), by = "dish_id") |>
        pull(product)

      all_items <- unique(c(dish_ingredients_to_add, input$fixed_selection, values$extra_items))

      for (product_name in all_items) {
        product_url <- get_product_url(product_name)
        plus_add_products(product_url, quantity = 1, credentials = values$credentials, info = FALSE)
      }
    })

    output$cart_table_ui <- renderUI({
      req(values$logged_in)
      cart <- plus_current_cart(credentials = values$credentials, info = FALSE)

      output$cart_table <- renderDT({
        cart |>
          select(Artikel = product, Prijs = price, Aantal = quantity, Totaal = price_total) |>
          arrange(Artikel) |>
          datatable(options = list(dom = 't'))
      })

      DTOutput("cart_table")
    })

    output$cart_summary <- renderDT({
      req(values$logged_in)
      cart <- plus_current_cart(credentials = values$credentials, info = FALSE)

      total_items <- sum(cart$quantity, na.rm = TRUE)
      unique_items <- nrow(cart)
      total_price <- sum(cart$price_total, na.rm = TRUE)

      summary_df <- tibble(
        Samenvatting = c("Aantal artikelen", "Uniek", "Totale prijs"),
        Waarde = c(
          formatC(total_items, big.mark = ".", decimal.mark = ",", format = "d"),
          formatC(unique_items, big.mark = ".", decimal.mark = ",", format = "d"),
          paste0("\u20ac ", format(round(total_price, 2), nsmall = 2, big.mark = ".", decimal.mark = ","))
        )
      )

      datatable(summary_df, options = list(dom = 't'), rownames = FALSE)
    })

    observeEvent(input$checkout, {
      session$sendCustomMessage("openCheckout", "https://www.plus.nl/checkout")
    })

  }

  shinyApp(ui, server)
}
