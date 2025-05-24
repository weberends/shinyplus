#' Launch PLUS Weekly Grocery App
#' @export
open_app2 <- function() {
  library(shiny)
  library(bslib)
  library(DT)
  library(tibble)
  library(dplyr)
  library(stringr)

  data_dir <- system.file("plus_data", package = "shinyplus")
  dishes_file <- file.path(data_dir, "dishes.rds")
  ingredients_file <- file.path(data_dir, "ingredients.rds")
  weekplan_file <- file.path(data_dir, "weekplan.rds")
  weekly_basics_file <- file.path(data_dir, "weekly_basics.rds")

  if (!dir.exists(data_dir)) dir.create(data_dir)

  weekly_basics <- if (file.exists(weekly_basics_file)) readRDS(weekly_basics_file) else character()

  weekdays_list <- c("Maandag", "Dinsdag", "Woensdag", "Donderdag", "Vrijdag", "Zaterdag", "Zondag")

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
          background-size: cover;
          background-position: center;
          opacity: 0.05;
          z-index: -1;
        }
      "))
    ),
    tags$style(".navbar { background: none; }"),
    tags$style(HTML("
      .card, .well {
        background: rgba(255, 255, 255, 0.7);
        color: RGB(var(--bs-emphasis-color-rgb, 0, 0, 0));
        border-radius: 10px;
      }

      .row.fixed-product-row {
        height: 50px;
        display: flex;
        align-items: center;
        margin-bottom: 5px;
      }

      .fixed-products-img img {
        max-height: 40px;
        max-width: 100%;
        object-fit: contain;
      }

      .fixed-products-p p {
        margin: 0;
        font-size: 0.8rem;
        display: flex;
        align-items: center;
        height: 100%;
      }

      .fixed-products-qty {
        padding-top: 10px;
      }

      .fixed-products-qty .form-control {
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
                          card(
                            h3("1. Weekmenu"), # Step 1 ----
                            lapply(weekdays_list, function(day) {
                              selectInput(paste0("dish_day_", day), label = day, choices = NULL, width = "100%")
                            }),
                            actionButton("save_weekplan", "Weekmenu opslaan", icon = icon("save")),
                            actionButton("add_weekplan_products_to_basket", "Toevoegen aan mandje", icon = icon("cart-plus")),
                          ),
                   ),
                   column(3,
                          card(
                            h3("2. Vaste boodschappen"), # Step 2 ----

                            h5("Selecteer uit je vaste producten:"),
                            uiOutput("fixed_items_ui"),
                            actionButton("add_fixed_to_basket", "Toevoegen aan mandje", icon = icon("cart-plus")),
                            actionButton("fixed_to_zero", "Alles op nul zetten", icon = icon("rotate-left")),
                            hr(),
                            h5("Beheer vaste producten:"),
                            selectizeInput("add_fixed_product", NULL,
                                           choices = recently_bought$name |> stats::setNames(get_product_name_unit(recently_bought$name)),
                                           options = list(placeholder = 'Type om te zoeken...',
                                                          onInitialize = I('function() { this.setValue(""); }')),
                                           width = "100%"),
                            actionButton("add_fixed_product_button", "Toevoegen aan vaste producten", icon = icon("plus")),
                            actionButton("remove_fixed_product_button", "Verwijderen aan vaste producten", icon = icon("trash"))
                          )
                   ),
                   column(3,
                          card(
                            h3("3. Extra artikelen"), # Step 3 ----
                            h5("Kies extra artikelen:"),
                            uiOutput("extra_inputs_ui"),
                            actionButton("add_all_extras_to_basket", "Toevoegen aan mandje", icon = icon("cart-plus")),
                            hr(),
                            h5("Reeds toegevoegde extra artikelen:"),
                            uiOutput("extra_items_list")
                          )
                   ),
                   column(4,
                          card(
                            h3("4. Mandje"), # Step 4 ----
                            DTOutput("basket_overview_table"),
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
                          h5("Ingrediënten bewerken"),
                          selectizeInput("ingredient_name_edit", "Ingredi\u00EBnt",
                                         choices = recently_bought$name,
                                         options = list(placeholder = 'Type om te zoeken...',
                                                        onInitialize = I('function() { this.setValue(""); }'))),
                          uiOutput("ingredient_unit_text"),
                          numericInput("ingredient_amount_edit", "Aantal", 1),
                          actionButton("add_ingredient_edit", "Toevoegen en opslaan"),

                          tags$hr(),
                          DTOutput("ingredients_table_edit")
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
      dishes = if (file.exists(dishes_file)) readRDS(dishes_file) else tibble(dish_id = numeric(), name = character(), people = numeric(), days = character()),
      ingredients = if (file.exists(ingredients_file)) readRDS(ingredients_file) else tibble(dish_id = numeric(), product = character(), amount = numeric(), unit = character()),
      weekplan = if (file.exists(weekplan_file)) readRDS(weekplan_file) else tibble(day = character(), dish = character()),
      weekly_basics = if (file.exists(weekly_basics_file)) readRDS(weekly_basics_file) else character(),
      fixed_items = character(),
      extra_items = character(),
      logged_in = FALSE
    )
    extra_input_count <- reactiveVal(1)
    extra_inputs <- reactiveValues(data = list(), expanded = list())


    # Login ----
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
      dish_names <- values$dishes$name
      for (day in weekdays_list) {
        updateSelectInput(session, paste0("dish_day_", day),
                          choices = c("", dish_names),
                          selected = values$weekplan |> filter(day == !!day) |> pull(dish))
      }
    })

    observeEvent(input$save_weekplan, {
      values$weekplan <- bind_rows(lapply(weekdays_list, function(day) {
        dish <- input[[paste0("dish_day_", day)]]
        tibble(day = day, dish = ifelse(is.null(dish) || dish == "", NA_character_, dish))
      }))
      saveRDS(values$weekplan, weekplan_file)
    })

    output$fixed_items_ui <- renderUI({
      if (length(values$weekly_basics) == 0) return(p("Nog geen vaste producten."))

      values$weekly_basics <- sort(values$weekly_basics)

      tagList(
        lapply(values$weekly_basics, function(prod) {
          fluidRow(
            class = "row fixed-product-row",
            column(2, div(class = "fixed-products-img", height = "100%", img(src = get_product_image(prod), width = "100%"))),
            column(7, div(class = "fixed-products-p", height = "100%", p(get_product_name_unit(prod)))),
            column(3, div(class = "fixed-products-qty", height = "100%", numericInput(paste0("qty_fixed_", make.names(prod)), NULL, value = 0, min = 0, step = 1, width = "100%")))
          )
        })
      )
    })

    ## Step 2 ----

    # Manage weekly_basics (add/remove)
    observeEvent(input$add_fixed_product_button, {
      prod <- input$add_fixed_product
      if (!is.null(prod) && !(prod %in% values$weekly_basics)) {
        values$weekly_basics <- unique(c(values$weekly_basics, prod))
        saveRDS(values$weekly_basics, weekly_basics_file)
      }
    })

    observeEvent(input$remove_fixed_product_button, {
      prod <- input$add_fixed_product
      values$weekly_basics <- setdiff(values$weekly_basics, prod)
      saveRDS(values$weekly_basics, weekly_basics_file)
    })

    # Add selected fixed items + quantities to planning basket
    observeEvent(input$add_fixed_to_basket, {
      qtys <- lapply(values$weekly_basics, function(prod) {
        qty <- input[[paste0("qty_fixed_", make.names(prod))]]
        if (is.null(qty) || qty <= 0) return("")
        rep(prod, qty)
      })

      qtys <- unlist(qtys)
      qtys <- qtys[qtys != ""]
      if (length(qtys) > 1) {
        values$fixed_items <- c(values$fixed_items, unlist(qtys))
      }
    })
    observeEvent(input$fixed_to_zero, {
      for (prod in values$weekly_basics) {
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
      items <- unlist(lapply(extra_inputs$data, function(x) {
        if (!is.null(x$product) && nzchar(x$product) && x$qty > 0) {
          rep(x$product, x$qty)
        }
      }))

      values$extra_items <- c(values$extra_items, items)

      # Reset
      extra_input_count(1)
      extra_inputs$data <- list()
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

    # Mandje overview table
    output$basket_overview_table <- renderDT({
      selected_dishes <- values$weekplan$dish[!is.na(values$weekplan$dish)]
      ingredients <- values$ingredients |>
        inner_join(values$dishes |> filter(name %in% selected_dishes), by = "dish_id") |>
        pull(product)

      items <- tibble(Artikel = c(ingredients, values$fixed_items, values$extra_items)) |>
        count(Artikel, name = "Aantal")

      datatable(items, options = list(dom = 't'))
    })

    # Send basket to cart
    observeEvent(input$send_basket_to_cart, {
      if (!values$logged_in) {
        showNotification("Log eerst in om te verzenden naar winkelwagen.", type = "error")
        return()
      }

      basket_items <- output$basket_overview_table$data()
      for (i in seq_len(NROW(basket_items))) {
        product <- unlist(basket_items[i, "Artikel"])
        quantity <- unlist(basket_items[i, "Aantal"])
        url <- tryCatch(get_product_url(product), error = function(e) NULL)
        if (!is.null(url)) {
          plus_add_product(product_url = url, quantity = quantity, credentials = values$credentials, info = FALSE)
        }
      }
      showNotification("Mandje in PLUS Winkelwagen geplaatst.", type = "message")
    })

    observeEvent(input$clear_basket, {
      values$fixed_items <- character(0)
      values$extra_items <- character(0)
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
      for (d in c("Alle", "Ma", "Di", "Wo", "Do", "Vr", "Za", "Zo", "Vr-Za")) {
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
      values$ingredients <- values$ingredients |> filter(dish_id != dish_current)

      saveRDS(values$dishes, dishes_file)
      saveRDS(values$ingredients, ingredients_file)

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
      day_keys <- c("Ma", "Di", "Wo", "Do", "Vr", "Za", "Zo")
      selected_days <- day_keys[vapply(day_keys, function(d) input[[paste0("day_", d)]], logical(1))]
      if (input$day_Alle) selected_days <- day_keys
      if (input$`day_Vr-Za`) selected_days <- unique(c(selected_days, "Vr", "Za"))

      values$dishes <- values$dishes |>
        mutate(
          name = if_else(name == input$selected_dish, input$dish_name_edit, name),
          people = if_else(name == input$selected_dish, input$dish_people_edit, people),
          days = if_else(name == input$selected_dish, paste(selected_days, collapse = ","), days)
        )
      saveRDS(values$dishes, dishes_file)
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
      values$ingredients <- bind_rows(values$ingredients, tibble(
        dish_id = selected_id,
        product = input$ingredient_name_edit,
        amount = input$ingredient_amount_edit,
        unit = unit[1]
      ))
      saveRDS(values$ingredients, ingredients_file)
    })

    output$ingredients_table_edit <- renderDT({
      req(input$selected_dish)
      selected_id <- values$dishes |> filter(name == input$selected_dish) |> pull(dish_id)
      values$ingredients |>
        filter(dish_id == selected_id) |>
        select("Ingredi\u00EBnt" = product, Aantal = amount, Eenheid = unit) |>
        datatable(options = list(dom = 't'))
    })




    observeEvent(input$send_to_cart, {
      if (!values$logged_in) return()
      selected_dishes <- values$weekplan$dish[!is.na(values$weekplan$dish)]
      ingredients_to_add <- values$ingredients |>
        inner_join(values$dishes |> filter(name %in% selected_dishes), by = "dish_id") |>
        pull(product)

      all_items <- unique(c(ingredients_to_add, input$fixed_selection, values$extra_items))

      for (product_name in all_items) {
        product_url <- get_product_url(product_name)
        plus_add_product(product_url = product_url, quantity = 1, credentials = values$credentials, info = FALSE)
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
          paste0("€ ", format(round(total_price, 2), nsmall = 2, big.mark = ".", decimal.mark = ","))
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
