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
    div(id = "background-image"),
    theme = bs_theme(version = 5, base_font = font_google("Open Sans")),
    navbarPage(
      title = div(
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/9/92/PLUS_supermarket_logo.svg", height = "40px", style = "margin-right: 10px;"),
        tags$span("ShinyPLUS", style = "font-weight: bold; font-size: 1.2rem; vertical-align: middle;")
      ),
      tabPanel("Boodschappen doen",
               sidebarLayout(
                 sidebarPanel(
                   width = 5,
                   h4("Stap 1: Gerechten"),
                   uiOutput("weekplan_ui"),
                   actionButton("save_weekplan", "Weekmenu opslaan"),

                   hr(),
                   h4("Stap 2: Vaste boodschappen"),
                   uiOutput("fixed_selection_ui"),
                   actionButton("add_weekly", "Toevoegen aan mandje", icon = icon("basket-shopping")),
                   br(),
                   br(),
                   selectizeInput("add_to_weekly", "Toevoegen aan vaste lijst:",
                                  choices = recently_bought$name |> stats::setNames(paste0(recently_bought$name, " (", recently_bought$unit, ")")),
                                  options = list(placeholder = 'Type om te zoeken...',
                                                 onInitialize = I('function() { this.setValue(""); }'))),
                   actionButton("add_weekly_product", "Toevoegen", icon = icon("plus")),
                   actionButton("remove_weekly_product", "Verwijderen", icon = icon("trash")),

                   hr(),
                   h4("Stap 3: Extra artikelen"),
                   selectizeInput("add_to_weekly", "Extra artikel toevoegen:",
                                  choices = recently_bought$name |> stats::setNames(paste0(recently_bought$name, " (", recently_bought$unit, ")")),
                                  options = list(placeholder = 'Type om te zoeken...',
                                                 onInitialize = I('function() { this.setValue(""); }'))),
                   actionButton("add_extra", "Toevoegen aan mandje", icon = icon("basket-shopping")),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br()
                 ),
                 mainPanel(
                   width = 7,
                   h3("Mandje"),
                   DTOutput("weekplan_table"),
                   hr(),
                   uiOutput("basket_section")
                 )
               )
      ),
      tabPanel("PLUS Winkelwagen",
               fluidRow(
                 column(8,
                        uiOutput("online_basket_summary"),
                        br(), br(),
                        DTOutput("online_basket_table"),
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
        values$online_basket <- plus_current_basket(credentials = values$credentials, info = FALSE)
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


    # Grocery Shopping ----
    output$fixed_selection_ui <- renderUI({
      if (length(values$weekly_basics) == 0) return(p("Geen vaste artikelen."))

      tagList(
        lapply(values$weekly_basics, function(product) {
          fluidRow(
            column(2,
                   numericInput(
                     inputId = paste0("fixed_qty_", make.names(product)),
                     label = NULL,
                     value = 0,
                     min = 0,
                     width = "80px"
                   )
            ),
            column(8,
                   tags$label(get_product_name_unit(product), `for` = paste0("fixed_qty_", make.names(product)))
            )
          )
        })
      )
    })

    observeEvent(input$add_weekly_product, {
      prod <- input$add_to_weekly
      if (!is.null(prod) && !(prod %in% values$weekly_basics)) {
        values$weekly_basics <- unique(c(values$weekly_basics, prod))
        saveRDS(values$weekly_basics, weekly_basics_file)
      }
    })

    observeEvent(input$remove_weekly_product, {
      to_remove <- input$fixed_selection
      values$weekly_basics <- setdiff(values$weekly_basics, to_remove)
      updateCheckboxGroupInput(session, "fixed_selection", choices = values$weekly_basics)
      saveRDS(values$weekly_basics, weekly_basics_file)
    })

    output$weekplan_ui <- renderUI({
      lapply(weekdays_list, function(day) {
        dish_choices <- values$dishes |>
          filter(is.na(days) | str_detect(days, day)) |>
          pull(name)
        selectInput(paste0("dish_", day), label = day, choices = c("", dish_choices))
      })
    })

    observeEvent(input$save_weekplan, {
      plan <- lapply(weekdays_list, function(day) {
        dish <- input[[paste0("dish_", day)]]
        tibble(day = day, dish = ifelse(is.null(dish) || dish == "", NA_character_, dish))
      }) |> bind_rows()
      values$weekplan <- plan
      saveRDS(plan, weekplan_file)
    })

    output$weekplan_table <- renderDT({
      req(values$weekplan)
      datatable(values$weekplan, colnames = c("Dag", "Gerecht"), options = list(dom = 't'))
    })

    observeEvent(input$add_extra, {
      if (!is.null(input$extra_selection)) {
        values$extra_items <- unique(c(values$extra_items, input$extra_selection))
      }
    })

    output$basket_section <- renderUI({
      if (values$logged_in) {
        tagList(
          br(),
          actionButton("send_to_basket", "Plaats in winkelwagen", class = "btn-info"),
          br(),
          uiOutput("basket_table_ui"),
          uiOutput("basket_summary"),
          br(),
          actionButton("checkout", "Afrekenen bij PLUS", class = "btn-success")
        )
      } else {
        p("Om artikelen aan de winkelwagen toe te voegen, log eerst in via het menu 'Inloggen'.", class = "text-danger")
      }
    })


    # PLUS basket ----

    output$online_basket_summary <- renderUI({
      if (!values$logged_in) {
        return(p("Om artikelen in de PLUS Winkelwagen te zien, log eerst in via het menu 'Inloggen'.", class = "text-danger"))
      }
      req(values$logged_in)
      req(values$online_basket)

      total_items <- sum(values$online_basket$quantity, na.rm = TRUE)
      unique_items <- nrow(values$online_basket)
      total_price <- sum(values$online_basket$price_total, na.rm = TRUE)

      tagList(
        h3("Inhoud PLUS Winkelwagen (online)"),
        h4(HTML(paste0("<strong>Totale prijs:</strong> ", as_euro(sum(total_price, na.rm = TRUE))))),
        p(HTML(paste0("<strong>Totaal artikelen:</strong> ", total_items, " (uniek: ", unique_items, ")"))),
        actionButton("checkout", "Afrekenen bij PLUS", class = "btn-success", icon = icon("right-from-bracket")),
        actionButton("refresh_online_basket", "Vernieuwen", icon = icon("refresh")),
      )
    })
    output$online_basket_table <- renderDT({
      req(values$logged_in)
      req(values$online_basket)

      basket <- values$online_basket |>
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

      display_basket <- basket |>
        select(" " = img, Artikel = product, Prijs = price, Aantal = quantity, Totaal = price_total, Artikelinfo = url)

      datatable(
        display_basket,
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

    observeEvent(input$refresh_online_basket, {
      req(values$logged_in)
      values$online_basket <- plus_current_basket(credentials = values$credentials, info = FALSE)
    })


    # Dishes ----

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




    observeEvent(input$send_to_basket, {
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

    output$basket_table_ui <- renderUI({
      req(values$logged_in)
      basket <- plus_current_basket(credentials = values$credentials, info = FALSE)

      output$basket_table <- renderDT({
        basket |>
          select(Artikel = product, Prijs = price, Aantal = quantity, Totaal = price_total) |>
          arrange(Artikel) |>
          datatable(options = list(dom = 't'))
      })

      DTOutput("basket_table")
    })

    output$basket_summary <- renderDT({
      req(values$logged_in)
      basket <- plus_current_basket(credentials = values$credentials, info = FALSE)

      total_items <- sum(basket$quantity, na.rm = TRUE)
      unique_items <- nrow(basket)
      total_price <- sum(basket$price_total, na.rm = TRUE)

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
