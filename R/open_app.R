#' Open Shiny App
#'
#' This opens the Shiny app.
#' @importFrom shiny shinyApp tags textInput selectInput actionButton checkboxGroupInput numericInput updateTextInput updateCheckboxGroupInput updateSelectInput req reactiveValues observeEvent renderUI tabPanel uiOutput fluidRow column h3 h4 navlistPanel
#' @importFrom bslib page_navbar nav_panel bs_theme font_google
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom glue glue
#' @importFrom rvest read_html html_element html_text
#' @importFrom tibble tibble
#' @importFrom dplyr filter select pull bind_rows mutate arrange inner_join group_by summarise %>%
#' @importFrom stringr str_detect
#' @importFrom purrr map map_chr
#' @rdname open_app
#' @export
open_app <- function() {

  # Dagcategorieen voor weekmenu restricties
  dag_categorieen <- list(
    Maandag = c("wraps"),
    Dinsdag = c("aardappel"),
    Woensdag = c("bami", "pita", "gyros"),
    Donderdag = c("pasta"),
    Vrijdag = c("pizza", "patat"),
    Zaterdag = NULL,
    Zondag = NULL
  )

  # UI STRUCTUUR
  ui <- page_navbar(
    title = tags$div(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/9/92/PLUS_supermarket_logo.svg", height = "40px", style = "margin-right: 10px;"),
      tags$span("PLUS Weekmenu", style = "font-weight: bold; font-size: 1.2rem; vertical-align: middle;")
    ),
    theme = bs_theme(
      version = 5,
      primary = "#84BD00",
      secondary = "#FFD100",
      base_font = font_google("Open Sans"),
      bg = "#ffffff",
      fg = "#222222"
    ),
    fillable = TRUE,
    padding = 3,

    nav_panel("Weekmenu Flow",
              navlistPanel(
                id = "sidebar_steps",
                widths = c(2, 10),
                tabPanel("1. Kies weekmenu", value = "stap1",
                         h3("Stap 1: Kies gerechten voor het weekmenu"),
                         uiOutput("weekmenu_ui"),
                         actionButton("opslaan_weekmenu", "Weekmenu opslaan"),
                         h4("Huidig opgeslagen weekmenu"),
                         DTOutput("weekmenu_tabel")
                ),
                tabPanel("2. Vaste boodschappen", value = "stap2",
                         h3("Stap 2: Vink vaste boodschappen aan"),
                         uiOutput("vaste_boodschappen_ui")
                ),
                tabPanel("3. Extra producten", value = "stap3",
                         h3("Stap 3: Voeg een PLUS-product toe via URL"),
                         tags$iframe(
                           src = "https://www.plus.nl",
                           width = "100%",
                           height = "400px",
                           frameborder = "0"
                         ),
                         textInput("plus_url", "Plak hier de URL van het PLUS product:", placeholder = "https://www.plus.nl/product/..."),
                         actionButton("haal_plus_product_op", "Product ophalen en toevoegen")
                ),
                tabPanel("4. Eindlijst", value = "stap4",
                         h3("Stap 4: Eindlijst met alle boodschappen"),
                         DTOutput("eindlijst_tabel")
                )
              )
    ),

    nav_panel("Gerechten beheren", value = "beheer_gerechten",
              navlistPanel(
                id = "sidebar_gerechten",
                widths = c(2, 10),
                tabPanel("Toevoegen gerecht",
                         fluidRow(
                           column(6,
                                  textInput("gerecht_naam", "Naam gerecht"),
                                  numericInput("personen", "Aantal personen", 2, min = 1),
                                  checkboxGroupInput("gerecht_dagen", "Toon op dagen:", choices = names(dag_categorieen)),
                                  actionButton("voeg_toe", "Gerecht toevoegen")
                           )
                         )
                ),
                tabPanel("Ingredienten toevoegen",
                         fluidRow(
                           column(6,
                                  selectInput("kies_gerecht", "Selecteer gerecht", choices = NULL),
                                  textInput("ingredient_naam", "Ingredient"),
                                  numericInput("hoeveelheid", "Hoeveelheid", value = 1),
                                  selectInput("eenheid", "Eenheid", choices = c("stuks", "gram", "kg", "liter", "ml")),
                                  actionButton("voeg_ingredient_toe", "Ingredient toevoegen")
                           ),
                           column(6,
                                  h4("Huidige gerechten"),
                                  DTOutput("gerechten_tabel"),
                                  h4("Ingredienten van geselecteerd gerecht"),
                                  DTOutput("ingredienten_tabel")
                           )
                         )
                )
              )
    ),

    nav_panel("Vaste boodschappen beheren", value = "beheer_vaste",
              navlistPanel(
                id = "sidebar_basis",
                widths = c(2, 10),
                tabPanel("Toevoegen vast product",
                         fluidRow(
                           column(6,
                                  textInput("nieuwe_basis_naam", "Naam vast product"),
                                  numericInput("nieuwe_basis_hoeveelheid", "Hoeveelheid", value = 1),
                                  selectInput("nieuwe_basis_eenheid", "Eenheid", choices = c("stuks", "gram", "kg", "liter", "ml")),
                                  actionButton("voeg_basis_toe", "Vast product toevoegen")
                           )
                         )
                ),
                tabPanel("Bestaande producten",
                         fluidRow(
                           column(12,
                                  h4("Huidige vaste boodschappen"),
                                  DTOutput("basisproducten_tabel"),
                                  actionButton("verwijder_basis", "Verwijder geselecteerde")
                           )
                         )
                )
              )
    )
  )

  server <- function(input, output, session) {

    data_dir <- "data"
    gerechten_file <- file.path(data_dir, "gerechten.rds")
    ingredienten_file <- file.path(data_dir, "ingredienten.rds")
    weekmenu_file <- file.path(data_dir, "weekmenu.rds")

    if (!dir.exists(data_dir)) dir.create(data_dir)

    values <- reactiveValues(
      gerechten = if (file.exists(gerechten_file)) readRDS(gerechten_file) else tibble(
        gerecht_id = numeric(), naam = character(), aantal_personen = numeric(), dagen = character()
      ),
      ingredienten = if (file.exists(ingredienten_file)) readRDS(ingredienten_file) else tibble(
        gerecht_id = numeric(), ingredient = character(), hoeveelheid = numeric(), eenheid = character()
      ),
      weekmenu = if (file.exists(weekmenu_file)) readRDS(weekmenu_file) else tibble(
        dag = character(), gerecht = character()
      ),
      basisproducten = tibble(
        ingredient = c(
          "Neutraal Bananen Fairtrade", "PLUS Hazelnootpasta", "PLUS Boeren bruin tijger heel",
          "PLUS Houdbare Halfvolle Melk", "Banderos Tortilla wraps volkoren", "PLUS Smeerkaas naturel 20+",
          "PLUS Speculoospasta", "PLUS Gepofte tarwe", "PLUS Sinas zero", "Appels",
          "PLUS Koffiecapsules Latte macchiato", "PLUS Honing ringetjes", "PLUS Limonadesiroop aardbei zero",
          "PLUS Extra aardbeienjam", "PLUS Pindakaas met stukjes pinda", "Theha Kokosbrood",
          "Melkan Vanilleyoghurt", "Melkunie Griesmeelpap", "Stroopmeesters Stroopwafels"
        ),
        hoeveelheid = 1,
        eenheid = "stuks"
      ),
      plus_extra = tibble(ingredient = character(), hoeveelheid = numeric(), eenheid = character())
    )

    dagen <- names(dag_categorieen)

    output$weekmenu_ui <- renderUI({
      req(values$gerechten)
      lapply(dagen, function(dag) {
        keuzes <- values$gerechten %>%
          filter(is.na(dagen) | str_detect(dagen, dag)) %>%
          pull(naam)
        selectInput(paste0("gerecht_", dag), label = dag, choices = c("", keuzes))
      })
    })

    observeEvent(input$voeg_toe, {
      nieuw_id <- ifelse(nrow(values$gerechten) == 0, 1, max(values$gerechten$gerecht_id) + 1)
      values$gerechten <- bind_rows(values$gerechten, tibble(
        gerecht_id = nieuw_id,
        naam = input$gerecht_naam,
        aantal_personen = input$personen,
        dagen = paste(input$gerecht_dagen, collapse = ",")
      ))
      saveRDS(values$gerechten, gerechten_file)
      updateTextInput(session, "gerecht_naam", value = "")
      updateCheckboxGroupInput(session, "gerecht_dagen", selected = character())
    })

    observeEvent(input$voeg_ingredient_toe, {
      req(input$kies_gerecht)
      values$ingredienten <- bind_rows(values$ingredienten, tibble(
        gerecht_id = as.numeric(input$kies_gerecht),
        ingredient = input$ingredient_naam,
        hoeveelheid = input$hoeveelheid,
        eenheid = input$eenheid
      ))
      saveRDS(values$ingredienten, ingredienten_file)
      updateTextInput(session, "ingredient_naam", value = "")
    })

    output$gerechten_tabel <- renderDT({
      values$gerechten %>% select(Naam = naam, Personen = aantal_personen, Dagen = dagen)
    })

    output$ingredienten_tabel <- renderDT({
      req(input$kies_gerecht)
      values$ingredienten %>% filter(gerecht_id == as.numeric(input$kies_gerecht))
    })

    observeEvent(input$voeg_basis_toe, {
      values$basisproducten <- bind_rows(values$basisproducten, tibble(
        ingredient = input$nieuwe_basis_naam,
        hoeveelheid = input$nieuwe_basis_hoeveelheid,
        eenheid = input$nieuwe_basis_eenheid
      ))
      updateTextInput(session, "nieuwe_basis_naam", value = "")
    })

    output$basisproducten_tabel <- renderDT({
      datatable(values$basisproducten, selection = "single")
    })

    observeEvent(input$verwijder_basis, {
      row <- input$basisproducten_tabel_rows_selected
      if (length(row) > 0) {
        values$basisproducten <- values$basisproducten[-row, ]
      }
    })

    output$vaste_boodschappen_ui <- renderUI({
      checkboxGroupInput("vaste_selectie", "Selecteer boodschappen:", choices = values$basisproducten$ingredient)
    })

    observeEvent(input$haal_plus_product_op, {
      req(input$plus_url)
      pagina <- tryCatch(read_html(input$plus_url), error = function(e) NULL)
      if (is.null(pagina)) return()
      naam <- pagina %>% html_element("h1") %>% html_text(trim = TRUE)
      if (!is.na(naam)) {
        values$plus_extra <- bind_rows(values$plus_extra, tibble(
          ingredient = naam, hoeveelheid = 1, eenheid = "stuks"
        ))
      }
    })

    observeEvent(input$opslaan_weekmenu, {
      geselecteerd <- lapply(dagen, function(dag) {
        gerecht <- input[[paste0("gerecht_", dag)]]
        if (!is.null(gerecht) && gerecht != "") {
          tibble(dag = dag, gerecht = gerecht)
        } else {
          tibble(dag = dag, gerecht = NA_character_)
        }
      }) %>% bind_rows()
      values$weekmenu <- geselecteerd
      saveRDS(values$weekmenu, weekmenu_file)
    })

    output$weekmenu_tabel <- renderDT({
      req(values$weekmenu)
      values$weekmenu %>% select(Dag = dag, Gerecht = gerecht)
    })

    output$eindlijst_tabel <- renderDT({
      geselecteerde_gerechten <- values$weekmenu$gerecht[!is.na(values$weekmenu$gerecht)]
      geselecteerde_ingredienten <- values$ingredienten %>%
        inner_join(values$gerechten %>% filter(naam %in% geselecteerde_gerechten), by = "gerecht_id") %>%
        select(ingredient, hoeveelheid, eenheid)
      geselecteerde_basis <- values$basisproducten %>% filter(ingredient %in% input$vaste_selectie)
      eindlijst <- bind_rows(geselecteerde_ingredienten, geselecteerde_basis, values$plus_extra) %>%
        group_by(ingredient, eenheid) %>%
        summarise(hoeveelheid = sum(hoeveelheid), .groups = "drop") %>%
        arrange(ingredient)
      datatable(eindlijst, colnames = c("Product", "Eenheid", "Hoeveelheid"))
    })

  }
  # UI openen
  shinyApp(ui, server)
}
