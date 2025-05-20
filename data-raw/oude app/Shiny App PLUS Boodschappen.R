# app.R

library(shiny)
library(tidyverse)
library(DT)
# library(openfoodfacts)
library(httr)
library(jsonlite)
library(rvest)
library(glue)
library(dplyr)
library(stringr)
library(barcode)
library(bslib)

# 🎨 PLUS huisstijl
plus_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",           # achtergrond van pagina
  fg = "#222222",           # standaard tekst
  primary = "#84BD00",      # navigatiebalk + knoppen = PLUS groen
  secondary = "#FFD100",    # accentkleur = geel
  navbar_bg = "#84BD00",    # navigatiebalk achtergrond
  navbar_fg = "#ffffff",    # witte tekst in navbar
  base_font = font_google("Open Sans"),
  heading_font = font_google("Open Sans"),
  "border-radius" = "0.5rem",
  "btn-border-radius" = "0.4rem",
  "navbar-padding-y" = "1rem"
)





ui <- page_navbar(
  title = tags$div(
    tags$img(src = "PLUS_supermarket_logo.svg.png", height = "40px", style = "margin-right: 10px;"),
    tags$span("PLUS Weekmenu", style = "font-weight: bold; font-size: 1.2rem; vertical-align: middle;")
  ),
  theme = plus_theme,
  fillable = TRUE,
  padding = 3,
  
  nav_panel("1. Gerechten beheren",
            fluidRow(
              column(6,
                     textInput("gerecht_naam", "Naam gerecht"),
                     numericInput("personen", "Aantal personen", 2, min = 1),
                     actionButton("voeg_toe", "Gerecht toevoegen")
              ),
              column(6,
                     h4("Huidige gerechten"),
                     DTOutput("gerechten_tabel")
              )
            )
  ),
  
  nav_panel("2. Ingrediënten toevoegen",
            fluidRow(
              column(6,
                     selectInput("kies_gerecht", "Kies gerecht", choices = NULL),
                     textInput("ingredient_naam", "Ingrediënt"),
                     numericInput("hoeveelheid", "Hoeveelheid", 1),
                     textInput("eenheid", "Eenheid (bv. gram, stuks)"),
                     actionButton("voeg_ingredient_toe", "Ingrediënt toevoegen")
              ),
              column(6,
                     h4("Ingrediënten voor gekozen gerecht"),
                     DTOutput("ingredienten_tabel")
              )
            )
  ),
  
  nav_panel("3. Weekmenu & Lijst",
            sidebarLayout(
              sidebarPanel(
                checkboxGroupInput("weekmenu_selectie", "Kies gerechten voor deze week:",
                                   choices = NULL),
                actionButton("genereer_lijst", "Genereer boodschappenlijst")
              ),
              mainPanel(
                h4("Boodschappenlijst"),
                DTOutput("boodschappenlijst_output")
              )
            )
  ),
  
  nav_panel("4. Barcodes ophalen",
            fluidRow(
              column(6,
                     actionButton("haal_barcodes", "Zoek barcodes voor boodschappenlijst")
              ),
              column(6,
                     h4("Gevonden barcodes"),
                     DTOutput("producten_met_barcodes")
              )
            )
  ),
  
  nav_panel("5. Exporteren",
            fluidRow(
              column(12,
                     h4("Definitieve boodschappenlijst met barcodes"),
                     DTOutput("definitieve_lijst"),
                     downloadButton("download_csv", "Download als CSV")
              )
            )
  ),
  
  nav_panel("6. Scannen",
            fluidRow(
              column(12,
                     h4("Scanbare barcodes voor PLUS-app"),
                     DTOutput("barcode_tabel")
              )
            )
  )
)

server <- function(input, output, session) {
  
  data_dir <- "data"
  gerechten_file <- file.path(data_dir, "gerechten.rds")
  ingredienten_file <- file.path(data_dir, "ingredienten.rds")
  
  # Zorg dat map bestaat
  if (!dir.exists(data_dir)) dir.create(data_dir)
  
  # Laad data bij start of begin leeg
  values <- reactiveValues(
    gerechten = if (file.exists(gerechten_file)) readRDS(gerechten_file) else tibble(
      gerecht_id = numeric(),
      naam = character(),
      aantal_personen = numeric()
    ),
    ingredienten = if (file.exists(ingredienten_file)) readRDS(ingredienten_file) else tibble(
      gerecht_id = numeric(),
      ingrediënt = character(),
      hoeveelheid = numeric(),
      eenheid = character()
    )
  )
  
  
  # 1. Zoek eerst via Open Food Facts API
  zoek_openfoodfacts <- function(zoekterm) {
    res <- GET(
      url = "https://world.openfoodfacts.org/cgi/search.pl",
      query = list(
        search_terms = zoekterm,
        search_simple = 1,
        action = "process",
        json = 1,
        page_size = 1
      )
    )
    
    if (res$status_code != 200) return(NULL)
    
    data <- content(res, as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE)
    
    if (length(data$products) == 0 || is.null(data$products[[1]]$code)) return(NULL)
    
    product <- data$products[[1]]
    
    tibble(
      bron = "OpenFoodFacts",
      product = zoekterm,
      match_naam = product$product_name,
      merk = product$brands,
      barcode = product$code,
      link = paste0("https://world.openfoodfacts.org/product/", product$code)
    )
  }
  
  # 2. Als OF niet lukt, val terug op Plus.nl scraping
  zoek_barcode_plus <- function(product_naam) {
    Sys.sleep(1.5)  # vertraging om de site te ontzien
    
    zoek_url <- glue("https://www.plus.nl/zoeken?query={URLencode(product_naam)}")
    pagina <- tryCatch(read_html(zoek_url), error = function(e) return(NULL))
    if (is.null(pagina)) return(NULL)
    
    eerste_link <- pagina %>%
      html_element(".product-tile a") %>%
      html_attr("href")
    
    if (is.na(eerste_link) || is.null(eerste_link)) return(NULL)
    
    product_url <- glue("https://www.plus.nl{eerste_link}")
    product_pagina <- tryCatch(read_html(product_url), error = function(e) return(NULL))
    if (is.null(product_pagina)) return(NULL)
    
    barcode <- product_pagina %>%
      html_element("meta[property='og:gtin13']") %>%
      html_attr("content")
    
    tibble(
      bron = "Plus.nl",
      product = product_naam,
      match_naam = NA,
      merk = NA,
      barcode = barcode,
      link = product_url
    )
  }
  
  # 3. Slimme combi-functie
  zoek_product_info <- function(product_naam) {
    resultaat <- zoek_openfoodfacts(product_naam)
    if (is.null(resultaat)) {
      resultaat <- zoek_barcode_plus(product_naam)
    }
    if (is.null(resultaat)) {
      resultaat <- tibble(
        bron = "Geen resultaat",
        product = product_naam,
        match_naam = NA,
        merk = NA,
        barcode = NA,
        link = NA
      )
    }
    return(resultaat)
  }
  
  observeEvent(input$haal_barcodes, {
    req(input$weekmenu_selectie)
    
    geselecteerde_ingredienten <- values$ingredienten %>%
      filter(gerecht_id %in% as.numeric(input$weekmenu_selectie)) %>%
      group_by(ingrediënt) %>%
      summarise(.groups = "drop")
    
    alle_producten <- bind_rows(
      geselecteerde_ingredienten,
      basisproducten %>% select(ingrediënt)
    ) %>%
      distinct(ingrediënt) %>%
      pull(ingrediënt)
    
    # Zoek per product info via OF > Plus fallback
    withProgress(message = "Productinfo ophalen...", value = 0, {
      resultaten <- map_dfr(seq_along(alle_producten), function(i) {
        incProgress(1 / length(alle_producten), detail = alle_producten[i])
        zoek_product_info(alle_producten[i])
      })
    })
    
    values$producten_met_barcodes <- resultaten
    
    output$producten_met_barcodes <- renderDT({
      values$producten_met_barcodes %>%
        mutate(Link = if_else(!is.na(link), paste0("<a href='", link, "' target='_blank'>Bekijk</a>"), "")) %>%
        select(Bron = bron, Product = product, Match = match_naam, Merk = merk, Barcode = barcode, Link) %>%
        datatable(escape = FALSE, options = list(pageLength = 10))
    })
  })
  
  
  
  # Reactieve datasets
  values <- reactiveValues(
    gerechten = tibble(gerecht_id = numeric(), naam = character(), aantal_personen = numeric()),
    ingredienten = tibble(gerecht_id = numeric(), ingrediënt = character(), hoeveelheid = numeric(), eenheid = character())
  )
  
  # Voeg gerecht toe
  observeEvent(input$voeg_toe, {
    req(input$gerecht_naam)
    
    nieuw_id <- ifelse(nrow(values$gerechten) == 0, 1, max(values$gerechten$gerecht_id) + 1)
    nieuw_gerecht <- tibble(
      gerecht_id = nieuw_id,
      naam = input$gerecht_naam,
      aantal_personen = input$personen
    )
    
    values$gerechten <- bind_rows(values$gerechten, nieuw_gerecht)
    
    saveRDS(values$gerechten, gerechten_file)
    
    updateTextInput(session, "gerecht_naam", value = "")
    updateNumericInput(session, "personen", value = 2)
    
    updateSelectInput(session, "kies_gerecht",
                      choices = setNames(values$gerechten$gerecht_id, values$gerechten$naam)
    )
  })
  
  # Toon gerechten
  output$gerechten_tabel <- renderDT({
    values$gerechten %>% select(Naam = naam, Personen = aantal_personen)
  })
  
  # Voeg ingrediënt toe
  observeEvent(input$voeg_ingredient_toe, {
    req(input$kies_gerecht, input$ingredient_naam, input$hoeveelheid, input$eenheid)
    
    saveRDS(values$ingredienten, ingredienten_file)
    
    nieuw_ingrediënt <- tibble(
      gerecht_id = as.numeric(input$kies_gerecht),
      ingrediënt = input$ingredient_naam,
      hoeveelheid = input$hoeveelheid,
      eenheid = input$eenheid
    )
    
    values$ingredienten <- bind_rows(values$ingredienten, nieuw_ingrediënt)
    
    updateTextInput(session, "ingredient_naam", value = "")
    updateNumericInput(session, "hoeveelheid", value = 1)
    updateTextInput(session, "eenheid", value = "")
  })
  
  # Update weekmenu-selectie dropdown als gerechten veranderen
  observe({
    updateCheckboxGroupInput(session, "weekmenu_selectie",
                             choices = setNames(values$gerechten$gerecht_id, values$gerechten$naam)
    )
  })
  
  # Vaste producten (kun je later dynamisch maken)
  basisproducten <- tibble(
    ingrediënt = c(
      "Neutraal Bananen Fairtrade",
      "PLUS Hazelnootpasta",
      "PLUS Boeren bruin tijger heel",
      "PLUS Houdbare Halfvolle Melk",
      "Banderos Tortilla wraps volkoren",
      "PLUS Smeerkaas naturel 20+",
      "PLUS Speculoospasta",
      "PLUS Gepofte tarwe",
      "PLUS Sinas zero",
      "Appels",
      "PLUS Koffiecapsules Latte macchiato",
      "PLUS Honing ringetjes",
      "PLUS Limonadesiroop aardbei zero",
      "PLUS Extra aardbeienjam",
      "PLUS Pindakaas met stukjes pinda",
      "Theha Kokosbrood",
      "Melkan Vanilleyoghurt",
      "Melkunie Griesmeelpap",
      "Stroopmeesters Stroopwafels"
    ),
    hoeveelheid = 1,
    eenheid = "stuks"
  )
  
  # Genereer boodschappenlijst
  observeEvent(input$genereer_lijst, {
    req(input$weekmenu_selectie)
    
    geselecteerde_ingredienten <- values$ingredienten %>%
      filter(gerecht_id %in% as.numeric(input$weekmenu_selectie)) %>%
      group_by(ingrediënt, eenheid) %>%
      summarise(hoeveelheid = sum(hoeveelheid), .groups = "drop")
    
    boodschappenlijst <- bind_rows(
      geselecteerde_ingredienten,
      basisproducten
    ) %>%
      group_by(ingrediënt, eenheid) %>%
      summarise(totaal = sum(hoeveelheid), .groups = "drop") %>%
      arrange(ingrediënt)
    
    output$boodschappenlijst_output <- renderDT({
      boodschappenlijst %>%
        rename(Hoeveelheid = totaal, Eenheid = eenheid, Product = ingrediënt)
    })
  })
  
  
  
  # Toon ingrediënten voor geselecteerd gerecht
  output$ingredienten_tabel <- renderDT({
    req(input$kies_gerecht)
    
    values$ingredienten %>%
      filter(gerecht_id == as.numeric(input$kies_gerecht)) %>%
      select(Ingrediënt = ingrediënt, Hoeveelheid = hoeveelheid, Eenheid = eenheid)
  })
  
  
  # Maak definitieve lijst: ingrediënten + vaste producten met barcode
  make_definitieve_lijst <- reactive({
    req(values$producten_met_barcodes)
    req(input$weekmenu_selectie)
    
    geselecteerde_ingredienten <- values$ingredienten %>%
      filter(gerecht_id %in% as.numeric(input$weekmenu_selectie)) %>%
      group_by(ingrediënt, eenheid) %>%
      summarise(hoeveelheid = sum(hoeveelheid), .groups = "drop")
    
    alle_producten <- bind_rows(
      geselecteerde_ingredienten,
      basisproducten
    ) %>%
      group_by(ingrediënt, eenheid) %>%
      summarise(hoeveelheid = sum(hoeveelheid), .groups = "drop") %>%
      rename(product = ingrediënt)
    
    final <- alle_producten %>%
      left_join(values$producten_met_barcodes, by = "product") %>%
      select(
        Product = product,
        Hoeveelheid = hoeveelheid,
        Eenheid = eenheid,
        Barcode = barcode,
        Link = link
      )
    
    final
  })
  
  output$barcode_tabel <- renderDT({
    df <- make_definitieve_lijst() %>%
      filter(!is.na(Barcode)) %>%
      select(Product, Hoeveelheid, Eenheid, Barcode)
    
    datatable(df, escape = FALSE, options = list(pageLength = 15))
  })
  
  
  # output$barcode_tabel <- renderDT({
  #   df <- make_definitieve_lijst() %>%
  #     filter(!is.na(Barcode)) %>%
  #     mutate(
  #       BarcodeImg = paste0(
  #         "<img src='https://barcode.tec-it.com/barcode.ashx?data=", Barcode,
  #         "&code=EAN13&dpi=96' height='60'/>"
  #       )
  #     ) %>%
  #     select(Product, Hoeveelheid, Eenheid, BarcodeImg)
  #   
  #   datatable(df, escape = FALSE, options = list(pageLength = 15))
  # })

# Toon boodschappenlijst met barcode
output$definitieve_lijst <- renderDT({
  make_definitieve_lijst() %>%
    mutate(Link = if_else(!is.na(Link), paste0("<a href='", Link, "' target='_blank'>Bekijk</a>"), "")) %>%
    datatable(escape = FALSE, options = list(pageLength = 15))
})

# Downloadknop CSV
output$download_csv <- downloadHandler(
  filename = function() {
    paste0("boodschappenlijst_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write_csv(make_definitieve_lijst() %>% select(-Link), file)
  })

# Toon boodschappenlijst met barcode
output$definitieve_lijst <- renderDT({
  make_definitieve_lijst() %>%
    mutate(Link = if_else(!is.na(Link), paste0("<a href='", Link, "' target='_blank'>Bekijk</a>"), "")) %>%
    datatable(escape = FALSE, options = list(pageLength = 15))
})

# Downloadknop CSV
output$download_csv <- downloadHandler(
  filename = function() {
    paste0("boodschappenlijst_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write_csv(make_definitieve_lijst() %>% select(-Link), file)
  }
)


}

shinyApp(ui, server)
