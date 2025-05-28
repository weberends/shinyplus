globalVariables(c("aantal_personen",
                  "amount",
                  "Artikel",
                  "dag",
                  "days",
                  "dish",
                  "dish_id",
                  "eenheid",
                  "gerecht",
                  "gerecht_id",
                  "hoeveelheid",
                  "ingredient",
                  "meat",
                  "name",
                  "people",
                  "preptime",
                  "price",
                  "price_total",
                  "quantity",
                  "vegetables"))

as_euro <- function(x) {
  paste("\u20ac", format(round(x, 2), nsmall = 2, decimal.mark = ",", big.mark = "."))
}

#' @importFrom tibble tibble
#' @importFrom dplyr filter
get_product_tbl <- function(html_txt) {
  items_html <- paste(html_txt, collapse = "") |> rvest::read_html() |> rvest::html_element(".plp-results-list") |> rvest::html_elements("a")
  product_list <- tibble()

  for (i in seq_along(items_html)) {
    item <- items_html[[i]]
    unit <- item |> rvest::html_element(".plp-item-complementary") |> rvest::html_children()
    if (length(unit) == 0) {
      product_list[i, "name"] <- NA_character_
      next
    }

    product_list[i, "name"] <- item |> rvest::html_element(".plp-item-name") |> rvest::html_text()
    unit <- unit[[1]] |> rvest::html_text() |> gsub("^Per ", "", x = _)
    unit <- strsplit(unit, " ")[[1]]
    unit_amount <- as.numeric(gsub(",", ".", unit[1]))
    unit[2] <- gsub("ml", "ml", unit[2], ignore.case = TRUE)
    unit[2] <- gsub("gram", "g", unit[2])
    if (unit_amount >= 1000 && unit[2] == "g") {
      unit[1] <- trimws(format(unit_amount / 1000, decimal.mark = ",", big.mark = "."))
      unit[2] <- "kg"
    } else if (unit_amount >= 1000 && unit[2] == "ml") {
      unit[1] <- trimws(format(unit_amount / 1000, decimal.mark = ",", big.mark = "."))
      unit[2] <- "L"
    }
    unit <- paste(unit, collapse = " ")
    product_list[i, "unit"] <- unit
    product_list[i, "url"] <- item |> rvest::html_attr("href")
    product_list[i, "img"] <- item |> rvest::html_element("img") |> rvest::html_attr("src") |> gsub("[?].*$", "", x = _)
  }
  product_list <- product_list |> filter(!is.na(name))

  message(nrow(product_list), " producten gevonden")
  return(product_list)
}

# AMR:::get_n_cores()
# get_n_cores <- function(max_cores = Inf) {
#   if ("parallelly" %in% rownames(utils::installed.packages())) {
#     n_cores <- min(parallelly::availableCores(), na.rm = TRUE)
#   } else {
#     # `parallel` is part of base R since 2.14.0, but detectCores() is not very precise on exotic systems like Docker and quota-set Linux environments
#     n_cores <- parallel::detectCores()[1]
#     if (is.na(n_cores)) {
#       n_cores <- 1
#     }
#   }
#   max_cores <- floor(max_cores)
#   if (max_cores == 0) {
#     n_cores <- 1
#   } else if (max_cores < 0) {
#     n_cores <- max(1, n_cores - abs(max_cores))
#   } else if (max_cores > 0) {
#     n_cores <- min(n_cores, max_cores)
#   }
#   n_cores
# }

