globalVariables(c("Artikel",
                  "days",
                  "dish",
                  "dish_id",
                  "ingredient",
                  "label",
                  "meat",
                  "name",
                  "name_unit",
                  "preptime",
                  "price",
                  "price_total",
                  "product",
                  "quantity",
                  "unit",
                  "vegetables"))

as_euro <- function(x, trim = FALSE) {
  x.bak <- x
  out <- paste0("\u20ac", ifelse(trim, "", " "), trimws(format(round(as.numeric(gsub(",", ".", x, fixed = TRUE)), 2), nsmall = 2, decimal.mark = ",", big.mark = ".")))
  out[x.bak %in% c("", NA)] <- ""
  out
}

format_unit <- function(units) {
  sapply(units, function(unit) {
    unit <- gsub("^Per ", "", x = unit)
    parts <- strsplit(unit, " ")[[1]]

    if (length(parts) > 1) {
      unit_amount <- as.numeric(gsub(",", ".", parts[1]))
      parts[2] <- gsub("ml", "ml", parts[2], ignore.case = TRUE)
      parts[2] <- gsub("gram", "g", parts[2])

      if (!is.na(unit_amount)) {
        if (unit_amount >= 1000 && parts[2] == "g") {
          parts[1] <- trimws(format(unit_amount / 1000, decimal.mark = ",", big.mark = "."))
          parts[2] <- "kg"
        } else if (unit_amount >= 1000 && parts[2] == "ml") {
          parts[1] <- trimws(format(unit_amount / 1000, decimal.mark = ",", big.mark = "."))
          parts[2] <- "L"
        }
      }
      unit <- paste(parts, collapse = " ")
    }

    if (unit == "") {
      unit <- "1 st"
    }

    unit
  }, USE.NAMES = FALSE)
}

#' @importFrom chromote ChromoteSession
#' @importFrom rvest read_html html_elements html_element html_text html_text2 html_attr
#' @importFrom dplyr filter
get_sales <- function() {
  if (is.null(plus_env$browser)) {
    # initialise browser
    plus_env$browser <- ChromoteSession$new()
  }
  open_url_if_not_already_there("https://www.plus.nl/aanbiedingen")
  wait_for_element(".promotions-category-list")
  # Sys.sleep(3)
  html <- plus_env$browser$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE)$result$value
  html <- read_html(html)

  promo_period <- html |> html_element(".promo-period-display") |> html_text()
  links <- html |> html_element(".promotions-category-list") |> html_elements("a")

  sale_tbl <- tibble()
  for (i in seq_len(length(links))) {
    item <- links[[i]]
    sale_tbl[i, "name"] <- item |> html_element(".plp-item-name") |> html_text()
    sale_tbl[i, "sale_txt"] <- item |> html_element(".plp-item-complementary-top") |> html_text2() |> gsub("(\n)+", ", ", x = _)
    sale_tbl[i, "unit"] <- item |> html_element(".plp-item-complementary") |> html_text() |> gsub("^Per ", "", x = _)

    price_current <- item |> html_elements(".product-header-price-integer, .product-header-price-decimals") |> html_text() |> paste(collapse = ".") |> gsub("[.]+", ".", x = _) |> trimws()
    if (price_current == ".") {
      price_current <- ""
    }
    price_previous <- item |> html_element(".product-header-price-previous") |> html_text() |> trimws()
    sale_tbl[i, "price_current"] <- gsub(".", ",", price_current, fixed = TRUE)
    sale_tbl[i, "price_previous"] <- gsub(".", ",", price_previous, fixed = TRUE)

    url <- item |> html_attr("href")
    sale_tbl[i, "is_product"] <- grepl("^/product/", url)
    sale_tbl[i, "url"] <- url
    img <- item |> html_element(".plp-item-image") |> html_element("img") |> html_attr("src") |> gsub("[?].*$", "", x = _)
    if (grepl("^//", img)) {
      img <- paste0("https:", img)
    }
    sale_tbl[i, "img"] <- img
  }

  sale_tbl <- sale_tbl |> filter(!trimws(name) %in% c("", NA))

  structure(sale_tbl,
            promo_period = promo_period)
}

escape_js_string <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
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

