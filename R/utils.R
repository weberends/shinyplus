globalVariables(c("Artikel",
                  "days",
                  "dish",
                  "dish_id",
                  "ingredient",
                  "is_product",
                  "label",
                  "meat",
                  "name",
                  "name_unit",
                  "per_kg_l_st",
                  "preptime",
                  "price",
                  "price_total",
                  "product",
                  "quantity",
                  "unit",
                  "unit_dbl",
                  "unit_fct",
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
#' @importFrom dplyr filter mutate select bind_rows arrange group_by ungroup n distinct
get_sales <- function() {
  if (is.null(plus_env$browser)) {
    # initialise browser
    plus_env$browser <- ChromoteSession$new()
  }
  open_url_if_not_already_there("https://www.plus.nl/aanbiedingen")
  wait_for_element(".promotions-category-list")
  Sys.sleep(3) # extra waiting time for images to load
  html <- plus_env$browser$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE)$result$value
  html <- read_html(html)

  promo_period <- html |> html_element(".promo-period-display") |> html_text()

  sections <- html |> html_elements(".content-section-full, .cf-product-promotion-banner")
  sale_tbl <- tibble()

  for (section in sections) {
    group <- section |> html_element("h2") |> html_text()
    links <- section |> html_elements("a")
    if (length(links) == 0) {
      next
    }

    for (i in seq_len(length(links))) {
      item <- links[[i]]
      new_row <- nrow(sale_tbl) + 1
      sale_tbl[new_row, "group"] <- group
      sale_tbl[new_row, "single_label"] <- FALSE
      sale_tbl[new_row, "name"] <- item |> html_element(".plp-item-name") |> html_text()
      sale_tbl[new_row, "sale_txt"] <- item |> html_element(".plp-item-complementary-top") |> html_text2() |> gsub("(\n)+", ", ", x = _)
      sale_tbl[new_row, "unit"] <- item |> html_element(".plp-item-complementary") |> html_text2() |> gsub("[[:cntrl:]]", "; ", x = _) |> gsub("^Per ", "", x = _)

      price_current <- item |> html_elements(".product-header-price-integer, .product-header-price-decimals") |> html_text() |> paste(collapse = ".") |> gsub("[.]+", ".", x = _) |> trimws()
      if (price_current == ".") {
        price_current <- ""
      }
      price_previous <- item |> html_element(".product-header-price-previous") |> html_text() |> trimws()
      sale_tbl[new_row, "price_current"] <- gsub(".", ",", price_current, fixed = TRUE)
      sale_tbl[new_row, "price_previous"] <- gsub(".", ",", price_previous, fixed = TRUE)

      url <- item |> html_attr("href") |> gsub("https://www.plus.nl", "", x = _, fixed = TRUE)
      sale_tbl[new_row, "is_product"] <- grepl("^/product/", url)
      sale_tbl[new_row, "url"] <- url
      img <- item |> html_element(".plp-item-image") |> html_element("img") |> html_attr("src") |> gsub("[?].*$", "", x = _)
      if (is.na(img)) {
        img <- "shinyplus-assets/questionmark.png"
      }
      if (grepl("^//", img)) {
        img <- paste0("https:", img)
      }
      sale_tbl[new_row, "img"] <- img
    }
  }

  sale_tbl <- sale_tbl |>
    group_by(group) |>
    mutate(single_label = n() == 1) |>
    ungroup() |>
    filter(single_label | !is.na(name)) |>
    distinct(name, url, .keep_all = TRUE)

  # sale items not existing in product list must be added there
  new_products <- sale_tbl |>
    filter(is_product & !url %in% plus_env$product_list$url)
  if (NROW(new_products) > 0) {
    new_products <- new_products |>
      mutate(unit = sub(";.*", "", unit),
             unit = format_unit(unit)) |>
      select(name, unit, url, img)
    plus_env$product_list <- plus_env$product_list |>
      bind_rows(new_products) |>
      arrange(name)
  }

  structure(sale_tbl,
            promo_period = promo_period,
            new_rows = NROW(new_products))
}

backup_product_list <- function() {
  saveRDS(plus_env$product_list, file = file.path(plus_env$data_dir, paste0("product_list.rds.", format(Sys.time(), "%Y%m%d-%H%M%S"), ".bak")))
}

escape_js_string <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

plus_url <- function(x) {
  is_factor <- is.factor(x)
  is_ordered <- is.ordered(x)
  x <- as.character(x)
  prefix <- "https://www.plus.nl/"
  x[!grepl(prefix, x, fixed = TRUE)] <- paste0(prefix, x[!grepl(prefix, x, fixed = TRUE)])
  x <- gsub("//", "/", x, fixed = TRUE)
  x <- gsub("https:/", "https://", x, fixed = TRUE)
  if (is_factor) {
    factor(x, unique(x), ordered = is_ordered)
  } else {
    x
  }
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

