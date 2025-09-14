#' Extract Product Information from HTML
#'
#' Parses HTML content containing product listings from the PLUS website and updates the local product list with names, units, URLs, and images.
#' @param html_txt Text to include in project list, as HTML element containing product entries as `<a>` elements.
#' @details
#' Steps to create local products list file:
#'
#' 1. Go to:
#'    * <https://plus.nl/eerder-gekochte-producten> for personal, previously purchased products
#'    * <https://www.plus.nl/producten> for all products
#'    * <https://www.plus.nl/zoekresultaten?SearchTerm=YOURSEARCHTERM> to search for products
#' 2. Scroll down as far as you like to load more products
#' 3. Open the Web Info panel and copy a high-level HTML element to the clipboard (so that all `<a>` elements of products are contained within)
#' 4. Run:
#'
#'    ```r
#'    html_txt <- clipr::read_clip()
#'    new_product_list <- update_product_list_from_html(html_txt)
#'
#'    # this is the local filepath for products (the R package does not contain product data)
#'    path <- file.path(shinyplus:::shinyplus_env$data_dir, "product_list.rds")
#'
#'    # make a backup
#'    file.copy(from = path, to = paste0(path, ".", format(Sys.time(), "%Y%m%d-%H%M%S"), ".bak"))
#'
#'    # save new product list
#'    saveRDS(new_product_list, path)
#'    ```
#'
#' Alternatively, visit the relevant tab in the [shinyplus()] app.
#'
#' This process can be repeated as often as needed.
#' @importFrom rvest html_attr html_children html_element html_elements html_text read_html
#' @importFrom tibble tibble
#' @importFrom dplyr filter bind_rows distinct arrange
#' @inheritSection shinyplus-package Disclaimer
#' @export
update_product_list_from_html <- function(html_txt) {
  items_html <- paste(html_txt, collapse = "") |> read_html() |> html_element(".plp-results-list") |> html_elements("a")
  new_product_list <- tibble()

  cli_alert_info("{length(items_html)} products in clipboard.")

  for (i in seq_along(items_html)) {
    item <- items_html[[i]]
    unit <- item |> html_element(".plp-item-complementary") |> html_children()
    if (length(unit) == 0) {
      new_product_list[i, "name"] <- NA_character_
      next
    }

    new_product_list[i, "name"] <- item |> html_element(".plp-item-name") |> html_text()
    unit <- unit[[1]] |> html_text() |> format_unit()
    new_product_list[i, "unit"] <- unit
    new_product_list[i, "url"] <- item |> html_attr("href")
    new_product_list[i, "img"] <- item |> html_element("img") |> html_attr("src") |> gsub("[?].*$", "", x = _)
  }
  new_product_list <- new_product_list |> filter(!is.na(name))

  current_product_list <- shinyplus_env$product_list

  product_list <- new_product_list |>
    bind_rows(current_product_list) |>
    distinct()

  # some images were not well selected, they could be promotion images and then occur more than once
  img_dups <- product_list$img[duplicated(product_list$img)]
  product_list$img[product_list$img %in% img_dups] <- NA_character_

  # double entries with an NA url will be filtered out
  out <- product_list |>
    arrange(name, unit, url, img) |>
    distinct(name, unit, .keep_all = TRUE)

  cli_alert_success("{NROW(out) - NROW(current_product_list)} new products added.")
  out
}

create_product_list_internal <- function(x) {

  x <- paste(as.character(x), collapse = " ")

  if (grepl("^http", x)) {
    # we launch a browser to visit the page
    if (is.null(shinyplus_env$browser)) {
      # initialise browser
      shinyplus_env$browser <- ChromoteSession$new()
      Sys.sleep(3)
    }
    shinyplus_env$browser$Page$navigate(x)
    wait_for_element("body", b = shinyplus_env$browser) # minimal page check
    Sys.sleep(3)

    html_txt <- shinyplus_env$browser$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE)$result$value

  } else {
    # HTML code was given in app
    html_txt <- x
  }

  items_html <- paste(html_txt, collapse = "") |> read_html() |> html_element(".plp-results-list") |> html_elements("a")
  new_product_list <- tibble()

  for (i in seq_along(items_html)) {
    item <- items_html[[i]]
    unit <- item |> html_element(".plp-item-complementary") |> html_children()
    if (length(unit) == 0) {
      new_product_list[i, "name"] <- NA_character_
      next
    }

    new_product_list[i, "name"] <- item |> html_element(".plp-item-name") |> html_text()
    unit <- unit[[1]] |> html_text() |> format_unit()
    new_product_list[i, "unit"] <- unit
    new_product_list[i, "url"] <- item |> html_attr("href")
    new_product_list[i, "img"] <- item |> html_element("img") |> html_attr("src") |> gsub("[?].*$", "", x = _)
  }
  new_product_list <- new_product_list |> filter(!is.na(name))

  current_product_list <- shinyplus_env$product_list

  product_list <- new_product_list |>
    bind_rows(current_product_list) |>
    distinct()

  # some images were not well selected, they could be promotion images and then occur more than once
  img_dups <- product_list$img[duplicated(product_list$img)]
  product_list$img[product_list$img %in% img_dups] <- NA_character_

  # double entries with an NA url will be filtered out
  out <- product_list |>
    arrange(name, unit, url, img) |>
    distinct(name, unit, .keep_all = TRUE)

  new <- out |>
    filter(!url %in% current_product_list$url)
  invisible(new)
}
