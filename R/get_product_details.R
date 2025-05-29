#' Retrieve Product Attributes from Local Data
#'
#' Provides offline access to product information such as name, unit, image, and URL that is available after running [update_product_list_from_html()].
#' @param x Name or URL of a product.
#' @name get_product_details
#' @rdname get_product_details
#' @inheritSection shinyplus-package Disclaimer
#' @export
get_product_url <- function(x) {
  plus_get_urls(x, offline_only = TRUE)
}

#' @rdname get_product_details
#' @export
get_product_name <- function(x) {
  urls <- plus_get_urls(x, offline_only = TRUE)
  trimws(plus_env$product_list$name[match(urls, paste0("https://www.plus.nl", plus_env$product_list$url))])
}

#' @rdname get_product_details
#' @export
get_product_unit <- function(x) {
  urls <- plus_get_urls(x, offline_only = TRUE)
  plus_env$product_list$unit[match(urls, paste0("https://www.plus.nl", plus_env$product_list$url))]
}

#' @rdname get_product_details
#' @export
get_product_name_unit <- function(x) {
  urls <- plus_get_urls(x, offline_only = TRUE)
  name <- trimws(plus_env$product_list$name[match(urls, paste0("https://www.plus.nl", plus_env$product_list$url))])
  unit <- trimws(plus_env$product_list$unit[match(urls, paste0("https://www.plus.nl", plus_env$product_list$url))])
  out <- rep(NA_character_, length(name))
  out[!is.na(name)] <- paste0(name[!is.na(name)], " (", unit[!is.na(name)], ")")
  out
}

#' @rdname get_product_details
#' @export
get_product_image <- function(x) {
  urls <- plus_get_urls(x, offline_only = TRUE)
  plus_env$product_list$img[match(urls, paste0("https://www.plus.nl", plus_env$product_list$url))]
}
