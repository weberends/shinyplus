#' Get Product Details
#' @param x Name or URL of a product.
#' @name get_product_details
#' @rdname get_product_details
#' @export
#' @examples
#' get_product_url("PLUS Aardbeien")
#'
#' get_product_name("/product/plus-aardbeien-doos-225-g-244585")
#'
#' get_product_name_unit("PLUS Aardbeien")
#'
#' get_product_image("PLUS Aardbeien")
get_product_url <- function(x) {
  plus_get_urls(x, offline_only = TRUE)
}

#' @rdname get_product_details
#' @export
get_product_name <- function(x) {
  urls <- plus_get_urls(x, offline_only = TRUE)
  trimws(recently_bought$name[match(urls, paste0("https://www.plus.nl", recently_bought$url))])
}

#' @rdname get_product_details
#' @export
get_product_unit <- function(x) {
  urls <- plus_get_urls(x, offline_only = TRUE)
  recently_bought$unit[match(urls, paste0("https://www.plus.nl", recently_bought$url))]
}

#' @rdname get_product_details
#' @export
get_product_name_unit <- function(x) {
  urls <- plus_get_urls(x, offline_only = TRUE)
  name <- trimws(recently_bought$name[match(urls, paste0("https://www.plus.nl", recently_bought$url))])
  unit <- trimws(recently_bought$unit[match(urls, paste0("https://www.plus.nl", recently_bought$url))])
  out <- rep(NA_character_, length(name))
  out[!is.na(name)] <- paste0(name[!is.na(name)], " (", unit[!is.na(name)], ")")
  out
}

#' @rdname get_product_details
#' @export
get_product_image <- function(x) {
  urls <- plus_get_urls(x, offline_only = TRUE)
  recently_bought$img[match(urls, paste0("https://www.plus.nl", recently_bought$url))]
}
