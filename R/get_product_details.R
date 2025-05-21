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
#' get_product_name_quantity("PLUS Aardbeien")
#'
#' get_product_image("PLUS Aardbeien")
get_product_url <- function(x) {
  if (x %in% recently_bought$name) {
    recently_bought$url[match(x, recently_bought$name)]
  } else if (x %in% recently_bought$url) {
    recently_bought$url[match(x, recently_bought$url)]
  } else {
    stop("Product not found: ", x)
  }
}

#' @rdname get_product_details
#' @export
get_product_name <- function(x) {
  if (x %in% recently_bought$name) {
    recently_bought$name[match(x, recently_bought$name)]
  } else if (x %in% recently_bought$url) {
    recently_bought$name[match(x, recently_bought$url)]
  } else {
    stop("Product not found: ", x)
  }
}

#' @rdname get_product_details
#' @export
get_product_name_quantity <- function(x) {
  if (x %in% recently_bought$name) {
    paste0(recently_bought$name, " (", recently_bought$unit, ")")[match(x, recently_bought$name)]
  } else if (x %in% recently_bought$url) {
    paste0(recently_bought$name, " (", recently_bought$unit, ")")[match(x, recently_bought$url)]
  } else {
    stop("Product not found: ", x)
  }
}

#' @rdname get_product_details
#' @export
get_product_image <- function(x) {
  if (x %in% recently_bought$name) {
    recently_bought$img[match(x, recently_bought$name)]
  } else if (x %in% recently_bought$url) {
    recently_bought$img[match(x, recently_bought$url)]
  } else {
    stop("Product not found: ", x)
  }
}
