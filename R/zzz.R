plus_env <- new.env()

#' @importFrom cli cli_alert_danger cli_warn cli_alert_success
#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("plus_data_folder", default = NULL)) || !dir.exists(getOption("plus_data_folder"))) {
    plus_env$data_dir <- system.file("plus_data", package = "shinyplus")
    if (interactive()) {
      cli_alert_danger("DATA FOLDER SET TO {.path {plus_env$data_dir}}.")
      cli_alert_danger("This package folder will be {.strong erased} upon package {.strong update/re-installation} and all settings (dishes / products) {.strong will be lost}.")
      cli_alert_danger("Set the R option `plus_data_folder` to any local folder to prevent this.")
    }
  } else {
    plus_env$data_dir <- getOption("plus_data_folder", default = system.file("plus_data", package = "shinyplus"))
  }

  if (!dir.exists(plus_env$data_dir)) {
    dir_try <- tryCatch(dir.create(plus_env$data_dir, recursive = TRUE), error = function(e) NULL)
    if (is.null(dir_try)) {
      plus_env$data_dir <- system.file("plus_data", package = "shinyplus")
      if (interactive()) {
        cli_alert_danger("DATA FOLDER SET TO {.path {plus_env$data_dir}}.")
        cli_alert_danger("This package folder will be {.strong erased} upon package {.strong update/re-installation} and all settings (dishes / products) {.strong will be lost}.")
        cli_alert_danger("Set the R option `plus_data_folder` to any local folder to prevent this.")
      }
    }
  }

  product_list_path <- file.path(plus_env$data_dir, "product_list.rds")

  if (file.exists(product_list_path)) {
    try({
      plus_env$product_list <- readRDS(product_list_path)
      if (interactive()) {
        cli_alert_success(paste("Imported", NROW(plus_env$product_list), "products"))
      }
    }, silent = TRUE)
  }

  if (NROW(plus_env$product_list) == 0) {
    plus_env$product_list <- tibble(name = character(0),
                                    unit = character(0),
                                    url = character(0),
                                    img = character(0))
    if (interactive()) {
      cli_warn("No product list! Refer to {.help update_product_list_from_html}.")
    }
  }
  plus_env$product_list$img[is.na(plus_env$product_list$img)] <- "shinyplus-assets/questionmark.png"
}
