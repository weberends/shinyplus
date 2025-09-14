shinyplus_env <- new.env()

#' @importFrom cli cli_alert_danger cli_warn cli_alert_success
#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("plus_data_folder", default = NULL)) || !dir.exists(getOption("plus_data_folder"))) {
    shinyplus_env$data_dir <- system.file("plus_data", package = "shinyplus")
    if (interactive()) {
      cli_alert_danger("DATA FOLDER SET TO {.path {shinyplus_env$data_dir}}.")
      cli_alert_danger("This package folder will be {.strong erased} upon package {.strong update/re-installation} and all settings (dishes / products) {.strong will be lost}.")
      cli_alert_danger("Set the R option `plus_data_folder` to any local folder to prevent this.")
    }
  } else {
    shinyplus_env$data_dir <- getOption("plus_data_folder", default = system.file("plus_data", package = "shinyplus"))
  }

  if (!dir.exists(shinyplus_env$data_dir)) {
    dir_try <- tryCatch(dir.create(shinyplus_env$data_dir, recursive = TRUE), error = function(e) NULL)
    if (is.null(dir_try)) {
      shinyplus_env$data_dir <- system.file("plus_data", package = "shinyplus")
      if (interactive()) {
        cli_alert_danger("DATA FOLDER SET TO {.path {shinyplus_env$data_dir}}.")
        cli_alert_danger("This package folder will be {.strong erased} upon package {.strong update/re-installation} and all settings (dishes / products) {.strong will be lost}.")
        cli_alert_danger("Set the R option `plus_data_folder` to any local folder to prevent this.")
      }
    }
  }

  product_list_path <- file.path(shinyplus_env$data_dir, "product_list.rds")

  if (file.exists(product_list_path)) {
    try({
      shinyplus_env$product_list <- readRDS(product_list_path)
      if (interactive()) {
        cli_alert_success(paste("Imported", NROW(shinyplus_env$product_list), "products"))
      }
    }, silent = TRUE)
  }

  if (NROW(shinyplus_env$product_list) == 0) {
    shinyplus_env$product_list <- tibble(name = character(0),
                                    unit = character(0),
                                    url = character(0),
                                    img = character(0))
    if (interactive()) {
      cli_warn("No product list! Refer to {.help update_product_list_from_html}.")
    }
  }
  shinyplus_env$product_list$img[is.na(shinyplus_env$product_list$img)] <- "shinyplus-assets/questionmark.png"
}
