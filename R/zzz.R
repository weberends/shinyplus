plus_env <- new.env()

#' @importFrom cli cli_alert_danger cli_warn cli_alert_success
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("plus_data_folder", default = NULL)) || !dir.exists(getOption("plus_data_folder"))) {
    plus_env$data_dir <- system.file("plus_data", package = "shinyplus")
    if (interactive()) {
      cli_alert_danger(paste("DATA FOLDER SET TO {.path {plus_env$data_dir}}.",
                             "This package folder will be {.strong erased} upon package update and all settings (dishes / products) {.strong will be lost}.",
                             "Set the R option `plus_data_folder` to any local folder to prevent this.",
                             sep = "\n"))
    } else {
      plus_env$data_dir <- getOption("plus_data_folder", default = system.file("plus_data", package = "shinyplus"))
    }
  }

  if (!dir.exists(plus_env$data_dir)) {
    dir_try <- tryCatch(dir.create(plus_env$data_dir, recursive = TRUE), error = function(e) NULL)
    if (is.null(dir_try)) {
      plus_env$data_dir <- system.file("plus_data", package = "shinyplus")
      if (interactive()) {
        cli_alert_danger("Folder {.path {plus_env$data_dir}} is not writable!")
        cli_alert_danger(paste("DATA FOLDER SET TO {.path {plus_env$data_dir}}.",
                               "This package folder will be {.strong erased} upon package update and all settings (dishes / products) {.strong will be lost}.",
                               "Set the R option `plus_data_folder` to any local folder to prevent this.",
                               sep = "\n"))
      }
    }
  }

  if (!file.exists(file.path(plus_env$data_dir, "product_list.rds")) && interactive()) {
    cli_warn("No product list! Refer to {.url https://github.com/weberends/shinyplus/blob/main/data-raw/update_product_list.R}.")
  } else {
    plus_env$product_list <- readRDS(file.path(plus_env$data_dir, "product_list.rds"))
    if (interactive()) {
      cli_alert_success(paste("Imported", NROW(plus_env$product_list), "products"))
    }
  }
}
