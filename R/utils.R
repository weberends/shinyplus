globalVariables(c("Artikel",
                  "days",
                  "dish",
                  "dish_id",
                  "ingredient",
                  "meat",
                  "name",
                  "preptime",
                  "price",
                  "price_total",
                  "quantity",
                  "vegetables"))

as_euro <- function(x) {
  paste("\u20ac", trimws(format(round(x, 2), nsmall = 2, decimal.mark = ",", big.mark = ".")))
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

