plus_env <- new.env()

globalVariables(c("aantal_personen",
                  "dag",
                  "eenheid",
                  "gerecht",
                  "gerecht_id",
                  "hoeveelheid",
                  "ingredient",
                  "price",
                  "quantity"))

as_euro <- function(x) {
  paste("\u20ac", format(round(x, 2), nsmall = 2, decimal.mark = ",", big.mark = "."))
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

