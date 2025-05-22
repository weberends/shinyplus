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
