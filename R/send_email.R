#' Send Through GMail
#' @param weekmenu The [data.frame] of weekmenu to send
#' @param credentials Path to a YAML file containing fields `email` and/or `email_cc` (to send mail to), `gmail_user_name` (to send mail from) and `gmail_app_password`, or a [list] contains those names. Can be set with `options(plus_credentials = "...")`
#' @importFrom blastula compose_email md smtp_send
#' @importFrom yaml read_yaml
#' @importFrom dplyr mutate select arrange distinct pull filter
#' @importFrom shiny HTML h3
#' @importFrom cli symbol
#' @importFrom commonmark markdown_html
#' @rdname send_mail
#' @export
send_email_weekmenu <- function(weekmenu, credentials = getOption("plus_credentials")) {
  get_credentials(credentials, require_fields = c("email", "gmail_user_name", "gmail_app_password"))

  per_day <- weekmenu |>
    distinct(day, name, .keep_all = TRUE) |>
    mutate(day = ifelse(grepl("lunch", day), "Lunch/Extra", as.character(day))) |>
    mutate(text = paste0("<li>", day, ": ", name, " ", symbol$bullet, " ", meat_icon(meat), " ", symbol$bullet, " ", vegetables_icon(vegetables), "</li>")) |>
    pull(text) |>
    paste(collapse = "")
  per_day <- paste0("<ul>", per_day, "</ul>")
  ingredients <- weekmenu |>
    arrange(day, product_name) |>
    filter(!is.na(product_name)) |>
    mutate(name = ifelse(duplicated(name), "", name)) |>
    select(Gerecht = name, "Ingredi\u00EBnten" = product_name) |>
    plain_html_table()
  instructions <- weekmenu |>
    filter(instructions != "")
  has_instructions <- NROW(instructions) > 0
  instructions <- instructions |>
    distinct(day, name, .keep_all = TRUE) |>
    arrange(day) |>
    mutate(name = ifelse(duplicated(name), "", name),
           instructions = vapply(FUN.VALUE = character(1), instructions, function(x) markdown_html(x, extensions = TRUE, smart = TRUE), USE.NAMES = FALSE)) |>
    select(Gerecht = name, Instructies = instructions) |>
    plain_html_table()

  first_monday <- Sys.Date() + (1 - as.integer(format(Sys.Date(), "%u"))) %% 7
  sunday_after <- first_monday + 6

  custom_css <- paste0(collapse = "\n", c(
    "body, .header, .footer {",
    "  background: rgb(128, 189, 29) !important;",
    "}",
    ".footer {",
    "  color: white !important;",
    "}",
    ".footer a {",
    "  color: rgb(85, 77, 167) !important;",
    "}",
    "ul {",
    "  font-family: sans-serif;",
    "  font-size: 14px;",
    "  color: #333333;",
    "  padding-left: 20px;",
    "}",
    "table {",
    "  border-collapse: collapse;",
    "  width: 100%;",
    "  border: none !important;",
    "}",
    "table th, table td {",
    "  border: 1px solid #dddddd;",
    "  padding: 8px;",
    "}",
    ".ingredients_tbl {",
    "  border: 1px solid rgb(34, 118, 71) !important;",
    "}",
    ".header h1, .header h2 {",
    "  color: white;",
    "}",
    "h2 {",
    "  font-size: 14px;",
    "}",
    "h3 {",
    "  margin-top: 20px;",
    "  font-family: sans-serif;",
    "  color: rgb(34, 118, 71);",
    "}"))

  email_body <- HTML(paste0(
    "<html><head><style>", custom_css, "</style></head><body>",
    h3(HTML("Overzicht")), per_day,
    h3(HTML("Ingredi\u00EBnten")), "<div class='ingredients_tbl'>", ingredients, "</div>",
    ifelse(has_instructions, paste0(h3(HTML("Instructies")), "<div class='ingredients_tbl'>", instructions, "</div>"), ""),
    "</body></html>"
  ))

  mail <- compose_email(header = md(paste0("# PLUS Weekmenu\n\n## ", trimws(format(first_monday, "%e %B %Y")), " - ", trimws(format(sunday_after, "%e %B %Y")))),
                        body = email_body,
                        footer = md("Verzonden door de ShinyPLUS app.\n\nGitHub: [`weberends/shinyplus`](https://github.com/weberends/shinyplus)"))
  smtp_send(email = mail,
            to = shinyplus_env$credentials$email_list,
            from = shinyplus_env$credentials$gmail_account,
            subject = paste0("PLUS Weekmenu (", trimws(format(first_monday, "%e %b")), " - ", trimws(format(sunday_after, "%e %b")), ")"),
            credentials = shinyplus_env$credentials$smtp_creds)
}

#' @rdname send_mail
#' @param basket The [data.frame] of basket items to send
#' @importFrom blastula compose_email md smtp_send
#' @importFrom yaml read_yaml
#' @importFrom dplyr transmute
#' @export
send_email_basket <- function(basket, credentials = getOption("plus_credentials")) {
  get_credentials(credentials, require_fields = c("email", "gmail_user_name", "gmail_app_password"))

  basket <- basket |>
    transmute(" " = paste0("<img src='", get_product_image(product_url), "' style='max-width=50px; max-height: 50px; display:block; margin:auto;'/>"),
              Aantal = paste0(quantity, "x"),
              Product = get_product_name_unit(product_url),
              Groep = label) |>
    plain_html_table()

  custom_css <- paste0(collapse = "\n", c(
    "body {",
    "  background: rgb(128, 189, 29) !important;",
    "}",
    ".footer {",
    "  color: white !important;",
    "}",
    ".footer a {",
    "  color: rgb(85, 77, 167) !important;",
    "}",
    "ul {",
    "  font-family: sans-serif;",
    "  font-size: 14px;",
    "  color: #333333;",
    "  padding-left: 20px;",
    "}",
    "table {",
    "  border-collapse: collapse;",
    "  width: 100%;",
    "  border: none !important;",
    "}",
    "table th, table td {",
    "  border: 1px solid #dddddd;",
    "  padding: 8px;",
    "}",
    "table.ingredients_tbl {",
    "  border: 1px solid rgb(34, 118, 71) !important;",
    "}",
    "h1, h2 {",
    "  color: white;",
    "}",
    "h2 {",
    "  font-size: 14px;",
    "}",
    "h3 {",
    "  margin-top: 20px;",
    "  font-family: sans-serif;",
    "  color: rgb(34, 118, 71);",
    "}"))

  email_body <- HTML(paste0(
    "<html><head><style>", custom_css, "</style></head><body>",
    "Op het moment van versturen, bevatte het mandje de volgende producten:<br><br>",
    basket,
    "</body></html>"
  ))

  mail <- compose_email(header = md("# Overzicht mandje"),
                        body = email_body,
                        footer = md("Verzonden door de ShinyPLUS app.\n\nGitHub: [`weberends/shinyplus`](https://github.com/weberends/shinyplus)"))
  smtp_send(email = mail,
            to = shinyplus_env$credentials$email_list,
            from = shinyplus_env$credentials$gmail_account,
            subject = "Overzicht mandje",
            credentials = shinyplus_env$credentials$smtp_creds)

}

plain_html_table <- function (x, max_col = Inf) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  if (!all(rownames(x) == as.character(seq_len(nrow(x))))) {
    cols <- colnames(x)
    x$` ` <- rownames(x)
    x <- x[, c(" ", cols), drop = FALSE]
  }
  if (ncol(x) > max_col) {
    x <- x[, seq_len(max_col), drop = FALSE]
  }
  head <- paste0("<thead>", paste0("<td><strong>", colnames(x),
                                   "</strong></td>", collapse = ""), "</thead>")
  body <- lapply(x, function(col) paste0("<td>", as.character(col), "</td>"))
  body <- lapply(as.data.frame(t(as.data.frame(body))), function(row) paste0("<tr>", paste0(row, collapse = ""), "</tr>"))
  body <- paste0(unlist(body), collapse = "")
  paste0("<table>", head, body, "</table>")
}
