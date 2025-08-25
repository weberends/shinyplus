#' Send Weekmenu Through GMail
#' @param weekmenu The [data.frame] of weekmenu to send
#' @param credentials Path to a YAML file containing fields `email` and/or `email_cc` (to send mail to), `gmail_user_name` (to send mail from) and `gmail_app_password`, or a [list] contains those names. Can be set with `options(plus_credentials = "...")`
#' @importFrom blastula compose_email md smtp_send creds_envvar
#' @importFrom dplyr mutate select arrange distinct pull
#' @importFrom shiny HTML h3
send_email_weekmenu <- function(weekmenu, credentials = getOption("plus_credentials")) {
  weekmenu <<- weekmenu
  if (is.character(credentials) && grepl("[.]ya?ml$", credentials)) {
    email <- c(read_yaml(credentials)$email, read_yaml(credentials)$email_cc)
    gmail_user_name <- read_yaml(credentials)$gmail_user_name
    gmail_app_password <- read_yaml(credentials)$gmail_app_password
  } else if (is.list(credentials) && any(c("email", "email_cc") %in% names(credentials)) && all(c("gmail_user_name", "gmail_app_password") %in% names(credentials))) {
    email <- c(credentials$email, credentials$email_cc)
    gmail_user_name <- credentials$gmail_user_name
    gmail_app_password <- credentials$gmail_app_password
  } else {
    stop("Credentials must be named list or YAML file path.")
  }
  gmail_user_name <- gsub("@gmail.com", "", gmail_user_name, ignore.case = TRUE)
  Sys.setenv(plus_smtp_password = gmail_app_password)
  creds <- creds_envvar(user = gmail_user_name, pass_envvar = "plus_smtp_password", provider = "gmail")

  per_day <- weekmenu |>
    distinct(day, name) |>
    mutate(text = paste0("<li>", day, ": ", name, "</li>")) |>
    pull(text) |>
    paste(collapse = "")
  per_day <- paste0("<ul>", per_day, "</ul>")
  ingredients <- weekmenu |>
    arrange(day, product_name) |>
    mutate(name = ifelse(duplicated(name), "", name)) |>
    select(Gerecht = name, "Ingredi\u00EBnten" = product_name) |>
    plain_html_table()

  first_monday <- Sys.Date() + (1 - as.integer(format(Sys.Date(), "%u"))) %% 7
  sunday_after <- first_monday + 6
  mail <- compose_email(header = md(paste0("# PLUS Weekmenu\n\n### ", trimws(format(first_monday, "%e %B %Y")), " - ", trimws(format(sunday_after, "%e %B %Y")))),
                        body = paste0(h3(HTML("<u>Overzicht</u>")),
                                      per_day,
                                      h3(HTML("<u>Ingredi\u00EBnten</u>")),
                                      ingredients) |>
                          HTML(),
                        footer = md("Verzonden door de ShinyPLUS app.\n\nGitHub: [`weberends/shinyplus`](https://github.com/weberends/shinyplus)"))
  smtp_send(email = mail,
            to = email,
            from = c("ShinyPLUS" = paste0(gmail_user_name, "@gmail.com")),
            subject = paste0("PLUS Weekmenu (", trimws(format(first_monday, "%e %b")), " - ", trimws(format(sunday_after, "%e %b")), ")"),
            credentials = creds)
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
