#' Interact with PLUS.nl via Headless Browser
#'
#' Provides functions to log in, add products to the cart, retrieve the current cart, and navigate the [PLUS.nl website](https://www.plus.nl) using a [`Chromote`][chromote::Chromote] browser session.
#' @param credentials Path to a YAML file containing fields `email` and `password`, or a [list] contains those names. Can be set with `options(plus_credentials = "...")`
#' @param x Name of the product, such as "PLUS Houdbare Halfvolle Melk Pak 1000 ml", or URL of the product, such as "plus-houdbare-halfvolle-melk-pak-1000-ml-957806".
#' @param quantity Number of items to add to cart.
#' @param info Logical to print info, default is `TRUE` in interactive sessions.
#' @param ... arguments passed to [plus_login()].
#' @param b Browser to use.
#' @importFrom yaml read_yaml
#' @importFrom chromote ChromoteSession
#' @importFrom tibble tibble
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_success cli_progress_message cli_text
#' @rdname plus_remote_functions
#' @name plus_remote_functions
#' @aliases plus_login plus_logout plus_add_products plus_open_browser
#' @inheritSection shinyplus-package Disclaimer
#' @export
plus_login <- function(credentials = getOption("plus_credentials"), info = interactive()) {

  if (is.character(credentials) && grepl("[.]ya?ml$", credentials)) {
    email <- read_yaml(credentials)$email
    password <- read_yaml(credentials)$password
  } else if (is.list(credentials) && all(c("email", "password") %in% names(credentials))) {
    email <- credentials$email
    password <- credentials$password
  } else {
    stop("Credentials must be named list or YAML file path.")
  }

  if (is.null(plus_env$browser)) {
    # initialise browser
    plus_env$browser <- ChromoteSession$new()
  }
  if (plus_ascertain_logged_in(info = FALSE)) {
    plus_env$email <- email
    if (info) cli_alert_success("Already logged in as {.val {plus_env$email}}.")
    return(invisible(TRUE))
  }

  # login page
  if (info) cli_progress_message("Logging in at {.url https://www.plus.nl}...")
  login_url <- "https://aanmelden.plus.nl/plus/login/?plus_env$browserOnly=true&goto=https%3A%2F%2Faanmelden.plus.nl%2Fplus%2Fauth%2Foauth2.0%2Fv1%2Fauthorize%3Fresponse_type%3Dcode%26scope%3Dopenid%2Bprofile%26client_id%3Dweb_ecop_eprod%26redirect_uri%3Dhttps%253A%252F%252Fwww.plus.nl%252FCallback"
  open_url_if_not_already_there(login_url)

  # wait for page population of fields
  repeat {
    ready <- plus_env$browser$Runtime$evaluate("document.querySelector('#username') !== null && document.querySelector('#password') !== null && document.querySelector('#loginFormUsernameAndPasswordButton') !== null")$result$value
    if (isTRUE(ready)) break
    Sys.sleep(0.5)

    # check url to see if we're still logged in
    if (!identical(login_url, plus_current_url(plus_env$browser)) && !grepl("aanmelden.plus.nl", plus_current_url(plus_env$browser), fixed = TRUE)) {
      # already logged in
      plus_env$email <- email
      if (is.null(plus_env$browser_cart)) {
        plus_env$browser_cart <- plus_env$browser$new_session()
        open_url_if_not_already_there("https://www.plus.nl/winkelwagen", b = plus_env$browser_cart)
      }
      if (info) cli_alert_success("Already logged in as {.val {plus_env$email}}.")
      return(invisible(TRUE))
    }
  }

  # fill in fields
  plus_env$browser$Runtime$evaluate(paste0("
    var emailInput = document.querySelector('#username');
    var nativeInputValueSetter = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, 'value').set;
    nativeInputValueSetter.call(emailInput, '", email, "');
    emailInput.dispatchEvent(new Event('input', { bubbles: true }));
  "))
  plus_env$browser$Runtime$evaluate(paste0("
    var passInput = document.querySelector('#password');
    nativeInputValueSetter.call(passInput, '", password, "');
    passInput.dispatchEvent(new Event('input', { bubbles: true }));
  "))
  plus_env$browser$Runtime$evaluate("document.querySelector('#loginFormUsernameAndPasswordButton').click();")
  if (info) cli_progress_message("Logged in, redirecting to PLUS home page...")
  # wait_for_element(".input-search input")
  plus_env$email <- email
  plus_env$browser_cart <- plus_env$browser$new_session()
  open_url_if_not_already_there("https://www.plus.nl/winkelwagen", b = plus_env$browser_cart)
  if (info) cli_alert_success("Succesfully logged in as {.val {plus_env$email}}.")
  return(invisible(TRUE))
}

plus_ascertain_logged_in <- function(info = interactive(), b = plus_env$browser) {
  current_account <- tryCatch(
    b$Runtime$evaluate("document.querySelector('.gtm-account-options .popover-top-label span')?.textContent")$result$value,
    error = function(e) NULL
  )
  if (!is.null(current_account) && current_account != "") {
    return(TRUE)
  } else {
    if (info) cli_alert_danger("Not logged in.")
    return(FALSE)
  }
}

#' @rdname plus_remote_functions
#' @importFrom cli cli_alert_success
#' @export
plus_logout <- function(info = interactive()) {
  if (!inherits(plus_env$browser, "Chromoteplus_env$browser")) {
    if (info) cli_alert_danger("Was not logged in.")
    return(invisible())
  }
  # remove cookies
  plus_env$browser$Network$clearBrowserCookies()
  plus_env$browser$Runtime$evaluate("localStorage.clear(); plus_env$browserStorage.clear();")
  # close browser
  plus_env$browser$close()
  if (info) cli_alert_success("Succesfully logged out.")
  return(invisible())
}

#' @rdname plus_remote_functions
#' @importFrom tibble tibble
#' @importFrom dplyr group_by summarise
#' @importFrom cli cli_progress_message cli_alert_success cli_alert_danger
#' @export
plus_add_products <- function(x, quantity = 1, info = interactive(), ...) {
  if (length(quantity) == 1) quantity <- rep(quantity, length(x))
  stopifnot(length(x) == length(quantity))

  if (length(unique(x)) < length(x)) {
    summed <- tibble(x, quantity) |>
      group_by(x) |>
      summarise(quantity = sum(quantity, na.rm = TRUE))
    x <- summed$x
    quantity <- summed$quantity
  }

  if (!plus_ascertain_logged_in(info = info)) return(invisible())
  urls <- plus_get_urls(x, ..., info = info)
  sku <- gsub(".*-([0-9]+)$", "\\1", urls)
  urls <- paste0("https://www.plus.nl/zoekresultaten?SearchTerm=", sku)
  successfully_added <- integer(length(urls))

  for (i in seq_along(x)) {
    if (info) cli_progress_message("Adding {.url {urls[i]}}...")
    tryCatch({
      plus_env$browser$Page$navigate(urls[i])
      wait_for_element(".plp-item-quantity")
      wait_for_element(".cart-badge-link .badge.background-red")

      current_in_cart <- function() as.integer(plus_env$browser$Runtime$evaluate("document.querySelector('.cart-badge-link .badge.background-red')?.textContent.trim()")$result$value)

      product_title <- plus_env$product_list$name[which(grepl(paste0(sku, "$"), plus_env$product_list$url))]

      old_cart_count <- current_in_cart()
      goal_cart_count <- old_cart_count + quantity[i]
      current_cart_count <- old_cart_count
      tries <- 0

      while (tries <= 5 && current_cart_count != goal_cart_count) {
        for (j in seq_len(quantity[i])) {
          plus_env$browser$Runtime$evaluate("document.querySelector('button.gtm-add-to-cart')?.click();")
          Sys.sleep(1)
        }
        Sys.sleep(2)
        new_cart_count <- current_in_cart()
        if (current_cart_count == new_cart_count) {
          # cart number did not update, wait a bit longer
          Sys.sleep(2)
          new_cart_count <- current_in_cart()
        }
        current_cart_count <- new_cart_count
        tries <- tries + 1
      }

      last_cart_count <- current_cart_count

      if (old_cart_count == last_cart_count) {
        # still not updated - something went wrong
        stop("Cart badge was not updated - something went wrong in adding product")
      }

      successfully_added[i] <- last_cart_count - old_cart_count
      if (info) cli_alert_success("Added {.strong {successfully_added[i]} items} of {.val {product_title}} to cart.")
    }, error = function(e) {
      if (info) cli_alert_danger("Failed to add product: {.strong {conditionMessage(e)}}")
    })
  }
  return(successfully_added)
}

#' @importFrom cli cli_text cli_alert_danger
plus_get_urls <- function(x, ..., info = interactive(), b = plus_env$browser, offline_only = FALSE) {
  if (all(x %in% plus_env$product_list$url)) {
    return(plus_url(x))
  }
  x <- trimws(as.character(x))
  x <- gsub("https://www.plus.nl", "", x, fixed = TRUE)
  out <- rep(NA_character_, length(x))
  out[x %in% trimws(plus_env$product_list$name)] <- plus_env$product_list$url[match(x[x %in% trimws(plus_env$product_list$name)], trimws(plus_env$product_list$name))]
  out[x %in% plus_env$product_list$url] <- x[x %in% plus_env$product_list$url]
  out[x %in% plus_env$product_list$img] <- plus_env$product_list$url[match(x[x %in% plus_env$product_list$img], plus_env$product_list$img)]
  if (anyNA(out) && !offline_only) {
    # we need to search these on the PLUS website
    if (!plus_ascertain_logged_in(info = info)) return(invisible())
    to_search <- unique(x[is.na(out)])
    print(to_search)
    search_out <- rep(NA_character_, length(to_search))
    for (i in seq_len(length(to_search))) {
      search_value <- to_search[i]
      if (info) cli_text("Searching product {.val {search_value}}...")
      open_url_if_not_already_there(paste0("https://www.plus.nl/zoekresultaten?SearchTerm=", utils::URLencode(search_value)),
                                    b = b)
      wait_for_element(".plp-results-list a", b = b)
      url <- b$Runtime$evaluate("document.querySelector('.plp-results-list a[href]').href;")$result$value
      if (length(url) == 0 || url == "") {
        stop(cli_alert_danger("No URL found."))
      }
      search_out[i] <- url
      if (info) cli_text("Found URL {.url {url}}.")
    }
    out[is.na(out)] <- search_out[match(x, to_search)]
  }
  out[!is.na(out)] <- plus_url(out[!is.na(out)])
  out
}

#' @rdname plus_remote_functions
#' @importFrom tibble tibble
#' @importFrom dplyr case_when mutate
#' @export
plus_current_cart <- function(..., info = interactive()) {
  if (!plus_ascertain_logged_in(info = info)) return(invisible(NULL))

  if (is.null(plus_env$browser_cart)) {
    plus_env$browser_cart <- plus_env$browser$new_session()
    open_url_if_not_already_there("https://www.plus.nl/winkelwagen", b = plus_env$browser_cart)
  }

  # refresh cart page
  plus_env$browser_cart$Page$reload()
  wait_for_element("body", b = plus_env$browser_cart) # minimal page check
  # first wait for page to load
  wait_for_element(".cart-title-wrapper h1", b = plus_env$browser_cart)
  # and wait some more time
  Sys.sleep(3)

  html <- plus_env$browser_cart$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE)$result$value
  doc <- read_html(html)
  items <- doc |> html_elements(".cart-item-wrapper")

  final_total <- doc |>
    html_element(".total-receipt-item") |>
    html_text() |>
    gsub(".*?([0-9]+[.,][0-9]+)$", "\\1", x = _) |>
    as.numeric()

  cart_data <- tibble(
    product = items |> html_element(".cart-item-name span") |> html_text2(),
    unit = items |> html_element(".cart-item-complementary span") |> html_text2(),
    price = items |> html_element(".cart-item-price span") |> html_text2() |>
      gsub("[^0-9,\\.]", "", x = _) |> gsub(",", ".", x = _) |> as.double(),
    quantity = items |> html_element(".cart-item-quantity span") |> html_text2() |>
      as.integer()
  )

  cart_data <- cart_data |>
    filter(!is.na(product), nzchar(product)) |>
    mutate(
      unit = format_unit(unit),
      unit_dbl = as.double(gsub("[^0-9.,]", "", gsub(",", ".", unit))),
      unit_fct = case_when(
        grepl(" (g|ml)$", unit) ~ unit_dbl / 1000,
        grepl(" (kg|L|st)$", unit) ~ unit_dbl,
        .default = 1
      ),
      per_kg_l_st = price / unit_fct,
      price_total = round(price * quantity, 2)
    ) |>
    select(-unit_dbl, -unit_fct)

  structure(cart_data,
            class = c("plus_cart", class(cart_data)),
            final_total = final_total)
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.plus_cart<- function(x, ...) {
  cross_icon <- if (isTRUE(base::l10n_info()$`UTF-8`)) "\u00d7" else "x"
  dims <- paste(format(NROW(x), big.mark = ","), cross_icon, format(NCOL(x), big.mark = ","))
  names(dims) <- "A PLUS Cart"
  dims <- c(
    dims,
    "Total Items"      = paste0(sum(x$quantity, na.rm = TRUE), " (unique products: ", NROW(x), ")"),
    "Original Total"   = as_euro(sum(x$price_total, na.rm = TRUE), trim = TRUE),
    "Savings"          = as_euro(sum(x$price_total, na.rm = TRUE) - attributes(x)$final_total, trim = TRUE),
    "Final Total"      = as_euro(attributes(x)$final_total, trim = TRUE),
    "Account"          = plus_env$email
  )
  dims
}

#' @rdname plus_remote_functions
#' @export
plus_checkout <- function() {
  if (interactive() && !is.null(getOption("browser")) && is.function(getOption("browser"))) {
    utils::browseURL("https://www.plus.nl/checkout")
  } else {
    cli_alert_info("Visit {.url https://www.plus.nl/checkout} and proceed there.")
  }
}

#' @rdname plus_remote_functions
#' @export
plus_current_url <- function(b = plus_env$browser) {
  if (is.null(b)) {
    cli_alert_danger("No browser initiated yet, run {.fn plus_login} first.")
    return(invisible())
  }
  tryCatch(b$Runtime$evaluate("window.location.href")$result$value,
           error = function(e) cli_alert_danger("Browser not available."))

}

#' @rdname plus_remote_functions
#' @export
plus_open_browser <- function() {
  if (is.null(plus_env$browser)) {
    cli_alert_danger("No browser initiated yet, run {.fn plus_login} first.")
    return(invisible())
  }
  tryCatch(plus_env$browser$view(),
           error = function(e) cli_alert_danger("Browser not available."))
}

#' @rdname plus_remote_functions
#' @export
plus_page_reload <- function() {
  if (is.null(plus_env$browser)) {
    cli_alert_danger("No browser initiated yet, run {.fn plus_login} first.")
    return(invisible())
  }
  tryCatch(plus_env$browser$Page$reload(),
           error = function(e) cli_alert_danger("Browser not available."))
}

open_url_if_not_already_there <- function(url, b = plus_env$browser) {
  current_url <- tryCatch(b$Runtime$evaluate("window.location.href")$result$value,
                          error = function(e) "")
  if (!identical(url, current_url)) {
    tryCatch({
      b$go_to(url)
      wait_for_element("body", b = b)  # minimal page check
    }, error = function(e) {
      cli_alert_danger("Navigation failed: {conditionMessage(e)}")
      stop("Failed to navigate to: ", url, call. = FALSE)
    })
  }
}

wait_for_element <- function(selector, b = plus_env$browser, timeout = 10, interval = 0.2) {
  start_time <- Sys.time()
  while (Sys.time() - start_time < timeout) {
    found <- b$Runtime$evaluate(
      sprintf("document.querySelector('%s') !== null", selector)
    )$result$value
    if (isTRUE(found)) return(invisible(TRUE))
    Sys.sleep(interval)
  }
  warning(sprintf("Timeout waiting for selector: %s", selector))
  invisible(FALSE)
}

decline_cookies <- function() {
  # no cookies, remove screen (for easier debugging with `plus_open_browser()`)
  Sys.sleep(2)
  plus_env$browser$Runtime$evaluate("
    var buttons = Array.from(document.querySelectorAll('button'));
    var declineBtn = buttons.find(btn => btn.textContent.trim() === 'Weigeren');
    if (declineBtn) declineBtn.click();
  ")
}
