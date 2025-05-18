#' PLUS.nl Remote Functions
#'
#' These functions allow remote access to the [PLUS.nl website](https://www.plus.nl) through the [`Chromote`][chromote::Chromote] class.
#' @param credentials Path to a YAML file containing fields `email` and `password`, or a [list] contains those names. Can be set with `options(plus_credentials = "...")`
#' @param product_name Name of the product, such as "PLUS Houdbare Halfvolle Melk Pak 1000 ml".
#' @param product_url PLUS URL of the product, such as "plus-houdbare-halfvolle-melk-pak-1000-ml-957806".
#' @param quantity Number of items to add to basket.
#' @param info Logical to print info, default is `TRUE` in interactive sessions.
#' @param ... arguments passed to [plus_login()]
#' @importFrom yaml read_yaml
#' @importFrom chromote ChromoteSession
#' @importFrom tibble tibble
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_success cli_progress_message cli_text
#' @rdname plus_remote_functions
#' @name plus_remote_functions
#' @aliases plus_login plus_logout plus_add_product plus_open_browser
#' @export
#' @examples
#' \dontrun{
#' plus_login()
#'
#' plus_add_product(product_name = "PLUS Houdbare Halfvolle Melk Pak 1000 ml")
#' }
plus_login <- function(credentials = getOption("plus_credentials", default = system.file("login_credentials.yaml", package = "shinyplus")), info = interactive()) {

  if (is.null(plus_env$browser)) {
    plus_env$browser <- ChromoteSession$new()
  }

  # login page
  if (info) cli_progress_message("Logging in at {.url https://www.plus.nl}...")
  login_url <- "https://aanmelden.plus.nl/plus/login/?sessionOnly=true&goto=https%3A%2F%2Faanmelden.plus.nl%2Fplus%2Fauth%2Foauth2.0%2Fv1%2Fauthorize%3Fresponse_type%3Dcode%26scope%3Dopenid%2Bprofile%26client_id%3Dweb_ecop_eprod%26redirect_uri%3Dhttps%253A%252F%252Fwww.plus.nl%252FCallback"
  plus_env$browser$go_to(login_url)

  # wait for page population of fields
  repeat {
    ready <- plus_env$browser$Runtime$evaluate("document.querySelector('#username') !== null && document.querySelector('#password') !== null && document.querySelector('#loginFormUsernameAndPasswordButton') !== null")$result$value
    if (isTRUE(ready)) break
    Sys.sleep(0.1)

    # check url to see if we're still logged in
    if (!identical(login_url, plus_current_url())) {
      # logged in since we're not on the login page anymore, so quit this process
      if (info) cli_alert_success("Already logged in.")
      return(invisible(TRUE))
    }
  }

  # fill in fields
  if (is.character(credentials) && grepl("[.]ya?ml$", credentials)) {
    email <- read_yaml(credentials)$email
    password <- read_yaml(credentials)$password
  } else if (is.list(credentials) && all(c("email", "password") %in% names(credentials))) {
    email <- credentials$email
    password <- credentials$password
  } else {
    stop("Credentials must be named list or YAML file path.")
  }
  plus_env$browser$Runtime$evaluate(paste0("
  const emailInput = document.querySelector('#username');
  const nativeInputValueSetter = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, 'value').set;
  nativeInputValueSetter.call(emailInput, '", email, "');
  emailInput.dispatchEvent(new Event('input', { bubbles: true }));
"))
  plus_env$browser$Runtime$evaluate(paste0("
  const passInput = document.querySelector('#password');
  nativeInputValueSetter.call(passInput, '", password, "');
  passInput.dispatchEvent(new Event('input', { bubbles: true }));
"))
  plus_env$browser$Runtime$evaluate("document.querySelector('#loginFormUsernameAndPasswordButton').click();")
  if (info) cli_progress_message("Logged in, redirecting to PLUS home page...")
  wait_for_element(".input-search input")

  # no cookies, remove screen (for easier debugging with `plus_open_browser()`)
  plus_env$browser$Runtime$evaluate("
    const buttons = Array.from(document.querySelectorAll('button'));
    const declineBtn = buttons.find(btn => btn.textContent.trim() === 'Weigeren');
    if (declineBtn) declineBtn.click();
  ")
  if (info) cli_alert_success("Succesfully logged in.")
  return(invisible(TRUE))
}

#' @rdname plus_remote_functions
#' @export
plus_logout <- function(info = interactive()) {
  if (!inherits(plus_env$browser, "ChromoteSession")) {
    if (info) cli_alert_danger("Was not logged in.")
    return(invisible())
  }
  # remove cookies
  plus_env$browser$Network$clearBrowserCookies()
  plus_env$browser$Runtime$evaluate("localStorage.clear(); sessionStorage.clear();")
  # close browser
  plus_env$browser$close()
  if (info) cli_alert_success("Succesfully logged out.")
  return(invisible())
}

#' @rdname plus_remote_functions
#' @export
plus_add_product <- function(product_name = NULL, product_url = NULL, quantity = 1, info = interactive(), ...) {
  plus_login(..., info = info)

  if (!is.null(product_url)) {
    url <- paste0("https://www.plus.nl/product/", product_url)
  } else {
    # we need to search for the product
    if (!is.null(product_name)) {
      search_value <- product_name
    } else {
      stop("You must provide a product name, ID, or URL.")
    }
    if (info) cli_text("Searching product {.val {search_value}}...")
    plus_env$browser$go_to(paste0("https://www.plus.nl/zoekresultaten?SearchTerm=", search_value))
    wait_for_element(".plp-results-list a")
    url <- plus_env$browser$Runtime$evaluate("document.querySelector('.plp-results-list a[href]').href;")$result$value
    if (length(url) == 0 || url == "") {
      stop(cli_alert_danger("No URL found."))
    }
    if (info) cli_text("Found URL {.url {url}}.")
  }

  # visit the product page
  plus_env$browser$go_to(url)
  wait_for_element("button.gtm-add-to-cart")
  product_title <- plus_env$browser$Runtime$evaluate("document.querySelector('.product-header-title').textContent;")$result$value

  # add to basket
  for (i in seq_len(quantity)) {
    plus_env$browser$Runtime$evaluate("
    if (typeof add_button === 'undefined') {
      const add_button = document.querySelector('button.gtm-add-to-cart');
    }
    add_button.click();
    }
  ")
    Sys.sleep(0.2)
  }
  if (info) cli_alert_success("Added {.strong {quantity}} item{?s} of {.val {product_title}}.")
}

#' @rdname plus_remote_functions
#' @export
plus_current_basket <- function(..., info = interactive()) {
  plus_login(..., info = info)

  # visit the basket page
  plus_env$browser$go_to("https://www.plus.nl/winkelwagen")
  # wait_for_element("button.gtm-add-to-cart")
  tibble(product = character(0),
         quantity = integer(0))
}

#' @rdname plus_remote_functions
#' @export
plus_current_url <- function() {
  if (is.null(plus_env$browser)) {
    cli_alert_danger("No browser initiated yet, run {.fn plus_login} first.")
    return(invisible())
  }
  tryCatch(plus_env$browser$Runtime$evaluate("window.location.href")$result$value,
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
