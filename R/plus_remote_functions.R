#' PLUS.nl Remote Functions
#'
#' These functions allow remote access to the [PLUS.nl website](https://www.plus.nl) through the [`Chromote`][chromote::Chromote] class.
#' @param credentials Path to a YAML file containing fields `email` and `password`, or a [list] contains those names. Can be set with `options(plus_credentials = "...")`
#' @param product_name Name of the product, such as "PLUS Houdbare Halfvolle Melk Pak 1000 ml".
#' @param product_id PLUS ID of the product, such as "957806".
#' @param product_url PLUS URL of the product, such as "plus-houdbare-halfvolle-melk-pak-1000-ml-957806".
#' @param quantity Number of items to add to basket.
#' @param info Logical to print info, default is `TRUE` in interactive sessions.
#' @param ... arguments passed to [plus_login()]
#' @importFrom yaml read_yaml
#' @importFrom chromote ChromoteSession
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
  if (info) cli::cli_progress_message("Visiting {.url https://www.plus.nl}...")
  login_url <- "https://aanmelden.plus.nl/plus/login/?sessionOnly=true&goto=https%3A%2F%2Faanmelden.plus.nl%2Fplus%2Fauth%2Foauth2.0%2Fv1%2Fauthorize%3Fresponse_type%3Dcode%26scope%3Dopenid%2Bprofile%26client_id%3Dweb_ecop_eprod%26redirect_uri%3Dhttps%253A%252F%252Fwww.plus.nl%252FCallback"
  plus_env$browser$go_to(login_url)

  # wait for page population of fields
  repeat {
    ready <- plus_env$browser$Runtime$evaluate("document.querySelector('#username') !== null && document.querySelector('#password') !== null && document.querySelector('#loginFormUsernameAndPasswordButton') !== null")$result$value
    if (isTRUE(ready)) break
    Sys.sleep(0.1)

    # check url to see if we're still logged in
    current_url <- plus_env$browser$Runtime$evaluate("window.location.href")$result$value
    if (!identical(login_url, current_url)) {
      # logged in, so quit this process
      if (info) cli::cli_alert_info("Already logged in.")
      return(invisible(TRUE))
    }
  }

  # fill in fields
  if (grepl("[.]ya?ml$", credentials)) {
    email <- read_yaml(credentials)$email
    password <- read_yaml(credentials)$password
  } else if (is.list(credentials) && all(c("email", "password") %in% names(credentials))) {
    email <- credentials$email
    password <- credentials$password
  } else {
    stop("Credentials must be named list or YAML file.")
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
  if (info) cli::cli_progress_message("Logged in, redirecting to PLUS home page...")
  wait_for_element(".input-search input")

  # no cookies, remove screen (for easier debugging with `plus_env$browser$view()`)
  plus_env$browser$Runtime$evaluate("
    const buttons = Array.from(document.querySelectorAll('button'));
    const declineBtn = buttons.find(btn => btn.textContent.trim() === 'Weigeren');
    if (declineBtn) declineBtn.click();
  ")
  if (info) cli::cli_alert_success("Succesfully logged in.")
  return(invisible(TRUE))
}

#' @rdname plus_remote_functions
#' @export
plus_logout <- function(info = interactive()) {
  if (!inherits(plus_env$browser, "ChromoteSession")) {
    if (info) cli::cli_alert_info("Was not logged in.")
    return(invisible())
  }
  # remove cookies
  plus_env$browser$Network$clearBrowserCookies()
  plus_env$browser$Runtime$evaluate("localStorage.clear(); sessionStorage.clear();")
  # close browser
  plus_env$browser$close()
  if (info) cli::cli_alert_success("Succesfully logged out.")
  return(invisible())
}

#' @rdname plus_remote_functions
#' @export
plus_add_product <- function(product_name = NULL, product_id = NULL, product_url = NULL, quantity = 1, info = interactive(), ...) {
  plus_login(..., info = FALSE)

  if (!is.null(product_url)) {
    url <- paste0("https://www.plus.nl/product/", product_url)
  } else {
    # we need to search for the product
    if (!is.null(product_id)) {
      search_value <- product_id
    } else if (!is.null(product_name)) {
      search_value <- product_name
    } else {
      stop("You must provide a product name, ID, or URL.")
    }
    if (info) cli::cli_text("Searching product {.val {search_value}}...")
    plus_env$browser$go_to(paste0("https://www.plus.nl/zoekresultaten?SearchTerm=", search_value))
    wait_for_element(".plp-results-list")
    url <- plus_env$browser$Runtime$evaluate("document.querySelector('.plp-results-list a[href]').href;")$result$value
    if (info) cli::cli_text("Found URL {.url {url}}.")
  }

  # visit the product page
  plus_env$browser$go_to(url)
  wait_for_element(".product-header-title")

  product_title <- plus_env$browser$Runtime$evaluate("document.querySelector('.product-header-title').textContent;")$result$value

  # add to basket
  for (i in seq_len(quantity)) {
    plus_env$browser$Runtime$evaluate("
    if (typeof add_button === 'undefined') {
      const add_button = document.querySelector('button.gtm-add-to-cart');
      if (add_button) add_button.click();
    } else {
      add_button.click();
    }
  ")
    Sys.sleep(0.1)
  }
  if (info) cli::cli_text("Added {.strong {quantity}} item{?s} of {.val {product_title}}.")
}

#' @rdname plus_remote_functions
#' @export
plus_open_browser <- function() {
  if (is.null(plus_env$browser)) {
    cli_alert_danger("No browser initiated yet, run {.fn plus_login} first.")
    return(invisible())
  }
  plus_env$browser$view()
}

wait_for_element <- function(selector, b = plus_env$browser, timeout = 10, interval = 0.1) {
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

