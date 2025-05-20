# shinyplus

[![R-CMD-check](https://github.com/weberends/shinyplus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/weberends/shinyplus/actions/workflows/R-CMD-check.yaml)

## A Shiny App to Automate Grocery Shopping at PLUS.nl

1. Create an account at <https://www.plus.nl>
2. Select your local PLUS store in your account
3. Save your email address and password to a `.yaml` file and set its location with `options(plus_credentials = "your_file.yaml")`
4. Run `shinyplus::open_app()` to open the app

## Installation

You can install the development version of shinyplus like so:

``` r
remotes::install_github("weberends/shinyplus")
```

---

<small>

**Disclaimer:**  

This open-source R package, *shinyplus*, is independently developed and has no affiliation with, endorsement by, or association with PLUS Retail B.V. or the website [plus.nl](https://www.plus.nl).  

The package merely uses *client-side browser automation* via the [`chromote`](https://rstudio.github.io/chromote/) package to interact with publicly available content on plus.nl, simulating user behaviour (such as logging in, browsing, and selecting products) in a private, ephemeral local browser session. Any icons or images used (e.g. the PLUS logo) are sourced from [Wikipedia](https://nl.wikipedia.org/wiki/PLUS_(Nederlandse_supermarkt)) under appropriate free-use licensing. All trademarks, product names, and brand assets remain the property of their respective owners.

</small>
