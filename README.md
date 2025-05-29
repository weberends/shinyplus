# shinyplus

[![R-CMD-check](https://github.com/weberends/shinyplus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/weberends/shinyplus/actions/workflows/R-CMD-check.yaml)

A Shiny web application for planning weekly groceries. Users can select meals for each day, manage recurring and extra items, and combine them into a shopping basket. The app supports integration with the PLUS.nl online supermarket for sending the basket to the user's online shopping cart.

### Installation

You can install the development version of shinyplus like so:

``` r
remotes::install_github("weberends/shinyplus")
```

### Getting Started

1. Create an account at <https://www.plus.nl>
2. Select your local PLUS Store in your account
3. Save your email address and password to a `.yaml` file and set its location with `options(plus_credentials = "your_file.yaml")`
4. Create the product list, following the instructions in of `shinyplus::update_product_list_from_html()`
5. Run `shinyplus::shinyplus()` to open the app

---

<small>

**Disclaimer:**  

This open-source R package, *shinyplus*, is an independent software project and is not affiliated with, endorsed by, or associated in any way with PLUS Retail B.V. or the website [plus.nl](https://www.plus.nl).

The package does **not** collect, scrape, download, or store any data from plus.nl. It contains **no product information, pricing data, or content** originating from PLUS Retail B.V. or its systems. Users are required to provide their own data (e.g., product lists) manually. This is typically done by copying HTML content from their personal shopping history or PLUS search result pages and importing it locally using `shinyplus::update_product_list_from_html()`.

All interactions with plus.nl occur through **local, client-side browser automation**, using the [`chromote`](https://rstudio.github.io/chromote/) package. This mechanism emulates a user operating a private browser session on their own machine. For example, when a user submits a basket to their PLUS shopping cart, this package simply opens a browser in the background and automates user-visible steps — such as logging in, navigating to product pages, and clicking buttons — exactly as a person would. No API calls or server-side data access is used or implied.

Any logos, icons, or brand references (e.g., the PLUS logo) used in the user interface are publicly available media assets sourced from [Wikipedia](https://nl.wikipedia.org/wiki/PLUS_(Nederlandse_supermarkt)) under appropriate free-use licensing. All trademarks, service marks, and product names are the property of their respective owners.

</small>
