# 1. Go to:
#    * https://www.plus.nl/producten for all products
#    * https://plus.nl/eerder-gekochte-producten for previously purchased products
#    * https://www.plus.nl/zoekresultaten?SearchTerm=YOURSEARCHTERM to search for products
# 2. Scroll down as far as you like to load more products
# 3. Open the Web Info panel and copy a high-level HTML element to the clipboard (so that all <a> elements of products are contained within)
# 4. Run the syntax below

library(dplyr)

new_product_list <- clipr::read_clip() |>
  shinyplus:::create_product_tbl_from_html() |>
  shinyplus:::update_current_product_tbl()

# compare to shinyplus_env$product_list, then:
file.copy()
saveRDS(product_list,
        file.path(shinyplus_env$data_dir, "product_list.rds"))

# done.







# TRY LATER AUTOMATIC SCROLLING ----------------------------------



library(chromote)
library(rvest)

b <- ChromoteSession$new()
b$Page$navigate("https://www.plus.nl/producten")
Sys.sleep(5)

# decline cookies
b$Runtime$evaluate("
    var buttons = Array.from(document.querySelectorAll('button'));
    var declineBtn = buttons.find(btn => btn.textContent.trim() === 'Weigeren');
    if (declineBtn) declineBtn.click();
  ")

simulate_scroll <- function(session, n_times = 30, wait = 2) {
  for (i in seq_len(n_times)) {
    session$Runtime$evaluate("
      window.scrollBy(0, window.innerHeight);
      window.dispatchEvent(new Event('scroll'));
    ")
    Sys.sleep(wait)
  }
}

simulate_scroll(b, n_times = 5, wait = 2)


b$Runtime$evaluate("
  var container2 = document.querySelector('.screen-container');
  container2.scrollTop = container2.scrollHeight;
")


products_a <- b$DOM$getDocument()$root$nodeId |>
  b$DOM$getOuterHTML()
products_a <- products_a[["outerHTML"]]
products_a <- products_a |>
  read_html() |>
  html_element(".plp-results-list") |>
  html_elements("a")

product_list <- tibble()

for (i in seq_along(products_a)) {
  item <- products_a[[i]]
  product_list[i, "name"] <- item |> html_element(".plp-item-name") |> html_text()
  product_list[i, "unit"] <- item |> html_element(".plp-item-complementary") |> html_children() |> magrittr::extract2(1) |> html_text()
  product_list[i, "unit"] <- gsub("^Per ", "", product_list[i, "unit"])
  product_list[i, "url"] <- item |> html_attr("href")
  product_list[i, "img"] <- gsub("[?].*$", "", item |> html_element("img") |> html_attr("src"))
}
