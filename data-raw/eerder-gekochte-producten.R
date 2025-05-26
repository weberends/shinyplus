# 1. Ga naar https://www.plus.nl/producten
# 2. Scroll zo ver naar beneden als je wilt
# 3. Zoek het HTML element 'plp-results-list' en kopieer het als HTML (zodat alle <a> elementen eronder gekopieerd worden)
# 4. Run onderstaand command

x <- clipr::read_clip()
x <- paste(x, collapse = "")
items <- strsplit(x, "<a data-link")[[1]]

saveRDS(items, "data-raw/products_raw.rds")


# eerste item is niks
items <- items[-1]
# split-item terugbrengen zodat we met rvest kunnen inlezen
items <- paste0("<a data-link", items)

product_list <- tibble()

for (i in seq_along(items)) {
  item <- items[i]
  product_list[i, "name"] <- item |> read_html() |> html_element(".plp-item-name") |> html_text()
  product_list[i, "unit"] <- item |> read_html() |> html_element(".plp-item-complementary") |> html_children() |> magrittr::extract2(1) |> html_text()
  product_list[i, "unit"] <- gsub("^Per ", "", product_list[i, "unit"])
  product_list[i, "url"] <- item |> read_html() |> html_element("a") |> html_attr("href")
  product_list[i, "img"] <- gsub("[?].*$", "", item |> read_html() |> html_element("img") |> html_attr("src"))
}

product_list <- product_list |>
  dplyr::bind_rows(recently_bought) |>
  distinct() |>
  dplyr::arrange(name, unit)

recently_bought <- product_list
usethis::use_data(recently_bought, internal = TRUE, overwrite = TRUE)

saveRDS("~/product_list.rds")









# TRY LATER AUTOMATIC SCROLLING



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
