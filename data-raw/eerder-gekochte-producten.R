# 1. Ga naar https://www.plus.nl/eerder-gekochte-producten
# 2. Scroll zo ver naar beneden als je wilt
# 3. Zoek het HTML element 'recently-bought-products__list' en kopieer het als HTML (zodat alle <a> elementen eronder gekopieerd worden)
# 4. Run onderstaand command

x <- clipr::read_clip()
items <- strsplit(x, "<a data-link")[[1]]

saveRDS(items, "data-raw/eerder_gekocht_raw_items.rds")
items <- readRDS("data-raw/eerder_gekocht_raw_items.rds")

# eerste item is niks
items <- items[-1]
# split-item terugbrengen zodat we met rvest kunnen inlezen
items <- paste0("<a data-link", items)

recently_bought <- tibble()

for (i in seq_along(items)) {
  item <- items[i]
  recently_bought[i, "name"] <- item |> rvest::read_html() |> rvest::html_element(".plp-item-name") |> rvest::html_text()
  recently_bought[i, "unit"] <- item |> rvest::read_html() |> rvest::html_element(".plp-item-complementary") |> rvest::html_children() |> magrittr::extract2(1) |> rvest::html_text()
  recently_bought[i, "unit"] <- gsub("^Per ", "", recently_bought[i, "unit"])
  recently_bought[i, "url"] <- item |> rvest::read_html() |> rvest::html_element("a") |> rvest::html_attr("href")
  recently_bought[i, "img"] <- gsub("[?].*$", "", item |> rvest::read_html() |> rvest::html_element("img") |> rvest::html_attr("src"))
}

recently_bought <- recently_bought |> dplyr::arrange(name, unit)

usethis::use_data(recently_bought, overwrite = TRUE)
