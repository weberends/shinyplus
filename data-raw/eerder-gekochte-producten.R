# 1. Ga naar https://www.plus.nl/eerder-gekochte-producten
# 2. Scroll zo ver naar beneden als je wilt
# 3. Zoek het HTML element 'recently-bought-products__list' en kopieer het als HTML (zodat alle <a> elementen eronder gekopieerd worden)
# 4. Run onderstaand command

x <- clipr::read_clip()
items <- strsplit(x, "<a data-link")[[1]]

saveRDS(items, "data-raw/eerder_gekocht_raw_items.rds")

# eerste item is niks
items <- items[-1]

eerder_gekocht <- tibble()

for (i in seq_along(items)) {
  item <- items[i]
  url <- stringr::str_match(item, 'href="([^"]+)"')[,2]
  img <- gsub("[?].*$", "", stringr::str_match(item, 'src="([^"]+)"')[,2])
  name <- gsub("</span>.*$", "", stringr::str_match(item, '<span data-expression=\"\">([^"]+)"')[,2])
  eerder_gekocht[i, "name"] <- name
  eerder_gekocht[i, "url"] <- url
  eerder_gekocht[i, "img"] <- img
}

usethis::use_data(eerder_gekocht, overwrite = TRUE)
