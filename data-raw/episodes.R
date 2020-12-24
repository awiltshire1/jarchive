pkgload::load_all()

jarchive <- polite::bow("http://www.j-archive.com")

newest_id <- jarchive %>%
  polite::nod("listseasons.php") %>%
  polite::scrape() %>%
  rvest::html_node("#content > table > tr:first-child > td > a") %>%
  rvest::html_attr("href") %>%
  polite::nod(bow = jarchive) %>%
  polite::scrape() %>%
  rvest::html_node("#content > table > tr:first_child > td > a") %>%
  rvest::html_attr("href") %>%
  stringr::str_match(".*=(\\d+)") %>%
  magrittr::extract(, 2)

ids <- 1:newest_id

scraped_ids <- fs::dir_ls(here::here("data-raw/games")) %>%
  fs::path_file() %>%
  stringr::str_extract("\\d+") %>%
  as.integer()

new_ids <- ids[!ids %in% scraped_ids]

purrr::walk(
  new_ids,
  ~ scrape_jarchive(.x, verbose = TRUE) %>%
    readr::write_csv(here::here("data-raw", "games", paste0(.x, ".csv")))
)
