pkgload::load_all()
library(magrittr)

future::plan("multiprocess")

cutoff_date <- min(jeopardy_clues$date)

jeopardy_clues <- furrr::future_map_dfr(
  fs::dir_ls(here::here("data-raw/games")),
  vroom::vroom,
  col_types = "Dccciccc"
) %>%
  dplyr::select(-round) %>%
  dplyr::filter(date > cutoff_date, !is.na(clue), !is.na(response)) %>%
  dplyr::mutate(
    comment = comment %>%
      stringr::str_replace_all(c("^\\(\\w+: " = "", "^\\(" = "", "\\)$" = ""))
  )

jeopardy_clues <- dplyr::bind_rows(
  jeopardy_clues %>%
    dplyr::filter(!is.na(link)) %>%
    dplyr::filter(furrr::future_map_lgl(link, RCurl::url.exists)),
  jeopardy_clues %>%
    dplyr::filter(is.na(link))
) %>%
  dplyr::arrange(date, category, value)

usethis::use_data(jeopardy_clues, overwrite = TRUE, compress = "gzip")

while (
  fs::file_size(here::here("data/jeopardy_clues.Rda")) > fs::fs_bytes("4.9M")
) {
  avg_size <- fs::file_size(here::here("data/jeopardy_clues.Rda")) /
    length(unique(jeopardy_clues$date))

  difference <- ceiling(
    (
      fs::file_size(here::here("data/jeopardy_clues.Rda")) -
      fs::fs_bytes("4.9M")
    ) /
    avg_size
  )

  cutoff_date <- min(jeopardy_clues$date) + as.integer(difference)

  message("Trying a cutoff date of ", cutoff_date)

  jeopardy_clues <- jeopardy_clues %>%
    dplyr::filter(date > cutoff_date)

  usethis::use_data(jeopardy_clues, overwrite = TRUE, compress = "gzip")
}
