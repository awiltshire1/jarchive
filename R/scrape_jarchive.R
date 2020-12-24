#' Scrape a set of Jeopardy! clues and responses from J! Archive
#'
#' @param id The final digits of a J! Archive URL representing a game's ID.
#' @param verbose A logical indicating whether to message the game ID that is
#'   being scraped and whether the game is skipped.
#'
#' @return A [tibble][tibble::tibble]
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' scrape_jarchive(6895)

scrape_jarchive <- function(id, verbose = FALSE) {
  # Initialize global variables to avoid CRAN NOTE
  cat_id <- category <- clue <- link <- response <- value <- NULL

  url <- paste0("http://www.j-archive.com/showgame.php?game_id=", id)

  html <- url %>%
    polite::bow() %>%
    polite::scrape()

  date <- html %>%
    rvest::html_node("#game_title") %>%
    rvest::html_text() %>%
    stringr::str_extract("\\w+ \\d+, \\d+$")

  if (verbose) {message("Scraping game ", id, " (", date, ")")}

  rounds <- rvest::html_nodes(html, ".round")

  if (!length(rounds)) {
    if (verbose) {message("Skipping game ", id, " (", date, ")")}

    return(
      tibble::tibble(
        date     = character(0),
        round    = character(0),
        category = character(0),
        comment  = character(0),
        value    = character(0),
        clue     = character(0),
        response = character(0),
        link     = character(0)
      )
    )
  }

  `%xmod%` <- function(lhs, rhs) {
    res           <- lhs %% rhs
    res[res == 0] <- rhs
    res
  }

  categories <- rounds %>%
    rvest::html_nodes(".category_name") %>%
    rvest::html_text() %>%
    tibble::enframe(name = "id", value = "category") %>%
    dplyr::mutate(
      round = incase::switch_case(
        (id + 5) %/% 6, 1 ~ "J", 2 ~ "DJ", 3 ~ "FJ"
      ),
      cat_id  = as.character(id %xmod% 6),
      id      = NULL,
      comment = rvest::html_text(
        rvest::html_nodes(rounds, ".category_comments")
      )
    )

  round_clues <- rounds %>%
    rvest::html_nodes(".clue") %>%
    purrr::map_dfr(
      ~ tibble::tibble(
        id       = rvest::html_attr(rvest::html_node(., ".clue_text"), "id"),
        value    = rvest::html_text(rvest::html_node(., ".clue_value")),
        clue     = rvest::html_text(rvest::html_node(., ".clue_text")),
        response = rvest::html_attr(rvest::html_node(., "div"), "onmouseover"),
        link     = rvest::html_attr(
          rvest::html_node(., ".clue_text > a:last-child"), "href"
        )
      )
    ) %>%
    dplyr::mutate(
      cat_id = stringr::str_match(id, "clue_(.+)_(.+)_(.+)"),
      round  = cat_id[, 2],
      cat_id = cat_id[, 3]
    ) %>%
    dplyr::left_join(categories, by = c("cat_id", "round")) %>%
    dplyr::select(-cat_id)

  final_jeopardy <- html %>%
    rvest::html_nodes(".final_round") %>%
    purrr::map_dfr(
      ~ tibble::tibble(
        round = rvest::html_node(., ".clue_text") %>%
          rvest::html_attr("id") %>%
          stringr::str_extract("[A-Z]{2}"),
        category = rvest::html_text(rvest::html_node(., ".category_name")),
        comment  = rvest::html_text(rvest::html_node(., ".category_comments")),
        value    = rvest::html_text(rvest::html_node(., ".clue_value")),
        clue     = rvest::html_text(rvest::html_node(., ".clue_text")),
        response = rvest::html_attr(rvest::html_node(., "div"), "onmouseover"),
        link     = rvest::html_attr(
          rvest::html_node(., ".clue_text > a:last-child"), "href"
        )
      )
    )

  clues <- dplyr::bind_rows(round_clues, final_jeopardy) %>%
    dplyr::mutate(
      response = response %>%
        stringr::str_match(".*<em class=.*correct_response.*?>(.*)</em>.*") %>%
        magrittr::extract(, 2) %>%
        stringr::str_replace_all("</?i>", ""),
      dplyr::across(c(clue, response), dplyr::na_if, "="),
      round = round,
      value = readr::parse_number(value),
      date  = lubridate::mdy(date)
    ) %>%
    dplyr::select(date, round, category, comment, value, clue, response, link)
}
