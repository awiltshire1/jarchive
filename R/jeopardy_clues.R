#' Jeopardy! clues
#'
#' A dataset containing clues and correct responses for 2660 games of
#' *Jeopardy!* since 2013.
#'
#' @format A data frame with 98,333 rows and 7 variables:
#' * `date`: The episode's airdate.
#' * `category`: The clue's category.
#' * `comment`: Any helpful comments announced to explain the category.
#' * `value`: The clue's dollar value. If `NA`, the clue is either a Daily
#'   Double or Final Jeopardy question, where contestants can wager a dollar
#'   value, or a Tiebreaker question, which has no dollar value.
#' * `clue`: The clue text or "answer".
#' * `response`: The correct response, which should be phrased in the form of
#'   a question.
#' * `link`: A link to a visual or audio file which appeared alongside the clue.
#'
#' @source http://www.j-archive.com/
"jeopardy_clues"
