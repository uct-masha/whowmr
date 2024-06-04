#' Helper function useful when mapping values in character strings
#' @param .x a vector to match against
#' @param dict a named list - the names should correspond to the values in .x
#'             and the values should be the new values
#'
#' @examples
#' df |> dplyr::mutate(Footnotes=case_match_dict(Footnotes, dict=c("2"="footnote1", "5,6"="footnote 5. footnote6."))
case_match_dict <- function(.x, dict, trimFirst=TRUE) {
  if (trimFirst) {
    .x <- gsub("^\\s+|\\s+$", "", .x)
  }
  fnMap <- as.list(dict)
  factor(unname(fnMap[.x]), levels = unname(fnMap))
}

