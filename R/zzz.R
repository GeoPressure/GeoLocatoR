#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @noRd
is_non_empty_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' @noRd
first_non_empty_string <- function(...) {
  values <- list(...)
  for (value in values) {
    scalar <- as.character(value %||% NA_character_)[1]
    if (is_non_empty_string(scalar)) {
      return(scalar)
    }
  }
  NULL
}
