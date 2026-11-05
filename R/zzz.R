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

# Escape cli/glue braces in free text so field values are printed literally.
cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", as.character(x), fixed = TRUE), fixed = TRUE)
}

# Returns TRUE when a value contains at least one meaningful entry.
has_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(FALSE)
  }
  if (is.character(value)) {
    return(any(!is.na(value) & nzchar(trimws(value))))
  }
  if (is.atomic(value)) {
    return(!all(is.na(value)))
  }
  TRUE
}
