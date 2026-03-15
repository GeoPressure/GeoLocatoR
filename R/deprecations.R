#' Deprecated: `config2tibble()`
#'
#' @description
#' `config2tibble()` was renamed to [config_to_tibble()].
#'
#' @param file Character string specifying the path to the `config.yml` file.
#' @param filter_return Logical. If `TRUE`, only columns that vary across tags are returned.
#' @rdname config_to_tibble
#' @export
config2tibble <- function(
  file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
  filter_return = TRUE
) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "config2tibble()",
    with = "config_to_tibble()"
  )
  config_to_tibble(file = file, filter_return = filter_return)
}
