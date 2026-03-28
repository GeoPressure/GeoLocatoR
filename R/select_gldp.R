#' Select or drop tag IDs from a GeoLocator Data Package
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Keeps or removes rows linked to selected `tag_id` values in every resource that has
#' a `tag_id` column.
#'
#' Use a negative selection to drop tags:
#' `select_gldp(pkg, -"14AC")` or `select_gldp(pkg, -c("14AC", "15BC"))`.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param tag_id A character vector of tag IDs to keep, or a negative character
#'   selection to remove.
#'
#' @return A filtered GeoLocator Data Package object.
#' @export
select_gldp <- function(pkg, tag_id) {
  check_gldp(pkg)

  expr <- substitute(tag_id)
  exclude <- is.call(expr) && identical(expr[[1]], as.name("-"))
  if (exclude) {
    expr <- expr[[2]]
  }

  tag_id <- eval(expr, parent.frame())
  tag_id <- unique(as.character(tag_id))
  tag_id <- tag_id[!is.na(tag_id) & nzchar(trimws(tag_id))]

  if (length(tag_id) == 0) {
    cli::cli_abort("{.arg tag_id} must contain at least one non-empty value.")
  }

  available_tag_ids <- unique(as.character(tags(pkg)$tag_id))
  available_tag_ids <- available_tag_ids[!is.na(available_tag_ids)]

  missing_tag_ids <- setdiff(tag_id, available_tag_ids)
  if (length(missing_tag_ids) > 0) {
    cli::cli_abort(c(
      "x" = "Tag ID(s) not found in package: {.val {missing_tag_ids}}",
      "i" = "Available tag IDs: {.val {available_tag_ids}}"
    ))
  }

  keep_tag_ids <- if (exclude) setdiff(available_tag_ids, tag_id) else tag_id

  for (i in seq_along(pkg$resources)) {
    data <- pkg$resources[[i]]$data
    if (!is.data.frame(data) || !("tag_id" %in% names(data))) {
      next
    }

    pkg$resources[[i]]$data <- data[data$tag_id %in% keep_tag_ids, , drop = FALSE]
  }

  update_gldp(pkg)
}
