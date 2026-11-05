#' Select or drop tag IDs from a GeoLocator Data Package
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Keeps or removes rows linked to selected `tag_id` values in every resource that has
#' a `tag_id` column. Observations without `tag_id` are kept when their `ring_number`
#' belongs to a kept tag. The `params` of removed tags are dropped.
#'
#' Use a negative selection to drop tags:
#' `select_gldp(pkg, -"14AC")` or `select_gldp(pkg, -c("14AC", "15BC"))`.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param tag_id A character vector of tag IDs to keep, or a negative character
#'   selection to remove.
#'
#' @return A filtered GeoLocator Data Package object with derived properties
#'   recomputed via [update_gldp()].
#'
#' @seealso [tags()] to inspect available tag IDs and [update_gldp()] for the
#'   derived properties recomputed after filtering.
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

  tags_data <- tags(pkg)
  available_tag_ids <- unique(as.character(tags_data$tag_id))
  available_tag_ids <- available_tag_ids[!is.na(available_tag_ids)]

  missing_tag_ids <- setdiff(tag_id, available_tag_ids)
  if (length(missing_tag_ids) > 0) {
    cli::cli_abort(c(
      "x" = "Tag ID(s) not found in package: {.val {missing_tag_ids}}",
      "i" = "Available tag IDs: {.val {available_tag_ids}}"
    ))
  }

  keep_tag_ids <- if (exclude) setdiff(available_tag_ids, tag_id) else tag_id
  keep_rings <- stats::na.omit(tags_data$ring_number[tags_data$tag_id %in% keep_tag_ids])

  for (i in seq_along(pkg$resources)) {
    data <- pkg$resources[[i]]$data
    # File-backed tables need the same filtering and schema types as loaded tables.
    if (is.null(data)) {
      data <- gldp_cast_table(
        gldp_read_resource(pkg, pkg$resources[[i]]$name),
        pkg$resources[[i]]$schema
      )
    }
    if (!is.data.frame(data) || !("tag_id" %in% names(data))) {
      next
    }

    keep <- data$tag_id %in% keep_tag_ids
    # Observations without tag_id (e.g. a capture before deployment) follow the bird's ring.
    if ("ring_number" %in% names(data)) {
      keep <- keep | (is.na(data$tag_id) & data$ring_number %in% keep_rings)
    }
    pkg$resources[[i]][["data"]] <- data[keep, , drop = FALSE]
    # Store the selected rows in memory; a resource cannot carry both data and path.
    pkg$resources[[i]][["path"]] <- NULL
  }
  # Drop the GeoPressureR parameters of the removed tags.
  pkg[["params"]] <- purrr::keep(
    pkg$params,
    \(param) !param$id %in% setdiff(available_tag_ids, keep_tag_ids)
  )

  update_gldp(pkg)
}
