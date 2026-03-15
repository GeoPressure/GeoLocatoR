# Default GeoLocator-DP schema version used when creating/upgrading packages.
.gldp_supported_versions <- c("main", "v0.1", "v0.2", "v0.3", "v0.4", "v0.5", "v0.6", "v1.0")
#' Get GeoLocator DP version
#'
#' @description
#' Extract the GeoLocator-DP version from `x$schema`.
#'
#' @param x A GeoLocator Data Package object.
#' @return Character string version (e.g. `"v1.0"`), or `NA` when not parseable.
#' @family misc functions
#' @export
gldp_version <- function(x) {
  check_gldp(x)
  supported_versions <- .gldp_supported_versions

  schema <- purrr::pluck(x, "$schema", .default = NULL)

  if (is.null(schema) || !nzchar(schema)) {
    cli_abort("{.field $schema} is missing or empty, cannot determine the GLDP version.")
  }

  m <- regexec("GeoLocator-DP/([0-9A-Za-z.-]+)/", schema)
  out <- regmatches(schema, m)[[1]]

  if (length(out) < 2) {
    cli_abort("Cannot extract version from {.field $schema}: {.val {schema}}.")
  }

  version <- out[2]

  if (!version %in% supported_versions) {
    cli_abort(
      c(
        "Unsupported GeoLocator-DP version: {.val {version}}.",
        "i" = "Supported versions are: {.val {supported_versions}}."
      )
    )
  }

  version
}
