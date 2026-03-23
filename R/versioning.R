# Supported GeoLocator-DP versions.
# This vector is the single source of truth for runtime and schema sync.
.gldp_supported_versions <- c("main", "v0.1", "v0.2", "v0.3", "v0.4", "v0.5", "v0.6", "v1.0")
.gldp_default_version <- utils::tail(.gldp_supported_versions, 1)

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

  if (!version %in% .gldp_supported_versions) {
    cli_abort(
      c(
        "Unsupported GeoLocator-DP version: {.val {version}}.",
        "i" = "Supported versions are: {.val {.gldp_supported_versions}}."
      )
    )
  }

  version
}

#' @noRd
gldp_schema_url <- function(version = NULL) {
  if (is.null(version) || is.na(version)) {
    version <- .gldp_default_version
  }

  if (!version %in% .gldp_supported_versions) {
    cli_abort(c(
      "!" = "Unsupported GeoLocator-DP version {.val {version}}.",
      "i" = "Allowed versions are {.val {.gldp_supported_versions}}."
    ))
  }

  schema_url <- glue::glue(
    "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/{version}/geolocator-dp-profile.json"
  ) |>
    as.character()

  schema_url
}

#' @noRd
gldp_schema_file <- function(version, file) {
  # Runtime schema resolution is offline-only from inst/schemas.
  schema_path <- system.file("schemas", version, file, package = "GeoLocatoR")
  if (!nzchar(schema_path)) {
    cli_abort(
      c(
        "x" = "Missing local schema file for version {.val {version}}.",
        "i" = "Expected file: {.file inst/schemas/{version}/{file}}.",
        "i" = "Run the schema sync script to refresh local schemas."
      )
    )
  }
  schema_path
}

#' @noRd
gldp_profile_schema <- function(version) {
  # Read profile schema JSON for a given version from local bundled files.
  jsonlite::fromJSON(
    gldp_schema_file(version, "geolocator-dp-profile.json"),
    simplifyDataFrame = FALSE,
    simplifyVector = TRUE
  )
}

#' @noRd
gldp_resource_schema <- function(version, resource_name) {
  # Read resource table schema JSON for a given version from local bundled files.
  jsonlite::fromJSON(
    gldp_schema_file(version, glue::glue("{resource_name}-table-schema.json")),
    simplifyDataFrame = FALSE,
    simplifyVector = TRUE
  )
}
