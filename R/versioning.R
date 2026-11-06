# Supported GeoLocator-DP versions.
# This vector is the single source of truth for runtime and schema sync.
# `main` is a legacy alias read from packages GeoLocatoR wrote in 2024; it is
# upgraded from v0.1 and never written.
.gldp_supported_versions <- c(
  "main",
  "v0.1",
  "v0.2",
  "v0.3",
  "v0.4",
  "v0.5",
  "v0.6",
  "v1.0",
  "v1.1",
  "v1.2",
  "v1.3"
)

#' Data Package profile of a tabular resource descriptor
#'
#' GeoLocator-DP requires this on its tabular resources from v1.1.
#'
#' @noRd
.gldp_data_resource_profile <- "https://datapackage.org/profiles/2.0/dataresource.json"

.gldp_default_version <- utils::tail(.gldp_supported_versions, 1)

#' Get GeoLocator DP version
#'
#' @description
#' Extract the GeoLocator-DP version from `` x$`$schema` ``.
#'
#' The function aborts if `$schema` is missing, cannot be parsed, or refers to
#' an unsupported version, including a development (`dev`) schema.
#'
#' @param x A GeoLocator Data Package object.
#' @return Character string version (e.g. `"v1.0"`).
#' @family misc functions
#' @export
gldp_version <- function(x) {
  check_gldp(x)

  schema <- purrr::pluck(x, "$schema", .default = NULL)

  if (is.null(schema) || !nzchar(schema)) {
    cli_abort("{.field $schema} is missing or empty, cannot determine the GeoLocator-DP version.")
  }

  # The profile accepts a short ref (`v1.2`) or a tag ref (`refs/tags/v1.2`).
  m <- regexec("GeoLocator-DP/(?:refs/tags/)?([0-9A-Za-z.-]+)/", schema, perl = TRUE)
  out <- regmatches(schema, m)[[1]]

  if (length(out) < 2) {
    cli_abort("Cannot extract version from {.field $schema}: {.val {schema}}.")
  }

  version <- out[2]

  if (identical(version, "dev")) {
    cli_abort(c(
      "x" = "{.field $schema} points to the GeoLocator-DP {.val dev} branch: {.val {schema}}.",
      "i" = "Development schemas are not supported; use a released version such as {.val {(.gldp_default_version)}}."
    ))
  }

  if (!version %in% .gldp_supported_versions) {
    cli_abort(
      c(
        "Unsupported GeoLocator-DP version: {.val {version}}.",
        "i" = "Supported versions are: {.val {(.gldp_supported_versions)}}."
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

  # `main` is readable as a legacy alias but no package is written against it.
  released <- setdiff(.gldp_supported_versions, "main")
  if (!version %in% released) {
    cli_abort(c(
      "!" = "Unsupported GeoLocator-DP version {.val {version}}.",
      "i" = "Allowed versions are {.val {released}}."
    ))
  }

  schema_url <- glue::glue(
    "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/{version}/geolocator-dp-profile.json"
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

#' Resource branches declared by a GeoLocator-DP profile
#'
#' @description
#' A profile constrains each resource through a list of alternatives keyed on
#' the resource `name`: one for the fixed tables, one for `params`, one for
#' custom resources.
#'
#' Up to GeoLocator-DP v1.0 that list is a `oneOf`. From v1.1 it is an `allOf`
#' of `if`/`then` pairs, so a validator can report the property at fault rather
#' than only that the resource matched no branch. Both shapes are read here
#' because `upgrade_gldp()` walks packages written against any earlier version.
#'
#' @param profile A GeoLocator-DP profile, as returned by
#'   `gldp_profile_schema()`.
#' @return List of branch schemas, tables first.
#' @noRd
gldp_profile_resource_branches <- function(profile) {
  items <- profile$allOf[[2]]$properties$resources$items

  if (!is.null(items$oneOf)) {
    return(items$oneOf)
  }

  purrr::map(items$allOf %||% list(), ~ .x$then)
}

#' Resource names a GeoLocator-DP profile declares
#'
#' @param profile A GeoLocator-DP profile.
#' @return Character vector of names, in the order the profile declares them.
#'   Custom resources contribute nothing, having no fixed name.
#' @noRd
gldp_profile_resource_names <- function(profile) {
  gldp_profile_resource_branches(profile) |>
    purrr::map(
      ~ .x$properties$name$enum %||% .x$properties$name$const %||% character(0)
    ) |>
    purrr::list_c(ptype = character())
}

#' Table names a GeoLocator-DP profile declares
#'
#' @param profile A GeoLocator-DP profile.
#' @return Character vector of the fixed table names, excluding `params` and
#'   custom resources.
#' @noRd
gldp_profile_table_names <- function(profile) {
  # The first branch is the one for the fixed tables.
  gldp_profile_resource_branches(profile)[[1]]$properties$name$enum
}
