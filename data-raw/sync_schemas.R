# Manual maintainer script:
# - Refresh local GeoLocator-DP schemas bundled in inst/schemas.
# - Run this when a GeoLocator-DP schema version/branch changes.
# - Runtime code is offline-only and reads schemas from inst/schemas.
# - Versions synced are taken from .gldp_supported_versions in R/versioning.R.
# - `main` is excluded (only released version schemas are stored locally).
# - Existing local schema folders are deleted before download (full overwrite).
# Usage:
# source("data-raw/sync_schemas.R")
# sync_gldp_schemas()

source("R/versioning.R")

sync_gldp_schemas <- function(
  versions = setdiff(.gldp_supported_versions, "main"),
  repo = "GeoPressure/GeoLocator-DP",
  root = "inst/schemas"
) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(root, "main"), recursive = TRUE, force = TRUE)

  for (version in versions) {
    out_dir <- file.path(root, version)
    unlink(out_dir, recursive = TRUE, force = TRUE)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    base_url <- glue::glue("https://raw.githubusercontent.com/{repo}/{version}")
    profile_url <- glue::glue("{base_url}/geolocator-dp-profile.json")
    profile_path <- file.path(out_dir, "geolocator-dp-profile.json")
    utils::download.file(profile_url, profile_path, mode = "wb", quiet = TRUE)

    profile <- jsonlite::fromJSON(
      profile_path,
      simplifyDataFrame = FALSE,
      simplifyVector = TRUE
    )

    one_of <- profile$allOf[[2]]$properties$resources$items$oneOf
    resource_names <- unique(unlist(lapply(one_of, function(x) {
      if (!is.null(x$properties$name$enum)) {
        x$properties$name$enum
      } else if (!is.null(x$properties$name$const)) {
        x$properties$name$const
      } else {
        character(0)
      }
    })))

    for (resource_name in resource_names) {
      schema_url <- glue::glue("{base_url}/{resource_name}-table-schema.json")
      schema_path <- file.path(out_dir, glue::glue("{resource_name}-table-schema.json"))
      tryCatch(
        utils::download.file(schema_url, schema_path, mode = "wb", quiet = TRUE),
        warning = function(...) NULL,
        error = function(...) NULL
      )
    }
  }

  invisible(root)
}
