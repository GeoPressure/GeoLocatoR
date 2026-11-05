#' Validate a GeoLocator Data Package
#'
#' @description
#' `validate_gldp()` runs the full validation workflow for a GeoLocator Data Package.
#' It checks schema conformance, resource-level consistency, and cross-table coherence,
#' then reports additional metadata recommendations.
#'
#' @details
#' Validation runs in four steps:
#'
#' 1. **Package profile validation**
#'    The package-level properties the GeoLocator-DP profile declares are checked against
#'    it. From GeoLocator-DP v1.0 the profile declares only `$schema`, so this checks that
#'    `$schema` is a GeoLocator-DP profile URL. Earlier profiles also declare package
#'    metadata, checked for presence, type, format, enumerated values, regular-expression
#'    patterns, item counts, and numeric bounds. Neither the Data Package profile that
#'    GeoLocator-DP extends nor the profile's rules on resource descriptors (`type`, `path`,
#'    `$schema`, `schema`, at least three resources) are checked.
#'
#' 2. **Resource validation**
#'    File-backed tables are loaded for field and foreign-key checks; source files
#'    are not modified.
#'    Resources are checked against their schema: columns must match the
#'    `fieldsMatch` rule, and each column must satisfy its declared constraints
#'    such as type, requiredness, uniqueness, allowed values, patterns,
#'    and numeric or length limits. Rows must be unique on the schema `primaryKey`.
#'    The tables GeoLocator-DP defines (`tags`, `observations`, ...) must be tabular
#'    resources with a table schema. The `params` resource and custom resources are
#'    checked when they carry a table schema, and are otherwise reported as not checked.
#'
#' 3. **Coherence validation**
#'    Cross-resource consistency checks are then applied.
#'
#'    Core package coherence checks require the resources `tags`, `observations`, and
#'    `measurements`, and verify that:
#'    - one `ring_number` is not associated with multiple `scientific_name` values in `tags`;
#'    - every `measurements$tag_id` exists in `tags$tag_id`;
#'    - every `tags$ring_number` appears in `observations$ring_number`;
#'    - every `observations` `tag_id` / `ring_number` combination exists in `tags`;
#'    - every tag declared in `tags` has at least one observation (reported as a warning only);
#'    - every tag with measurement data has both an `equipment` and a `retrieval` observation.
#'
#'    Observation-sequence checks verify that:
#'    - one `tag_id` is not associated with multiple `ring_number` values;
#'    - `equipment` and `retrieval` observations always have a `tag_id`;
#'    - `equipment` and `retrieval` observations have `device_status == "present"`
#'      (a missing `device_status` fails);
#'    - `device_status` `"none"` and `"missing"` are used only for `capture` observations,
#'      and `"unknown"` only for `sighting` observations;
#'    - `capture` observations with `device_status` `"missing"` or `"present"` have a `tag_id`;
#'    - a second tag is not attached without a prior retrieval or `capture` with
#'      `device_status == "missing"`;
#'    - each `tag_id` has an `equipment` observation;
#'    - retrieval does not precede equipment for the same `tag_id`;
#'    - duplicated observations are not present for the same `ring_number`, `datetime`,
#'      and `observation_type`;
#'    - `device_status` transitions are plausible across the observation history;
#'    - `mass` above 10,000 g is reported as a warning only, a guideline rather than a limit.
#'
#'    GeoPressure-specific coherence checks are applied when the corresponding resources are
#'    present. These include:
#'    - realistic ranges for `edges$n`;
#'    - duration checks for `staps` and `edges`;
#'    - schema-declared foreign key consistency across all resources, core ones included;
#'    - warnings for unusual `edges` distances, durations, or wind/ground-speed magnitudes,
#'      for `pressurepaths$altitude` outside -200 to 10,000 m, for `staps$stap_id` of 300
#'      or more, for `paths$j` of 500 or more, and for datetime fields in `staps`,
#'      `twilights`, `pressurepaths`, and `edges` dated before 1990 or after the current
#'      year. These are guidelines, not limits.
#'
#' 4. **Metadata recommendations**
#'    Additional metadata quality checks are reported without affecting the final validity.
#'    These recommendations cover:
#'    - the `title` is prefixed by "GeoLocator Data Package: ", which the GeoLocator-DP
#'      Zenodo community expects;
#'    - presence of contributor roles;
#'    - `record_type == "dataset"`;
#'    - expected manufacturer names in `tags$manufacturer`: those GeoPressureR can import
#'      ("Swiss Ornithological Institute", "Migrate Technology Limited", "Lund University",
#'      "British Antarctic Survey", "PresTag");
#'    - presence of `relatedIdentifiers`;
#'    - presence of a selected GeoLocator-DP Zenodo community;
#'    - a GitHub repository URL in `codeRepository`;
#'    - scientific names in `pkg$taxonomic` against the eBird taxonomy API when available.
#'
#' See the [GeoLocator-DP curation guide](https://geopressure.org/GeoLocator-DP/curation/) for
#' more information.
#'
#' @param pkg A GeoLocator Data Package object to be validated.
#'
#' @return An invisible logical value: `TRUE` when all profile, resource, and coherence checks
#' pass, `FALSE` otherwise. Metadata recommendations are always reported but do not affect this
#' return value.
#'
#' @examples
#' \dontrun{
#' pkg <- read_zenodo("17367319", quiet = TRUE)
#' validate_gldp(pkg)
#' }
#'
#' @export
validate_gldp <- function(pkg) {
  check_gldp(pkg)
  pkg <- gldp_materialize_tables(pkg)

  valid <- validate_gldp_profile(pkg)

  valid <- valid & validate_gldp_resources(pkg)

  valid <- valid & validate_gldp_coherence(pkg)

  # Metadata recommendations are advisory and do not change overall validity.
  validate_gldp_meta(pkg)

  cli_h1("Final:")
  if (valid) {
    cli_alert_success("Package validation succeeded.")
  } else {
    cli_alert_danger("Package validation failed. Review messages above.")
  }

  invisible(valid)
}

#' Validate GeoLocator Data Package resources
#'
#' Internal helper that loads file-backed tables and validates each tabular
#' resource against its own table schema. The tables GeoLocator-DP defines must
#' be tabular; other resources without a table schema are reported, not checked.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Invisibly returns `TRUE` when all GeoLocator-DP tables can be validated
#'   and all tabular resources pass their schema checks, `FALSE` otherwise.
#' @noRd
validate_gldp_resources <- function(pkg) {
  pkg <- gldp_materialize_tables(pkg)
  pkg <- gldp_update_order_resources(pkg)

  valid <- TRUE

  if (length(pkg$resources) == 0) {
    cli_alert_danger("Package must contain at least one resource.")
    valid <- FALSE
  }

  # Only the tables GeoLocator-DP defines must be tabular. `params` and custom
  # resources may be any file, so they have no table schema to check against.
  tables <- setdiff(gldp_profile_resource_names(gldp_profile_schema(gldp_version(pkg))), "params")

  for (i in seq_along(pkg$resources)) {
    resource <- pkg$resources[[i]]

    # `type: "table"` is the Data Package v2 spelling, `profile:
    # "tabular-data-resource"` the v1 one. Accept either so packages written with
    # GeoLocatoR < 1.4.0 (frictionless 1.x) keep validating.
    is_tabular_resource <- (identical(resource$type, "table") ||
      identical(resource$profile, "tabular-data-resource")) &&
      is.list(resource$schema) &&
      !is.null(resource$schema$fields)

    if (is_tabular_resource) {
      valid <- valid & validate_gldp_table(resource$data, resource$schema, resource$name)
    } else if (resource$name %in% tables) {
      cli_h3("Check Resources {.field {resource$name}}")
      cli_alert_danger(
        "{.field {resource$name}} is not a tabular resource with a table schema, so it could not be checked."
      )
      valid <- FALSE
    } else {
      cli_h3("Check Resources {.field {resource$name}}")
      cli_alert_info("{.field {resource$name}} has no table schema and is not checked.")
    }
  }

  invisible(valid)
}
