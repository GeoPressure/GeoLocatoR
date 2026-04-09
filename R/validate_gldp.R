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
#'    The package metadata is checked against the GeoLocator-DP profile schema. This includes
#'    required package-level fields and schema-defined constraints such as type, format,
#'    uniqueness, enumerated values, regular-expression patterns, and minimum or maximum
#'    lengths, item counts, and numeric values.
#'
#' 2. **Resource validation**
#'    Resources are checked against their schema: columns must match the
#'    `fieldsMatch` rule, and each column must satisfy its declared constraints
#'    such as type, format, requiredness, uniqueness, allowed values, patterns,
#'    and numeric or length limits.
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
#'    - `equipment` and `retrieval` observations have `device_status == "present"`;
#'    - `capture` observations with `device_status` `"missing"` or `"present"` have a `tag_id`;
#'    - a second tag is not attached without a prior retrieval or `capture` with
#'      `device_status == "missing"`;
#'    - each `tag_id` has an `equipment` observation;
#'    - retrieval does not precede equipment for the same `tag_id`;
#'    - duplicated observations are not present for the same `ring_number`, `datetime`,
#'      and `observation_type`;
#'    - `device_status` transitions are plausible across the observation history.
#'
#'    GeoPressure-specific coherence checks are applied when the corresponding resources are
#'    present. These include:
#'    - realistic ranges for `staps$stap_id`, `paths$j`, `edges$n`, and
#'      `pressurepaths$altitude`;
#'    - realistic year windows for datetime fields in `staps`, `twilights`,
#'      `pressurepaths`, and `edges`;
#'    - duration checks for `staps` and `edges`;
#'    - schema-declared foreign key consistency across resources;
#'    - warnings for unusual `edges` distances, durations, or wind/ground-speed magnitudes.
#'
#' 4. **Metadata recommendations**
#'    Additional metadata quality checks are reported without affecting the final validity.
#'    These recommendations cover:
#'    - the `title` is prefixed by "GeoLocator Data Package: ";
#'    - presence of contributor roles;
#'    - `record_type == "dataset"`;
#'    - expected manufacturer names in `tags$manufacturer`;
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
#' pkg <- read_zenodo("17367319", quiet = TRUE)
#' validate_gldp(pkg)
#'
#' @export
validate_gldp <- function(pkg) {
  check_gldp(pkg)

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

#' Validate GeoLocator Data Package profile
#'
#' Internal helper that validates package-level metadata against the GeoLocator-DP
#' profile schema. Resource definitions are excluded here and validated separately.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Invisibly returns `TRUE` when the package-level profile validation passes,
#'   `FALSE` otherwise.
#' @noRd
validate_gldp_profile <- function(pkg) {
  cli_h3("Check Profile")
  version <- gldp_version(pkg)

  pkg_schema <- gldp_profile_schema(version)

  required <- unlist(pkg_schema$allOf[[2]]$required)
  properties <- pkg_schema$allOf[[2]]$properties

  # Skip resource validation at the profile level (handled separately)
  required <- setdiff(required, "resources")
  properties$resources <- NULL

  # Ignore all additional field not present in the schema
  ignore_fields <- setdiff(names(pkg), names(properties))

  valid <- validate_gldp_object(
    pkg,
    required,
    properties,
    defs = pkg_schema$`$defs`,
    ignore_fields = ignore_fields
  )

  invisible(valid)
}

#' Validate GeoLocator Data Package resources
#'
#' Internal helper that validates each package resource against its own resource
#' schema. Resources must be tabular data resources with a declared schema and
#' in-memory data.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Invisibly returns `TRUE` when all resources can be validated and pass
#'   their schema checks, `FALSE` otherwise.
#' @noRd
validate_gldp_resources <- function(pkg) {
  pkg <- update_gldp_order_resources(pkg)

  valid <- TRUE

  if (length(pkg$resources) == 0) {
    cli_alert_danger("Package must contain at least one resource.")
    valid <- FALSE
  }

  for (i in seq_along(pkg$resources)) {
    resource <- pkg$resources[[i]]

    is_tabular_resource <- identical(resource$profile, "tabular-data-resource") &&
      is.list(resource$schema) &&
      !is.null(resource$schema$fields)

    if (is_tabular_resource) {
      valid <- valid & validate_gldp_table(resource$data, resource$schema)
    } else {
      cli_h3("Check Resources {.field {resource$name}}")
      cli::cli_alert_warning("Could not check {.field {resource$name}}")
      valid <- FALSE
    }
  }

  invisible(valid)
}

#' Validate a tabular resource against its schema
#'
#' Internal helper that checks a single resource table against its field
#' definitions. It validates data availability, column matching through the
#' schema `fieldsMatch` rule, and per-field constraints.
#'
#' @param data A data frame stored in the resource.
#' @param schema The frictionless schema describing the resource.
#'
#' @return Invisibly returns `TRUE` when the table is consistent with the schema,
#'   `FALSE` otherwise.
#' @noRd
validate_gldp_table <- function(data, schema) {
  cli_h3("Check Resources {.field {schema$name}}")

  if (is.null(data)) {
    cli_alert_danger("data is not available.")
    return(FALSE)
  }

  schema_fields <- stats::setNames(
    schema$fields,
    sapply(schema$fields, \(x) x$name)
  )

  valid <- validate_gldp_fields_match(
    names(schema_fields),
    names(data),
    schema$fieldsMatch[[1]]
  )

  fields <- intersect(names(schema_fields), names(data))

  for (field in fields) {
    prop <- schema_fields[[field]]
    if (!is.null(prop$constraints)) {
      prop <- c(prop, prop$constraints)
      prop$constraints <- NULL # Remove the 'constraints' field after merging
    }

    field_name <- glue::glue("{schema$name}${field}")
    prop <- resolve_prop(data[[field]], prop, defs = NULL, field = field_name)

    valid <- valid & validate_gldp_item(data[[field]], prop, field_name)
  }

  if (valid) {
    cli_alert_success(
      "Table {.field {schema$name}} is consistent with the schema."
    )
  }

  invisible(valid)
}
