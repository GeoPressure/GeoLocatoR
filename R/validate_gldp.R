#' Validate a GeoLocator Data Package
#'
#' @description
#' `validate_gldp()` runs the standard quality-control workflow for a GeoLocator Data Package.
#' It provides an automated check that a package is structurally valid and internally coherent
#' before publication or reuse.
#'
#' @details
#' Validation is organized in two complementary layers:
#' 1. **Technical conformance**: checks compliance with the GeoLocator Data Package specification,
#'    including required resources, schema conformity, required fields, and controlled vocabularies.
#' 2. **Coherence and metadata quality**: checks consistency of identifiers and relationships across
#'    key tables (e.g. `tags`, `observations`, and `measurements`) and reports metadata best-practice
#'    mismatches that should be justified during curation.
#'
#' See the [GeoLocator-DP curation guide](https://raphaelnussbaumer.com/GeoLocator-DP/curation/) for
#' more informations.
#'
#' @param pkg A GeoLocator Data Package object to be validated.
#'
#' @return A silent logical value indicating whether the package validation was successful (`TRUE`) or
#' failed (`FALSE`).
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
#' Internal helper function to validate that a GeoLocator Data Package conforms
#' to the expected profile schema.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Logical indicating whether the profile validation passed
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
#' Internal helper function to validate all resources within a GeoLocator Data Package
#' against their respective schemas.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Logical indicating whether all resource validations passed
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
