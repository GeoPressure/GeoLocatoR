#' Add a GeoLocator Data Resource
#'
#' @description
#' Add a standard GeoLocator-DP resource to a package. This essentially wraps
#' [frictionless::add_resource()] but with prepartions steps of the data:
#' - extending the schema with extra columns present in `data` when allowed by
#'   `fieldsMatch`;
#' - adding missing schema columns to `data` with typed `NA` values when
#'   needed;
#' - reordering `data` columns to match the schema, because
#'   [frictionless::add_resource()] expects fields in schema order.
#' - casting columns to schema types when `cast_type = TRUE`, for example
#'   converting strings to dates, numbers, or integers when required by the
#'   schema, so the stored resource is consistent with the declared schema.
#'
#' For common package resources, the resource accessors are usually a more
#' convenient user-facing interface.
#'
#' @param pkg A GeoLocator Data Package object to which the resource will be added.
#' @param resource_name A character string specifying the name of the resource. This name is used
#' to locate the schema file.
#' @param data A data frame containing the data to be added as a resource. The data frame will be
#' adjusted according to the schema.
#' @param cast_type A logical value indicating whether the data frame should be cast to the types
#' specified in the schema. Defaults to `FALSE`.
#' @inheritParams frictionless::add_resource
#'
#' @return The updated GeoLocator Data Package object with the new resource added.
#'
#' @seealso [tags()], [observations()], and the other resource accessors for the
#'   usual user-facing way to modify standard GeoLocator-DP resources.
#'
#' @export
add_gldp_resource <- function(
  pkg,
  resource_name,
  data,
  cast_type = FALSE,
  replace = FALSE,
  delim = ","
) {
  check_gldp(pkg)
  version <- gldp_version(pkg)

  # Retrieve full schema (pkg$resources) does not have schema at first
  pkg_schema <- gldp_profile_schema(version)
  possible_gldp_resources <-
    pkg_schema$allOf[[2]]$properties$resources$items$oneOf[[
      1
    ]]$properties$name$enum

  if (!resource_name %in% possible_gldp_resources) {
    cli_abort(c(
      "x" = "{.val {resource_name}} is not a supported GeoLocatoR resource for {.fn add_gldp_resource}.",
      "i" = "Supported GeoLocatoR resources are: {.val {possible_gldp_resources}}.",
      "i" = "To add custom resources, use {.fn frictionless::add_resource} directly."
    ))
  }

  # Retrieve the resource schema

  schema <- gldp_resource_schema(version, resource_name)

  # We need to massage a bit the data to make it adequate for add_resource in v1.
  # https://github.com/frictionlessdata/frictionless-r/issues/254

  schema_fields <- sapply(schema$fields, \(x) x$name)
  schema_types <- sapply(schema$fields, \(x) x$type)
  # schema_required <- sapply(schema$fields, \(x) x$constraints$required)
  # data_fields <- names(data)

  if (schema$fieldsMatch == "equal") {
    # The data source MUST have exactly the same fields as defined in the fields array.
    # Fields MUST be mapped by their names.
    # NOT SURE THE ORDER BY NAME IS CHECKED!!!
  }

  if (schema$fieldsMatch == "subset" || schema$fieldsMatch == "partial") {
    # The data source MUST have all the fields defined in the fields array, but MAY have more.
    # Fields MUST be mapped by their names.

    # Create a schema from the data (adding all possible field)
    schema_data <- frictionless::create_schema(data)

    # Add fields not existing in the initial schema
    for (f in schema_data$fields) {
      if (!(f$name %in% schema_fields)) {
        schema$fields <- append(schema$fields, list(f))
      }
    }

    # Update schema_fields
    schema_fields <- sapply(schema$fields, \(x) x$name)
  }

  if (schema$fieldsMatch == "superset" || schema$fieldsMatch == "partial") {
    # superset: The data source MUST only have fields defined in the fields array, but MAY have
    # fewer. Fields MUST be mapped by their names.

    for (i in seq_along(schema_fields)) {
      # Check if column already exists
      if (!(schema_fields[i] %in% names(data))) {
        na_type <- switch(
          schema_types[i],
          string = NA_character_,
          number = NA_real_,
          integer = NA_integer_,
          date = as.Date(NA), # NA_Date_,
          duration = NA_character_,
          NA
        )
        # Add the column with NA values of the specified type
        data[[schema_fields[i]]] <- na_type
      }
    }
  }

  if (schema$fieldsMatch == "partial") {
    # Partial: The data source MUST have at least one field defined in the fields array.
    # Fields MUST be mapped by their names.
    # NOT CHECKING FOR AT LEAST ONE FIELD
  }

  # Update schema_fields
  data <- data |> select(all_of(schema_fields))

  if (cast_type) {
    data <- cast_table(data, schema)
  }

  pkg <- frictionless::add_resource(
    package = pkg,
    resource_name = resource_name,
    data = data,
    schema = schema,
    replace = replace,
    delim = delim
  )

  return(pkg)
}
