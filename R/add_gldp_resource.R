#' Add a GeoLocator Data Resource
#'
#' @description
#' Add a standard GeoLocator-DP resource to a package. This essentially wraps
#' [frictionless::add_resource()], preparing the data first:
#' - extending the schema with extra columns present in `data` when allowed by
#'   `fieldsMatch`;
#' - adding missing schema columns to `data` with typed `NA` values when
#'   needed;
#' - reordering `data` columns to match the schema, because
#'   [frictionless::add_resource()] expects fields in schema order;
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
#' @param delim `r lifecycle::badge("deprecated")` Not used:
#'   [frictionless::add_resource()] only uses it when `data` is a path to a
#'   file, and `data` is always a data frame here.
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
  delim = lifecycle::deprecated()
) {
  check_gldp(pkg)
  if (lifecycle::is_present(delim)) {
    lifecycle::deprecate_warn("1.4.0", "add_gldp_resource(delim)")
  }
  version <- gldp_version(pkg)

  # The profile lists the tables, whose schemas are not in `pkg` until added.
  pkg_schema <- gldp_profile_schema(version)
  possible_gldp_resources <- gldp_profile_table_names(pkg_schema)

  if (!resource_name %in% possible_gldp_resources) {
    cli_abort(c(
      "x" = "{.val {resource_name}} is not a GeoLocator-DP table supported by {.fn add_gldp_resource}.",
      "i" = "Supported GeoLocator-DP tables are: {.val {possible_gldp_resources}}.",
      "i" = "To add custom resources, use {.fn frictionless::add_resource} directly."
    ))
  }

  schema <- gldp_resource_schema(version, resource_name)

  # frictionless does not implement `fieldsMatch`
  # (frictionlessdata/frictionless-r#216), and `add_resource()` matches schema
  # fields to data columns by name *and* order, so the data is reconciled with
  # the schema here first.
  #
  # GeoLocator-DP only ever declares two of the five modes, so only those two
  # are handled. Both allow the data to have fewer columns than the schema
  # declares; they differ on columns the schema does not define, which
  # `partial` keeps and `superset` drops.
  fields_match <- as.character(schema$fieldsMatch %||% "exact")[1]

  if (!fields_match %in% c("superset", "partial")) {
    cli_abort(c(
      "x" = "Unsupported {.field fieldsMatch} {.val {fields_match}} for resource
             {.val {resource_name}}.",
      "i" = "GeoLocatoR handles {.val superset} and {.val partial}, the only modes
             used by GeoLocator-DP."
    ))
  }

  undeclared <- setdiff(names(data), schema_field_names(schema))

  if (fields_match == "partial") {
    # `partial` keeps undeclared columns, so describe them in the schema.
    if (length(undeclared) > 0) {
      schema$fields <- append(
        schema$fields,
        frictionless::create_schema(data[undeclared])$fields
      )
    }
  } else if (length(undeclared) > 0) {
    # `superset` allows only columns the schema defines, so the selection below
    # drops these. Say so, rather than losing them silently.
    cli_warn(
      c(
        "!" = "Dropped {length(undeclared)} column{?s} from {.val {resource_name}}
               not defined by its schema: {.field {undeclared}}.",
        "i" = "{.val {resource_name}} declares {.code fieldsMatch: superset}, which
               allows only the columns its schema defines."
      ),
      class = "gldp_warning_undeclared_columns_dropped"
    )
  }

  # Materialise declared-but-absent columns as typed NA, so every written
  # resource carries the full set of columns its schema declares.
  for (field in schema$fields) {
    if (!field$name %in% names(data)) {
      # Types not listed fall back to a logical NA, which writes as an empty
      # CSV column and is typed by `frictionless::read_resource()` on the way in.
      # Repeated to the row count: a base data.frame does not recycle to 0 rows.
      data[[field$name]] <- rep(
        switch(
          as.character(field$type)[1],
          string = NA_character_,
          number = NA_real_,
          integer = NA_integer_,
          date = as.Date(NA),
          duration = NA_character_,
          NA
        ),
        nrow(data)
      )
    }
  }

  # `add_resource()` matches schema fields to data columns positionally, so the
  # selection below is what makes the pair acceptable to it. For `superset` it
  # is also what drops columns the schema does not define.
  data <- data |> select(all_of(schema_field_names(schema)))

  if (cast_type) {
    data <- gldp_cast_table(data, schema)
  }

  pkg <- frictionless::add_resource(
    package = pkg,
    resource_name = resource_name,
    data = data,
    schema = schema,
    replace = replace
  )

  gldp_resource(pkg, resource_name) <- gldp_mark_table_resource(gldp_resource(pkg, resource_name))

  return(pkg)
}

#' Mark a resource descriptor as a GeoLocator-DP table
#'
#' @description
#' GeoLocator-DP requires `$schema` and `type: "table"` on its tabular resources
#' from v1.1. frictionless 2 sets both on resources it adds, but a resource read
#' from an older package keeps what it was written with. Up to v1.0, the
#' profile used the resource `$schema` for the table schema URL instead, so a
#' resource of a package created for those versions does not match that
#' version's profile; [read_gldp()] upgrades such packages.
#'
#' `profile: "tabular-data-resource"`, the Data Package v1 spelling that
#' `$schema` and `type` replace, is dropped.
#'
#' @param resource A resource descriptor.
#' @return `resource`, with `$schema` first, `type` after `name` and no
#'   `profile`.
#' @noRd
gldp_mark_table_resource <- function(resource) {
  # Remove any earlier value, such as a v1.0 table schema URL in `$schema`.
  resource[["$schema"]] <- NULL
  resource[["type"]] <- NULL
  resource[["profile"]] <- NULL
  resource <- append(list(`$schema` = .gldp_data_resource_profile), resource)
  append(resource, list(type = "table"), after = which(names(resource) == "name"))
}
