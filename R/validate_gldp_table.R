#' Validate a tabular resource against its schema
#'
#' Internal helper that checks a single resource table against its field
#' definitions. It validates data availability, column matching through the
#' schema `fieldsMatch` rule, and per-field constraints.
#'
#' @param data A data frame stored in the resource.
#' @param schema The frictionless schema describing the resource.
#' @param name Resource name used in messages. Custom resources have no schema name.
#'
#' @return Invisibly returns `TRUE` when the table is consistent with the schema,
#'   `FALSE` otherwise.
#' @noRd
validate_gldp_table <- function(data, schema, name = schema$name) {
  cli_h3("Check Resources {.field {name}}")

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

  # `fieldsMatch` is a rule over the whole set of fields and says nothing about
  # which ones are required, so every mode GeoLocator-DP uses would let a table
  # leave out a column carrying `required`. The standard has no way to say "the
  # required columns must be present, the optional ones may be absent"
  # (frictionlessdata/datapackage#1126), so it is checked here instead.
  required <- names(schema_fields)[purrr::map_lgl(
    schema_fields,
    \(f) isTRUE(f$constraints$required)
  )]
  absent_required <- setdiff(required, names(data))

  if (length(absent_required) > 0) {
    cli_alert_danger(
      "{.field {name}} is missing {length(absent_required)} required
       column{?s}: {.field {absent_required}}."
    )
    valid <- FALSE
  }

  fields <- intersect(names(schema_fields), names(data))

  for (field in fields) {
    prop <- schema_fields[[field]]
    if (!is.null(prop$constraints)) {
      prop <- c(prop, prop$constraints)
      prop$constraints <- NULL # Remove the 'constraints' field after merging
    }

    field_name <- glue::glue("{name}${field}")
    # A Table Schema `format` (`default`, `any`, a strptime pattern) says how the
    # reader parses a column; it is not one of the JSON-Schema formats checked for
    # the profile. A field without `type` is `any`, so no type is inferred either.
    prop$format <- NULL
    # A Table Schema `pattern` is an XML Schema regular expression, which must
    # match the whole value; the profile's JSON-Schema patterns match anywhere.
    if (!is.null(prop$pattern)) {
      prop$pattern <- glue::glue("^(?:{prop$pattern})$")
    }

    valid <- valid & validate_gldp_item(data[[field]], prop, field_name)
  }

  # A primary key identifies each row, e.g. one row per trajectory and stap in `paths`.
  primary_key <- unlist(schema$primaryKey)
  if (length(primary_key) > 0 && all(primary_key %in% names(data))) {
    n_duplicates <- nrow(data) - nrow(dplyr::distinct(data[primary_key]))
    if (n_duplicates > 0) {
      cli_alert_danger(
        "{.field {name}} has {format(n_duplicates, big.mark = ',')} duplicated \\
        {cli::qty(n_duplicates)}row{?s} for its primary key {.field {primary_key}}."
      )
      valid <- FALSE
    }
  }

  if (valid) {
    cli_alert_success(
      "Table {.field {name}} is consistent with the schema."
    )
  }

  invisible(valid)
}

#' Validate schema and data field matching
#'
#' Internal helper that applies the frictionless `fieldsMatch` rule to compare
#' schema field names against data column names.
#'
#' @param schema_fields Field names declared in the schema.
#' @param data_fields Field names present in the data.
#' @param fields_match Matching mode from the schema.
#'
#' @return Invisibly returns `TRUE` when the field match rule passes, `FALSE`
#'   otherwise.
#' @noRd
validate_gldp_fields_match <- function(
  schema_fields,
  data_fields,
  fields_match
) {
  if (is.null(fields_match)) {
    fields_match <- "exact"
  }

  valid <- TRUE
  # Perform the check based on the fieldsMatch value
  if (fields_match == "exact") {
    if (!identical(schema_fields, data_fields)) {
      cli_alert_danger(
        "Exact match failed. Schema fields: {schema_fields}, Data fields:
                       {data_fields}"
      )
      valid <- FALSE
    }
  } else if (fields_match == "equal") {
    if (!setequal(schema_fields, data_fields)) {
      cli_alert_danger(
        "Equal match failed. Schema fields: {schema_fields}, Data fields:
                       {data_fields}"
      )
      valid <- FALSE
    }
  } else if (fields_match == "subset") {
    if (!all(schema_fields %in% data_fields)) {
      missing_fields <- schema_fields[!schema_fields %in% data_fields]
      cli_alert_danger("Subset match failed. Missing fields: {missing_fields}")
      valid <- FALSE
    }
  } else if (fields_match == "superset") {
    if (!all(data_fields %in% schema_fields)) {
      extra_fields <- data_fields[!data_fields %in% schema_fields]
      cli_alert_danger("Superset match failed. Extra fields: {extra_fields}")
      valid <- FALSE
    }
  } else if (fields_match == "partial") {
    if (!any(schema_fields %in% data_fields)) {
      cli_alert_danger("Partial match failed. No schema fields found in data.")
      valid <- FALSE
    }
  } else {
    cli_alert_danger("Unknown {.field fieldsMatch} value {.val {fields_match}}.")
    valid <- FALSE
  }

  invisible(valid)
}

#' Validate schema-declared foreign keys
#'
#' Internal helper that checks the foreign keys declared in each resource
#' schema against the rows of the parent resource.
#'
#' @param pkg A GeoLocator Data Package object.
#'
#' @return Invisibly returns `TRUE` when all foreign keys resolve, `FALSE`
#'   otherwise.
#' @noRd
validate_gldp_foreign_keys <- function(pkg) {
  # Foreign-key checks require both child and parent tables, including file-backed ones.
  pkg <- gldp_materialize_tables(pkg)
  valid <- TRUE
  resources <- pkg$resources %||% list()
  resource_names <- vapply(resources, \(x) x$name %||% NA_character_, character(1))

  for (resource in resources) {
    child_name <- as.character(resource$name %||% NA_character_)[1]
    child_data <- resource$data %||% NULL
    foreign_keys <- resource$schema$foreignKeys %||% list()

    if (!is.data.frame(child_data) || length(foreign_keys) == 0) {
      next
    }

    for (fk in foreign_keys) {
      child_fields <- as.character(unlist(fk$fields %||% character(0), use.names = FALSE))
      # A reference without `resource` (v2) or with `resource: ""` (v1) points
      # at the resource itself.
      parent_name <- as.character(fk$reference$resource %||% "")[1]
      if (!nzchar(parent_name)) {
        parent_name <- child_name
      }
      parent_fields <- as.character(unlist(
        fk$reference$fields %||% character(0),
        use.names = FALSE
      ))

      if (
        length(child_fields) == 0 ||
          length(child_fields) != length(parent_fields)
      ) {
        next
      }

      parent_idx <- match(parent_name, resource_names)
      parent_data <- if (is.na(parent_idx)) NULL else resources[[parent_idx]]$data %||% NULL
      if (is.null(parent_data) || !is.data.frame(parent_data)) {
        cli_alert_danger(
          "Foreign key check for {.field {child_name}} failed: parent resource {.field {parent_name}} is missing."
        )
        valid <- FALSE
        next
      }

      absent_child_fields <- setdiff(child_fields, names(child_data))
      if (length(absent_child_fields) > 0) {
        # A column the schema declares optional (e.g. `observations$tag_id`)
        # may be left out of the file and then holds no key to check.
        optional_fields <- purrr::map_chr(
          purrr::discard(resource$schema$fields, \(f) isTRUE(f$constraints$required)),
          "name"
        )
        if (!all(absent_child_fields %in% optional_fields)) {
          cli_alert_danger(
            "Foreign key check for {.field {child_name}} failed: missing child fields: {.field {child_fields}}."
          )
          valid <- FALSE
        }
        next
      }
      if (!all(parent_fields %in% names(parent_data))) {
        cli_alert_danger(
          "Foreign key check for {.field {child_name}} failed: missing parent fields in {.field {parent_name}}: {.field {parent_fields}}."
        )
        valid <- FALSE
        next
      }

      child_keys <- child_data[, child_fields, drop = FALSE]
      child_keys <- child_keys[stats::complete.cases(child_keys), , drop = FALSE]
      if (nrow(child_keys) == 0) {
        next
      }
      child_keys[] <- purrr::map(child_keys, as.character)
      child_keys <- unique(child_keys)

      parent_keys <- parent_data[, parent_fields, drop = FALSE]
      parent_keys <- parent_keys[stats::complete.cases(parent_keys), , drop = FALSE]
      parent_keys[] <- purrr::map(parent_keys, as.character)
      parent_keys <- unique(parent_keys)
      names(parent_keys) <- child_fields

      missing_keys <- anti_join(child_keys, parent_keys, by = child_fields)
      if (nrow(missing_keys) > 0) {
        cli_alert_danger(
          "{nrow(missing_keys)} foreign key row{?s} in {.field {child_name}} {cli::qty(nrow(missing_keys))}{?is/are} missing in parent {.field {parent_name}}."
        )
        valid <- FALSE
      }
    }
  }

  if (valid) {
    cli_alert_success("Foreign key checks passed.")
  }

  invisible(valid)
}
