#' Read a GeoLocator-DP resource, matching its schema to the file
#'
#' @description
#' Internal helpers backing every read of a tabular resource. They exist
#' because [frictionless::read_resource()] matches schema fields to data
#' columns by position rather than by name; `gldp_align_schema_to_header()`
#' below says what that costs and why it matters here.
#'
#' @name gldp_read_resource_internals
#' @noRd
NULL

#' Column names of the file backing a resource
#'
#' @param pkg A GeoLocator Data Package object.
#' @param resource A resource descriptor.
#' @return Character vector of column names, or `NULL` when the resource has no
#'   path or its local file does not exist.
#' @noRd
gldp_resource_header <- function(pkg, resource) {
  path <- as.character(resource$path %||% character(0))[1]
  if (is.na(path) || !nzchar(path)) {
    return(NULL)
  }

  # A relative path resolves against the package directory, which is a URL for a
  # remote package: only a local file can be checked for existence.
  directory <- attr(pkg, "directory")
  if (!grepl("^https?://", path) && is_non_empty_string(directory)) {
    path <- file.path(directory, path)
  }
  if (!grepl("^https?://", path) && !file.exists(path)) {
    return(NULL)
  }

  # Only the header is needed, so the rest of the file is never parsed.
  names(readr::read_delim(
    path,
    delim = as.character(resource$dialect$delimiter %||% ",")[1],
    n_max = 0,
    show_col_types = FALSE,
    progress = FALSE,
    locale = readr::locale(
      encoding = as.character(resource$encoding %||% "UTF-8")[1]
    )
  ))
}

#' Match a resource schema to the columns its file actually has
#'
#' @description
#' [frictionless::read_resource()] maps schema fields to data columns by
#' position, because `fieldsMatch` is not implemented
#' (frictionlessdata/frictionless-r#216). A file that leaves out a column its
#' schema declares is therefore read with every later column under the wrong
#' name, and neither frictionless nor GeoLocatoR notices: by the time the table
#' is validated, the names it is checked against are the ones frictionless
#' assigned.
#'
#' Every GeoLocator-DP table declares `fieldsMatch: superset` or `partial`, both
#' of which let a file leave columns out, so this reconciles the schema with the
#' header before reading. Declared fields absent from the file are dropped, the
#' rest are put in file order, and columns the schema does not describe are
#' described as `any` when `fieldsMatch` allows them.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param resource_name Name of the resource to align.
#' @return `pkg`, with that resource's schema matched to its file.
#' @noRd
gldp_align_schema_to_header <- function(pkg, resource_name) {
  resource <- gldp_resource(pkg, resource_name)

  # Nothing to align against: a resource carrying its data inline already has
  # its column names, and a schema given by URL is not resolved here.
  if (
    is.null(resource) ||
      is.null(resource$path) ||
      !is.list(resource$schema) ||
      length(resource$schema$fields) == 0
  ) {
    return(pkg)
  }

  schema <- resource$schema
  declared <- schema_field_names(schema)
  header <- gldp_resource_header(pkg, resource)

  if (is.null(header) || identical(header, declared)) {
    return(pkg)
  }

  fields_match <- as.character(schema$fieldsMatch %||% "exact")[1]
  undeclared <- setdiff(header, declared)

  if (length(intersect(header, declared)) == 0) {
    cli_abort(
      c(
        "x" = "None of the columns of {.val {resource_name}} are declared by its
               schema.",
        "i" = "Columns found: {.field {header}}.",
        "i" = "Expected some of: {.field {declared}}."
      ),
      class = "gldp_error_header_unrelated"
    )
  }

  # `subset` and `partial` are the modes that allow a column the schema does not
  # define; the others make one an error rather than something to describe.
  if (length(undeclared) > 0 && !fields_match %in% c("subset", "partial")) {
    cli_abort(
      c(
        "x" = "{.val {resource_name}} has {length(undeclared)} column{?s} its
               schema does not define: {.field {undeclared}}.",
        "i" = "{.val {resource_name}} declares {.code fieldsMatch: {fields_match}},
               which allows only the columns its schema defines."
      ),
      class = "gldp_error_undeclared_columns"
    )
  }

  fields <- stats::setNames(schema$fields, declared)
  schema$fields <- purrr::map(
    header,
    \(name) fields[[name]] %||% list(name = name, type = "any")
  )
  resource$schema <- schema
  gldp_resource(pkg, resource_name) <- resource

  pkg
}

#' Read a resource, matching its schema to the file by column name
#'
#' @param pkg A GeoLocator Data Package object.
#' @param resource_name Name of the resource to read.
#' @param ... Passed to [frictionless::read_resource()].
#' @return The resource data.
#' @noRd
gldp_read_resource <- function(pkg, resource_name, ...) {
  pkg <- gldp_align_schema_to_header(pkg, resource_name)

  # Replace readr's parsing warning with one naming the resource and the column.
  has_parsing_warning <- FALSE
  data <- withCallingHandlers(
    frictionless::read_resource(pkg, resource_name, ...),
    warning = function(w) {
      if (grepl("One or more parsing issues", conditionMessage(w), fixed = TRUE)) {
        has_parsing_warning <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  if (has_parsing_warning) {
    path <- as.character(gldp_resource(pkg, resource_name)$path)[1]
    directory <- attr(pkg, "directory")
    if (!grepl("^https?://", path) && is_non_empty_string(directory)) {
      path <- file.path(directory, path)
    }
    problems <- dplyr::slice_head(readr::problems(data), n = 3)
    problem_preview <- purrr::pmap_chr(problems, \(row, col, expected, actual, ...) {
      glue::glue("row {row}, col {col}: expected {expected}, got {actual}")
    })
    cli_warn(
      c(
        "!" = "Parsing issues detected while reading resource {.field {resource_name}}.",
        "i" = "Location: {.file {path}}",
        "i" = if (length(problem_preview) > 0) {
          "First issues: {glue::glue_collapse(problem_preview, sep = '; ')}"
        } else {
          "Run {.code readr::problems()} on this resource to inspect row-level parsing issues."
        }
      ),
      class = "gldp_warning_parsing_issues"
    )
  }

  data
}

#' Load file-backed tables for operations that inspect or modify resource data
#'
#' @param pkg A GeoLocator Data Package object.
#' @return A package with tabular resources loaded and their file paths removed.
#' @noRd
gldp_materialize_tables <- function(pkg) {
  for (i in seq_along(pkg$resources)) {
    resource <- pkg$resources[[i]]
    if (is.null(resource$path) || !is.list(resource$schema) || is.null(resource$schema$fields)) {
      next
    }
    # Preserve the declared schema: validation must still detect missing columns.
    pkg[["resources"]][[i]] <- gldp_load_table(pkg, resource$name)
  }
  pkg
}

#' Load a resource table into memory
#'
#' @param pkg A GeoLocator Data Package object.
#' @param resource_name Name of the resource to load.
#' @return The resource, with its data read and cast to its schema, and its
#'   `path` removed.
#' @noRd
gldp_load_table <- function(pkg, resource_name) {
  resource <- gldp_resource(pkg, resource_name)
  resource[["data"]] <- gldp_cast_table(gldp_read_resource(pkg, resource_name), resource$schema)
  # A resource carries its data or a path to it, never both.
  resource[["path"]] <- NULL
  resource
}

#' Field names declared by a table schema
#'
#' @param schema A table schema.
#' @return Character vector of field names, in schema order.
#' @noRd
schema_field_names <- function(schema) {
  purrr::map_chr(schema$fields, \(f) as.character(f$name)[1])
}

#' Cast data frame columns according to schema types
#'
#' Internal helper function to cast data frame columns to the appropriate types
#' based on a table schema specification. Values that cannot be cast to a
#' number, integer, boolean, year, date or datetime become `NA` with a warning of
#' class `gldp_warning_cast_loss`, whose `field` element names the field.
#'
#' @param data A data frame to cast
#' @param schema A table schema object with field definitions
#' @return The data frame with properly cast column types
#' @noRd
gldp_cast_table <- function(data, schema) {
  schema_fields <- schema_field_names(schema)
  # A field without `type` is `any`, the Table Schema default.
  schema_types <- vapply(schema$fields, \(x) x$type %||% "any", character(1))

  for (i in seq_along(schema_fields)) {
    field <- schema_fields[i]
    type <- schema_types[i]

    if (field %in% names(data)) {
      # Cast factors from their labels: as.numeric() would return level codes.
      if (is.factor(data[[field]])) {
        data[[field]] <- as.character(data[[field]])
      }
      original <- data[[field]]
      if (type == "string") {
        data[[field]] <- as.character(data[[field]])
      } else if (type == "number") {
        # Lost values are reported below, so the base coercion warning is not needed.
        data[[field]] <- suppressWarnings(as.numeric(data[[field]]))
      } else if (type %in% c("integer", "year")) {
        # A value with a fractional part is not an integer: make it NA, reported below,
        # rather than truncate it.
        value <- suppressWarnings(as.numeric(data[[field]]))
        value[which(value != trunc(value))] <- NA
        data[[field]] <- suppressWarnings(as.integer(value))
      } else if (type == "boolean") {
        # Match the field's trueValues and falseValues, or the Table Schema defaults;
        # other values become NA, reported below. Logical columns are already cast.
        if (!is.logical(data[[field]])) {
          f <- schema$fields[[i]]
          value <- as.character(data[[field]])
          data[[field]] <- case_when(
            value %in% (f$trueValues %||% c("true", "True", "TRUE", "1")) ~ TRUE,
            value %in% (f$falseValues %||% c("false", "False", "FALSE", "0")) ~ FALSE,
            .default = NA
          )
        }
      } else if (type == "date") {
        value <- data[[field]]
        # Strings are read as ISO 8601 only; others become NA, reported below.
        data[[field]] <- if (is.character(value)) {
          value[!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", value)] <- NA
          as.Date(value, format = "%Y-%m-%d")
        } else {
          as.Date(value)
        }
      } else if (type == "datetime") {
        value <- data[[field]]
        # Strings are read as ISO 8601 only; others become NA, reported below.
        data[[field]] <- if (is.character(value)) {
          value[!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", value)] <- NA
          value <- sub("T", " ", value, fixed = TRUE)
          # Strip a trailing `Z` or UTC offset (`+02:00`, `-0530`) after the time and
          # shift the parsed time by it, so that the value is stored in UTC.
          zone <- "^(.*[0-9]:[0-9]{2}(:[0-9]{2}(\\.[0-9]*)?)?) ?(Z|([+-])([0-9]{2}):?([0-9]{2}))$"
          zoned <- grepl(zone, value)
          offset <- numeric(length(value))
          offset[zoned] <- as.numeric(sub(zone, "\\51", value[zoned])) *
            (as.numeric(sub(zone, "0\\6", value[zoned])) *
              3600 +
              as.numeric(sub(zone, "0\\7", value[zoned])) * 60)
          value[zoned] <- sub(zone, "\\1", value[zoned])
          purrr::map(
            c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d"),
            \(format) as.POSIXct(strptime(value, format, tz = "UTC"))
          ) |>
            purrr::reduce(coalesce) -
            offset
        } else {
          as.POSIXct(value, tz = "UTC")
        }
      } else if (type == "time") {
        # For time fields, convert to character if not already
        if (!is.character(data[[field]])) {
          data[[field]] <- as.character(data[[field]])
        }
      } else if (type == "yearmonth") {
        # For yearmonth fields, keep as character
        data[[field]] <- as.character(data[[field]])
      } else if (type == "duration") {
        # For duration fields, keep as character (ISO 8601 format)
        data[[field]] <- as.character(data[[field]])
      } else if (type == "geopoint") {
        # For geopoint fields, keep as character
        data[[field]] <- as.character(data[[field]])
      } else if (type == "geojson") {
        # For geojson fields, keep as character
        data[[field]] <- as.character(data[[field]])
      } else if (type == "list") {
        # Data Package v2 `list` fields are read as character by frictionless
        data[[field]] <- as.character(data[[field]])
      } else if (type != "any") {
        # `any` columns are left as they are.
        cli_warn(c(
          "!" = "No casting for {.field {field}} of type {.val {type}}."
        ))
      }

      # Report non-missing values the cast turned into NA; empty strings count as missing.
      if (type %in% c("number", "integer", "boolean", "year", "date", "datetime")) {
        lost <- is.na(data[[field]]) & !is.na(original)
        n_lost <- sum(nzchar(trimws(as.character(original[lost]))))
        if (n_lost > 0) {
          cli_warn(
            c(
              "!" = "{format(n_lost, big.mark = ',')} non-missing {cli::qty(n_lost)}value{?s} of {.field {field}} \\
              became NA when cast to {.val {type}}."
            ),
            class = "gldp_warning_cast_loss",
            field = field
          )
        }
      }
    }
  }
  data
}
