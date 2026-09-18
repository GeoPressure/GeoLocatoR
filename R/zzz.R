#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @noRd
is_non_empty_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' @noRd
first_non_empty_string <- function(...) {
  values <- list(...)
  for (value in values) {
    scalar <- as.character(value %||% NA_character_)[1]
    if (is_non_empty_string(scalar)) {
      return(scalar)
    }
  }
  NULL
}

#' Field names declared by a table schema
#'
#' @param schema A table schema.
#' @return Character vector of field names, in schema order.
#' @noRd
schema_field_names <- function(schema) {
  vapply(schema$fields, \(f) as.character(f$name)[1], character(1))
}

#' Get or set a resource by name
#'
#' @description
#' Internal accessors mirroring [frictionless::resource()] and its replacement
#' form, minus their path handling.
#'
#' frictionless resolves `resource$path` against the package directory and then
#' probes the file, which for a remote package is an HTTP request per resource
#' (`frictionless:::check_path()`). That is wasteful when all we need is to read
#' or replace a descriptor entry, and it is actively wrong for Zenodo file URLs,
#' which `httr::http_error()` reports as errors even when the file is there.
#'
#' The Data Package `path` XOR `data` rule *is* enforced, because a resource
#' carrying both is written back out as an invalid `datapackage.json`.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param resource_name Name of the resource.
#' @param value Replacement resource.
#'
#' @return `gldp_resource()` returns the resource, or `NULL` when absent.
#' @noRd
gldp_resource <- function(pkg, resource_name) {
  idx <- gldp_resource_index(pkg, resource_name)
  if (length(idx) == 0) {
    return(NULL)
  }

  resource <- pkg$resources[[idx[1]]]

  if (!is.null(resource$path) && !is.null(resource$data)) {
    cli_abort(
      c(
        "Resource {.val {resource_name}} must have a {.field path} or a
         {.field data} property, not both.",
        "i" = "A Data Package resource points at a file or carries its table,
               never both."
      ),
      class = "gldp_error_resource_both_path_data"
    )
  }

  resource
}

#' @noRd
`gldp_resource<-` <- function(pkg, resource_name, value) {
  idx <- gldp_resource_index(pkg, resource_name)
  if (length(idx) == 0) {
    cli_abort(
      "Can't find resource {.val {resource_name}} in {.arg pkg}.",
      class = "gldp_error_resource_not_found"
    )
  }

  pkg$resources[[idx[1]]] <- value
  pkg
}

#' Column names of the file backing a resource
#'
#' @param pkg A GeoLocator Data Package object.
#' @param resource A resource descriptor.
#' @return Character vector of column names, or `NULL` when the file cannot be
#'   read or the resource has no file.
#' @noRd
gldp_resource_header <- function(pkg, resource) {
  path <- as.character(resource$path %||% character(0))[1]
  if (is.na(path) || !nzchar(path)) {
    return(NULL)
  }

  if (!grepl("^https?://", path)) {
    directory <- attr(pkg, "directory")
    if (is_non_empty_string(directory)) {
      path <- file.path(directory, path)
    }
    if (!file.exists(path)) {
      return(NULL)
    }
  }

  # Only the header is needed, so the rest of the file is never parsed.
  tryCatch(
    names(readr::read_delim(
      path,
      delim = as.character(resource$dialect$delimiter %||% ",")[1],
      n_max = 0,
      show_col_types = FALSE,
      progress = FALSE,
      locale = readr::locale(
        encoding = as.character(resource$encoding %||% "UTF-8")[1]
      )
    )),
    error = \(e) NULL,
    warning = \(w) NULL
  )
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
  frictionless::read_resource(pkg, resource_name, ...)
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
#'   [gldp_profile_schema()].
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
    purrr::flatten_chr()
}

#' Locate a resource by name
#'
#' @param pkg A GeoLocator Data Package object.
#' @param resource_name Name of the resource to locate.
#' @return Integer index into `pkg$resources`, or `integer(0)` when absent.
#' @noRd
gldp_resource_index <- function(pkg, resource_name) {
  which(vapply(
    pkg$resources %||% list(),
    \(r) identical(r$name, resource_name),
    logical(1)
  ))
}

#' Convert contributors to person objects
#'
#' Internal helper function to convert a list of contributors to person objects
#' for use in DESCRIPTION files.
#'
#' @param contributors A list of contributor objects from a GeoLocator Data Package
#' @return A list of person objects
#' @noRd
contributors_to_persons <- function(contributors) {
  role_mapping <- c(
    "contactperson" = "ctr", # Contractor (assumed due to lack of clear match)
    "contributor" = "ctb", # Contributor
    "datacollector" = "dtc", # Data contributor
    "datacurator" = "dtc", # Data contributor (no closer match)
    "datamanager" = "dtc", # Data contributor (no closer match)
    "distributor" = "ctr", # Contractor (assumed)
    "editor" = "rev", # Reviewer
    "hostinginstitution" = "cph", # Copyright holder (legal entity)
    "producer" = "aut", # Author (substantial contribution)
    "projectleader" = "cre", # Creator (project leader matches)
    "projectmanager" = "cre", # Creator (manager of the project)
    "projectmember" = "ctb", # Contributor (smaller contributions)
    "registrationagency" = "ctr", # Contractor (assumed)
    "registrationauthority" = "ctr", # Contractor (assumed)
    "relatedperson" = "trl", # Translator (assumed)
    "researcher" = "aut", # Author
    "researchergroup" = "aut", # Author (group treated as authors)
    "rightsholder" = "cph", # Copyright holder
    "sponsor" = "fnd", # Funder
    "supervisor" = "ths", # Thesis advisor
    "workpackageleader" = "cre" # Creator (assumed leader role)
  )

  persons <- contributors |>
    purrr::map(
      ~ {
        utils::person(
          given = ifelse(
            is.null(.x$givenName) & !is.null(.x$title),
            .x$title,
            .x$givenName
          ),
          family = .x$familyName,
          email = .x$email,
          role = purrr::map_vec(
            .x$roles,
            ~ coalesce(role_mapping[tolower(.x)], "ctb")
          ),
          comment = c(.x$path, .x$organization)
        )
      }
    )

  persons <- do.call(c, Filter(Negate(is.null), persons))

  persons
}

#' Cast data frame columns according to schema types
#'
#' Internal helper function to cast data frame columns to the appropriate types
#' based on a table schema specification.
#'
#' @param data A data frame to cast
#' @param schema A table schema object with field definitions
#' @return The data frame with properly cast column types
#' @noRd
cast_table <- function(data, schema) {
  schema_fields <- schema_field_names(schema)
  schema_types <- sapply(schema$fields, \(x) x$type)

  for (i in seq_along(schema_fields)) {
    field <- schema_fields[i]
    type <- schema_types[i]

    if (field %in% names(data)) {
      if (type == "string") {
        data[[field]] <- as.character(data[[field]])
      } else if (type == "number") {
        # Legacy upgrades can carry non-numeric IDs in numeric schema fields; coerce silently to NA.
        data[[field]] <- suppressWarnings(as.numeric(data[[field]]))
      } else if (type == "integer") {
        data[[field]] <- suppressWarnings(as.integer(data[[field]]))
      } else if (type == "boolean") {
        data[[field]] <- as.logical(data[[field]])
      } else if (type == "date") {
        data[[field]] <- as.Date(data[[field]])
      } else if (type == "datetime") {
        data[[field]] <- as.POSIXct(data[[field]], tz = "UTC")
      } else if (type == "time") {
        # For time fields, convert to character if not already
        if (!is.character(data[[field]])) {
          data[[field]] <- as.character(data[[field]])
        }
      } else if (type == "year") {
        # For year fields, convert to integer
        data[[field]] <- suppressWarnings(as.integer(data[[field]]))
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
      } else {
        cli_warn(c(
          "!" = "No casting for {.field {field}} of type {.val {type}}."
        ))
      }
    }
  }
  data
}
