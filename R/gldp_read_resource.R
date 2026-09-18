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
