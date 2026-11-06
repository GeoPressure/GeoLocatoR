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

#' Locate a resource by name
#'
#' @param pkg A GeoLocator Data Package object.
#' @param resource_name Name of the resource to locate.
#' @return Integer index into `pkg$resources`, or `integer(0)` when absent.
#' @noRd
gldp_resource_index <- function(pkg, resource_name) {
  which(purrr::map_lgl(
    pkg$resources %||% list(),
    \(r) identical(r$name, resource_name)
  ))
}
