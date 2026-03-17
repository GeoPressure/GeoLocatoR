# nocov start
#' Deprecated: `read_gdl()`
#'
#' @description
#' `read_gdl()` is kept for backward compatibility in the SOI import workflow.
#'
#' @param access_file A path to an Access file containing both GDL data and
#'   order information. If provided, it takes precedence over `data_file` and
#'   `order_file`.
#' @param data_file A path to the GDL data file. Required when `access_file` is
#'   not provided.
#' @param order_file A path to the GDL order file. Required when `access_file`
#'   is not provided.
#' @param filter_col A logical flag or a character vector controlling selected
#'   columns.
#' @return Same as `read_soi_gld()`.
#' @export
read_gdl <- function(
  access_file = NA,
  data_file = NA,
  order_file = NA,
  filter_col = TRUE
) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "read_gdl()",
    details = "Use the SOI workflow with read_soi(). This compatibility helper will be removed in a future release."
  )
  read_soi_gld(
    access_file = access_file,
    data_file = data_file,
    order_file = order_file,
    filter_col = filter_col
  )
}

#' Deprecated: `version()`
#'
#' @description
#' `version()` was renamed to [gldp_version()].
#'
#' @param x A GeoLocator Data Package object.
#' @return A GLDP version string.
version <- function(x) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "version()",
    with = "gldp_version()"
  )
  gldp_version(x)
}

#' Deprecated: `config2tibble()`
#'
#' @description
#' `config2tibble()` was renamed to [config_to_tibble()].
#'
#' @param file Character string specifying the path to the `config.yml` file.
#' @param filter_return Logical. If `TRUE`, only columns that vary across tags are returned.
#' @rdname config_to_tibble
#' @export
config2tibble <- function(
  file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
  filter_return = TRUE
) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "config2tibble()",
    with = "config_to_tibble()"
  )
  config_to_tibble(file = file, filter_return = filter_return)
}

#' @rdname update_gldp
#' @export
update_gldp_metadata <- function(pkg) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "update_gldp_metadata()",
    details = "Metadata ordering is now handled by core package internals."
  )
  pkg
}

#' @rdname update_gldp
#' @export
update_gldp_spatial <- function(pkg) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "update_gldp_spatial()",
    details = "Spatial summary was removed from computed fields."
  )
  pkg
}

#' @rdname update_gldp
#' @export
update_gldp_reference_location <- function(pkg) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "update_gldp_reference_location()",
    details = "Reference location was removed from computed fields."
  )
  pkg
}

#' Deprecated: `add_gldp_geopressuretemplate()`
#'
#' @description
#' `add_gldp_geopressuretemplate()` was renamed to [read_geopressuretemplate()].
#'
#' @param pkg A GeoLocator Data Package object.
#' @param directory A GeoPressureTemplate directory.
#' @param from Data sources to import.
#' @return Updated package.
#' @export
add_gldp_geopressuretemplate <- function(
  pkg,
  directory = ".",
  from = c("raw-tag", "interim")
) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "add_gldp_geopressuretemplate()",
    with = "read_geopressuretemplate()"
  )
  read_geopressuretemplate(
    directory = directory,
    from = from,
    pkg = pkg
  )
}

#' Deprecated: `create_gldp_geopressuretemplate()`
#'
#' @description
#' `create_gldp_geopressuretemplate()` was superseded by [read_geopressuretemplate()].
#' This compatibility helper initializes `pkg = create_gldp()` and forwards the call.
#'
#' @param directory A GeoPressureTemplate directory.
#' @return A `geolocatordp` object.
#' @export
create_gldp_geopressuretemplate <- function(directory = ".") {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "create_gldp_geopressuretemplate()",
    with = "read_geopressuretemplate()"
  )
  read_geopressuretemplate(directory = directory, pkg = create_gldp())
}

#' Deprecated: `zenodo_to_gldp()`
#'
#' @description
#' `zenodo_to_gldp()` was removed. Use [read_zenodo()] after adapting arguments,
#' because it is not a strict drop-in replacement.
#'
#' @param id Zenodo identifier.
#' @param ... Unused.
#' @return This function is defunct and always errors with migration guidance.
#' @export
zenodo_to_gldp <- function(id, ...) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "zenodo_to_gldp()",
    with = "read_zenodo()",
    details = glue::glue(
      "`read_zenodo()` has different behavior/arguments. ",
      "Migrate your call explicitly (notably `endpoint`, `token`, and `sandbox`)."
    )
  )
}

#' Deprecated: `gldp_to_zenodo()`
#'
#' @description
#' `gldp_to_zenodo()` was removed with no replacement.
#'
#' @param ... Unused.
#' @export
gldp_to_zenodo <- function(...) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "gldp_to_zenodo()",
    details = "Zenodo write helpers were removed from GeoLocatoR."
  )
}
# nocov end
