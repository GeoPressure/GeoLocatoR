#' Create a GeoLocator Data Package
#'
#' @description
#' Create a new GeoLocator Data Package shell with the GeoLocator-DP profile
#' schema.
#'
#' @param version Optional GeoLocator-DP version. Use `NULL` for the latest
#'   version, or set it explicitly when creating a package for an older schema
#'   such as `"v0.3"` or `"v1.0"`.
#' @param ... Additional top-level descriptor fields, such as `title`, `id`,
#'   `contributors`, or `licenses`, used to initialize package metadata.
#'
#' @return A `geolocatordp` object with the requested schema URL and no
#'   resources.
#'
#' @examples
#' pkg <- create_gldp()
#' pkg
#'
#' # Previous version
#' pkg <- create_gldp(version = "v0.2")
#' pkg
#'
#' @seealso [read_gldp()] to read an existing package, [write_gldp()] to write
#'   a package to disk, and [gldp_version()] to inspect the package schema
#'   version.
#'
#' @export
create_gldp <- function(version = NULL, ...) {
  descriptor_fields <- list(...)

  pkg <- frictionless::create_package(
    descriptor = c(
      list(
        "$schema" = gldp_schema_url(version)
      ),
      descriptor_fields
    )
  )

  class(pkg) <- c("geolocatordp", class(pkg))

  pkg
}
