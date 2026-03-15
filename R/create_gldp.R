#' Create a GeoLocator Data Package
#'
#' Create a GeoLocator Data Package shell with a minimal descriptor.
#'
#' @param spec_version (optional) Version of GeoLocator Data Package to use. Defaults to the
#' GeoLocatoR default (latest).
#' @param ... Additional descriptor fields to include
#'
#' @return A GeoLocator Data Package object.
#'
#' @examples
#' pkg <- create_gldp()
#'
#' # Previous version
#' pkg <- create_gldp(spec_version = "v0.2")
#'
#' @export
create_gldp <- function(spec_version = NULL, ...) {
  descriptor_fields <- list(...)

  pkg <- frictionless::create_package(
    descriptor = c(
      list(
        "$schema" = gldp_schema_url(spec_version)
      ),
      descriptor_fields
    )
  )

  class(pkg) <- c("geolocatordp", class(pkg))

  pkg
}
