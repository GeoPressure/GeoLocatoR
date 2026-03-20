#' Create a GeoLocator Data Package
#'
#' Create a GeoLocator Data Package shell with a minimal descriptor.
#'
#' @param version (optional) Version of GeoLocator Data Package to use. Defaults to the
#' GeoLocatoR default (latest).
#' @param ... Additional descriptor fields to include
#'
#' @return A GeoLocator Data Package object.
#'
#' @examples
#' pkg <- create_gldp()
#'
#' # Previous version
#' pkg <- create_gldp(version = "v0.2")
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
