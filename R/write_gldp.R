#' Write a GeoLocator Data Package to disk
#'
#' Wrapper around [frictionless::write_package()] so users can write a package
#' without attaching {.pkg frictionless}.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param directory Directory where `datapackage.json` and resources are written.
#' @param compress Logical. Passed to [frictionless::write_package()].
#'
#' @return Invisibly returns `directory`.
#' @export
write_gldp <- function(pkg, directory = ".", compress = FALSE) {
  check_gldp(pkg)

  frictionless::write_package(
    package = pkg,
    directory = directory,
    compress = compress
  )

  invisible(directory)
}
