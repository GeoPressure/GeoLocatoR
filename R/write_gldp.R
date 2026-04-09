#' Write a GeoLocator Data Package to disk
#'
#' @description
#' Write a GeoLocator Data Package to disk with
#' [frictionless::write_package()]. Standard datapackage files are written from
#' the package descriptor and resources, while `pkg$params` is serialized
#' separately to `params.json` when present.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param directory Directory where `datapackage.json` and resources are written.
#' @param compress Logical. Passed to [frictionless::write_package()].
#'
#' @return Invisibly returns `directory`.
#'
#' @seealso [create_gldp()] to create a package and [read_gldp()] to read one
#'   back from disk.
#' @export
write_gldp <- function(pkg, directory = ".", compress = FALSE) {
  check_gldp(pkg)

  pkg_without_params <- pkg
  pkg_without_params$params <- NULL

  frictionless::write_package(
    package = pkg_without_params,
    directory = directory,
    compress = compress
  )

  if (!is.null(pkg$params)) {
    writeLines(
      jsonlite::serializeJSON(pkg$params),
      file.path(directory, "params.json")
    )
  }

  invisible(directory)
}
