#' Check GeoLocator Data Package
#'
#' This function checks that the provided object is a valid GeoLocator Data Package and conforms to
#' the expected structure. It verifies that the object has the `"geolocatordp"` class and performs
#' a general package validation using the `frictionless` package.
#'
#' @param pkg An object of class `"geolocatordp"` to be validated.
#'
#' @return The validated GeoLocator Data Package object (invisible).
#'
#' @noRd
check_gldp <- function(pkg) {
  frictionless::check_package(pkg)

  if (!("geolocatordp" %in% class(pkg))) {
    cli_abort(
      c(
        "{.arg pkg} must be a GeoLocator Data Package object.",
        "x" = "{.arg pkg} is missing a {.val geolocatordp} class."
      )
    )
  }
  invisible(pkg)
}
