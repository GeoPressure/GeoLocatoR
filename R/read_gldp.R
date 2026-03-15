#' Read a GeoLocator Data Package
#'
#' Reads a local or remote `datapackage.json`, validates that it uses the
#' GeoLocator-DP profile schema, upgrades older versions when needed, and
#' computes derived metadata.
#'
#' @param x Path or URL to a GeoLocator-DP `datapackage.json` file.
#' @param force_read If `TRUE` (default), loads resource data into memory for
#'   resources defined by path/URL.
#'
#' @return A `geolocatordp` object.
#'
#' @examples
#' \dontrun{
#' pkg <- read_gldp("datapackage.json")
#' pkg_remote <- read_gldp("https://example.org/datapackage.json")
#' }
#' @export
read_gldp <- function(x = "datapackage.json", force_read = TRUE) {
  pkg <- frictionless::read_package(x)

  if (!grepl("geolocator-dp-profile\\.json$", pkg$`$schema`)) {
    cli_abort(
      "The datapackage provided does not seem to be a GeoLocator Data Package."
    )
  }

  # Add class
  class(pkg) <- c("geolocatordp", class(pkg))

  # Force the read of the data as
  if (force_read) {
    pkg$resources <- purrr::map(pkg$resources, \(r) {
      resource <- frictionless:::get_resource(pkg, r$name)
      if (resource$read_from == "path" || resource$read_from == "url") {
        r$data <- frictionless:::read_from_path(pkg, r$name, col_select = NULL)
        r$data <- cast_table(r$data, r$schema)
        r$path <- NULL
      }
      r
    })
  }

  }

  # Conversion
  pkg <- upgrade_gldp(pkg)

  pkg <- update_gldp(pkg)

  pkg
}
