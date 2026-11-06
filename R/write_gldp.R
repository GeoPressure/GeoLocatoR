#' Write a GeoLocator Data Package to disk
#'
#' @description
#' Write a GeoLocator Data Package to disk with
#' [frictionless::write_package()]. Standard datapackage files are written from
#' the package descriptor and resources, while `pkg$params` is written
#' separately to `params.json` when it is not empty, and declared as the
#' `params` resource of the package.
#'
#' `params.json` is plain JSON, one object per parameter list, readable without
#' R. JSON has no R types, so reading it back with [read_gldp()] returns:
#' - numbers to 15 significant digits;
#' - dates and times as ISO 8601 text, in UTC and to the second;
#' - version numbers (e.g. `GeoPressureR_version`) as text;
#' - functions and unevaluated calls as their R source text;
#' - a scalar `NA`, and `NA` in text, as the text `"NA"` (numeric vectors of
#'   length > 1 keep it), and a text `"Inf"`, `"-Inf"` or `"NaN"` of length 1
#'   as a number;
#' - a vector or column of missing values only as logical;
#' - empty vectors as empty lists, and atomic vectors without their names;
#' - objects without their R class (such as `param`, `twl_calib` or glue
#'   text), and factors as text.
#'
#' Data frames, such as `tag_set_map$known`, `NULL` values, logical, integer and
#' double types, and the length of vectors are kept.
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

  # The params resource is written from `pkg$params` below, not copied from a source file.
  pkg_without_params <- pkg
  pkg_without_params[["params"]] <- NULL
  pkg_without_params[["resources"]] <- purrr::discard(pkg$resources, \(r) {
    identical(r$name, "params")
  })

  frictionless::write_package(
    package = pkg_without_params,
    directory = directory,
    compress = compress
  )

  # read_gldp() sets `params` to an empty list when there is no params.json.
  if (length(pkg$params) > 0) {
    # JSON has no R classes: version objects are written as text, and functions
    # and unevaluated calls (default arguments) as their R source.
    as_json <- function(x) {
      if (inherits(x, "numeric_version")) {
        return(as.character(x))
      }
      if (is.function(x) || is.language(x)) {
        return(as.character(glue::glue_collapse(deparse(x), "\n")))
      }
      if (is.list(x) && !is.data.frame(x)) {
        return(purrr::map(unclass(x), as_json))
      }
      x
    }
    # always_decimal keeps doubles apart from integers; na = "string" keeps Inf,
    # which jsonlite otherwise writes as null; force unclasses other objects.
    writeLines(
      jsonlite::toJSON(
        as_json(pkg$params),
        auto_unbox = TRUE,
        digits = NA,
        null = "null",
        na = "string",
        always_decimal = TRUE,
        POSIXt = "ISO8601",
        UTC = TRUE,
        pretty = TRUE,
        force = TRUE
      ),
      file.path(directory, "params.json")
    )

    # frictionless writes the descriptor before params.json exists, so the
    # resource is declared afterwards, with the same JSON options.
    descriptor_path <- file.path(directory, "datapackage.json")
    descriptor <- jsonlite::read_json(descriptor_path)
    descriptor$resources <- c(
      descriptor$resources,
      list(list(name = "params", path = "params.json"))
    )
    write(
      jsonlite::toJSON(descriptor, pretty = TRUE, null = "null", na = "null", auto_unbox = TRUE),
      descriptor_path
    )
  }

  invisible(directory)
}
