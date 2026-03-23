#' Read a GeoLocator Data Package
#'
#' Reads a local or remote `datapackage.json`, validates that it uses the
#' GeoLocator-DP profile schema, upgrades older versions when needed, and
#' computes derived metadata.
#'
#' Schema/profile validation uses bundled local schema files shipped with
#' GeoLocatoR (`inst/schemas`) for supported versions. Runtime reading and
#' upgrade do not fetch GeoLocator-DP schemas from the internet.
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
  base_dir <- dirname(x)

  if (!grepl("geolocator-dp-profile\\.json$", pkg$`$schema`)) {
    cli_abort(
      "The datapackage provided does not seem to be a GeoLocator Data Package."
    )
  }

  # Add class
  class(pkg) <- c("geolocatordp", class(pkg))

  # Optionally load each resource table immediately into memory.
  # Goal: return a self-contained package where `resources[[i]]$data` is available
  # and `path` is dropped, while still surfacing readr parsing issues with location.
  if (force_read) {
    pkg$resources <- purrr::map(pkg$resources, \(r) {
      resource_name <- as.character(r$name %||% NA_character_)[1]
      resource_path <- as.character(r$path %||% NA_character_)[1]

      # When modification of the csv were made without adapting the datapackage.json,
      # this create issue in the reading of the resources. For now, only if
      # a change of variable type is observed, we get a read_csv warning.
      # But this warning is impossible to know where it occurs which resources
      # and which column. So we add this bad catching of error
      has_parsing_warning <- FALSE
      df <- withCallingHandlers(
        frictionless::read_resource(pkg, resource_name),
        warning = function(w) {
          msg <- conditionMessage(w)
          if (grepl("One or more parsing issues", msg, fixed = TRUE)) {
            has_parsing_warning <<- TRUE
            invokeRestart("muffleWarning")
          }
        }
      )

      if (has_parsing_warning) {
        resource_location <- if (!is.na(resource_path) && nzchar(resource_path)) {
          if (grepl("^https?://", resource_path)) {
            resource_path
          } else {
            file.path(base_dir, resource_path)
          }
        } else {
          base_dir
        }

        problems <- tryCatch(
          readr::problems(df),
          error = \(e) tibble::tibble()
        )
        problem_preview <- if (nrow(problems) > 0) {
          probs <- dplyr::slice_head(problems, n = 3)
          purrr::pmap_chr(probs, \(row, col, expected, actual, file, ...) {
            glue::glue("row {row}, col {col}: expected {expected}, got {actual}")
          })
        } else {
          character(0)
        }

        cli_warn(c(
          "!" = "Parsing issues detected while reading resource {.field {resource_name}}.",
          "i" = "Location: {.file {resource_location}}",
          "i" = if (length(problem_preview) > 0) {
            "First issues: {glue::glue_collapse(problem_preview, sep = '; ')}"
          } else {
            "Run {.code readr::problems()} on this resource to inspect row-level parsing issues."
          }
        ))
      }

      # Store casted data in the resource and clear path to keep the object in-memory.
      r$data <- cast_table(df, r$schema)
      r$path <- NULL
      r
    })
  }

  pkg$params <- list()
  params_path <- file.path(base_dir, "params.json")
  if (file.exists(params_path)) {
    pkg$params <- tryCatch(
      jsonlite::unserializeJSON(paste(readLines(params_path, warn = FALSE), collapse = "\n")),
      error = function(e) {
        cli_warn("Could not parse {.file params.json}: {e$message}")
        list()
      }
    )
  }

  # Conversion
  pkg <- upgrade_gldp(pkg)

  pkg <- update_gldp(pkg)

  pkg
}
