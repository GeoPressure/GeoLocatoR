#' Read a GeoLocator Data Package
#'
#' @description
#' Read a local or remote `datapackage.json` and build a `geolocatordp` object.
#' The function:
#' 1. reads the package descriptor with [frictionless::read_package()];
#' 2. checks that it uses the GeoLocator-DP profile schema;
#' 3. optionally loads resource tables into memory;
#' 4. reads `params.json` when present;
#' 5. upgrades older package versions when needed;
#' 6. recomputes derived package properties with [update_gldp()].
#'
#' @param x Path to a GeoLocator-DP package directory or `datapackage.json`
#'   file, or a URL to `datapackage.json`.
#' @param force_read If `TRUE` (default), loads resource data into memory so
#'   the returned object is self-contained in memory. This means that each
#' resource is read immediately with [frictionless::read_resource()], cast to
#' the schema types, stored in `resource$data`, and its `path` field is removed.
#'   The `measurements` resource is read last.
#' @param drop_measurements `r lifecycle::badge("experimental")` If `TRUE`,
#'   drops the `measurements` resource after reading `datapackage.json` and
#'   before the optional `force_read` step loads resource data into memory. Use
#'   this to save memory and avoid slowdowns when working mainly with derived
#'   GeoPressure resources such as `staps`, `paths`, `edges`, or
#'   `pressurepaths`.
#'
#' @return A `geolocatordp` object.
#'
#' @examples
#' \dontrun{
#' # Read a local package
#' pkg <- read_gldp("datapackage.json")
#'
#' # Read a remote package
#' pkg_remote <- read_gldp("https://example.org/datapackage.json")
#'
#' # Keep resource paths instead of loading all tables into memory
#' pkg_lazy <- read_gldp("datapackage.json", force_read = FALSE)
#'
#' # Skip the measurements table to reduce memory use
#' pkg_light <- read_gldp("datapackage.json", drop_measurements = TRUE)
#' }
#'
#' @seealso [create_gldp()] to create a new package shell, [write_gldp()] to
#'   write a package to disk, and [read_zenodo()] to read a package from Zenodo.
#'
#' @export
read_gldp <- function(x = "datapackage.json", force_read = TRUE, drop_measurements = FALSE) {
  if (dir.exists(x)) {
    x <- file.path(x, "datapackage.json")
    if (!file.exists(x)) {
      cli_abort(c(
        "x" = "Directory {.file {dirname(x)}} does not contain {.file datapackage.json}.",
        "i" = "Supply a GeoLocator Data Package directory or the path to its {.file datapackage.json}."
      ))
    }
  }

  pkg <- withCallingHandlers(
    frictionless::read_package(x),
    warning = function(w) {
      if (inherits(w, "frictionless_warning_version_not_supported")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  base_dir <- dirname(x)

  if (!grepl("geolocator-dp-profile\\.json$", pkg$`$schema`)) {
    cli_abort(
      "The datapackage provided does not seem to be a GeoLocator Data Package."
    )
  }

  # Add class
  class(pkg) <- c("geolocatordp", class(pkg))
  pkg[["$schema"]] <- gsub("Rafnuss", "GeoPressure", pkg[["$schema"]], fixed = TRUE)
  pkg[["resources"]] <- purrr::map(pkg[["resources"]] %||% list(), \(r) {
    # Only rewrite an existing `$schema`; adding an empty one would write
    # `"$schema": ""` back out, which no Data Package version allows.
    if (!is.null(r[["$schema"]])) {
      r[["$schema"]] <- gsub("Rafnuss", "GeoPressure", r[["$schema"]], fixed = TRUE)
    }
    r
  })

  # Drop measurements
  if (isTRUE(drop_measurements)) {
    pkg$resources <- purrr::keep(pkg$resources, \(r) !identical(r$name, "measurements"))
  }

  # Optionally load each resource table immediately into memory.
  # Goal: return a self-contained package where `resources[[i]]$data` is available
  # and `path` is dropped, while still surfacing readr parsing issues with location.
  # Note that `path` is dropped as `data` is set: the Data Package standard
  # allows one or the other, never both, and frictionless enforces it.
  # frictionlessdata/frictionless-r#372 proposes this as
  # `read_package(load = ...)` upstream; once it lands this block can delegate
  # to it, with `drop_measurements` becoming a `load` selection.
  if (force_read) {
    read_order <- order(vapply(
      pkg[["resources"]],
      \(r) identical(r[["name"]], "measurements"),
      logical(1)
    ))
    pkg[["resources"]][read_order] <- purrr::map(pkg[["resources"]][read_order], \(r) {
      resource_name <- as.character(r$name %||% NA_character_)[1]
      resource_path <- as.character(r$path %||% NA_character_)[1]
      resource_location <- if (!is.na(resource_path) && nzchar(resource_path)) {
        if (grepl("^https?://", resource_path)) {
          resource_path
        } else {
          file.path(base_dir, resource_path)
        }
      } else {
        base_dir
      }

      # When modification of the csv were made without adapting the datapackage.json,
      # this create issue in the reading of the resources. For now, only if
      # a change of variable type is observed, we get a read_csv warning.
      # But this warning is impossible to know where it occurs which resources
      # and which column. So we add this bad catching of error
      has_parsing_warning <- FALSE
      df <- tryCatch(
        withCallingHandlers(
          gldp_read_resource(pkg, resource_name),
          warning = function(w) {
            msg <- conditionMessage(w)
            if (grepl("One or more parsing issues", msg, fixed = TRUE)) {
              has_parsing_warning <<- TRUE
              invokeRestart("muffleWarning")
            }
          }
        ),
        error = function(e) {
          cli_abort(
            c(
              "x" = "Could not read resource {.field {resource_name}}.",
              "i" = "Location: {.file {resource_location}}",
              "i" = "The resource descriptor or CSV does not match the declared schema.",
              "i" = "Original error: {conditionMessage(e)}"
            ),
            parent = e
          )
        }
      )

      if (has_parsing_warning) {
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
