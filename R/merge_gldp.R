#' Merge Two GeoLocator Data Packages
#'
#' Merges two GeoLocator Data Package objects (`x` and `y`) into a single combined package.
#' The metadata properties from both packages are merged according to specific rules,
#' and resource data is combined based on their presence in either package.
#'
#' @details
#' **Resource merging logic:**
#' - Each resource is checked for its presence in both `x` and `y`.
#' - Data from both sources is combined if the resource exists in either `x` or `y`.
#' - Resources are only included if they exist in at least one of the packages.
#'
#' **Check for unique tag_id:**
#' - The merge aborts if any `tag_id` is present in both packages.
#' - This prevents ambiguous joins across resources after merge.
#'
#' **datapackage_id in tags:**
#' - For the `tags` resource, each row must have a `datapackage_id`.
#' - Missing `datapackage_id` values are filled with the package `id`.
#' - The merge aborts if `tags$datapackage_id` overlaps between the two inputs.
#'
#' **Metadata merging rules:**
#' - **version**: Using the package default one
#' - **title**: Combined from both packages, separated by a "/".
#' - **contributors**: Combined from both packages, with duplicates removed.
#' - **licenses**: Combined, with duplicates removed.
#' - **relatedIdentifiers**: Combined, with duplicates removed, and each source
#'   package `id` is added as `IsCompiledBy`.
#' - **created**: Set to the current timestamp at the time of merging.
#' - All others are dropped
#'
#' @param x A GeoLocator Data Package object.
#' @param y A GeoLocator Data Package object.
#' @return A GeoLocator Data Package object containing the merged data from both `x` and `y`.
#'
#' @export
merge_gldp <- function(x, y) {
  # Validate input packages
  check_gldp(x)
  check_gldp(y)
  x <- update_gldp(x)
  y <- update_gldp(y)

  # Check if the versions are the same
  vx <- gldp_version(x)
  vy <- gldp_version(y)
  if (!is.na(vx) && !is.na(vy) && vx != vy) {
    cli_warn(c(
      "!" = "The spec versions of {.pkg x} ({vx}) and {.pkg y} ({vy}) differ.",
      ">" = "This might cause merging to fail."
    ))
  }

  # Check for duplicate tag_ids
  common_tags <- intersect(tags(x)$tag_id, tags(y)$tag_id)
  if (length(common_tags) > 0) {
    cli_abort(c(
      "Duplicate {.field tag_id} detected: {col_red(glue::glue_collapse(common_tags, sep = ', '))}.",
      "x" = "Merging may fail due to duplicate tag IDs.",
      "i" = "Consider renaming or deleting one of the conflicting tag IDs."
    ))
  }

  # Merge related identifiers and add source datapackage ids.
  relatedIdentifiers <- unique(c(x$relatedIdentifiers, y$relatedIdentifiers))
  add_related_id <- function(id, related_ids) {
    if (is.null(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
      return(related_ids)
    }

    id_lower <- tolower(id)
    related_identifier_type <- if (
      grepl("^10\\.", id) || grepl("doi.org/", id_lower, fixed = TRUE)
    ) {
      "DOI"
    } else {
      "URL"
    }

    new_related_id <- list(
      relationType = "IsCompiledBy",
      relatedIdentifier = id,
      resourceTypeGeneral = "Dataset",
      relatedIdentifierType = related_identifier_type
    )
    related_ids <- c(related_ids, list(new_related_id))
    related_ids
  }
  relatedIdentifiers <- add_related_id(x$id, relatedIdentifiers)
  relatedIdentifiers <- add_related_id(y$id, relatedIdentifiers)
  relatedIdentifiers <- unique(relatedIdentifiers)

  # Combine metadata fields
  xy <- create_gldp()

  # Merge resources from both packages
  res_x <- frictionless::resources(x)
  res_y <- frictionless::resources(y)
  res <- unique(c(res_x, res_y)) # Combine resource names without duplicates

  # Iterate over all resources and merge their data if available
  xy <- purrr::reduce(
    res,
    function(xy, r) {
      # Read resource data if it exists in x or y
      data_x <- if (r %in% res_x) {
        frictionless::read_resource(x, resource_name = r)
      } else {
        NULL
      }
      data_y <- if (r %in% res_y) {
        frictionless::read_resource(y, resource_name = r)
      } else {
        NULL
      }

      # Tag-specific merge rules.
      if (r == "tags") {
        # Require a non-empty package id for both inputs before merging.
        ensure_pkg_id <- function(id, which_pkg) {
          if (is.null(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
            cli_abort(
              "Missing {.field id} in {.pkg {which_pkg}}. Merging requires a non-empty {.field id} for each datapackage.",
            )
          }
        }
        ensure_pkg_id(x$id, "x")
        ensure_pkg_id(y$id, "y")

        # Add missing datapackage_id values without overwriting existing ones.
        fill_datapackage_id <- function(df, id) {
          if (!"datapackage_id" %in% names(df)) {
            df$datapackage_id <- id
            return(df)
          }

          i <- is.na(df$datapackage_id) | df$datapackage_id == ""
          df$datapackage_id[i] <- id
          df
        }
        data_x <- fill_datapackage_id(data_x, x$id)
        data_y <- fill_datapackage_id(data_y, y$id)

        if (!is.null(data_x) && !is.null(data_y)) {
          # Prevent overlap between packages.
          ids_x <- unique(data_x$datapackage_id)
          ids_y <- unique(data_y$datapackage_id)
          common_ids <- intersect(ids_x, ids_y)

          if (length(common_ids) > 0) {
            cli_abort(c(
              "Duplicate {.field datapackage_id} detected: {col_red(glue::glue_collapse(common_ids, sep = ', '))}.",
              "x" = "Tags from both packages share the same datapackage_id.",
              "i" = "Ensure each package has distinct datapackage_id values before merging."
            ))
          }
        }
      }

      # Only add the resource if data is available in either x or y
      if (!is.null(data_x) || !is.null(data_y)) {
        combined_data <- dplyr::bind_rows(data_x, data_y) # Combine data from x and y
        xy <- add_gldp_resource(
          pkg = xy,
          resource_name = r,
          data = combined_data,
          cast_type = TRUE # Ensure data types are correctly cast
        )
      }

      xy
    },
    .init = xy
  )

  merged_licenses <- unique(c(x$licenses, y$licenses))

  # Keep only merged metadata fields using current top-level names.
  title_parts <- c(x$title %||% NA_character_, y$title %||% NA_character_)
  title_parts <- title_parts[!is.na(title_parts) & nzchar(title_parts)]
  xy$title <- if (length(title_parts) > 0) {
    paste(title_parts, collapse = " / ")
  } else {
    NULL
  }
  xy$contributors <- unique(c(x$contributors, y$contributors))
  xy$licenses <- merged_licenses
  xy$relatedIdentifiers <- relatedIdentifiers
  xy$created <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

  # Final update for any remaining metadata or properties
  xy <- update_gldp(xy)

  xy
}
