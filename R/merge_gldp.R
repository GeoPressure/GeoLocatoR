#' Merge Two GeoLocator Data Packages
#'
#' Merges two GeoLocator Data Package objects (`x` and `y`) into a single combined package.
#' The metadata properties from both packages are merged according to specific rules,
#' and resource data is combined based on their presence in either package.
#'
#' **Metadata merging rules:**
#' - **title**: Combined from both packages, separated by a "/".
#' - **contributors**: Combined from both packages, with duplicates removed.
#' - **embargo**: Set to the latest date from both packages.
#' - **licenses**: Combined from both packages, with duplicates removed.
#' - **id**: Replaced with a new UUID for the merged package.
#' - **source_ids**: Added (custom property) storing the original package IDs.
#' - **description**: Combined as two separate paragraphs, with a newline separator.
#' - **version**: Use the latest version (same as `create_gldp()`)
#' - **relatedIdentifiers**: Combined, with duplicates removed.
#' - **grants**: Combined from both packages, with duplicates removed.
#' - **keywords**: Combined from both packages, with duplicates removed.
#' - **created**: Set to the current timestamp at the time of merging.
#' - **bibliographicCitation**: Removed from the merged package.
#' - Custom properties from `x` are retained in the merged package.
#'
#' @details
#' Merging requires the [`uuid`](https://cran.r-project.org/package=uuid) package to generate
#' a globally unique identifier for the merged package.
#'
#' **Resource merging logic:**
#' - Each resource is checked for its presence in both `x` and `y`.
#' - Data from both sources is combined if the resource exists in either `x` or `y`.
#' - Resources are only included if they exist in at least one of the packages.
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
      "Duplicate {.field tag_id} detected: {col_red(paste(common_tags, collapse = ', '))}.",
      "x" = "Merging may fail due to duplicate tag IDs.",
      "i" = "Consider renaming or deleting one of the conflicting tag IDs."
    ))
  }

  # Merge related identifiers (DOIs, if available, will be added)
  relatedIdentifiers <- unique(c(x$relatedIdentifiers, y$relatedIdentifiers))
  add_related_id <- function(id, related_ids) {
    if (grepl("doi", id %||% "", fixed = TRUE)) {
      new_related_id <- list(
        relationType = "IsCompiledBy",
        relatedIdentifier = id,
        resourceTypeGeneral = "Dataset",
        relatedIdentifierType = "DOI"
      )
      related_ids <- c(related_ids, list(new_related_id))
    }
    related_ids
  }
  relatedIdentifiers <- add_related_id(x$id, relatedIdentifiers)
  relatedIdentifiers <- add_related_id(y$id, relatedIdentifiers)

  if (!requireNamespace("uuid", quietly = TRUE)) {
    cli_abort("The {.pkg uuid} package is required for merging.")
  }

  # Combine metadata fields
  xy <- create_gldp(
    title = paste(x$title, y$title, sep = " / "), # Combine titles
    contributors = unique(c(x$contributors, y$contributors)), # Remove duplicates
    licenses = unique(c(x$licenses, y$licenses)), # Remove duplicates
    embargo = format(max(as.Date(x$embargo), as.Date(y$embargo)), "%Y-%m-%d"), # Latest embargo date
    id = uuid::UUIDgenerate(), # New globally unique id for the merged package
    description = paste(x$description, y$description, sep = "\n"), # Combine descriptions
    version = NULL, # Remove version
    relatedIdentifiers = relatedIdentifiers, # Merge related identifiers
    grants = unique(c(x$grants, y$grants)), # Remove duplicates
    keywords = unique(c(x$keywords, y$keywords)), # Remove duplicates
    created = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"), # Current timestamp
    bibliographicCitation = NULL # Remove bibliographic citation
  )

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
              "Duplicate {.field datapackage_id} detected: {col_red(paste(common_ids, collapse = ', '))}.",
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
          package = xy,
          resource_name = r,
          data = combined_data,
          cast_type = TRUE # Ensure data types are correctly cast
        )
      }

      xy
    },
    .init = xy
  )

  # Final update for any remaining metadata or properties
  xy <- update_gldp(xy)

  xy
}
