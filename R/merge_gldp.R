#' Merge GeoLocator Data Packages
#'
#' Merges two or more GeoLocator Data Package objects into a single combined package.
#' The metadata properties from both packages are merged according to specific rules,
#' and resource data is combined based on their presence in either package.
#'
#' @details
#' **Input formats:**
#' - `merge_gldp(x, y, ...)`
#' - `merge_gldp(list(x, y, ...))`
#' - `merge_gldp(x, list(y, ...))`
#'
#' **Resource merging logic:**
#' - Resource names come from the union of resources present in all inputs.
#' - For each resource, rows from available inputs are combined with `dplyr::bind_rows()`.
#' - A resource is added when at least one input package contains it.
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
#' **datapackages provenance table:**
#' - `pkg$datapackages` is a one-row-per-source tibble keyed by `datapackage_id`
#'   (`pkg$id`) and accumulated across sequential merges.
#' - The merge aborts if two input provenance rows share the same
#'   `datapackage_id`.
#' - Metadata vectors/lists are flattened to plain text for easy comparison
#'   between source packages.
#' - It includes key source metadata fields (e.g. title/version/status/access),
#'   temporal coverage (`temporal_start`, `temporal_end`), taxonomic summary
#'   (`taxonomic`), and resource/sensor counts as `numberTags_*` columns.
#'
#' **Metadata merging rules:**
#' - **version**: Uses the package default version from `create_gldp()`.
#' - **title**: Combined from both packages, separated by a "/".
#' - **contributors**: Combined from both packages, with duplicates removed.
#' - **licenses**: Combined, with duplicates removed.
#' - **record_type**: Combined from both packages, with duplicates removed.
#' - **codeRepository**: Combined from both packages, with duplicates removed.
#' - **datapackages**: Carries the merged provenance table described above.
#' - **relatedIdentifiers**: Combined, with duplicates removed, and each unique
#'   source `datapackage_id` (or package `id`) is added as `IsCompiledBy`.
#' - **params**: Concatenated from all inputs; every entry must contain a
#'   non-empty `param$id`.
#' - **created**: Set to the current timestamp at the time of merging.
#' - All others are dropped
#'
#' @param x A GeoLocator Data Package object, or a list of them.
#' @param y A GeoLocator Data Package object.
#' @param ... Additional GeoLocator Data Package objects, or lists of them.
#' @return A GeoLocator Data Package object containing the merged data from all inputs.
#'
#' @export
merge_gldp <- function(x, y = NULL, ...) {
  # Collect packages from positional and list-style input.
  pkgs <- merge_gldp_collect_inputs(x, y, ...)
  if (length(pkgs) < 2) {
    cli_abort("{.fn merge_gldp} requires at least two GeoLocator Data Package objects.")
  }

  # Normalize input packages and cache their available resources.
  purrr::walk(pkgs, check_gldp)
  pkgs <- purrr::map(pkgs, update_gldp)
  resources_by_pkg <- purrr::map(pkgs, frictionless::resources)
  resources <- unique(unlist(resources_by_pkg, use.names = FALSE))

  # Warn when several spec versions are present.
  versions <- unique(purrr::map_chr(pkgs, gldp_version))
  versions <- versions[!is.na(versions)]
  if (length(versions) > 1) {
    cli_warn(c(
      "!" = "Input packages use different spec versions: {glue::glue_collapse(versions, sep = ', ')}.",
      ">" = "Merging may fail or produce inconsistent merged fields."
    ))
  }

  # Build and validate source provenance table.
  datapackages <- merge_gldp_datapackages(pkgs)

  # Read and normalize tags once for provenance checks and merge.
  tags_by_pkg <- purrr::map2(pkgs, resources_by_pkg, merge_gldp_prepare_tags)

  # Prevent ambiguous joins from duplicated tag_id and datapackage_id.
  all_tags <- purrr::compact(tags_by_pkg)
  if (length(all_tags) > 1) {
    tag_ids_by_pkg <- purrr::map(all_tags, \(tags) {
      ids <- unique(as.character(tags$tag_id))
      ids[!is.na(ids) & nzchar(trimws(ids))]
    })
    all_tag_ids <- unlist(tag_ids_by_pkg, use.names = FALSE)
    duplicate_tag_ids <- unique(all_tag_ids[duplicated(all_tag_ids)])
    if (length(duplicate_tag_ids) > 0) {
      cli_abort(c(
        "Cannot merge: duplicate {.field tag_id} across input packages: {col_red(glue::glue_collapse(duplicate_tag_ids, sep = ', '))}.",
        "x" = "Each {.field tag_id} must be globally unique across all merged packages.",
        "i" = "Rename conflicting tag IDs or remove duplicate tag rows before merging."
      ))
    }

    datapackage_ids_by_pkg <- purrr::map(all_tags, \(tags) {
      ids <- unique(as.character(tags$datapackage_id))
      ids[!is.na(ids) & nzchar(trimws(ids))]
    })
    all_datapackage_ids <- unlist(datapackage_ids_by_pkg, use.names = FALSE)
    duplicate_datapackage_ids <- unique(all_datapackage_ids[duplicated(all_datapackage_ids)])
    if (length(duplicate_datapackage_ids) > 0) {
      cli_abort(c(
        "Cannot merge: duplicate {.field datapackage_id} in {.field tags} across input packages: {col_red(glue::glue_collapse(duplicate_datapackage_ids, sep = ', '))}.",
        "x" = "Each source package must have a distinct {.field datapackage_id}.",
        "i" = "Set unique {.field datapackage_id} values (or package {.field id}) before merging."
      ))
    }
  }

  # Merge related identifiers and add one IsCompiledBy entry per source id.
  source_ids <- purrr::map2(pkgs, tags_by_pkg, \(pkg, tags) {
    if (is.null(tags)) {
      return(as.character(pkg$id %||% NA_character_)[1])
    }
    as.character(tags$datapackage_id)
  })
  source_ids <- unique(purrr::flatten_chr(source_ids))
  source_ids <- source_ids[!is.na(source_ids) & nzchar(trimws(source_ids))]
  source_related <- purrr::map(source_ids, \(id) {
    list(
      relationType = "IsCompiledBy",
      relatedIdentifier = id,
      resourceTypeGeneral = "Dataset",
      relatedIdentifierType = if (
        grepl("^10\\.", id) || grepl("doi.org/", tolower(id), fixed = TRUE)
      ) {
        "DOI"
      } else {
        "URL"
      }
    )
  })
  related_identifiers <- unique(c(
    purrr::flatten(purrr::map(pkgs, \(pkg) pkg$relatedIdentifiers %||% list())),
    source_related
  ))

  # Merge resources from all packages.
  merged <- merge_gldp_resources(pkgs, resources, resources_by_pkg, tags_by_pkg)

  # Keep merged top-level metadata only.
  title_parts <- purrr::map_chr(pkgs, \(pkg) as.character(pkg$title %||% NA_character_)[1])
  title_parts <- title_parts[!is.na(title_parts) & nzchar(title_parts)]
  record_type <- unlist(
    purrr::map(pkgs, \(pkg) as.character(pkg$record_type %||% character(0))),
    use.names = FALSE
  )
  record_type <- unique(record_type[!is.na(record_type) & nzchar(trimws(record_type))])
  code_repository <- unlist(
    purrr::map(pkgs, \(pkg) as.character(pkg$codeRepository %||% character(0))),
    use.names = FALSE
  )
  code_repository <- unique(
    code_repository[!is.na(code_repository) & nzchar(trimws(code_repository))]
  )
  contributors <- unique(purrr::flatten(purrr::map(pkgs, \(pkg) pkg$contributors %||% list())))
  licenses <- unique(purrr::flatten(purrr::map(pkgs, \(pkg) pkg$licenses %||% list())))

  merged[["title"]] <- if (length(title_parts) > 0) {
    glue::glue_collapse(title_parts, sep = " / ")
  } else {
    NULL
  }
  merged[["contributors"]] <- if (length(contributors) > 0) contributors else NULL
  merged[["licenses"]] <- if (length(licenses) > 0) licenses else NULL
  merged[["record_type"]] <- if (length(record_type) > 0) record_type else NULL
  merged[["codeRepository"]] <- if (length(code_repository) > 0) code_repository else NULL
  merged[["relatedIdentifiers"]] <- if (length(related_identifiers) > 0) {
    related_identifiers
  } else {
    NULL
  }
  # Merge params from all sources and keep them as top-level package payload.
  all_params <- purrr::flatten(purrr::map(pkgs, \(pkg) pkg$params %||% list()))
  merged[["params"]] <- normalize_gldp_params(all_params)
  merged[["datapackages"]] <- datapackages
  merged[["created"]] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  update_gldp(merged)
}

#' @noRd
merge_gldp_collect_inputs <- function(x, y, ...) {
  # Flatten one level so callers can mix positional and list inputs.
  inputs <- c(list(x), list(y), list(...))

  purrr::reduce(
    inputs,
    function(out, input) {
      if (is.null(input)) {
        return(out)
      }
      if (inherits(input, "geolocatordp")) {
        return(c(out, list(input)))
      }
      if (is.list(input)) {
        return(c(out, input))
      }
      c(out, list(input))
    },
    .init = list()
  )
}

#' @noRd
merge_gldp_prepare_tags <- function(pkg, resources) {
  if (!"tags" %in% resources) {
    return(NULL)
  }

  tags <- frictionless::read_resource(pkg, resource_name = "tags")
  if (!"datapackage_id" %in% names(tags)) {
    tags$datapackage_id <- pkg$id
  }
  tags$datapackage_id[is.na(tags$datapackage_id) | tags$datapackage_id == ""] <- pkg$id
  tags
}

#' @noRd
merge_gldp_resources <- function(pkgs, resources, resources_by_pkg, tags_by_pkg) {
  purrr::reduce(
    resources,
    function(pkg, resource_name) {
      data_parts <- purrr::map(
        seq_along(pkgs),
        \(idx) {
          if (identical(resource_name, "tags")) {
            return(tags_by_pkg[[idx]])
          }
          if (!resource_name %in% resources_by_pkg[[idx]]) {
            return(NULL)
          }
          frictionless::read_resource(pkgs[[idx]], resource_name = resource_name)
        }
      )
      data_parts <- purrr::compact(data_parts)
      if (length(data_parts) == 0) {
        return(pkg)
      }
      add_gldp_resource(
        pkg = pkg,
        resource_name = resource_name,
        data = dplyr::bind_rows(data_parts),
        cast_type = TRUE
      )
    },
    .init = create_gldp()
  )
}

#' @noRd
merge_gldp_datapackages <- function(pkgs) {
  collapse_chr <- function(x, sep = ", ") {
    x <- as.character(unlist(x, recursive = TRUE, use.names = FALSE))
    x <- unique(x[!is.na(x) & nzchar(trimws(x))])
    if (length(x) == 0) NA_character_ else glue::glue_collapse(x, sep = sep)
  }
  collapse_related_ids <- function(x) {
    x <- x %||% list()
    vals <- purrr::map_chr(x, \(ri) {
      rel <- as.character(ri$relationType %||% "")[1]
      id <- as.character(ri$relatedIdentifier %||% "")[1]
      if (!nzchar(trimws(id))) {
        return(NA_character_)
      }
      if (nzchar(trimws(rel))) glue::glue("{rel}:{id}") else id
    })
    vals <- unique(vals[!is.na(vals) & nzchar(trimws(vals))])
    if (length(vals) == 0) NA_character_ else glue::glue_collapse(vals, sep = ", ")
  }
  collapse_contributors <- function(x) {
    x <- x %||% list()
    vals <- purrr::map_chr(x, \(ctr) {
      title <- as.character(ctr$title %||% "")[1]
      if (nzchar(trimws(title))) {
        return(title)
      }
      given <- as.character(ctr$givenName %||% "")[1]
      family <- as.character(ctr$familyName %||% "")[1]
      fullname <- trimws(glue::glue("{given} {family}"))
      if (nzchar(fullname)) fullname else NA_character_
    })
    vals <- unique(vals[!is.na(vals) & nzchar(trimws(vals))])
    if (length(vals) == 0) NA_character_ else glue::glue_collapse(vals, sep = ", ")
  }
  collapse_licenses <- function(x) {
    x <- x %||% list()
    vals <- purrr::map_chr(x, \(lic) {
      title <- as.character(lic$title %||% "")[1]
      name <- as.character(lic$name %||% "")[1]
      if (nzchar(trimws(title)) && nzchar(trimws(name))) {
        return(glue::glue("{title} ({name})"))
      }
      if (nzchar(trimws(title))) {
        return(title)
      }
      if (nzchar(trimws(name))) {
        return(name)
      }
      NA_character_
    })
    vals <- unique(vals[!is.na(vals) & nzchar(trimws(vals))])
    if (length(vals) == 0) NA_character_ else glue::glue_collapse(vals, sep = ", ")
  }
  number_tag_value <- function(pkg, key) {
    v <- pkg$numberTags[[key]] %||% NA_integer_
    if (length(v) == 0 || is.null(v) || is.na(v)) {
      return(NA_integer_)
    }
    as.integer(v)[1]
  }

  datapackage_columns <- c(
    "datapackage_id",
    "title",
    "version",
    "created",
    "status",
    "is_draft",
    "is_published",
    "access_status",
    "embargo",
    "conceptid",
    "codeRepository",
    "homepage",
    "communities",
    "contributors",
    "licenses",
    "keywords",
    "grants",
    "relatedIdentifiers",
    "temporal_start",
    "temporal_end",
    "taxonomic",
    "numberTags_tags",
    "numberTags_measurements",
    "numberTags_light",
    "numberTags_pressure",
    "numberTags_activity",
    "numberTags_temperature_external",
    "numberTags_temperature_internal",
    "numberTags_magnetic",
    "numberTags_wet_count",
    "numberTags_conductivity",
    "numberTags_paths",
    "numberTags_pressurepaths",
    "bibliographicCitation"
  )

  datapackage_row <- function(pkg) {
    tibble::tibble(
      datapackage_id = as.character(pkg$id %||% NA_character_)[1],
      title = as.character(pkg$title %||% NA_character_)[1],
      version = as.character(pkg$version %||% NA_character_)[1],
      created = as.character(pkg$created %||% NA_character_)[1],
      status = as.character(pkg$status %||% NA_character_)[1],
      is_draft = as.logical(pkg$is_draft %||% NA),
      is_published = as.logical(pkg$is_published %||% NA),
      access_status = as.character(pkg$access_status %||% NA_character_)[1],
      embargo = as.character(pkg$embargo %||% NA_character_)[1],
      conceptid = as.character(pkg$conceptid %||% NA_character_)[1],
      codeRepository = collapse_chr(pkg$codeRepository),
      homepage = as.character(pkg$homepage %||% NA_character_)[1],
      communities = collapse_chr(pkg$communities),
      contributors = collapse_contributors(pkg$contributors),
      licenses = collapse_licenses(pkg$licenses),
      keywords = collapse_chr(pkg$keywords),
      grants = collapse_chr(pkg$grants),
      relatedIdentifiers = collapse_related_ids(pkg$relatedIdentifiers),
      temporal_start = as.character(pkg$temporal$start %||% NA_character_)[1],
      temporal_end = as.character(pkg$temporal$end %||% NA_character_)[1],
      taxonomic = collapse_chr(pkg$taxonomic),
      numberTags_tags = number_tag_value(pkg, "tags"),
      numberTags_measurements = number_tag_value(pkg, "measurements"),
      numberTags_light = number_tag_value(pkg, "light"),
      numberTags_pressure = number_tag_value(pkg, "pressure"),
      numberTags_activity = number_tag_value(pkg, "activity"),
      numberTags_temperature_external = number_tag_value(pkg, "temperature_external"),
      numberTags_temperature_internal = number_tag_value(pkg, "temperature_internal"),
      numberTags_magnetic = number_tag_value(pkg, "magnetic"),
      numberTags_wet_count = number_tag_value(pkg, "wet_count"),
      numberTags_conductivity = number_tag_value(pkg, "conductivity"),
      numberTags_paths = number_tag_value(pkg, "paths"),
      numberTags_pressurepaths = number_tag_value(pkg, "pressurepaths"),
      bibliographicCitation = as.character(pkg$bibliographicCitation %||% NA_character_)[1]
    )
  }

  init_datapackages <- function(pkg) {
    row <- datapackage_row(pkg)
    row_id <- as.character(row$datapackage_id)[1]
    has_row_id <- !is.na(row_id) && nzchar(trimws(row_id))
    empty <- row[0, ]

    dp <- pkg$datapackages
    if (is.null(dp)) {
      return(if (has_row_id) row else empty)
    }
    dp <- tibble::as_tibble(dp)
    for (nm in datapackage_columns) {
      if (!nm %in% names(dp)) dp[[nm]] <- NA
    }
    dp <- dplyr::select(dp, dplyr::all_of(datapackage_columns))
    ids <- as.character(dp$datapackage_id)
    keep <- !is.na(ids) & nzchar(trimws(ids))
    dp <- dp[keep, , drop = FALSE]
    if (has_row_id && !row_id %in% dp$datapackage_id) {
      dp <- dplyr::bind_rows(dp, row)
    }
    dplyr::distinct(dp, .data$datapackage_id, .keep_all = TRUE)
  }

  datapackages <- dplyr::bind_rows(purrr::map(pkgs, init_datapackages))
  ids <- as.character(datapackages$datapackage_id)
  common <- unique(ids[duplicated(ids) & !is.na(ids) & nzchar(trimws(ids))])
  if (length(common) > 0) {
    cli_abort(c(
      "Cannot merge: duplicate {.field datapackage_id} in provenance {.field datapackages}: {col_red(glue::glue_collapse(common, sep = ', '))}.",
      "i" = "Merging requires distinct source package IDs."
    ))
  }

  dplyr::distinct(datapackages, .data$datapackage_id, .keep_all = TRUE)
}
