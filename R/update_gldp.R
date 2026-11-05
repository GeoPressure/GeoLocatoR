#' Update derived properties of a GeoLocator Data Package
#'
#' @description
#' Recomputes and stores the derived package properties:
#'
#' - `resources`: reordered following the schema order.
#' - `temporal`: date range from `measurements$datetime`, left unchanged when
#'   the package has no `measurements`.
#' - `taxonomic`: species vector from `tags`, restricted to tags present in
#'   `measurements` when the package has measurements.
#' - `numberTags`: counts by resource and sensor.
#' - `bibliographicCitation`: formatted citation from package metadata.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param ... Overrides passed to [utils::bibentry()] when computing
#'   `bibliographicCitation`.
#'
#' @return An updated GeoLocator Data Package object with derived properties
#'   populated.
#'
#' @seealso [read_gldp()] and [read_zenodo()] call this function after reading,
#'   and the resource setters such as [tags()] and [measurements()] update
#'   selected derived properties automatically.
#' @export
update_gldp <- function(pkg, ...) {
  pkg <- gldp_update_order_resources(pkg)

  # Several derived fields read tags and measurements: a copy holding the
  # file-backed ones in memory reads each file once, and pkg keeps its paths.
  loaded <- pkg
  for (name in intersect(c("tags", "measurements"), frictionless::resource_names(pkg))) {
    resource <- gldp_resource(pkg, name)
    if (!is.null(resource$path) && is.list(resource$schema)) {
      gldp_resource(loaded, name) <- gldp_load_table(pkg, name)
    }
  }
  loaded <- loaded |>
    gldp_update_temporal() |>
    gldp_update_taxonomic() |>
    gldp_update_number_tags()
  for (field in c("temporal", "taxonomic", "numberTags")) {
    pkg[[field]] <- loaded[[field]]
  }

  pkg <- gldp_update_bibliographic_citation(pkg, ...)

  pkg
}

#' Reorder resources to schema order
#'
#' @noRd
gldp_update_order_resources <- function(pkg) {
  check_gldp(pkg)
  version <- gldp_version(pkg)

  pkg_schema <- gldp_profile_schema(version)

  resource_order <- gldp_profile_resource_names(pkg_schema)

  if (length(resource_order) == 0) {
    return(pkg)
  }

  resources <- pkg$resources %||% list()
  if (length(resources) <= 1) {
    return(pkg)
  }

  resource_names <- purrr::map_chr(resources, ~ as.character((.x$name %||% NA_character_)[1]))
  rank <- match(resource_names, resource_order)

  unknown <- is.na(rank)
  rank[unknown] <- length(resource_order) + seq_len(sum(unknown))

  pkg$resources <- resources[order(rank)]
  pkg
}


#' Recompute package temporal coverage
#'
#' @noRd
gldp_update_temporal <- function(pkg) {
  check_gldp(pkg)
  resources <- frictionless::resource_names(pkg)

  # Without measurements (e.g. `read_gldp(drop_measurements = TRUE)`), keep the recorded coverage.
  if (!"measurements" %in% resources) {
    return(pkg)
  }

  m <- measurements(pkg)
  if (!"datetime" %in% names(m)) {
    pkg$temporal <- NULL
    return(pkg)
  }

  datetime <- stats::na.omit(m$datetime)
  if (length(datetime) == 0) {
    pkg$temporal <- NULL
    return(pkg)
  }

  pkg$temporal <- list(
    start = format(min(datetime), "%Y-%m-%d"),
    end = format(max(datetime), "%Y-%m-%d")
  )

  pkg
}

#' Recompute package taxonomic coverage
#'
#' @noRd
gldp_update_taxonomic <- function(pkg) {
  check_gldp(pkg)
  resources <- frictionless::resource_names(pkg)

  if (!"tags" %in% resources) {
    pkg$taxonomic <- NULL
    return(pkg)
  }

  t <- tags(pkg)
  if (!("scientific_name" %in% names(t))) {
    pkg$taxonomic <- NULL
    return(pkg)
  }

  if ("measurements" %in% resources) {
    m <- measurements(pkg)
  } else {
    m <- NULL
  }

  if (is.data.frame(m) && "tag_id" %in% names(m)) {
    measured_tag_ids <- unique(m$tag_id)
    species <- t |>
      dplyr::filter(.data$tag_id %in% measured_tag_ids) |>
      dplyr::pull(.data$scientific_name)
  } else {
    species <- t |>
      dplyr::pull(.data$scientific_name)
  }

  species <- unique(as.character(stats::na.omit(species)))
  pkg$taxonomic <- if (length(species) > 0) species else NULL

  pkg
}

#' Recompute tag counts by resource and sensor
#'
#' @noRd
gldp_update_number_tags <- function(pkg) {
  check_gldp(pkg)
  resources <- frictionless::resource_names(pkg)

  out <- list()

  t <- if ("tags" %in% resources) tags(pkg) else NULL
  if (is.data.frame(t) && "tag_id" %in% names(t)) {
    out$tags <- length(unique(t$tag_id))
  }

  m <- if ("measurements" %in% resources) measurements(pkg) else NULL
  if (is.data.frame(m) && all(c("tag_id", "sensor") %in% names(m))) {
    if ("label" %in% names(m)) {
      m <- dplyr::filter(m, .data$label != "discard" | is.na(.data$label))
    }

    out$measurements <- length(unique(m$tag_id))
    # A missing sensor is not counted.
    out$light <- length(unique(m$tag_id[m$sensor %in% "light"]))
    out$pressure <- length(unique(m$tag_id[m$sensor %in% "pressure"]))
    out$activity <- length(unique(m$tag_id[m$sensor %in% c("activity", "mean_acceleration_z")]))
    out$temperature_external <- length(unique(m$tag_id[m$sensor %in% "temperature_external"]))
    out$temperature_internal <- length(unique(m$tag_id[m$sensor %in% "temperature_internal"]))
    out$magnetic <- length(unique(m$tag_id[m$sensor %in% "magnetic_x"]))
    out$wet_count <- length(unique(m$tag_id[m$sensor %in% "wet_count"]))
    out$conductivity <- length(unique(m$tag_id[m$sensor %in% "conductivity"]))
  }

  p <- if ("paths" %in% resources) paths(pkg) else NULL
  if (is.data.frame(p) && "tag_id" %in% names(p)) {
    out$paths <- length(unique(p$tag_id))
  }

  pp <- if ("pressurepaths" %in% resources) pressurepaths(pkg) else NULL
  if (is.data.frame(pp) && "tag_id" %in% names(pp)) {
    out$pressurepaths <- length(unique(pp$tag_id))
  }

  pkg$numberTags <- if (length(out) > 0) out else NULL

  pkg
}

#' Recompute bibliographic citation text
#'
#' @noRd
gldp_update_bibliographic_citation <- function(pkg, ...) {
  check_gldp(pkg)

  # First DOI among the concept id (citing all versions) and the version id.
  doi_candidates <- sub(
    "^https?://(dx\\.)?doi\\.org/",
    "",
    as.character(c(pkg$conceptid, pkg$id)),
    ignore.case = TRUE
  )
  doi <- purrr::detect(doi_candidates, \(x) grepl("^10\\.", x))

  has_doi <- !is.null(doi) && nzchar(doi)

  # Year of the ISO 8601 `created` date, which may also be a bare year or year-month.
  year_source <- first_non_empty_string(pkg$created)
  year <- if (!is.null(year_source) && grepl("^[0-9]{4}", year_source)) {
    substr(year_source, 1, 4)
  }

  author <- if (!is.null(pkg$contributors)) contributors_to_persons(pkg$contributors) else NULL
  if (length(author) == 0) {
    author <- NULL
  }

  title <- first_non_empty_string(pkg$title)
  publisher <- first_non_empty_string(pkg$publisher)
  if (is.null(publisher) && has_doi && grepl("zenodo", doi, ignore.case = TRUE)) {
    publisher <- "Zenodo"
  }
  url <- first_non_empty_string(pkg$homepage)

  defaults <- list(
    bibtype = "Misc", # "dataset" is not available in utils::bibentry
    author = author,
    doi = doi,
    publisher = publisher,
    title = title,
    year = year,
    url = url
  )
  defaults <- defaults[
    !vapply(defaults, is.null, logical(1)) &
      !vapply(defaults, \(v) is.character(v) && length(v) == 1 && !nzchar(v), logical(1))
  ]

  bib_args <- utils::modifyList(defaults, list(...))

  has_payload <- any(c("author", "doi", "title", "year", "publisher") %in% names(bib_args))
  if (!has_payload) {
    pkg$bibliographicCitation <- NULL
    return(pkg)
  }

  pkg$bibliographicCitation <- tryCatch(
    {
      bib <- do.call(utils::bibentry, bib_args)
      format(bib, style = "text")
    },
    error = function(e) {
      NULL
    }
  )

  pkg
}

#' Convert contributors to person objects
#'
#' Internal helper function to convert a list of contributors to person objects
#' for use in DESCRIPTION files.
#'
#' @param contributors A list of contributor objects from a GeoLocator Data Package
#' @return A list of person objects
#' @noRd
contributors_to_persons <- function(contributors) {
  role_mapping <- c(
    "contactperson" = "ctr", # Contractor (assumed due to lack of clear match)
    "contributor" = "ctb", # Contributor
    "datacollector" = "dtc", # Data contributor
    "datacurator" = "dtc", # Data contributor (no closer match)
    "datamanager" = "dtc", # Data contributor (no closer match)
    "distributor" = "ctr", # Contractor (assumed)
    "editor" = "rev", # Reviewer
    "hostinginstitution" = "cph", # Copyright holder (legal entity)
    "producer" = "aut", # Author (substantial contribution)
    "projectleader" = "cre", # Creator (project leader matches)
    "projectmanager" = "cre", # Creator (manager of the project)
    "projectmember" = "ctb", # Contributor (smaller contributions)
    "registrationagency" = "ctr", # Contractor (assumed)
    "registrationauthority" = "ctr", # Contractor (assumed)
    "relatedperson" = "trl", # Translator (assumed)
    "researcher" = "aut", # Author
    "researchergroup" = "aut", # Author (group treated as authors)
    "rightsholder" = "cph", # Copyright holder
    "sponsor" = "fnd", # Funder
    "supervisor" = "ths", # Thesis advisor
    "workpackageleader" = "cre" # Creator (assumed leader role)
  )

  persons <- contributors |>
    purrr::map(
      ~ {
        utils::person(
          # An organisation has a title instead of a given name; either can be absent.
          given = .x$givenName %||% .x$title,
          family = .x$familyName,
          email = .x$email,
          role = purrr::map_vec(
            .x$roles,
            ~ coalesce(role_mapping[tolower(.x)], "ctb")
          ),
          comment = c(.x$path, .x$organization)
        )
      }
    )

  persons <- do.call(c, Filter(Negate(is.null), persons))

  persons
}
