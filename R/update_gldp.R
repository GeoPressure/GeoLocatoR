#' Update derived values of a GeoLocator Data Package
#'
#' @description
#' Updates and stores derived package values directly on `pkg`.
#'
#' - `update_gldp_temporal()`: Date range from `measurements$datetime`.
#' - `update_gldp_taxonomic()`: Species vector from tags (optionally filtered by measured tag IDs).
#' - `update_gldp_number_tags()`: Counts by resource/sensor.
#' - `update_gldp_bibliographic_citation()`: Formatted citation from package metadata.
#' - `update_gldp_order_resources()`: Reorder resources following schema order.
#'
#' `update_gldp()` runs all functions above.
#'
#' @param pkg A GeoLocator Data Package object
#' @param ... overwrite parameters for `utils::bibentry()`
#'
#' @return An updated GeoLocator Data Package object with derived fields populated.
#' @export
update_gldp <- function(pkg, ...) {
  pkg <- pkg |>
    update_gldp_order_resources() |>
    update_gldp_temporal() |>
    update_gldp_taxonomic() |>
    update_gldp_number_tags() |>
    update_gldp_bibliographic_citation(...)

  pkg
}

#' @rdname update_gldp
#' @export
update_gldp_order_resources <- function(pkg) {
  check_gldp(pkg)

  pkg_schema <- jsonlite::fromJSON(
    pkg$`$schema`,
    simplifyDataFrame = FALSE,
    simplifyVector = TRUE
  )

  resource_order <- pkg_schema$allOf[[2]]$properties$resources$items$oneOf |>
    purrr::map(~ .x$properties$name$enum %||% .x$properties$name$const %||% character(0)) |>
    purrr::flatten_chr()

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


#' @rdname update_gldp
#' @export
update_gldp_temporal <- function(pkg) {
  check_gldp(pkg)
  resources <- frictionless::resources(pkg)

  if (!"measurements" %in% resources) {
    pkg$temporal <- NULL
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

#' @rdname update_gldp
#' @export
update_gldp_taxonomic <- function(pkg) {
  check_gldp(pkg)
  resources <- frictionless::resources(pkg)

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

#' @rdname update_gldp
#' @export
update_gldp_number_tags <- function(pkg) {
  check_gldp(pkg)
  resources <- frictionless::resources(pkg)

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
    out$light <- length(unique(m$tag_id[m$sensor == "light"]))
    out$pressure <- length(unique(m$tag_id[m$sensor == "pressure"]))
    out$activity <- length(unique(m$tag_id[m$sensor == "activity" | m$sensor == "pitch"]))
    out$temperature_external <- length(unique(m$tag_id[m$sensor == "temperature_external"]))
    out$temperature_internal <- length(unique(m$tag_id[m$sensor == "temperature_internal"]))
    out$magnetic <- length(unique(m$tag_id[m$sensor == "magnetic_x"]))
    out$wet_count <- length(unique(m$tag_id[m$sensor == "wet_count"]))
    out$conductivity <- length(unique(m$tag_id[m$sensor == "conductivity"]))
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

#' @rdname update_gldp
#' @export
update_gldp_bibliographic_citation <- function(pkg, ...) {
  check_gldp(pkg)

  doi_raw <- first_non_empty_string(pkg$id, pkg$conceptdoi)
  doi <- if (!is.null(doi_raw)) {
    sub(
      "^https?://(dx\\.)?doi\\.org/|^https?://handle(\\.test)?\\.datacite\\.org/",
      "",
      doi_raw,
      ignore.case = TRUE
    )
  } else {
    NULL
  }

  has_doi <- !is.null(doi) && nzchar(doi)

  year_source <- first_non_empty_string(pkg$created)
  year <- NULL
  if (is_non_empty_string(year_source)) {
    parsed <- suppressWarnings(as.Date(substr(year_source, 1, 10)))
    if (!is.na(parsed)) {
      year <- format(parsed, "%Y")
    } else if (grepl("^[0-9]{4}", year_source)) {
      year <- substr(year_source, 1, 4)
    }
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
