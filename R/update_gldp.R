#' Update computed metadata of a GeoLocator Data Package
#'
#' @description
#' This function update/create the following metadata of an existing GeoLocator Data Package
#' based on the data contained within the package.
#'
#' - `update_gldp_temporal()`: Sets `pkg$temporal` with the date range of the `datetime` values from
#'    the `measurements` resource, if available.
#' - `update_gldp_spatial()`: Sets `pkg$spatial` to the GeoJSON bounding box of all locations found
#'    in observation, paths and pressurepaths.
#' - `update_gldp_taxonomic()`: Sets `pkg$taxonomic` to the unique `tags$scientific_name` found in
#'    the observations.
#' - `update_gldp_number_tags()`:
#' - `update_gldp_bibliographic_citation()`: Sets `pkg$bibliographicCitation` to the formatted
#'    `utils::bibentry()` of the current package.
#' - `update_gldp_reference_location()`: Sets the `pkg$reference_location` field to the median
#'    latitude and longitude values from the `observations` resource, if available.
#' - `update_gldp_metadata`: Re-arrange the order of the properties according to the order of the
#' schema
#'
#' `update_gldp` performs all of the functions mentioned above.
#'
#' @param pkg A GeoLocator Data Package object
#' @param ... overwrite parameters for `utils::bibentry()`
#'
#' @return An updated GeoLocator Data Package object with modified metadata.
#'
#' @export
update_gldp <- function(pkg) {
  pkg <- pkg %>%
    update_gldp_temporal() %>%
    update_gldp_spatial() %>%
    update_gldp_taxonomic() %>%
    update_gldp_number_tags() %>%
    update_gldp_bibliographic_citation() %>%
    update_gldp_reference_location() %>%
    update_gldp_metadata()

  pkg
}


#' @rdname update_gldp
#' @export
update_gldp_temporal <- function(pkg) {
  check_gldp(pkg)
  if ("measurements" %in% frictionless::resources(pkg)) {
    m <- measurements(pkg)

    pkg$temporal <- list(
      start = format(min(m$datetime, na.rm = TRUE), "%Y-%m-%d"),
      end = format(max(m$datetime, na.rm = TRUE), "%Y-%m-%d")
    )
  } else {
    pkg$temporal <- NULL
  }
  pkg
}

#' @rdname update_gldp
#' @export
update_gldp_spatial <- function(pkg) {
  check_gldp(pkg)

  if ("observations" %in% frictionless::resources(pkg)) {
    spatial <- observations(pkg) %>% select("latitude", "longitude")

    if ("paths" %in% frictionless::resources(pkg)) {
      spatial <- bind_rows(
        spatial,
        paths(pkg) %>%
          transmute(
            latitude = .data$lat,
            longitude = .data$lon
          )
      )
    }
    if ("pressurepaths" %in% frictionless::resources(pkg)) {
      spatial <- bind_rows(
        spatial,
        pressurepaths(pkg) %>%
          transmute(
            latitude = .data$lat,
            longitude = .data$lon
          )
      )
    }

    # Check for valid (non-NA) latitude and longitude values
    valid_latitude <- spatial$latitude[!is.na(spatial$latitude)]
    valid_longitude <- spatial$longitude[!is.na(spatial$longitude)]

    if (length(valid_longitude) == 0 || length(valid_latitude) == 0) {
      pkg$spatial <- NULL
      return(pkg)
    }

    # Compute the min and max
    lat_min <- min(valid_latitude)
    lat_max <- max(valid_latitude)
    long_min <- min(valid_longitude)
    long_max <- max(valid_longitude)

    pkg$spatial <- list(
      type = "Polygon",
      coordinates = array(
        c(
          long_min,
          long_max,
          long_max,
          long_min,
          long_min,
          lat_min,
          lat_min,
          lat_max,
          lat_max,
          lat_min
        ),
        dim = c(1, 5, 2)
      )
    )
  } else {
    pkg$spatial <- NULL
  }

  pkg
}

#' @rdname update_gldp
#' @export
update_gldp_taxonomic <- function(pkg) {
  check_gldp(pkg)
  if ("measurements" %in% frictionless::resources(pkg)) {
    # Only use the list of species with data
    sp_has_data <- unique(measurements(pkg)$tag_id)
    pkg$taxonomic <- tags(pkg) %>%
      filter(.data$tag_id %in% sp_has_data) %>%
      pull(.data$scientific_name) %>%
      unique() %>%
      stats::na.omit() %>%
      as.character()
  } else if ("tags" %in% frictionless::resources(pkg)) {
    pkg$taxonomic <- tags(pkg) %>%
      filter(!is.na(.data$tag_id)) %>%
      pull(.data$scientific_name) %>%
      unique() %>%
      stats::na.omit() %>%
      as.character()
  } else {
    pkg$taxonomic <- NULL
  }
  pkg
}

#' @rdname update_gldp
#' @export
update_gldp_number_tags <- function(pkg) {
  check_gldp(pkg)

  pkg$numberTags <- list()

  if ("tags" %in% frictionless::resources(pkg)) {
    pkg$numberTags$tags <- length(unique(tags(pkg)$tag_id))
  }

  if ("measurements" %in% frictionless::resources(pkg)) {
    m <- measurements(pkg) %>%
      filter(.data$label != "discard" | is.na(.data$label))

    pkg$numberTags$measurements <- length(unique(m$tag_id))
    # Type of measurements
    pkg$numberTags$light <- length(unique(m$tag_id[m$sensor == "light"]))
    pkg$numberTags$pressure <- length(unique(m$tag_id[m$sensor == "pressure"]))
    pkg$numberTags$activity <-
      length(unique(m$tag_id[m$sensor == "activity" | m$sensor == "pitch"]))
    pkg$numberTags$temperature_external <-
      length(unique(m$tag_id[m$sensor == "temperature_external"]))
    pkg$numberTags$temperature_internal <-
      length(unique(m$tag_id[m$sensor == "temperature_internal"]))
    pkg$numberTags$magnetic <- length(unique(m$tag_id[
      m$sensor == "magnetic_x"
    ]))
    pkg$numberTags$wet_count <- length(unique(m$tag_id[
      m$sensor == "wet_count"
    ]))
    pkg$numberTags$conductivity <- length(unique(m$tag_id[
      m$sensor == "conductivity"
    ]))
  }

  if ("paths" %in% frictionless::resources(pkg)) {
    pkg$numberTags$paths <- length(unique(paths(pkg)$tag_id))
  }
  if ("pressurepaths" %in% frictionless::resources(pkg)) {
    pkg$numberTags$pressurepaths <- length(unique(pressurepaths(pkg)$tag_id))
  }

  pkg
}

#' @rdname update_gldp
#' @export
update_gldp_bibliographic_citation <- function(pkg, ...) {
  check_gldp(pkg)

  has_id <- !is.null(pkg$id) && length(pkg$id) == 1 && !is.na(pkg$id) && nzchar(pkg$id)
  default <- list(
    "Misc", # should be "dataset" but not available
    author = contributors_to_persons(pkg$contributors),
    doi = if (has_id) pkg$id else NULL,
    publisher = if (has_id && grepl("zenodo", pkg$id, ignore.case = TRUE)) "Zenodo" else NULL,
    title = pkg$title,
    year = format(as.Date(pkg$created), "%Y")
  )

  # Merge the defaults with the overrides
  bib_args <- utils::modifyList(default, list(...))

  # Create the bibentry object
  bib <- do.call(utils::bibentry, bib_args)

  # Update the bibliographic citation in the package
  pkg$bibliographicCitation <- format(bib, style = "text")

  pkg
}

#' @rdname update_gldp
#' @export
update_gldp_reference_location <- function(pkg) {
  check_gldp(pkg)
  if ("observations" %in% frictionless::resources(pkg)) {
    pkg$referenceLocation <- observations(pkg) %>%
      summarise(
        latitude = stats::median(.data$latitude, na.rm = TRUE),
        longitude = stats::median(.data$longitude, na.rm = TRUE)
      ) %>%
      as.list()
  } else {
    pkg$referenceLocation <- NULL
  }
  pkg
}

#' @rdname update_gldp
#' @export
update_gldp_metadata <- function(pkg) {
  # Order properties according to the schema
  schema <- jsonlite::fromJSON(pkg$`$schema`, simplifyVector = FALSE)
  properties <- names(schema$allOf[[2]]$properties)
  sorted_pkg <- c(
    pkg[intersect(properties, names(pkg))],
    pkg[setdiff(names(pkg), properties)]
  )
  # Ensure the class attribute is preserved
  class(sorted_pkg) <- class(pkg)

  sorted_pkg
}
