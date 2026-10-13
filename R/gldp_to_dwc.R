#' Transform a GeoLocator Data Package to Darwin Core
#'
#' @description
#' Transforms a [GeoLocator Data Package (GLDP)](https://geopressure.org/GeoLocator-DP/)
#' to a Darwin Core occurrence table.
#' The resulting CSV files can be uploaded to an [IPT](https://www.gbif.org/ipt)
#' for publication to GBIF. Archive descriptors are configured separately.
#' See [gldp_to_eml()] to create an `eml.xml` file.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param directory Path to local directory to write file(s) to.
#'   If `NULL`, then a data frame is returned instead, which can be useful
#'   for extending/adapting the Darwin Core mapping before writing with
#'   [readr::write_csv()].
#' @param scientific_name_ids Optional data frame with unique `scientific_name`
#'   and `scientificNameID` columns. Supply this to avoid automatic Aphia lookups;
#'   for example, Avibase identifiers from an AviList species catalogue.
#'   Identifiers can be `NA` when no target taxonomy has been chosen.
#' @return `occurrence.csv` file written to disk.
#'   Invisibly, an occurrence data frame.
#'
#' @section Transformation details:
#' Data are transformed into an [Occurrence core](https://rs.gbif.org/core/dwc_occurrence).
#' This transformation combines data from three resources:
#' - [`tags`](https://geopressure.org/GeoLocator-DP/core/tags/): metadata about the device and deployment
#' - [`staps`](https://geopressure.org/GeoLocator-DP/geopressurer/staps/): stationary periods with temporal information
#' - [`paths`](https://geopressure.org/GeoLocator-DP/geopressurer/paths/): spatial positions estimated for each stationary period
#'
#' The following terms are set from normalized package metadata
#' (sourced from top-level package fields):
#' - `datasetName`: Package title.
#' - `datasetID`: Package identifier (DOI/ID).
#' - `rightsHolder`: Rights holder as provided in contributors
#'   (contributor with `"rightsHolder"` role). If no rightsHolder role is found,
#'   this field will be `NA`.
#' - `license`: License name.
#'
#' Key features of the Darwin Core transformation:
#' - Stationary periods (`staps`) are treated as events, with each position
#'   as an occurrence representing the bird's location during that period.
#' - Each occurrence represents one stationary period from the `most_likely`
#'   path reconstruction.
#' - The `eventDate` is expressed as an ISO 8601 interval
#'   (`start/end`) representing the duration of the stationary period.
#' - `basisOfRecord` is set to `"MachineObservation"` as data are derived
#'   from automated geolocator sensors.
#' - `samplingProtocol` is set to `"geolocator"`.
#' - `geodeticDatum` is `"EPSG:4326"` (WGS84).
#' - `scientificName` is taken from `tags$scientific_name`.
#' - `organismID` is set to `ring_number` to track individual birds across
#'   multiple observations and deployments.
#' - `organismName` is set to `ring_number` as the available label for the
#'   tracked individual.
#' - `occurrenceID` combines `ring_number`, `tag_id`, and `stap_id`.
#' - `occurrenceStatus` is set to `"present"`.
#' - `sex`, `lifeStage`, and `eventRemarks` are included from matching
#'   observations when available.
#' - `minimumElevationInMeters` and `maximumElevationInMeters` are estimated
#'   from `pressurepaths$altitude` when available.
#' - `coordinateUncertaintyInMeters` is calculated as the 50th percentile of
#'   the distance between simulation paths and the `most_likely` position for
#'   each stationary period. For known locations, uncertainty is the radius
#'   enclosing the analysis grid cell, using `params$tag_set_map$scale` or
#'   an ERA5 default resolution of 0.25 degrees when parameters are absent.
#'
#' @seealso [gldp_to_eml()] to create the matching `eml.xml` metadata file.
#'
#' @export
gldp_to_dwc <- function(pkg, directory, scientific_name_ids = NULL) {
  check_gldp(pkg)

  # Set properties from metadata
  dataset_name <- pkg$title %||% "GeoLocator Data Package"
  dataset_id <- pkg$id

  # Find rightsHolder from contributors (optional)
  rights_holder <- NA_character_
  for (contributor in (pkg$contributors %||% list())) {
    roles <- tolower(as.character(contributor$roles %||% character(0)))
    if ("rightsholder" %in% roles) {
      rights_holder <- contributor$title %||%
        paste(stats::na.omit(c(contributor$givenName, contributor$familyName)), collapse = " ")
      break
    }
  }

  # Get first license name
  license <- NA_character_
  if (is.list(pkg$licenses) && length(pkg$licenses) > 0) {
    first_license <- pkg$licenses[[1]]
    license <- first_license$name %||% first_license$title %||% first_license$id %||% NA_character_
  }

  # Read resources
  tags <- tags(pkg)
  obs <- observations(pkg)
  staps <- staps(pkg)
  paths <- paths(pkg)

  obs_dynamic_cols <- setdiff(
    names(obs),
    c(
      "ring_number",
      "tag_id",
      "datetime",
      "longitude",
      "latitude",
      "sex",
      "age_class",
      "observation_comments"
    )
  )
  if (length(obs_dynamic_cols) > 0) {
    obs <- obs |>
      dplyr::mutate(
        observation_dynamic_properties = purrr::pmap_chr(
          dplyr::pick(dplyr::all_of(obs_dynamic_cols)),
          \(...) {
            values <- list(...)
            names(values) <- obs_dynamic_cols
            values <- purrr::discard(
              values,
              \(x) is.null(x) || is.na(x) || identical(x, "")
            )
            if (length(values) == 0) {
              NA_character_
            } else {
              jsonlite::toJSON(values, auto_unbox = TRUE, null = "null")
            }
          }
        )
      )
  } else {
    obs <- obs |>
      dplyr::mutate(observation_dynamic_properties = NA_character_)
  }

  if (is.null(scientific_name_ids)) {
    scientific_name_ids <- tags |>
      dplyr::distinct(.data$scientific_name)
    if (requireNamespace("movepub", quietly = TRUE)) {
      scientific_name_ids <- scientific_name_ids |>
        dplyr::mutate(
          scientificNameID = purrr::map_chr(
            .data$scientific_name,
            \(scientific_name) {
              aphia <- movepub::get_aphia_id(scientific_name)
              aphia_lsid <- aphia$aphia_lsid[!is.na(aphia$aphia_lsid)]
              if (length(aphia_lsid) == 0) {
                NA_character_
              } else {
                aphia_lsid[[1]]
              }
            }
          )
        )
    } else {
      scientific_name_ids <- scientific_name_ids |>
        dplyr::mutate(scientificNameID = NA_character_)
    }
  }

  # Keep the most likely positions for the export itself.
  most_likely_paths <- paths |>
    dplyr::filter(.data$type == "most_likely")
  if (nrow(most_likely_paths) == 0) {
    cli_warn("No paths found for type {.val {'most_likely'}}.")
  }

  # Derive uncertainty from the spread of simulation paths around the most likely position.
  simulation_uncertainty <- paths |>
    dplyr::filter(.data$type == "simulation") |>
    dplyr::inner_join(
      most_likely_paths |>
        dplyr::select(
          "tag_id",
          "stap_id",
          lat_center = "lat",
          lon_center = "lon"
        ),
      by = c("tag_id", "stap_id")
    ) |>
    dplyr::mutate(
      dlat_m = (.data$lat - .data$lat_center) * 111000,
      dlon_m = (.data$lon - .data$lon_center) * 111000 * cos(.data$lat_center * pi / 180),
      distance_m = sqrt(.data$dlat_m^2 + .data$dlon_m^2)
    ) |>
    dplyr::group_by(.data$tag_id, .data$stap_id) |>
    dplyr::summarise(
      coordinateUncertaintyInMeters = round(stats::quantile(
        .data$distance_m,
        0.5,
        na.rm = TRUE
      )),
      .groups = "drop"
    )

  pressurepath_elevation <- tibble::tibble(
    tag_id = character(),
    stap_id = numeric(),
    minimumElevationInMeters = numeric(),
    maximumElevationInMeters = numeric()
  )
  if ("pressurepaths" %in% frictionless::resource_names(pkg)) {
    pp <- pressurepaths(pkg)
    if ("altitude" %in% names(pp)) {
      # Pressurepaths can include decimal stap_id values during flights.
      pressurepath_elevation <- pp |>
        dplyr::filter(
          .data$type == "most_likely",
          .data$stap_id == round(.data$stap_id),
          is.finite(.data$altitude)
        ) |>
        dplyr::group_by(.data$tag_id, .data$stap_id) |>
        dplyr::summarise(
          minimumElevationInMeters = round(min(.data$altitude, na.rm = TRUE)),
          maximumElevationInMeters = round(max(.data$altitude, na.rm = TRUE)),
          .groups = "drop"
        )
    }
  }

  # Keep only observation fields that can be added to Darwin Core occurrences.
  obs_staps <- obs |>
    dplyr::transmute(
      tag_id = .data$tag_id,
      observation_datetime = as.POSIXct(.data$datetime, tz = "UTC"),
      observation_sex = .data$sex,
      observation_life_stage = .data$age_class,
      eventRemarks = .data$observation_comments,
      observation_dynamic_properties = .data$observation_dynamic_properties
    ) |>
    # Compare each observation to all stationary periods from the same tag.
    dplyr::inner_join(
      staps |> dplyr::select("tag_id", "stap_id", "start", "end"),
      by = "tag_id",
      relationship = "many-to-many"
    ) |>
    # Prefer observations inside a stap; otherwise measure distance to stap boundaries.
    dplyr::mutate(
      observation_in_stap = .data$observation_datetime >= .data$start &
        .data$observation_datetime <= .data$end,
      observation_distance_days = dplyr::if_else(
        .data$observation_in_stap,
        0,
        pmin(
          abs(as.numeric(difftime(.data$observation_datetime, .data$start, units = "days"))),
          abs(as.numeric(difftime(.data$observation_datetime, .data$end, units = "days")))
        )
      )
    ) |>
    # Keep exact interval matches and nearest-stap fallback matches within two weeks.
    dplyr::filter(.data$observation_in_stap | .data$observation_distance_days <= 14) |>
    dplyr::arrange(.data$tag_id, .data$observation_datetime, .data$observation_distance_days) |>
    # If an observation could match several staps, keep the closest one.
    dplyr::distinct(.data$tag_id, .data$observation_datetime, .keep_all = TRUE) |>
    # Collapse multiple observations on the same stap into one occurrence row.
    dplyr::group_by(.data$tag_id, .data$stap_id) |>
    dplyr::summarise(
      observation_sex = dplyr::first(.data$observation_sex),
      observation_life_stage = dplyr::first(.data$observation_life_stage),
      eventRemarks = glue::glue_collapse(
        unique(.data$eventRemarks[!is.na(.data$eventRemarks) & .data$eventRemarks != ""]),
        sep = "; "
      ),
      observation_dynamic_properties = dplyr::first(
        .data$observation_dynamic_properties[!is.na(.data$observation_dynamic_properties)],
        default = NA_character_
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      eventRemarks = dplyr::na_if(.data$eventRemarks, ""),
      observation_dynamic_properties = dplyr::na_if(.data$observation_dynamic_properties, "")
    )

  # Join data and create occurrence data frame
  occurrence <- staps |>
    dplyr::inner_join(most_likely_paths, by = c("tag_id", "stap_id")) |>
    dplyr::left_join(simulation_uncertainty, by = c("tag_id", "stap_id")) |>
    dplyr::inner_join(tags, by = "tag_id") |>
    dplyr::left_join(scientific_name_ids, by = "scientific_name") |>
    dplyr::left_join(pressurepath_elevation, by = c("tag_id", "stap_id")) |>
    dplyr::left_join(obs_staps, by = c("tag_id", "stap_id"))

  # Ensure optional fields from observations / paths exist; otherwise create empty columns
  if (!("observation_sex" %in% names(occurrence))) {
    occurrence$observation_sex <- NA_character_
  }
  if (!("observation_life_stage" %in% names(occurrence))) {
    occurrence$observation_life_stage <- NA_character_
  }
  if (!("eventRemarks" %in% names(occurrence))) {
    occurrence$eventRemarks <- NA_character_
  }
  if (!("observation_dynamic_properties" %in% names(occurrence))) {
    occurrence$observation_dynamic_properties <- NA_character_
  }
  if (!("coordinateUncertaintyInMeters" %in% names(occurrence))) {
    occurrence$coordinateUncertaintyInMeters <- NA_real_
  }

  path_has_known <- "known" %in% names(occurrence)
  stap_has_known <- all(c("known_lat", "known_lon") %in% names(occurrence))

  # Match each tag’s analysis grid; missing scales use the ERA5 0.25-degree grid.
  grid_scales <- stats::setNames(
    purrr::map_dbl(pkg$params, \(param) param$tag_set_map$scale %||% NA_real_),
    purrr::map_chr(pkg$params, \(param) param$id)
  )

  occurrence <- occurrence |>
    dplyr::mutate(
      location_is_known = if (path_has_known) {
        as.logical(.data$known)
      } else if (stap_has_known) {
        !is.na(.data$known_lat) & !is.na(.data$known_lon)
      } else {
        FALSE
      },
      grid_scale = unname(grid_scales[.data$tag_id]),
      grid_resolution = 1 / dplyr::coalesce(.data$grid_scale, 4),
      # The corner towards the equator is furthest from the cell centre.
      grid_half_angle = .data$grid_resolution * pi / 360,
      grid_corner_lat = .data$lat *
        pi /
        180 +
        dplyr::if_else(.data$lat >= 0, -.data$grid_half_angle, .data$grid_half_angle),
      coordinateUncertaintyInMeters = dplyr::if_else(
        .data$location_is_known,
        ceiling(
          6371008.8 *
            acos(pmin(
              1,
              sin(.data$lat * pi / 180) *
                sin(.data$grid_corner_lat) +
                cos(.data$lat * pi / 180) * cos(.data$grid_corner_lat) * cos(.data$grid_half_angle)
            ))
        ),
        .data$coordinateUncertaintyInMeters
      ),
      georeferenceSources = dplyr::case_when(
        .data$location_is_known ~ paste(
          "GeoPressureR::tag_set_map()",
          "https://geopressure.com/GeoPressureR/reference/tag_set_map.html",
          sep = " | "
        ),
        TRUE ~ paste(
          "GeoPressureR::graph_most_likely()",
          "https://geopressure.com/GeoPressureR/reference/graph_most_likely.html",
          "https://geopressure.com/GeoPressureManual/trajectory.html",
          sep = " | "
        )
      ),
      eventRemarks = purrr::pmap_chr(
        list(
          .data$location_is_known,
          .data$eventRemarks,
          .data$coordinateUncertaintyInMeters,
          .data$grid_resolution,
          .data$grid_scale
        ),
        \(location_is_known, event_remarks, coordinate_uncertainty, grid_resolution, grid_scale) {
          values <- c(
            if (isTRUE(location_is_known)) {
              "Location taken from known stationary coordinates."
            } else {
              "Location reconstructed from `most_likely` trajectory."
            },
            event_remarks,
            if (!is.na(coordinate_uncertainty)) {
              if (isTRUE(location_is_known)) {
                glue::glue(
                  "Coordinate uncertainty encloses a {grid_resolution}-degree grid cell ({if (is.na(grid_scale)) 'ERA5 default' else 'analysis parameters'})."
                )
              } else {
                "Coordinate uncertainty estimated as the 50th percentile of simulation distances from the `most_likely` position."
              }
            }
          )
          values <- values[!is.na(values) & values != ""]
          if (length(values) == 0) {
            NA_character_
          } else {
            glue::glue_collapse(values, sep = " | ")
          }
        }
      ),
      dynamicProperties = purrr::pmap_chr(
        list(
          .data$observation_dynamic_properties,
          .data$location_is_known
        ),
        \(observation_dynamic_properties, location_is_known) {
          values <- list(
            pathType = "most_likely",
            locationSource = if (isTRUE(location_is_known)) "known" else "reconstructed"
          )
          if (!is.na(observation_dynamic_properties)) {
            values$observation <- jsonlite::fromJSON(
              observation_dynamic_properties,
              simplifyVector = TRUE
            )
          }
          jsonlite::toJSON(values, auto_unbox = TRUE, null = "null")
        }
      ),
      locationRemarks = purrr::pmap_chr(
        list(
          .data$location_is_known,
          .data$minimumElevationInMeters,
          .data$maximumElevationInMeters
        ),
        \(location_is_known, minimum_elevation, maximum_elevation) {
          values <- c(
            if (isTRUE(location_is_known)) {
              "Coordinates are known stationary locations assigned with GeoPressureR::tag_set_map()."
            },
            if (!is.na(minimum_elevation) || !is.na(maximum_elevation)) {
              "Elevations are altitude above mean sea level from pressurepaths altitude."
            }
          )
          values <- values[!is.na(values) & values != ""]
          if (length(values) == 0) {
            NA_character_
          } else {
            glue::glue_collapse(values, sep = " | ")
          }
        }
      )
    )

  occurrence <- occurrence |>
    dplyr::transmute(
      type = "Event",
      license = license,
      rightsHolder = rights_holder,
      datasetID = dataset_id,
      institutionCode = NA_character_,
      collectionCode = "geopressure.org",
      datasetName = dataset_name,
      basisOfRecord = "MachineObservation",
      dataGeneralizations = NA_character_,
      dynamicProperties = .data$dynamicProperties,
      occurrenceID = glue::glue("{.data$ring_number}_{.data$tag_id}_{.data$stap_id}"),
      sex = .data$observation_sex,
      lifeStage = .data$observation_life_stage,
      occurrenceStatus = "present",
      organismID = .data$ring_number,
      # Darwin Core organismName refers to the individual label, not the taxon name.
      organismName = .data$ring_number,
      eventID = .data$occurrenceID,
      parentEventID = glue::glue("{.data$ring_number}_{.data$tag_id}"),
      eventType = "geolocator",
      eventDate = paste(
        format(.data$start, "%Y-%m-%dT%H:%M:%SZ"),
        format(.data$end, "%Y-%m-%dT%H:%M:%SZ"),
        sep = "/"
      ),
      samplingProtocol = .data$eventType,
      samplingEffort = NA_character_,
      eventRemarks = .data$eventRemarks,
      minimumElevationInMeters = .data$minimumElevationInMeters,
      maximumElevationInMeters = .data$maximumElevationInMeters,
      locationRemarks = .data$locationRemarks,
      decimalLatitude = .data$lat,
      decimalLongitude = .data$lon,
      geodeticDatum = "EPSG:4326",
      coordinateUncertaintyInMeters = .data$coordinateUncertaintyInMeters,
      georeferenceSources = .data$georeferenceSources,
      identificationVerificationStatus = NA_character_,
      scientificNameID = .data$scientificNameID,
      scientificName = .data$scientific_name,
      kingdom = "Animalia"
    )

  if (is.null(directory)) {
    return(invisible(occurrence))
  }

  # Write files
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  occurrence_path <- file.path(directory, "occurrence.csv")

  readr::write_csv(occurrence, occurrence_path, na = "")

  cli_alert_success("Darwin Core occurrence file written to {.file {occurrence_path}}")

  invisible(occurrence)
}
