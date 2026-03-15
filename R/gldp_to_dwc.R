#' Transform a GeoLocator Data Package to Darwin Core
#'
#' @description
#' Transforms a [GeoLocator Data Package (GLDP)](https://raphaelnussbaumer.com/GeoLocator-DP/)
#' to a [Darwin Core Archive](https://dwc.tdwg.org/).
#' The resulting CSV files can be uploaded to an [IPT](https://www.gbif.org/ipt)
#' for publication to GBIF. A `meta.xml` file is included as well.
#' See [gldp_to_eml()] to create an `eml.xml` file.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param directory Path to local directory to write file(s) to.
#'   If `NULL`, then a data frame is returned instead, which can be useful
#'   for extending/adapting the Darwin Core mapping before writing with
#'   [readr::write_csv()].
#' @param path_type The type of path to use for the occurrence data.
#'   One of `"most_likely"`, `"mean_simulation"`, `"median_simulation"`, or
#'   `"geopressureviz"`. For simulation types, the mean or median position
#'   (lat/lon) is calculated for each `tag_id`-`stap_id` combination across
#'   all simulated paths. Defaults to `"most_likely"`. See [paths] for more details.
#'
#' @return `occurrence.csv` file written to disk.
#'   Invisibly, an occurrence data frame.
#'
#' @section Transformation details:
#' Data are transformed into an [Occurrence core](https://rs.gbif.org/core/dwc_occurrence).
#' This transformation combines data from three resources:
#' - [`tags`](https://raphaelnussbaumer.com/GeoLocator-DP/core/tags/): metadata about the device and deployment
#' - [`staps`](https://raphaelnussbaumer.com/GeoLocator-DP/geopressurer/staps/): stationary periods with temporal information
#' - [`paths`](https://raphaelnussbaumer.com/GeoLocator-DP/geopressurer/paths/): spatial positions estimated for each stationary period
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
#' - Each occurrence represents one stationary period from the path data,
#'   filtered by `path_type`.
#' - The `eventDate` is expressed as an ISO 8601 interval
#'   (`start/end`) representing the duration of the stationary period.
#' - `basisOfRecord` is set to `"MachineObservation"` as data are derived
#'   from automated geolocator sensors.
#' - `samplingProtocol` is set to `"geolocator"`.
#' - `geodeticDatum` is `"EPSG:4326"` (WGS84).
#' - `scientificName` is taken from `tags$scientific_name`.
#' - `organismID` is set to `ring_number` to track individual birds across
#'   multiple observations and deployments.
#' - `occurrenceID` is a unique identifier combining `tag_id` and `stap_id`.
#' - `individualCount` is set to `1`.
#' - `occurrenceStatus` is set to `"present"`.
#' - `sex` and `lifeStage` are included if available in the `tags` resource.
#' - `coordinateUncertaintyInMeters` is calculated for simulation path types
#'   (`"mean_simulation"` or `"median_simulation"`) as the 95th percentile of
#'   the distance between each simulation and the aggregated center.
#'
#' @export
gldp_to_dwc <- function(pkg, directory, path_type = "most_likely") {
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
  staps <- staps(pkg)
  paths <- paths(pkg)

  # Validate path_type
  allowed_types <- c("most_likely", "mean_simulation", "median_simulation", "geopressureviz")
  if (!path_type %in% allowed_types) {
    cli_abort(
      c(
        "x" = "{.arg path_type} must be one of {.val {allowed_types}}.",
        "i" = "You provided: {.val {path_type}}"
      )
    )
  }

  # Filter and aggregate paths
  if (path_type %in% c("mean_simulation", "median_simulation")) {
    # For simulation paths, aggregate across all simulations (j values)
    # and calculate uncertainty metrics

    # First calculate mean/median for each tag-stap
    paths_summary <- paths |>
      dplyr::filter(.data$type == "simulation") |>
      dplyr::group_by(.data$tag_id, .data$stap_id) |>
      dplyr::summarise(
        lat_center = if (path_type == "mean_simulation") {
          mean(.data$lat, na.rm = TRUE)
        } else {
          stats::median(.data$lat, na.rm = TRUE)
        },
        lon_center = if (path_type == "mean_simulation") {
          mean(.data$lon, na.rm = TRUE)
        } else {
          stats::median(.data$lon, na.rm = TRUE)
        },
        .groups = "drop"
      )

    # Calculate distances from center for each simulation point
    paths_with_distances <- paths |>
      dplyr::filter(.data$type == "simulation") |>
      dplyr::inner_join(paths_summary, by = c("tag_id", "stap_id")) |>
      dplyr::mutate(
        # Calculate distance in meters using haversine-like approximation
        # More accurate than simple lat/lon differences
        dlat_m = (.data$lat - .data$lat_center) * 111000,
        dlon_m = (.data$lon - .data$lon_center) * 111000 * cos(.data$lat_center * pi / 180),
        distance_m = sqrt(.data$dlat_m^2 + .data$dlon_m^2)
      )

    # Calculate 95th percentile of distances for each tag-stap
    paths <- paths_with_distances |>
      dplyr::group_by(.data$tag_id, .data$stap_id) |>
      dplyr::summarise(
        lat = first(.data$lat_center),
        lon = first(.data$lon_center),
        # 95th percentile: radius containing 95% of simulated positions
        coordinateUncertaintyInMeters = round(stats::quantile(
          .data$distance_m,
          0.95,
          na.rm = TRUE
        )),
        .groups = "drop"
      )
  } else {
    # For other types, filter directly (no uncertainty calculated)
    paths <- paths |> dplyr::filter(.data$type == !!path_type)
  }

  if (nrow(paths) == 0) {
    cli_warn("No paths found for type {.val {path_type}}.")
  }

  # Join data and create occurrence data frame
  occurrence <- staps |>
    dplyr::inner_join(paths, by = c("tag_id", "stap_id")) |>
    dplyr::inner_join(tags, by = "tag_id")

  # Ensure optional fields from tags / paths exist; otherwise create empty columns
  if (!("sex" %in% names(occurrence))) {
    occurrence$sex <- NA_character_
  }
  if (!("life_stage" %in% names(occurrence))) {
    occurrence$life_stage <- NA_character_
  }
  if (!("coordinateUncertaintyInMeters" %in% names(occurrence))) {
    occurrence$coordinateUncertaintyInMeters <- NA_real_
  }

  occurrence <- occurrence |>
    dplyr::transmute(
      type = "Event",
      license = license,
      rightsHolder = rights_holder,
      datasetID = dataset_id,
      datasetName = dataset_name,
      basisOfRecord = "MachineObservation",
      eventDate = paste(
        format(.data$start, "%Y-%m-%dT%H:%M:%SZ"),
        format(.data$end, "%Y-%m-%dT%H:%M:%SZ"),
        sep = "/"
      ),
      decimalLatitude = .data$lat,
      decimalLongitude = .data$lon,
      geodeticDatum = "EPSG:4326",
      coordinateUncertaintyInMeters = .data$coordinateUncertaintyInMeters,
      scientificName = .data$scientific_name,
      organismID = .data$ring_number,
      occurrenceID = glue::glue("{.data$tag_id}-{.data$stap_id}"),
      samplingProtocol = "geolocator",
      samplingEffort = glue::glue("{.data$start}/{.data$end}"),
      # Additional Darwin Core fields
      individualCount = 1L,
      occurrenceStatus = "present",
      dynamicProperties = glue::glue("{{\"pathType\":\"{path_type}\"}}"),
      sex = .data$sex,
      lifeStage = .data$life_stage
    )

  # Write files
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  occurrence_path <- file.path(directory, "occurrence.csv")

  readr::write_csv(occurrence, occurrence_path, na = "")

  cli_alert_success("Darwin Core occurrence file written to {.file {occurrence_path}}")

  invisible(occurrence)
}
