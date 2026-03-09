#' Convert GeoLocator Data Package to GeoPressureR tag object(s)
#'
#' @description
#' This function converts a GeoLocator Data Package (gldp) object to one or more GeoPressureR
#' `tag` objects. The gldp structure contains data for potentially multiple tags, while each
#' GeoPressureR `tag` object represents a single tag's sensor data.
#'
#' @param pkg A GeoLocator Data Package object (class `"geolocatordp"`), typically created with
#'   [`read_gldp()`] or [`create_gldp()`].
#' @param tag_id Character vector specifying which tag(s) to extract. If `NULL` (default), all
#'   tags in the package are converted.
#'
#' @return
#' If `tag_id` is a single value or if the package contains only one tag, returns a single
#' GeoPressureR `tag` object. Otherwise, returns a named list of `tag` objects, where names
#' correspond to the `tag_id` values.
#'
#' A GeoPressureR `tag` object contains:
#' - `param`: parameter object (see [`GeoPressureR::param_create()`])
#' - `pressure`: data.frame with columns `date` and `value` (if pressure data available)
#' - `light`: data.frame with columns `date` and `value` (if light data available)
#' - `acceleration`: data.frame with columns `date`, `value` (activity measure), and optionally
#'   `pitch` (if activity/pitch data available)
#' - `temperature_external`: data.frame with columns `date` and `value`
#'   (if external temperature data available)
#' - `temperature_internal`: data.frame with columns `date` and `value`
#'   (if internal temperature data available)
#' - `magnetic`: data.frame with columns `date`, `magnetic_x`, `magnetic_y`, `magnetic_z`
#'   (if magnetic data available)
#' - `stap`: data.frame with columns `stap_id`, `start`, `end`, and optionally `known_lat`,
#'   `known_lon`, `include` (if stationary periods defined)
#' - `twilight`: data.frame with columns `twilight`, `rise`, and optionally `label`
#'   (if twilight data available)
#'
#' @details
#' The function extracts sensor data from the gldp's `measurements` resource and converts it
#' to the format expected by GeoPressureR. The conversion includes:
#' - Filtering measurements by `tag_id`
#' - Reshaping data from long format (measurements) to wide format (sensor-specific data.frames)
#' - Combining related sensors (e.g., `activity` and `pitch` into `acceleration`;
#'   `magnetic_x/y/z` into `magnetic`)
#' - Adding stationary period (`stap`) data if available in the package
#' - Adding twilight data if available in the package
#' - Setting appropriate parameter values from the gldp metadata
#'
#' **Sensor mapping from GLDP to GeoPressureR:**
#' - `pressure` → `pressure` (date, value)
#' - `light` → `light` (date, value)
#' - `activity` + `pitch` → `acceleration` (date, value, pitch) where value = activity
#' - `temperature-external` → `temperature_external` (date, value)
#' - `temperature-internal` → `temperature_internal` (date, value)
#' - `magnetic_x/y/z` → `magnetic` (date, magnetic_x, magnetic_y, magnetic_z)
#'
#' **Additional data extracted:**
#' - `staps` table → `tag$stap` (stap_id, start, end, known_lat, known_lon, include)
#' - `twilights` table → `tag$twilight` (twilight, rise, label)
#'
#' Note that the resulting `tag` object will have `manufacturer = "datapackage"` in its
#' parameters, consistent with how GeoPressureR handles data from GeoLocator Data Packages.
#'
#' @examples
#' \dontrun{
#' # Read a gldp from a file
#' pkg <- read_gldp("path/to/datapackage.json")
#'
#' # Convert all tags
#' tags <- gldp_to_tag(pkg)
#'
#' # Convert a specific tag
#' tag <- gldp_to_tag(pkg, tag_id = "18LX")
#'
#' # Convert multiple specific tags
#' tags <- gldp_to_tag(pkg, tag_id = c("18LX", "18IC"))
#' }
#'
#' @seealso
#' - [`read_gldp()`] for reading a GeoLocator Data Package
#' - [`GeoPressureR::tag_create()`] for creating tag objects from raw files
#' - [GeoPressureR documentation](https://raphaelnussbaumer.com/GeoPressureR/)
#'
#' @export
gldp_to_tag <- function(pkg, tag_id = NULL) {
  # Check that pkg is a gldp
  check_gldp(pkg)

  # Get all available tag_ids
  all_tag_ids <- unique(tags(pkg)$tag_id)

  # If tag_id is NULL, use all tags
  if (is.null(tag_id)) {
    tag_id <- all_tag_ids
  }

  # Validate tag_id
  if (!all(tag_id %in% all_tag_ids)) {
    missing_ids <- setdiff(tag_id, all_tag_ids)
    cli::cli_abort(c(
      "x" = "Tag ID(s) not found in package: {.val {missing_ids}}",
      "i" = "Available tag IDs: {.val {all_tag_ids}}"
    ))
  }

  # Process each tag
  tag_list <- lapply(tag_id, function(tid) {
    gldp_to_tag_single(pkg, tid)
  })

  # Name the list
  names(tag_list) <- tag_id

  # If only one tag, return it directly instead of a list
  if (length(tag_list) == 1) {
    return(tag_list[[1]])
  } else {
    return(tag_list)
  }
}


#' Internal function to convert a single tag from gldp to tag object
#' @noRd
gldp_to_tag_single <- function(pkg, tid) {
  # Get measurements for this tag
  if ("measurements" %in% frictionless::resources(pkg)) {
    meas <- measurements(pkg) |>
      dplyr::filter(.data$tag_id == tid)
  } else {
    cli::cli_abort(c(
      "x" = "No {.field measurements} resource found in the package.",
      "i" = "The package must contain a measurements table to convert to tag objects."
    ))
  }

  # Get tag metadata
  tag_meta <- tags(pkg) |>
    dplyr::filter(.data$tag_id == tid)

  if (nrow(tag_meta) == 0) {
    cli::cli_abort("Tag ID {.val {tid}} not found in tags table.")
  }

  # Initialize tag structure using GeoPressureR's param_create
  tag <- structure(
    list(param = GeoPressureR::param_create(id = tid)),
    class = "tag"
  )

  # Update manufacturer info
  tag$param$tag_create$manufacturer <- "datapackage"

  # Extract sensor data
  # Map GLDP sensor types to GeoPressureR sensor names
  # GLDP uses: pressure, light, activity, pitch, temperature-external,
  # temperature-internal, acceleration_x/y/z, magnetic_x/y/z
  # GeoPressureR uses: pressure, light, acceleration (with activity and pitch columns),
  # temperature_external, temperature_internal, magnetic

  # Handle standard sensors that map 1:1
  sensor_simple <- c(
    "pressure" = "pressure",
    "light" = "light",
    "temperature-external" = "temperature_external",
    "temperature-internal" = "temperature_internal"
  )

  for (sensor_gldp in names(sensor_simple)) {
    sensor_gpr <- sensor_simple[[sensor_gldp]]

    # Filter measurements for this sensor type
    sensor_data <- meas |>
      dplyr::filter(.data$sensor == sensor_gldp)

    if (nrow(sensor_data) > 0) {
      df <- sensor_data |>
        dplyr::select("datetime", "value") |>
        dplyr::rename(date = "datetime") |>
        dplyr::arrange(.data$date)

      # Ensure UTC timezone
      attr(df$date, "tzone") <- "UTC"

      tag[[sensor_gpr]] <- as.data.frame(df)
    }
  }

  # Handle acceleration sensor (combines activity and pitch)
  # In GLDP: "activity" is the main acceleration measure, "pitch" is separate
  # In GeoPressureR: "acceleration" data.frame has columns: date, value, pitch (optional)
  # where value contains the activity data
  activity_data <- meas |> dplyr::filter(.data$sensor == "activity")
  pitch_data <- meas |> dplyr::filter(.data$sensor == "pitch")

  if (nrow(activity_data) > 0 || nrow(pitch_data) > 0) {
    # Start with activity data (renamed to "value")
    if (nrow(activity_data) > 0) {
      acc_df <- activity_data |>
        dplyr::select("datetime", "value") |>
        dplyr::rename(date = "datetime")
    } else {
      # Create empty data frame if no activity data
      acc_df <- tibble::tibble(
        date = as.POSIXct(character(), tz = "UTC"),
        value = numeric()
      )
    }

    # Merge with pitch data if available
    if (nrow(pitch_data) > 0) {
      pitch_df <- pitch_data |>
        dplyr::select("datetime", "value") |>
        dplyr::rename(date = "datetime", pitch = "value")

      acc_df <- dplyr::full_join(acc_df, pitch_df, by = "date")
    }

    # Arrange and set timezone
    acc_df <- acc_df |> dplyr::arrange(.data$date)
    attr(acc_df$date, "tzone") <- "UTC"

    tag[["acceleration"]] <- as.data.frame(acc_df)
  }

  # Handle magnetic sensor (combines x, y, z components)
  # In GLDP: magnetic_x, magnetic_y, magnetic_z are separate sensors
  # In GeoPressureR: "magnetic" data.frame with columns: date, magnetic_x, magnetic_y, magnetic_z
  mag_x <- meas |> dplyr::filter(.data$sensor == "magnetic_x")
  mag_y <- meas |> dplyr::filter(.data$sensor == "magnetic_y")
  mag_z <- meas |> dplyr::filter(.data$sensor == "magnetic_z")

  if (nrow(mag_x) > 0 || nrow(mag_y) > 0 || nrow(mag_z) > 0) {
    mag_df <- NULL

    if (nrow(mag_x) > 0) {
      mag_df <- mag_x |>
        dplyr::select("datetime", "value") |>
        dplyr::rename(date = "datetime", magnetic_x = "value")
    }

    if (nrow(mag_y) > 0) {
      mag_y_df <- mag_y |>
        dplyr::select("datetime", "value") |>
        dplyr::rename(date = "datetime", magnetic_y = "value")

      if (is.null(mag_df)) {
        mag_df <- mag_y_df
      } else {
        mag_df <- dplyr::full_join(mag_df, mag_y_df, by = "date")
      }
    }

    if (nrow(mag_z) > 0) {
      mag_z_df <- mag_z |>
        dplyr::select("datetime", "value") |>
        dplyr::rename(date = "datetime", magnetic_z = "value")

      if (is.null(mag_df)) {
        mag_df <- mag_z_df
      } else {
        mag_df <- dplyr::full_join(mag_df, mag_z_df, by = "date")
      }
    }

    # Arrange and set timezone
    mag_df <- mag_df |> dplyr::arrange(.data$date)
    attr(mag_df$date, "tzone") <- "UTC"

    tag[["magnetic"]] <- as.data.frame(mag_df)
  }

  # Add stap data if available
  if ("staps" %in% frictionless::resources(pkg)) {
    stap_data <- staps(pkg) |>
      dplyr::filter(.data$tag_id == tid) |>
      dplyr::select(-"tag_id")

    if (nrow(stap_data) > 0) {
      # Ensure datetime columns are POSIXct with UTC timezone
      if ("start" %in% names(stap_data)) {
        stap_data$start <- as.POSIXct(stap_data$start, tz = "UTC")
      }
      if ("end" %in% names(stap_data)) {
        stap_data$end <- as.POSIXct(stap_data$end, tz = "UTC")
      }

      tag[["stap"]] <- as.data.frame(stap_data)
    }
  }

  # Add twilight data if available
  if ("twilights" %in% frictionless::resources(pkg)) {
    twilight_data <- twilights(pkg) |>
      dplyr::filter(.data$tag_id == tid) |>
      dplyr::select(-"tag_id")

    if (nrow(twilight_data) > 0) {
      # Ensure twilight column is POSIXct with UTC timezone
      if ("twilight" %in% names(twilight_data)) {
        twilight_data$twilight <- as.POSIXct(twilight_data$twilight, tz = "UTC")
      }

      tag[["twilight"]] <- as.data.frame(twilight_data)
    }
  }

  # Validate that we have at least some data
  if (
    !any(
      c(
        "pressure",
        "light",
        "acceleration",
        "temperature_external",
        "temperature_internal",
        "magnetic"
      ) %in%
        names(tag)
    )
  ) {
    cli::cli_warn(c(
      "!" = "No sensor data found for tag {.val {tid}}",
      "i" = "The tag object will be empty."
    ))
  }

  tag
}
