#' Convert GeoLocator Data Package to GeoPressureR tag object(s)
#'
#' @description
#' This function converts a GeoLocator Data Package (GLDP) object to one or more GeoPressureR
#' `tag` objects. A GLDP can contain data for multiple tags, while each
#' GeoPressureR `tag` object represents a single tag's sensor data.
#'
#' It needs the `measurements` resource, so it does not work on a package without it, such as the
#' Master GeoLocator Data Package or a package read with `drop_measurements = TRUE`: use the source
#' package instead.
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
#' - `pressure`: data.frame with columns `date` and `value`, plus `label` and `stap_id` when
#'   labels are available
#' - `light`: data.frame with columns `date` and `value`, plus `stap_id` when labels are available
#'   (if light data available)
#' - `acceleration`: data.frame with columns `date`, `value` (activity measure), and optionally
#'   `mean_acceleration_z`, `label`, and `stap_id` (when labels are available)
#' - `temperature_external`: data.frame with columns `date` and `value`
#'   (if external temperature data available)
#' - `temperature_internal`: data.frame with columns `date` and `value`
#'   (if internal temperature data available)
#' - `magnetic`: data.frame with columns `date`, `magnetic_x`, `magnetic_y`, `magnetic_z`,
#'   `acceleration_x`, `acceleration_y`, `acceleration_z`, plus `stap_id` when labels are
#'   available (if magnetic or raw acceleration data available)
#' - `stap`: data.frame with columns `stap_id`, `start`, `end`, and optionally `known_lat` and
#'   `known_lon` (if stationary periods defined)
#' - `twilight`: data.frame with columns `twilight`, `rise`, and optionally `label`
#'   (if twilight data available)
#'
#' @details
#' The function extracts sensor data from the GLDP `measurements` resource and converts it
#' to the format expected by GeoPressureR. The conversion includes:
#' - Filtering measurements by `tag_id`
#' - Reshaping data from long format (measurements) to wide format (sensor-specific data.frames)
#' - Combining related sensors (e.g., `activity` and `mean_acceleration_z` into `acceleration`;
#'   `magnetic_x/y/z` and `acceleration_x/y/z` into `magnetic`)
#' - Adding stationary period (`stap`) data if available in the package
#' - Adding twilight data if available in the package
#' - Setting appropriate parameter values from GLDP metadata
#'
#' **Sensor mapping from GLDP to GeoPressureR:**
#' - `pressure` → `pressure` (date, value, label)
#' - `light` → `light` (date, value)
#' - `activity` + `mean_acceleration_z` → `acceleration` (date, value, mean_acceleration_z, label) where value = activity
#' - `temperature_external` → `temperature_external` (date, value)
#' - `temperature_internal` → `temperature_internal` (date, value)
#' - `magnetic_x/y/z` + `acceleration_x/y/z` → `magnetic` (date, magnetic_x/y/z, acceleration_x/y/z)
#'
#' **Additional data extracted:**
#' - `params.json` → `tag$param` when it contains parameters for the tag
#' - `staps` table → `tag$stap` (stap_id, start, end, known_lat, known_lon)
#' - `twilights` table → `tag$twilight` (twilight, rise, label)
#'
#' Saved parameters are returned as a GeoPressureR `param` object, in the layout they were
#' saved with (read packages are moved to the GeoPressureR >= 3.8.0 layout, with logger
#' metadata in `tag_create$settings`). When the package has no saved parameters for the tag,
#' or they do not record its manufacturer, `tag$param$tag_create$manufacturer`
#' is set to `"datapackage"` to record where the data came from. It is not a manufacturer that
#' [`GeoPressureR::tag_create()`] accepts, so the tag cannot be re-created from these parameters.
#'
#' @examples
#' \dontrun{
#' # Read a GLDP from Zenodo
#' pkg <- read_zenodo("17367319", quiet = TRUE)
#'
#' # Convert all tags
#' tags <- gldp_to_tag(pkg)
#'
#' # Convert a specific tag
#' tag <- gldp_to_tag(pkg, tag_id = "22PH")
#'
#' # Convert multiple specific tags
#' tags <- gldp_to_tag(pkg, tag_id = c("22PH", "22PO"))
#' }
#'
#' @seealso
#' - [`read_gldp()`] for reading a GeoLocator Data Package
#' - [`GeoPressureR::tag_create()`] for creating tag objects from raw files
#' - [GeoPressureR documentation](https://geopressure.org/GeoPressureR/)
#'
#' @export
gldp_to_tag <- function(pkg, tag_id = NULL) {
  # Check that pkg is a GLDP
  check_gldp(pkg)
  resources <- frictionless::resource_names(pkg)
  if (!"measurements" %in% resources) {
    cli_abort(c(
      "x" = "{.arg pkg} has no {.field measurements} resource, which {.fn gldp_to_tag} needs.",
      "i" = "Read the source package with its measurements (without {.code drop_measurements = TRUE})."
    ))
  }

  # Get all available tag_ids
  tags_data <- tags(pkg)
  all_tag_ids <- unique(tags_data$tag_id)

  # If tag_id is NULL, use all tags
  if (is.null(tag_id)) {
    tag_id <- all_tag_ids
  }

  # Validate tag_id
  if (!all(tag_id %in% all_tag_ids)) {
    missing_ids <- setdiff(tag_id, all_tag_ids)
    cli_abort(c(
      "x" = "Tag ID(s) not found in package: {.val {missing_ids}}",
      "i" = "Available tag IDs: {.val {all_tag_ids}}"
    ))
  }

  # Read each table once; every tag takes its rows from these.
  tables <- c(
    list(tags = tags_data),
    purrr::map(
      purrr::set_names(intersect(c("measurements", "staps", "twilights"), resources)),
      \(name) gldp_get_resource_data(pkg, name)
    )
  )
  # Split measurements by tag once rather than filtering the whole table per tag.
  tables$measurements <- split(
    tables$measurements,
    factor(tables$measurements$tag_id, levels = unique(tag_id))
  )

  # Process each tag
  tag_list <- purrr::map(tag_id, function(tid) {
    gldp_to_tag_single(pkg, tid, tables)
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


#' Internal function to convert a single tag from GLDP to a GeoPressureR tag object
#' @param tables Tables read by `gldp_to_tag()`, with measurements split by tag.
#' @noRd
gldp_to_tag_single <- function(pkg, tid, tables) {
  # Get measurements for this tag
  meas <- tables$measurements[[as.character(tid)]]
  # `label` is an optional measurements column.
  if (!"label" %in% names(meas)) {
    meas$label <- NA_character_
  }

  # Get tag metadata
  tag_meta <- tables$tags |>
    dplyr::filter(.data$tag_id == tid)

  # Reuse saved GeoPressureR parameters when available.
  param_saved <- purrr::detect(pkg$params, \(x) identical(x$id, tid))
  param <- GeoPressureR::param_create(id = tid)
  if (is.null(param_saved)) {
    param$tag_create$manufacturer <- "datapackage"
  }
  tag <- structure(
    list(param = param),
    class = "tag"
  )

  # Extract sensor data
  # Map GLDP sensor types to GeoPressureR sensor names
  # GLDP uses: pressure, light, activity, mean_acceleration_z, temperature_external,
  # temperature_internal, acceleration_x/y/z, magnetic_x/y/z
  # GeoPressureR uses: pressure, light, acceleration (with activity and mean_acceleration_z columns),
  # temperature_external, temperature_internal, magnetic

  # Handle standard sensors that map 1:1
  sensor_simple <- c(
    "pressure" = "pressure",
    "light" = "light",
    "temperature_external" = "temperature_external",
    "temperature_internal" = "temperature_internal"
  )

  for (sensor_gldp in names(sensor_simple)) {
    sensor_gpr <- sensor_simple[[sensor_gldp]]

    # Filter measurements for this sensor type
    sensor_data <- meas |>
      dplyr::filter(.data$sensor == sensor_gldp)

    if (nrow(sensor_data) > 0) {
      columns <- if (sensor_gldp == "pressure") {
        c("datetime", "value", "label")
      } else {
        c("datetime", "value")
      }
      df <- sensor_data |>
        dplyr::select(dplyr::all_of(columns)) |>
        dplyr::rename(date = "datetime") |>
        dplyr::arrange(.data$date)
      if (sensor_gldp == "pressure") {
        df <- df |> dplyr::mutate(label = dplyr::coalesce(.data$label, ""))
        # GeoPressureR::tag_label() labels pressure whenever pressure or acceleration is labelled.
        labelled <- meas$sensor %in%
          c("pressure", "activity", "mean_acceleration_z") &
          !is.na(meas$label) &
          meas$label != ""
        if (!any(labelled)) {
          df <- df |> dplyr::select(-"label")
        }
      }

      # Ensure UTC timezone
      attr(df$date, "tzone") <- "UTC"

      tag[[sensor_gpr]] <- as.data.frame(df)
    }
  }

  # Handle acceleration sensor (combines activity and mean_acceleration_z)
  # In GLDP: "activity" is the main acceleration measure, "mean_acceleration_z" is separate.
  # In GeoPressureR: "acceleration" data.frame has columns: date, value, mean_acceleration_z (optional)
  # where value contains the activity data
  activity_data <- meas |> dplyr::filter(.data$sensor == "activity")
  mean_acc_data <- meas |> dplyr::filter(.data$sensor == "mean_acceleration_z")

  if (nrow(activity_data) > 0 || nrow(mean_acc_data) > 0) {
    # Start with activity data (renamed to "value")
    if (nrow(activity_data) > 0) {
      acc_df <- activity_data |>
        dplyr::select("datetime", "value", "label") |>
        dplyr::rename(date = "datetime")
    } else {
      # Create empty data frame if no activity data
      acc_df <- tibble::tibble(
        date = as.POSIXct(character(), tz = "UTC"),
        value = numeric(),
        label = character()
      )
    }

    # Merge with mean_acceleration_z data if available
    if (nrow(mean_acc_data) > 0) {
      mean_acc_df <- mean_acc_data |>
        dplyr::select("datetime", "value", "label") |>
        dplyr::rename(
          date = "datetime",
          mean_acceleration_z = "value",
          mean_acceleration_z_label = "label"
        )

      acc_df <- acc_df |>
        dplyr::full_join(mean_acc_df, by = "date") |>
        dplyr::mutate(
          label = dplyr::coalesce(.data$label, .data$mean_acceleration_z_label, "")
        ) |>
        dplyr::select(-"mean_acceleration_z_label")
    }

    acc_df <- acc_df |> dplyr::mutate(label = dplyr::coalesce(.data$label, ""))
    if (all(acc_df$label == "")) {
      acc_df <- acc_df |> dplyr::select(-"label")
    }

    # Arrange and set timezone
    acc_df <- acc_df |> dplyr::arrange(.data$date)
    attr(acc_df$date, "tzone") <- "UTC"

    tag[["acceleration"]] <- as.data.frame(acc_df)
  }

  # GeoPressureR reads magnetic and acceleration axes from the same sensor table.
  magnetic_sensors <- c(
    "magnetic_x",
    "magnetic_y",
    "magnetic_z",
    "acceleration_x",
    "acceleration_y",
    "acceleration_z"
  )
  mag_df <- meas |>
    dplyr::filter(.data$sensor %in% magnetic_sensors) |>
    dplyr::select("datetime", "sensor", "value") |>
    tidyr::pivot_wider(names_from = "sensor", values_from = "value") |>
    dplyr::rename(date = "datetime")

  if (nrow(mag_df) > 0) {
    missing_sensors <- setdiff(magnetic_sensors, names(mag_df))
    mag_df[missing_sensors] <- NA_real_
    mag_df <- mag_df |> dplyr::select("date", dplyr::all_of(magnetic_sensors))

    # Arrange and set timezone
    mag_df <- mag_df |> dplyr::arrange(.data$date)
    attr(mag_df$date, "tzone") <- "UTC"

    tag[["magnetic"]] <- as.data.frame(mag_df)
  }

  # Add stap data if available
  if (!is.null(tables$staps)) {
    stap_data <- tables$staps |>
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
  if (!is.null(tables$twilights)) {
    twilight_data <- tables$twilights |>
      dplyr::filter(.data$tag_id == tid) |>
      dplyr::select(-"tag_id")

    if (nrow(twilight_data) > 0) {
      # Ensure twilight column is POSIXct with UTC timezone
      if ("twilight" %in% names(twilight_data)) {
        twilight_data$twilight <- as.POSIXct(twilight_data$twilight, tz = "UTC")
      }
      # GeoPressureR drops incomplete twilight rows, so an empty label must be "" rather than NA.
      if ("label" %in% names(twilight_data)) {
        twilight_data$label <- dplyr::coalesce(twilight_data$label, "")
      }

      tag[["twilight"]] <- as.data.frame(twilight_data)
    }
  }

  # Match the derived label state created by GeoPressureR::tag_label().
  has_label <- ("pressure" %in% names(tag) && "label" %in% names(tag$pressure)) ||
    ("acceleration" %in% names(tag) && "label" %in% names(tag$acceleration))
  if ("pressure" %in% names(tag) && has_label) {
    stap_data <- tag$stap
    tag <- GeoPressureR::tag_label_stap(tag, quiet = TRUE)
    if (!is.null(stap_data)) {
      tag[["stap"]] <- tag$stap |>
        dplyr::left_join(
          stap_data |> dplyr::select(-dplyr::any_of(c("start", "end"))),
          by = "stap_id"
        )
    }
  }

  # Params read from plain JSON have lost the class GeoPressureR prints them with,
  # and those kept by read_soi() only record the axis mapping.
  if (!is.null(param_saved)) {
    param_saved$tag_create$manufacturer <- param_saved$tag_create$manufacturer %||% "datapackage"
    tag[["param"]] <- structure(param_saved, class = "param")
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
    cli_warn(c(
      "!" = "No sensor data found for tag {.val {tid}}",
      "i" = "The tag object will be empty."
    ))
  }

  tag
}
