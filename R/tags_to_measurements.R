#' Transform Tags to a Tidy Data Frame
#'
#' @description
#' Convert a list of GeoPressureR `tag` objects into the long-format
#' `measurements` table used by GeoLocator Data Packages.
#'
#' @param tags A list of GeoPressureR `tag` objects.
#'
#' @return A [tibble::tibble()] data frame with columns:
#' \describe{
#'   \item{tag_id}{A character vector representing the unique identifier for each tag.}
#'   \item{sensor}{A character vector representing the type of sensor measurement, including types
#'   like "activity", "mean_acceleration_z", "light", "temperature_external", etc.}
#'   \item{datetime}{A POSIXct datetime object representing the timestamp of the measurements.}
#'   \item{value}{A numeric vector containing the sensor measurement values.}
#'   \item{label}{A character vector for additional labeling, which is NA if not present in the
#'   original data.}
#' }
#'
#' @details
#' The function extracts sensor tables from each tag, ensures a `label` column
#' is present, reshapes supported sensors into long format, and drops rows with
#' missing values.
#'
#' The current conversion supports pressure, light, acceleration/activity,
#' temperature, and magnetic sensors.
#'
#' If no tags are provided, the function returns an empty tibble with the appropriate column names
#'  and types.
#'
#' @seealso [params_to_tags()] and [params_to_observations()] for related
#'   conversions used when building GeoLocator-DP resources from GeoPressureR
#'   objects.
#'
#' @export
tags_to_measurements <- function(tags) {
  if (length(tags) == 0) {
    return(tibble::tibble(
      tag_id = character(),
      sensor = character(),
      datetime = as.POSIXct(character()),
      value = numeric(),
      label = character()
    ))
  }

  sensor_types <- c(
    "pressure",
    "acceleration",
    "light",
    "temperature_external",
    "temperature_internal",
    "magnetic"
  )
  select_cols <- c(
    "pressure",
    "activity",
    "mean_acceleration_z",
    "light",
    "temperature_external",
    "temperature_internal",
    "acceleration_x",
    "acceleration_y",
    "acceleration_z",
    "magnetic_x",
    "magnetic_y",
    "magnetic_z",
    "date",
    "label"
  )
  all_measurements <- list()

  for (i in seq_along(tags)) {
    tag <- tags[[i]]
    tag_id <- tryCatch(
      {
        tag$param$id
      },
      error = function(e) NA
    )
    tag_measurements <- list()
    tag_sensors <- names(tag)[names(tag) %in% sensor_types]
    for (sensor in tag_sensors) {
      df <- tag[[sensor]]
      sensor_name <- ifelse(sensor == "acceleration", "activity", sensor)
      if ("value" %in% colnames(df)) {
        names(df)[names(df) == "value"] <- sensor_name
      }
      if (!"label" %in% colnames(df)) {
        df$label <- NA_character_
      }
      # Select only relevant columns
      df <- df[, intersect(colnames(df), select_cols), drop = FALSE]
      # Pivot longer
      long_df <- tryCatch(
        {
          tidyr::pivot_longer(
            df,
            cols = setdiff(colnames(df), c("date", "label")),
            names_to = "sensor",
            values_to = "value"
          )
        },
        error = function(e) {
          cli_abort(
            c(
              "x" = glue::glue(
                "Error in {{.fun tags_to_measurements}} for tag {{.val {tag_id}}}, sensor {{.val {sensor}}}:"
              ),
              ">" = e$message
            )
          )
        }
      )
      long_df$tag_id <- tag_id
      tag_measurements[[sensor]] <- long_df
    }
    # Combine all sensors for this tag
    if (length(tag_measurements) > 0) {
      tag_df <- tryCatch(
        {
          dplyr::bind_rows(tag_measurements)
        },
        error = function(e) {
          value_types <- sapply(tag_measurements, function(x) class(x$value)[1])
          cli_abort(
            c(
              "x" = glue::glue(
                "Type mismatch in {{.fun tags_to_measurements}} for tag {{.val {tag_id}}}, sensor {{.val {sensor}}}:"
              ),
              "i" = "value column types: {.val {value_types}}",
              ">" = e$message
            )
          )
        }
      )
      all_measurements[[i]] <- tag_df
    }
  }
  # Combine all tags
  m <- tryCatch(
    {
      dplyr::bind_rows(all_measurements)
    },
    error = function(e) {
      value_types <- sapply(all_measurements, function(x) class(x$value)[1])
      cli_abort(
        c(
          "Type mismatch when combining tags in {.fun tags_to_measurements}:",
          "i" = "value column types: {.val {value_types}}",
          "x" = e$message
        )
      )
    }
  )
  m <- dplyr::rename(m, datetime = "date")
  m <- dplyr::filter(m, !is.na(.data$value))
  m
}
