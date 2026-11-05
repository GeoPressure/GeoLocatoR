#' Plot a GeoLocator Data Package
#'
#' @description
#' Creates visualizations for a GeoLocator Data Package: the data availability
#' across time for each sensor and tag, the observation history of each ring, or
#' a map of the most likely paths.
#'
#' @param x A GeoLocator Data Package object.
#' @param type Type of the plot to display: `"coverage"` (requires `measurements`,
#'   and draws `observations` when present), `"ring"` (requires `observations`,
#'   `measurements` and `tags`) or `"map"` (requires `paths` and `staps`). When
#'   `NULL`, the first type the package supports is used.
#' @param ... Unused, for compatibility with the [plot()] generic.
#'
#' @return A ggplot2 object for `"coverage"` and `"ring"`, a leaflet map for
#'   `"map"`.
#'
#' @examples
#' \dontrun{
#' pkg <- read_zenodo("17367319")
#'
#' # Plot data coverage
#' plot(pkg, type = "coverage")
#'
#' # Plot observation history by ring
#' plot(pkg, type = "ring")
#'
#' # Plot map view (requires leaflet)
#' plot(pkg, type = "map")
#' }
#'
#' @export
plot.geolocatordp <- function(x, type = NULL, ...) {
  required <- list(
    coverage = "measurements",
    ring = c("observations", "measurements", "tags"),
    map = c("paths", "staps")
  )
  available <- frictionless::resource_names(x)
  if (is.null(type)) {
    type <- names(required)[vapply(required, \(r) all(r %in% available), logical(1))][1]
    if (is.na(type)) {
      cli_abort(
        "This package has neither {.field measurements} nor {.field paths} and {.field staps} to plot."
      )
    }
  }
  if (!type %in% names(required)) {
    cli_abort("Unsupported plot type {.val {type}}; use one of {.val {names(required)}}.")
  }
  missing <- setdiff(required[[type]], available)
  if (length(missing) > 0) {
    cli_abort(
      "{.code type = \"{type}\"} needs {.field {missing}}, which this package does not have."
    )
  }

  if (type == "coverage") {
    plot_pkg_coverage(x)
  } else if (type == "ring") {
    plot_pkg_ring(x)
  } else if (type == "map") {
    plot_pkg_map(x)
  }
}

#' Plot coverage data for a GeoLocator Data Package
#'
#' Internal helper function to create a coverage plot showing data availability
#' across time for different sensors and tags.
#'
#' @param x A GeoLocator Data Package object
#' @return A ggplot2 object showing the coverage plot
#' @noRd
plot_pkg_coverage <- function(x) {
  m <- measurements(x) |>
    mutate(
      # Group the sensors read from one GeoPressureR sensor table: the acceleration and
      # magnetic axes come from `magnetic`, the mean z acceleration with `activity`
      sensor = case_when(
        sensor %in%
          c(
            "magnetic_x",
            "magnetic_y",
            "magnetic_z",
            "acceleration_x",
            "acceleration_y",
            "acceleration_z"
          ) ~
          "magnetic",
        sensor == "mean_acceleration_z" ~ "activity",
        TRUE ~ sensor # Keep other sensor names unchanged
      )
    ) |>
    mutate(date = as.Date(.data$datetime)) |>
    group_by(.data$tag_id, .data$sensor, .data$date) |> # Group by tag_id, sensor, and date
    summarize(has_data = sum(!is.na(.data$value)) > 0) |> # Check if there is data (non-NA)
    ungroup()

  # Observations only add equipment and retrieval lines, so the plot works without them
  o <- if ("observations" %in% frictionless::resource_names(x)) {
    observations(x)
  } else {
    tibble(tag_id = character(), datetime = as.POSIXct(character()), observation_type = character())
  }
  o <- o |>
    filter(!is.na(.data$datetime)) |>
    filter(.data$tag_id %in% unique(m$tag_id))

  ggplot2::ggplot(
    data = m,
    ggplot2::aes(x = .data$date, y = .data$sensor, fill = .data$has_data)
  ) +
    ggplot2::geom_tile() +
    ggplot2::facet_grid(.data$tag_id ~ ., scales = "free_y") +
    # Named, so that days with data are black whether or not days without data exist
    ggplot2::scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "grey")) +
    ggplot2::geom_vline(
      data = o |>
        filter(.data$observation_type %in% c("equipment", "retrieval")),
      ggplot2::aes(
        xintercept = as.Date(.data$datetime),
        color = .data$observation_type
      ),
      linetype = "solid",
      linewidth = 1.2
    ) +
    ggplot2::geom_vline(
      data = o |>
        filter(.data$observation_type %in% c("capture", "sighting", "other")),
      ggplot2::aes(
        xintercept = as.Date(.data$datetime),
        color = .data$observation_type
      ),
      linetype = "dashed",
      linewidth = 0.8
    ) + # Thinner dashed lines
    ggplot2::scale_x_date(limits = range(c(m$date, as.Date(o$datetime)))) +
    # Color scheme highlighting 'equipment' and 'retrieval'
    ggplot2::scale_color_manual(
      values = c(
        "equipment" = "red",
        "retrieval" = "green",
        "capture" = "blue",
        "sighting" = "orange",
        "other" = "purple"
      ),
      # The observed types, so that a package without observations draws without warning
      limits = force,
      name = "Observation Type"
    ) + # Customize colors and legend
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      x = "Date",
      y = "Sensor",
      title = "Package's Measurement Coverage"
    )
}


#' Plot the observation history of each ring
#'
#' Internal helper function to plot the observations of each ring over the period
#' covered by its tags' measurements.
#'
#' @param x A GeoLocator Data Package object
#' @return A ggplot2 object showing the ring plot
#' @noRd
plot_pkg_ring <- function(x) {
  o <- observations(x) |>
    filter(!is.na(.data$datetime))

  m <- measurements(x) |>
    left_join(
      tags(x) |> select("ring_number", "tag_id"),
      by = "tag_id"
    ) |>
    group_by(.data$ring_number) |>
    summarize(
      start = min(.data$datetime, na.rm = TRUE),
      end = max(.data$datetime, na.rm = TRUE)
    ) |>
    ungroup()

  ggplot2::ggplot() +
    # A fixed colour, outside aes(): the colour scale only knows observation types
    ggplot2::geom_segment(
      data = m,
      ggplot2::aes(
        x = .data$start,
        y = .data$ring_number,
        xend = .data$end,
        yend = .data$ring_number
      ),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = o,
      ggplot2::aes(
        x = .data$datetime,
        y = .data$ring_number,
        color = .data$observation_type
      )
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "equipment" = "red",
        "retrieval" = "green",
        "capture" = "blue",
        "sighting" = "orange",
        "other" = "purple"
      ),
      name = "Observation Type"
    ) + # Customize colors and legend
    ggplot2::labs(
      x = "Date",
      y = "Ring Number",
      title = "Package's Observations Coverage"
    )
}

#' Map the most likely paths of a GeoLocator Data Package
#'
#' Internal helper function to map the most likely path of each tag, with markers
#' sized by the duration of its stationary periods.
#'
#' @param x A GeoLocator Data Package object
#' @return A leaflet map
#' @noRd
plot_pkg_map <- function(x) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    cli_abort(c(
      "x" = "{.pkg leaflet} is required for {.val type = 'map'}.",
      "i" = "Install it with {.code install.packages('leaflet')}."
    ))
  }

  p <- paths(x) |>
    filter(.data$type == "most_likely") |>
    left_join(staps(x), by = c("stap_id", "tag_id"))
  if (nrow(p) == 0) {
    cli_abort(
      "{.code type = \"map\"} draws the {.val most_likely} {.field paths}, which this package does not have."
    )
  }

  # Marker radius from 3 to 12 by log duration, on one scale for all tags, and a single
  # size when all durations are equal
  log_duration <- log1p(GeoPressureR::stap2duration(p))
  log_range <- range(log_duration, na.rm = TRUE)
  p$radius <- if (diff(log_range) > 0) {
    3 + (log_duration - log_range[1]) / diff(log_range) * 9
  } else {
    7.5
  }

  # Generate a color palette for unique tag IDs
  tag_ids <- unique(p$tag_id)
  n_tags <- length(tag_ids)
  set1_colors <- c(
    "#E41A1C",
    "#377EB8",
    "#4DAF4A",
    "#984EA3",
    "#FF7F00",
    "#FFFF33",
    "#A65628",
    "#F781BF",
    "#999999"
  )
  palette_fun <- grDevices::colorRampPalette(set1_colors)
  color_palette <- leaflet::colorFactor(
    palette = palette_fun(n_tags),
    domain = tag_ids
  )

  leaflet_map <- leaflet::leaflet(height = 600) |> leaflet::addTiles()

  # Add polylines and markers for each tag_id
  for (tag in tag_ids) {
    pid <- p |> filter(.data$tag_id == tag) |> filter(!is.na(.data$lat) & !is.na(.data$lon))
    leaflet_map <- leaflet_map |>
      leaflet::addPolylines(
        lng = ~lon,
        lat = ~lat,
        data = pid,
        color = color_palette(tag),
        weight = 2,
        popup = ~ glue::glue("Tag ID: {tag_id}<br>stap #{stap_id}")
      ) |>
      leaflet::addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        data = pid,
        color = color_palette(tag),
        radius = ~radius,
        fillOpacity = 0.8,
        popup = ~ glue::glue("Tag ID: {tag_id}<br>stap #{stap_id}")
      )
  }

  # Add a legend
  leaflet_map <- leaflet_map |>
    leaflet::addLegend(
      position = "topright",
      pal = color_palette,
      values = tag_ids,
      title = "Bird Trajectories",
      opacity = 1
    )

  # Display the map
  leaflet_map
}
