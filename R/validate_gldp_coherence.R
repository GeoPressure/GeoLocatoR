#' Validate cross-resource coherence
#'
#' Internal helper that runs the package-level coherence checks, observation
#' sequence checks, and GeoPressure-specific consistency checks.
#'
#' @param pkg A GeoLocator Data Package object.
#'
#' @return Invisibly returns `TRUE` when all coherence checks pass, `FALSE`
#'   otherwise.
#' @noRd
validate_gldp_coherence <- function(pkg) {
  cli_h3("Check Coherence")

  valid_pkg <- validate_gldp_core_coherence(pkg)
  valid_obs <- validate_gldp_observations(observations(pkg))
  valid_gp <- validate_gldp_geopressure_coherence(pkg)
  valid <- valid_pkg & valid_obs & valid_gp

  if (valid) {
    cli_alert_success("Coherence checks passed.")
  } else {
    cli_alert_danger("Coherence checks failed.")
  }

  invisible(valid)
}

#' Validate GeoPressure-specific coherence checks
#'
#' Internal helper that applies additional checks for optional GeoPressure
#' resources when they are present. These checks cover selected numeric ranges,
#' datetime windows, duration windows, schema-declared foreign keys, and
#' unusual-but-non-failing movement metrics in `edges`.
#'
#' @param pkg A GeoLocator Data Package object.
#'
#' @return Invisibly returns `TRUE` when all failing GeoPressure coherence
#'   checks pass, `FALSE` otherwise. Some unusual values are reported only as
#'   warnings.
#' @noRd
validate_gldp_geopressure_coherence <- function(pkg) {
  valid <- TRUE
  resources <- pkg$resources %||% list()
  resource_names <- vapply(resources, \(x) x$name %||% NA_character_, character(1))

  # Validate numeric columns that should stay below strict thresholds.
  check_lt <- function(data, resource_name, field, upper) {
    if (is.null(data) || !field %in% names(data)) {
      return(invisible(NULL))
    }

    values <- suppressWarnings(as.numeric(data[[field]]))
    invalid <- !is.na(values) & values >= upper
    if (any(invalid)) {
      cli_alert_danger(
        "{sum(invalid)} value{?s} in {.field {resource_name}${field}} must be < {.val {upper}}."
      )
      valid <<- FALSE
    }
  }

  # Validate all date columns against a realistic year window.
  check_datetime_window <- function(data, resource_name, field) {
    if (is.null(data) || !field %in% names(data)) {
      return(invisible(NULL))
    }

    dt <- suppressWarnings(as.POSIXct(data[[field]], tz = "UTC"))
    year <- suppressWarnings(as.integer(format(dt, "%Y")))
    invalid <- !is.na(year) & (year <= 2000 | year >= 2050)
    if (any(invalid)) {
      cli_alert_danger(
        "{sum(invalid)} value{?s} in {.field {resource_name}${field}} must have year > 2000 and < 2050."
      )
      valid <<- FALSE
    }
  }

  # Validate durations between paired date columns.
  check_duration_window <- function(
    data,
    resource_name,
    start_field,
    end_field,
    min_seconds,
    max_seconds,
    warn_unusual = FALSE
  ) {
    if (
      is.null(data) ||
        !all(c(start_field, end_field) %in% names(data))
    ) {
      return(invisible(NULL))
    }

    start <- suppressWarnings(as.POSIXct(data[[start_field]], tz = "UTC"))
    end <- suppressWarnings(as.POSIXct(data[[end_field]], tz = "UTC"))
    duration <- as.numeric(difftime(end, start, units = "secs"))

    invalid <- !is.na(duration) &
      (duration <= min_seconds | duration >= max_seconds)
    if (any(invalid)) {
      format_duration <- function(seconds) {
        if (abs(seconds) >= 24 * 60 * 60) {
          return(glue::glue("{round(seconds / (24 * 60 * 60), 1)} days"))
        }
        if (abs(seconds) >= 60 * 60) {
          return(glue::glue("{round(seconds / (60 * 60), 1)} hours"))
        }
        glue::glue("{round(seconds / 60, 1)} min")
      }
      invalid_duration <- duration[invalid]
      expected_range_label <- purrr::map_chr(c(min_seconds, max_seconds), format_duration)
      invalid_high <- invalid_duration[invalid_duration >= max_seconds]
      invalid_low <- invalid_duration[invalid_duration <= min_seconds]

      if (length(invalid_high) > 0) {
        extreme_label <- format_duration(max(invalid_high))
        extreme_kind <- "Maximum"
      } else {
        extreme_label <- format_duration(min(invalid_low))
        extreme_kind <- "Minimum"
      }

      if (warn_unusual) {
        cli_alert_warning(
          "We detected unusual duration for {.field {resource_name}} ({sum(invalid)} value{?s} outside expected range: {expected_range_label[1]} < duration < {expected_range_label[2]}). {extreme_kind} unusual duration: {extreme_label}."
        )
      } else {
        cli_alert_danger(
          "{sum(invalid)} duration{?s} in {.field {resource_name}} are outside expected range ({expected_range_label[1]} < duration < {expected_range_label[2]}). {extreme_kind} invalid duration: {extreme_label}."
        )
        valid <<- FALSE
      }
    }
  }

  # Validate foreign keys declared in each resource schema against parent rows.
  check_foreign_keys <- function() {
    for (resource in resources) {
      child_name <- as.character(resource$name %||% NA_character_)[1]
      child_data <- resource$data %||% NULL
      foreign_keys <- resource$schema$foreignKeys %||% list()

      if (!is.data.frame(child_data) || length(foreign_keys) == 0) {
        next
      }

      for (fk in foreign_keys) {
        child_fields <- as.character(unlist(fk$fields %||% character(0), use.names = FALSE))
        parent_name <- as.character(fk$reference$resource %||% NA_character_)[1]
        parent_fields <- as.character(unlist(
          fk$reference$fields %||% character(0),
          use.names = FALSE
        ))

        if (
          length(child_fields) == 0 ||
            !nzchar(parent_name) ||
            length(child_fields) != length(parent_fields)
        ) {
          next
        }

        parent_idx <- match(parent_name, resource_names)
        parent_data <- if (is.na(parent_idx)) NULL else resources[[parent_idx]]$data %||% NULL
        if (is.null(parent_data) || !is.data.frame(parent_data)) {
          cli_alert_danger(
            "Foreign key check for {.field {child_name}} failed: parent resource {.field {parent_name}} is missing."
          )
          valid <<- FALSE
          next
        }

        if (!all(child_fields %in% names(child_data))) {
          cli_alert_danger(
            "Foreign key check for {.field {child_name}} failed: child field{?s} {.field {child_fields}} are missing."
          )
          valid <<- FALSE
          next
        }
        if (!all(parent_fields %in% names(parent_data))) {
          cli_alert_danger(
            "Foreign key check for {.field {child_name}} failed: parent field{?s} {.field {parent_fields}} are missing in {.field {parent_name}}."
          )
          valid <<- FALSE
          next
        }

        child_keys <- child_data[, child_fields, drop = FALSE]
        child_keys <- child_keys[stats::complete.cases(child_keys), , drop = FALSE]
        if (nrow(child_keys) == 0) {
          next
        }
        child_keys[] <- lapply(child_keys, as.character)
        child_keys <- unique(child_keys)

        parent_keys <- parent_data[, parent_fields, drop = FALSE]
        parent_keys <- parent_keys[stats::complete.cases(parent_keys), , drop = FALSE]
        parent_keys[] <- lapply(parent_keys, as.character)
        parent_keys <- unique(parent_keys)
        names(parent_keys) <- child_fields

        missing_keys <- anti_join(child_keys, parent_keys, by = child_fields)
        if (nrow(missing_keys) > 0) {
          cli_alert_danger(
            "{nrow(missing_keys)} foreign key row{?s} in {.field {child_name}} are missing in parent {.field {parent_name}}."
          )
          valid <<- FALSE
        }
      }
    }
  }

  # Check realistic value ranges in staps.
  if ("staps" %in% resource_names) {
    staps_data <- staps(pkg)
    check_lt(staps_data, "staps", "stap_id", 300)
    check_datetime_window(staps_data, "staps", "start")
    check_datetime_window(staps_data, "staps", "end")
    check_duration_window(
      staps_data,
      "staps",
      "start",
      "end",
      min_seconds = 5 * 60,
      max_seconds = 360 * 24 * 60 * 60
    )
  }

  # Check realistic date ranges in twilights and pressurepaths.
  if ("twilights" %in% resource_names) {
    twilights_data <- twilights(pkg)
    check_datetime_window(twilights_data, "twilights", "twilight")
  }

  if ("pressurepaths" %in% resource_names) {
    pressurepaths_data <- pressurepaths(pkg)
    check_datetime_window(pressurepaths_data, "pressurepaths", "datetime")
    if ("altitude" %in% names(pressurepaths_data)) {
      altitude <- suppressWarnings(as.numeric(pressurepaths_data$altitude))
      invalid_altitude <- !is.na(altitude) & (altitude <= -200 | altitude >= 7000)
      if (any(invalid_altitude)) {
        cli_alert_danger(
          "{sum(invalid_altitude)} value{?s} in {.field pressurepaths$altitude} must be > -200 and < 7000."
        )
        valid <- FALSE
      }
    }
  }

  # Check realistic value ranges in paths and edges.
  if ("paths" %in% resource_names) {
    paths_data <- paths(pkg)
    check_lt(paths_data, "paths", "j", 500)
  }

  if ("edges" %in% resource_names) {
    edges_data <- edges(pkg)
    check_datetime_window(edges_data, "edges", "start")
    check_datetime_window(edges_data, "edges", "end")
    check_duration_window(
      edges_data,
      "edges",
      "start",
      "end",
      min_seconds = 5 * 60,
      max_seconds = 50 * 60 * 60,
      warn_unusual = TRUE
    )
    check_lt(edges_data, "edges", "n", 10)

    if ("distance" %in% names(edges_data)) {
      distance_values <- suppressWarnings(as.numeric(edges_data$distance))
      unusual_distance <- !is.na(distance_values) & distance_values >= 3000
      if (any(unusual_distance)) {
        max_unusual_distance <- max(distance_values[unusual_distance])
        max_unusual_distance_label <- if (max_unusual_distance %% 1 == 0) {
          format(
            max_unusual_distance,
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          )
        } else {
          format(
            round(max_unusual_distance, 1),
            nsmall = 1,
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          )
        }
        cli_alert_warning(
          "We detected unusual distance for {.field edges} ({sum(unusual_distance)} value{?s} > 3000 km). Maximum unusual distance: {max_unusual_distance_label} km."
        )
      }
    }

    unusual_speed <- FALSE
    if (all(c("gs_u", "gs_v") %in% names(edges_data))) {
      gs_u <- suppressWarnings(as.numeric(edges_data$gs_u))
      gs_v <- suppressWarnings(as.numeric(edges_data$gs_v))
      gs_magnitude <- sqrt(gs_u^2 + gs_v^2)
      unusual_speed <- unusual_speed | any(!is.na(gs_magnitude) & gs_magnitude >= 150)
    }
    if (all(c("ws_u", "ws_v") %in% names(edges_data))) {
      ws_u <- suppressWarnings(as.numeric(edges_data$ws_u))
      ws_v <- suppressWarnings(as.numeric(edges_data$ws_v))
      ws_magnitude <- sqrt(ws_u^2 + ws_v^2)
      unusual_speed <- unusual_speed | any(!is.na(ws_magnitude) & ws_magnitude >= 100)
    }
    if (unusual_speed) {
      cli_alert_warning("We detected unusual speed for edges.")
    }
  }

  # Check that child foreign key values are present in parent resources.
  check_foreign_keys()

  invisible(valid)
}

#' Validate core package coherence
#'
#' Internal helper that checks consistency across the required `tags`,
#' `observations`, and `measurements` resources. It verifies key identifier
#' relationships, species consistency, and the presence of expected equipment
#' and retrieval events.
#'
#' @param pkg A GeoLocator Data Package object.
#'
#' @return Invisibly returns `TRUE` when all failing core coherence checks pass,
#'   `FALSE` otherwise. Missing observations for tags are reported as warnings.
#' @noRd
validate_gldp_core_coherence <- function(pkg) {
  valid <- TRUE

  min_res_required <- c("tags", "observations", "measurements")
  res_missing <- min_res_required[
    !(min_res_required %in% sapply(pkg$resources, \(x) x$name))
  ]
  if (length(res_missing) > 0) {
    cli::cli_alert_warning(
      "{.pkg pkg} is missing {.val {res_missing}}. We could not check package coherence."
    )
    valid <- FALSE
    return(valid)
  }

  t <- tags(pkg)
  o <- observations(pkg)
  m <- measurements(pkg)

  # Check for conflicting species assignments:
  # A single ring_number should be linked to only one scientific_name.
  conflicting_ring_numbers <- t |>
    filter(!is.na(.data$ring_number)) |>
    group_by(.data$ring_number) |>
    filter(n_distinct(.data$scientific_name) > 1) |>
    distinct(.data$ring_number) |>
    pull(.data$ring_number)
  if (length(conflicting_ring_numbers) > 0) {
    valid <- FALSE
    conflicting_ring_numbers |>
      purrr::walk(
        ~ cli_alert_danger(
          "Multiple scientific names used for ring_number {.strong {}}",
          .
        )
      )
  }

  # Check for measurements with tag_id not present in the tags table.
  # All tag_id entries in measurements must be declared in tags.
  midmissing <- unique(m$tag_id[!(m$tag_id %in% t$tag_id)])
  if (length(midmissing) > 1) {
    cli_alert_danger(
      "{.field tags} is missing {.field tag_id}={.val {midmissing}} which are present in {.field measurements}."
    )
    cli_alert_info(
      "All {.field tag_id} presents in the resource {.field measurements} need to also be present in the resource {.field tags}."
    )
    valid <- FALSE
  }

  # Check for ring_number present in tags but missing from observations.
  # All birds with a ring_number in tags should have at least one corresponding observation.
  tringmissing <- unique(t$ring_number[!(t$ring_number %in% o$ring_number)])
  if (length(tringmissing) > 1) {
    cli_alert_danger(
      "{.field observations} is missing {.field ring_number} {.val {tringmissing}} which are present in {.field tags}."
    )
    valid <- FALSE
  }

  # Check for mismatched tag_id and ring_number combinations between tags and observations.
  # If a combination exists in observations, it must also exist in tags.
  invalid_combinations <- o |>
    filter(!is.na(.data$tag_id)) |>
    anti_join(t, by = c("tag_id", "ring_number"))
  if (nrow(invalid_combinations) > 0) {
    cli_alert_danger(
      "The following {.field tag_id} and {.field ring_number} combinations in {.field observation} are not present in {.field tags}:"
    )
    print(invalid_combinations)
    valid <- FALSE
  }

  # Check for tag_id present in tags but missing from observations.
  # Each tag_id should have at least one observation record.
  tidmissing <- setdiff(t$tag_id, unique(o$tag_id))
  if (length(tidmissing) > 0) {
    cli::cli_alert_warning(
      "No observations found for {.val {tidmissing}} declared in {.field tags}."
    )
    # Still valid
    # valid <- FALSE
  }

  # Check for missing equipment or retrieval while measurement are present
  # If measurements are present, there should be at least one equipment or retrieval observation.
  tidmissingequip <- setdiff(
    unique(m$tag_id),
    unique(o$tag_id[o$observation_type == "equipment"])
  )
  if (length(tidmissingequip) > 0) {
    cli_alert_danger(
      "No equipment found for {.val {tidmissingequip}} in in {.field observations} while data present in {.field measurements}."
    )
    valid <- FALSE
  }
  tidmissingret <- setdiff(
    unique(m$tag_id),
    unique(o$tag_id[o$observation_type == "retrieval"])
  )
  if (length(tidmissingret) > 0) {
    cli_alert_danger(
      "No retrieval found for {.val {tidmissingret}} in in {.field observations} while data present in {.field measurements}."
    )
    valid <- FALSE
  }

  invisible(valid)
}


#' Validate observation histories
#'
#' Internal helper that validates ordering and state transitions in the
#' `observations` table, including tag assignment consistency, required
#' `tag_id` usage, equipment and retrieval ordering, duplicate events, and
#' plausible `device_status` transitions.
#'
#' @param o The `observations` data frame.
#'
#' @return Invisibly returns `TRUE` when the observation checks pass, `FALSE`
#'   otherwise.
#' @noRd
validate_gldp_observations <- function(o) {
  valid <- TRUE

  o <- o |>
    arrange(
      .data$ring_number,
      .data$datetime,
      factor(
        .data$observation_type,
        levels = c("capture", "retrieval", "equipment", "sighting", "other")
      )
    )

  # Check 1: tag_id is only associated with a single ring_number
  inconsistent_tag_ids <- o |>
    filter(!is.na(.data$tag_id)) |>
    group_by(.data$tag_id) |>
    summarize(unique_ring_numbers = n_distinct(.data$ring_number)) |>
    filter(.data$unique_ring_numbers > 1)

  if (nrow(inconsistent_tag_ids) > 0) {
    cli_alert_danger(
      "{nrow(inconsistent_tag_ids)} tag_id{?s} {?is/are} associated with multiple ring_numbers. Check: {.field {inconsistent_tag_ids$tag_id}}"
    )
    valid <- FALSE
  }

  # Check 2: equipment or retrieval must have a tag_id
  missing_tag_id <- o |>
    filter(
      .data$observation_type %in%
        c("equipment", "retrieval") &
        is.na(.data$tag_id)
    )

  if (nrow(missing_tag_id) > 0) {
    error_tag <- unique(missing_tag_id$tag_id)
    cli_alert_danger(
      "{length(error_tag)} equipment or retrieval observation{?s} {?is/are} missing a tag_id. Check: {.field {error_tag}}"
    )
    valid <- FALSE
  }

  # Check 3: equipment and retrieval can only have a device status present.
  obs_equi_retrieval_without_present <- o |>
    filter(
      .data$observation_type %in%
        c("equipment", "retrieval") &
        .data$device_status != "present"
    )

  if (nrow(obs_equi_retrieval_without_present) > 0) {
    error_tag <- unique(obs_equi_retrieval_without_present$tag_id)
    cli_alert_danger(
      "{length(error_tag)} equipment or retrieval observation{?s} don't have a device status 'present'. Check: {.field {error_tag}}"
    )
    valid <- FALSE
  }

  # Check 4: capture-missing and capture-present must have a tag_id
  missing_tag_id <- o |>
    filter(
      .data$observation_type == "capture" &
        (.data$device_status %in% c("missing", "present")) &
        is.na(.data$tag_id)
    )

  if (nrow(missing_tag_id) > 0) {
    error_ring_number <- unique(missing_tag_id$ring_number)
    cli_alert_danger(
      "{length(error_ring_number)} {.var ring_number} with a device status {.val missing} or {.val present} {?is/are} missing a {.var tag_id}. Check: {.field {error_ring_number}}"
    )
    valid <- FALSE
  }

  # Check 5: No second tag_id attached without a prior retrieval (or capture with missing.)
  multiple_tags_without_retrieval <- o |>
    group_by(.data$ring_number) |>
    filter(
      (.data$observation_type %in% c("retrieval", "equipment")) |
        (.data$observation_type == "capture" & .data$device_status == "missing")
    ) |>
    filter(
      (lag(.data$tag_id) != .data$tag_id) &
        !(lag(.data$observation_type) == "retrieval" |
          lag(.data$device_status) == "missing")
    )

  if (nrow(multiple_tags_without_retrieval) > 0) {
    error_ring_number <- unique(multiple_tags_without_retrieval$ring_number)
    cli_alert_danger(
      "{length(error_ring_number)} ring{?s} where a second tag is attached without a prior retrieval or capture-missing. Check: {.field {error_ring_number}}"
    )
    valid <- FALSE
  }

  # Check 6: A tag_id must follow an equipment event
  tag_without_equipment <- o |>
    group_by(.data$ring_number, .data$tag_id) |>
    filter(!is.na(.data$tag_id)) |>
    filter(!any(.data$observation_type == "equipment"))

  if (nrow(tag_without_equipment) > 0) {
    error_tag <- unique(tag_without_equipment$tag_id)
    cli_alert_danger(
      "{length(error_tag)} tag{?s} {?was/were} recorded without a preceding equipment event. Check: {.field {error_tag}}"
    )
    valid <- FALSE
  }

  # Check 7: Retrieval datetime must be equal to or after equipment datetime per tag_id.
  equipment_dates <- o |>
    filter(
      !is.na(.data$tag_id),
      .data$observation_type == "equipment"
    ) |>
    mutate(datetime = suppressWarnings(as.POSIXct(.data$datetime, tz = "UTC"))) |>
    filter(!is.na(.data$datetime)) |>
    group_by(.data$tag_id) |>
    summarize(equipment_datetime = min(.data$datetime), .groups = "drop")

  retrieval_dates <- o |>
    filter(
      !is.na(.data$tag_id),
      .data$observation_type == "retrieval"
    ) |>
    mutate(datetime = suppressWarnings(as.POSIXct(.data$datetime, tz = "UTC"))) |>
    filter(!is.na(.data$datetime)) |>
    group_by(.data$tag_id) |>
    summarize(retrieval_datetime = min(.data$datetime), .groups = "drop")

  invalid_equipment_retrieval_order <- inner_join(
    equipment_dates,
    retrieval_dates,
    by = "tag_id"
  ) |>
    filter(.data$retrieval_datetime < .data$equipment_datetime)

  if (nrow(invalid_equipment_retrieval_order) > 0) {
    error_tag <- invalid_equipment_retrieval_order$tag_id
    cli_alert_danger(
      "{length(error_tag)} tag{?s} {?has/have} retrieval before equipment: {.field {error_tag}}"
    )
    valid <- FALSE
  }

  # Check 8: duplicate
  duplicate_observations <- o |>
    group_by(.data$ring_number, .data$datetime, .data$observation_type) |>
    filter(n() > 1)

  if (nrow(duplicate_observations) > 0) {
    error_tag <- unique(duplicate_observations$tag_id)
    cli_alert_danger(
      "{length(error_tag)} duplicate observations found. Check: {.field {error_tag}}"
    )
    valid <- FALSE
  }

  # Check 9: Invalid transition
  invalid_transitions <- o |>
    group_by(.data$ring_number) |>
    filter(.data$device_status != "unknown") |>
    mutate(
      previous_status = lag(.data$device_status),
      previous_type = lag(.data$observation_type)
    ) |>
    filter(
      (.data$device_status == "missing" & is.na(.data$previous_status)) |
        (.data$device_status == "missing" & .data$previous_status == "none") |
        (.data$device_status == "none" &
          .data$previous_status == "present" &
          .data$previous_type != "retrieval") |
        (.data$device_status == "present" &
          .data$previous_status == "none" &
          .data$observation_type != "equipment")
    )

  if (nrow(invalid_transitions) > 0) {
    error_ring_number <- unique(invalid_transitions$ring_number)
    cli_alert_danger(
      "{length(error_ring_number)} invalid device_status transitions found. Check: {.field {error_ring_number}}"
    )
    valid <- FALSE
  }

  invisible(valid)
}
