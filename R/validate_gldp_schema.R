#' Validate a value against a schema property
#'
#' Internal helper that applies type checks, format checks, requiredness, and
#' selected frictionless constraint keywords such as uniqueness, enumerations,
#' patterns, length limits, item-count limits, and numeric bounds.
#'
#' @param item The value or vector of values to validate.
#' @param prop A resolved schema property definition.
#' @param field Field name used in validation messages.
#'
#' @return `TRUE` when the property checks pass, `FALSE` otherwise.
#' @noRd
validate_gldp_item <- function(item, prop, field) {
  valid <- TRUE

  # validate type
  valid <- valid && check_type(item, prop$type, field)

  # validate format
  valid <- valid && check_format(item, prop$format, field)

  # Bounds and lengths compared on a value of the wrong type mislead, e.g. "100" < "90".
  if (!valid) {
    return(FALSE)
  }

  # Check if 'required' property exists and is TRUE
  if (!is.null(prop$required) && is.logical(prop$required) && prop$required) {
    if (is.null(item)) {
      cli_alert_danger("{.field {field}} is required but missing.")
      valid <- FALSE
    } else {
      nn <- is.na(item)

      # If any NULL or NA items exist, report and set valid to FALSE
      if (any(nn)) {
        cli_alert_danger(
          "{.field {field}} is required but {sum(nn)} item{?s} {?is/are} missing."
        )
        valid <- FALSE
      }
    }
  }

  # Unique
  if (!is.null(prop$unique) && prop$unique) {
    # Check for duplicates in 'item'
    duplicates <- duplicated(item) & !is.na(item)

    if (any(duplicates)) {
      cli_alert_danger(
        "{.field {field}} must be unique but {sum(duplicates)} duplicate{?s} found."
      )
      valid <- FALSE
    }
  }

  # Check if 'minLength' property exists and is specified
  if (!is.null(prop$minLength)) {
    lengths <- nchar(as.character(item)) # Convert to character for length check
    too_short <- !is.na(lengths) & lengths < prop$minLength

    if (any(too_short)) {
      cli_alert_danger(
        "{.field {field}} has {sum(too_short)} item{?s} shorter than the minimum
                       length of {prop$minLength}."
      )
      valid <- FALSE
    }
  }

  # Check if 'maxLength' property exists and is specified
  if (!is.null(prop$maxLength)) {
    lengths <- nchar(as.character(item)) # Convert to character for length check
    too_long <- !is.na(lengths) & lengths > prop$maxLength

    if (any(too_long)) {
      cli_alert_danger(
        "{.field {field}} has {sum(too_long)} item{?s} longer than the maximum
                       length of {prop$maxLength}."
      )
      valid <- FALSE
    }
  }

  # Check if 'minItems' property exists and is specified
  if (!is.null(prop$minItems)) {
    if (length(item) < prop$minItems) {
      cli_alert_danger(
        "{.field {field}} must contain at least {prop$minItems} item{?s}, but only
                       {length(item)} item{?s} provided."
      )
      valid <- FALSE
    }
  }

  # Check if 'maxItems' property exists and is specified
  if (!is.null(prop$maxItems)) {
    if (length(item) > prop$maxItems) {
      cli_alert_danger(
        "{.field {field}} must contain no more than {prop$maxItems} item{?s}, but
                       {length(item)} item{?s} provided."
      )
      valid <- FALSE
    }
  }

  # Check if 'minimum' property exists and is specified (assuming numeric)
  if (!is.null(prop$minimum)) {
    below_min <- item < prop$minimum

    if (any(below_min, na.rm = TRUE)) {
      cli_alert_danger(
        "{.field {field}} has {sum(below_min, na.rm = TRUE)} item{?s} below the
                       minimum value of {prop$minimum}."
      )
      valid <- FALSE
    }
  }

  # Check if 'maximum' property exists and is specified (assuming numeric)
  if (!is.null(prop$maximum)) {
    above_max <- item > prop$maximum

    if (any(above_max, na.rm = TRUE)) {
      cli_alert_danger(
        "{.field {field}} has {sum(above_max, na.rm = TRUE)} item{?s} above the
                       maximum value of {prop$maximum}."
      )
      valid <- FALSE
    }
  }

  # Table Schema v2 strict bounds, e.g. a body mass must be > 0.
  if (!is.null(prop$exclusiveMinimum)) {
    n_bad <- sum(item <= prop$exclusiveMinimum, na.rm = TRUE)
    if (n_bad > 0) {
      cli_alert_danger(
        "{.field {field}} has {n_bad} item{?s} not greater than {prop$exclusiveMinimum}."
      )
      valid <- FALSE
    }
  }
  if (!is.null(prop$exclusiveMaximum)) {
    n_bad <- sum(item >= prop$exclusiveMaximum, na.rm = TRUE)
    if (n_bad > 0) {
      cli_alert_danger(
        "{.field {field}} has {n_bad} item{?s} not less than {prop$exclusiveMaximum}."
      )
      valid <- FALSE
    }
  }

  # Both `categories` and the `enum` constraint restrict a field to a fixed set
  # of values. GeoLocator-DP declares both, `categories` being the Data Package
  # v2 spelling that also carries a label per value, so read whichever is there.
  allowed <- if (!is.null(prop$categories)) {
    vapply(
      prop$categories,
      \(x) if (is.list(x)) as.character(x$value)[1] else as.character(x)[1],
      character(1)
    )
  } else if (!is.null(prop$enum)) {
    as.character(unlist(prop$enum))
  }

  if (!is.null(allowed)) {
    # Missing values are reported by the required check above.
    in_allowed <- item %in% allowed | is.na(item)

    if (!all(in_allowed)) {
      invalid_values <- unique(item[!in_allowed])
      cli_alert_danger(
        "{.field {field}} has {sum(!in_allowed)} item{?s} that {?is/are} not in the allowed values:
      {.val {allowed}}. Invalid value{?s}: {.val {invalid_values}}"
      )
      valid <- FALSE
    }
  }

  # Check if 'pattern' property exists and is specified
  if (!is.null(prop$pattern)) {
    # Apply the regular expression to each non-NA item
    non_na_items <- !is.na(item)
    if (any(non_na_items)) {
      pattern_match <- tryCatch(
        grepl(prop$pattern, as.character(item[non_na_items]), perl = TRUE),
        error = function(e) {
          cli::cli_alert_warning(
            "{.field {field}} pattern could not be compiled; skipping pattern check."
          )
          rep(TRUE, sum(non_na_items))
        }
      )

      # Identify items that do not match the pattern
      not_matching <- !pattern_match

      if (any(not_matching)) {
        cli_alert_danger(
          "{.field {field}} has {sum(not_matching)} item{?s} that do not match the
                         required pattern: {.val {prop$pattern}}."
        )
        valid <- FALSE
      }
    }
  }

  valid
}

#' Validate a declared field format
#'
#' Internal helper that checks whether a value matches one of the supported
#' frictionless format strings implemented in GeoLocatoR.
#'
#' @param value The value or vector of values to validate.
#' @param format The expected format string.
#' @param field Field name used in validation messages.
#'
#' @return `TRUE` when the format check passes, `FALSE` otherwise.
#' @noRd
check_format <- function(value, format, field) {
  if (is.null(format)) {
    return(TRUE)
  }

  format_map <- list(
    `date-time` = function(v) {
      # Data Package v2 allows optional milliseconds with `"format": "default"`.
      all(is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?Z$", v))
    },
    date = function(v) all(is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}$", v)),
    time = function(v) all(is.na(v) | grepl("^\\d{2}:\\d{2}:\\d{2}$", v)),
    `utc-millisec` = function(v) all(is.na(v) | is.numeric(v)),
    regex = function(v) {
      all(sapply(v, function(x) {
        if (is.na(x)) {
          return(TRUE)
        }
        tryCatch(
          {
            grepl(x, "")
            TRUE
          },
          error = function(e) FALSE
        )
      }))
    },
    color = function(v) {
      color_pattern <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$|^(red|blue|green|yellow|black|white)$"
      all(is.na(v) | grepl(color_pattern, v))
    },
    style = function(v) all(is.na(v) | grepl("^.+: .+;$", v)),
    phone = function(v) all(is.na(v) | grepl("^\\+?[0-9 .-]{7,}$", v)),
    uri = function(v) {
      uri_pattern <- "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?$"
      all(is.na(v) | grepl(uri_pattern, v))
    },
    email = function(v) {
      email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
      all(is.na(v) | grepl(email_pattern, v))
    },
    `ip-address` = function(v) {
      all(is.na(v) | grepl("^\\d{1,3}(\\.\\d{1,3}){3}$", v))
    },
    ipv6 = function(v) {
      all(is.na(v) | grepl("^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$", v))
    },
    `host-name` = function(v) all(is.na(v) | grepl("^[a-zA-Z0-9.-]+$", v)),
    textarea = function(v) {
      all(is.na(v) | grepl("^(\\S.*(?:\\r?\\n\\S.*)*)$", v) | v == "")
    }
  )

  valid <- TRUE
  if (format %in% names(format_map)) {
    format_func <- format_map[[format]]
    # validate the value using the corresponding validation function
    format_check_result <- format_func(value)
    if (!all(format_check_result)) {
      cli_alert_danger(
        "Format mismatch for {.field {field}}: Expected format {.val {format}},
                       but got {.val {value}}."
      )
      valid <- FALSE
    }
  } else {
    cli_alert_danger(
      "Unknown expected format {.val {format}} for {.field {field}}."
    )
    valid <- FALSE
  }
  valid
}

#' Return the internal type validators
#'
#' Internal helper that maps supported schema types to predicate functions used
#' during validation.
#'
#' @return A named list of type-checking functions.
#' @noRd
get_type_map <- function() {
  list(
    string = function(v) is.character(v),
    # Data Package v2 `list` fields are read as character by frictionless.
    list = function(v) is.character(v),
    number = function(v) is.numeric(v),
    integer = function(v) {
      is.integer(v) || (is.numeric(v) && all(v == floor(v), na.rm = TRUE))
    },
    boolean = function(v) is.logical(v),
    object = function(v) is.list(v) && !is.data.frame(v),
    array = function(v) is.vector(v) || is.list(v),
    null = function(v) is.null(v),
    datetime = function(v) {
      if (inherits(v, c("POSIXct", "POSIXlt"))) {
        return(TRUE)
      }
      if (is.character(v)) {
        return(all(
          grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?Z$", v),
          na.rm = TRUE
        ))
      }
      FALSE
    },
    date = function(v) {
      if (inherits(v, c("Date", "POSIXct", "POSIXlt"))) {
        return(TRUE)
      }
      if (is.character(v)) {
        return(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", v), na.rm = TRUE))
      }
      FALSE
    },
    time = function(v) {
      if (inherits(v, c("POSIXct", "POSIXlt"))) {
        return(TRUE)
      }
      if (is.character(v)) {
        return(all(grepl("^\\d{2}:\\d{2}:\\d{2}$", v), na.rm = TRUE))
      }
      FALSE
    },
    year = function(v) {
      if (inherits(v, c("Date", "POSIXct", "POSIXlt"))) {
        return(TRUE)
      }
      if (is.numeric(v)) {
        return(all(is.na(v) | (v >= 1000 & v <= 9999 & v == floor(v))))
      }
      if (is.character(v)) {
        return(all(is.na(v) | grepl("^\\d{4}$", v)))
      }
      FALSE
    },
    yearmonth = function(v) {
      if (inherits(v, c("Date", "POSIXct", "POSIXlt"))) {
        return(TRUE)
      }
      if (is.character(v)) {
        return(all(is.na(v) | grepl("^\\d{4}-\\d{2}$", v)))
      }
      FALSE
    },
    duration = function(v) {
      if (!is.character(v)) {
        return(FALSE)
      }
      # ISO 8601 duration pattern - simplified version
      pattern <- "^P(\\d+Y)?(\\d+M)?(\\d+D)?(T(\\d+H)?(\\d+M)?(\\d+(\\.\\d+)?S)?)?$"
      all(is.na(v) | (grepl(pattern, v) & grepl("[YMDHS]", v))) # Must have at least one component
    },
    geopoint = function(v) {
      if (!is.character(v)) {
        return(FALSE)
      }

      # Handle NA values - they are considered valid
      na_mask <- is.na(v)
      if (all(na_mask)) {
        return(TRUE)
      }

      # Validate non-NA values
      valid_vals <- v[!na_mask]
      pattern <- "^\\s*(-?\\d+(?:\\.\\d+)?)\\s*,\\s*(-?\\d+(?:\\.\\d+)?)\\s*$"
      matches <- grepl(pattern, valid_vals)

      if (length(valid_vals) > 0 && any(matches)) {
        # Extract coordinates and validate ranges using base R. The Table Schema
        # default geopoint is "lon, lat", longitude first.
        regex_matches <- regexec(pattern, valid_vals[matches])
        coords <- regmatches(valid_vals[matches], regex_matches)
        lon <- as.numeric(sapply(coords, function(x) {
          if (length(x) > 1) x[2] else NA
        }))
        lat <- as.numeric(sapply(coords, function(x) {
          if (length(x) > 2) x[3] else NA
        }))
        valid_coords <- all(
          lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180,
          na.rm = TRUE
        )
        return(all(matches) && valid_coords)
      }
      all(matches)
    },
    geojson = function(v) {
      if (!is.character(v)) {
        return(FALSE)
      }

      # Handle each element in the vector
      all(sapply(v, function(x) {
        if (is.na(x)) {
          return(TRUE)
        } # NA values are considered valid

        # Try to parse as JSON and validate structure
        tryCatch(
          {
            json_obj <- jsonlite::fromJSON(x, simplifyVector = FALSE)
            return(
              !is.null(json_obj$type) &&
                json_obj$type == "Feature" &&
                !is.null(json_obj$geometry) &&
                !is.null(json_obj$geometry$type) &&
                !is.null(json_obj$geometry$coordinates)
            )
          },
          error = function(e) FALSE
        )
      }))
    },
    any = function(v) TRUE # 'any' accepts everything
  )
}

#' Check a value type without emitting messages
#'
#' Internal helper used when selecting among alternative schema definitions.
#'
#' @param value The value to test.
#' @param type The expected schema type.
#'
#' @return `TRUE` when the type matches, `FALSE` otherwise.
#' @noRd
check_type_silent <- function(value, type) {
  if (is.null(type)) {
    return(FALSE)
  }

  type_map <- get_type_map()
  if (!(type %in% names(type_map))) {
    return(FALSE)
  }

  type_map[[type]](value)
}

#' Validate a declared field type
#'
#' Internal helper that checks whether a value matches the expected schema type
#' and reports a validation error when it does not.
#'
#' @param value The value to test.
#' @param type The expected schema type.
#' @param field Field name used in validation messages.
#'
#' @return `TRUE` when the type check passes, `FALSE` otherwise.
#' @noRd
check_type <- function(value, type, field) {
  if (is.null(type)) {
    return(TRUE)
  }

  if (!(type %in% names(get_type_map()))) {
    cli_alert_danger("Unknown expected type `{type}` for {.field {field}}.")
    return(FALSE)
  }

  if (!all(check_type_silent(value, type))) {
    cli_alert_danger(
      "Type mismatch for {.field {field}}: Expected `{type}`, but got
                       `{typeof(value)}`."
    )
    return(FALSE)
  }

  TRUE
}
