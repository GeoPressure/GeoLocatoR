#' Validate object against schema properties
#'
#' Internal helper function to validate an object against a set of required fields
#' and property definitions from a schema.
#'
#' @param obj The object to validate
#' @param required Vector of required field names
#' @param properties List of property definitions from schema
#' @param name Optional name prefix for error messages
#' @return Logical indicating whether the object validation passed
#' @noRd
validate_gldp_object <- function(
  obj,
  required,
  properties,
  name = "",
  defs = NULL,
  ignore_fields = c("directory")
) {
  name <- glue::glue("{name}{ifelse(name=='','','$')}")

  valid <- TRUE
  fields <- setdiff(names(obj), ignore_fields)
  for (field in fields) {
    if (field %in% names(properties)) {
      field_name <- glue::glue("{name}{field}")
      prop <- resolve_prop(obj[[field]], properties[[field]], defs, field_name)

      valid <- valid &
        validate_gldp_item(
          obj[[field]],
          prop,
          field_name
        )

      if (isTRUE(prop$type == "object")) {
        if (!is.null(prop$`$ref`) && !startsWith(prop$`$ref`, "#/$defs/")) {
          # Not easy to implement as rely on more complex schema with anyOf, allOf etc...
          # prop <- jsonlite::fromJSON(properties[[field]]$`$ref`, simplifyVector = FALSE)
          cli::cli_alert_warning(
            "{.field {field}} cannot be validated (external schema)."
          )
        } else if (!is.null(prop$properties)) {
          valid <- valid &
            validate_gldp_object(
              obj[[field]],
              prop$required,
              prop$properties,
              field,
              defs,
              ignore_fields
            )
        }
      }

      if (isTRUE(prop$type == "array")) {
        if (is.null(prop$items)) {
          cli::cli_alert_warning(
            "{.field {field}} array items schema is missing; skipping item validation."
          )
        } else {
          for (i in seq_len(length(obj[[field]]))) {
            item_field <- glue::glue("{name}{field}[[{i}]]")
            item_prop <- resolve_prop(
              obj[[field]][[i]],
              prop$items,
              defs,
              item_field
            )

            valid <- valid &
              validate_gldp_item(
                obj[[field]][[i]],
                item_prop,
                item_field
              )

            if (isTRUE(item_prop$type == "object")) {
              if (
                !is.null(item_prop$`$ref`) &&
                  !startsWith(item_prop$`$ref`, "#/$defs/")
              ) {
                cli::cli_alert_warning(
                  "{.field {item_field}} cannot be validated (external schema)."
                )
              } else if (!is.null(item_prop$properties)) {
                valid <- valid &
                  validate_gldp_object(
                    obj[[field]][[i]],
                    item_prop$required,
                    item_prop$properties,
                    item_field,
                    defs,
                    ignore_fields
                  )
              }
            }
          }
        }
      }
    } else {
      cli::cli_alert_warning("{.field {field}} does not exist in schema.")
    }
  }

  purrr::walk(required, function(r) {
    if (is.null(obj[[r]])) {
      cli_alert_danger("{.field {name}{r}} is required but missing.")
      valid <<- FALSE
    }
  })
  return(valid)
}

#' @noRd
resolve_local_ref <- function(prop, defs) {
  if (is.null(prop$`$ref`) || is.null(defs)) {
    return(prop)
  }

  ref <- prop$`$ref`
  if (!startsWith(ref, "#/$defs/")) {
    return(prop)
  }

  key <- sub("^#/$defs/", "", ref)
  if (is.null(defs[[key]])) {
    return(prop)
  }

  base <- defs[[key]]
  prop_no_ref <- prop
  prop_no_ref$`$ref` <- NULL

  utils::modifyList(base, prop_no_ref)
}

#' @noRd
infer_type <- function(prop) {
  if (!is.null(prop$type)) {
    return(prop)
  }

  if (!is.null(prop$properties) || !is.null(prop$required)) {
    prop$type <- "object"
  } else if (!is.null(prop$items)) {
    prop$type <- "array"
  }

  prop
}

#' @noRd
resolve_oneof <- function(value, prop, defs, field) {
  if (is.null(prop$oneOf)) {
    return(prop)
  }

  base <- prop
  base$oneOf <- NULL
  base <- resolve_local_ref(base, defs)

  alternatives <- lapply(prop$oneOf, function(alt) {
    alt <- resolve_local_ref(alt, defs)
    infer_type(alt)
  })

  matches <- vapply(
    alternatives,
    function(alt) check_type_silent(value, alt$type),
    logical(1)
  )

  if (!any(matches)) {
    cli::cli_alert_warning(
      "{.field {field}} could not be matched to a `oneOf` schema; using first option."
    )
    chosen <- alternatives[[1]]
  } else {
    chosen <- alternatives[[which(matches)[1]]]
  }

  utils::modifyList(chosen, base)
}

#' @noRd
resolve_prop <- function(value, prop, defs, field) {
  if (is.null(prop)) {
    return(list())
  }

  prop <- resolve_local_ref(prop, defs)

  if (!is.null(prop$oneOf)) {
    prop <- resolve_oneof(value, prop, defs, field)
  }

  infer_type(prop)
}

#' @noRd
validate_gldp_item <- function(item, prop, field) {
  valid <- TRUE

  # valid type
  valid <- valid && check_type(item, prop$type, field)

  # valid format
  valid <- valid && check_format(item, prop$format, field)

  # Check if 'required' property exists and is TRUE
  required <- FALSE
  if (!is.null(prop$required) && is.logical(prop$required) && prop$required) {
    required <- TRUE
    if (is.null(item)) {
      cli_alert_danger("{.field {field}} is required but missing.")
      valid <- FALSE
    } else {
      nn <- is.na(item)

      # If any NULL or NA items exist, report and set valid to FALSE
      if (any(nn)) {
        cli_alert_danger(
          "{.field {field}} is required but {sum(nn)} item{?s} are {.val NA}"
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
    too_short <- lengths < prop$minLength

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
    too_long <- lengths > prop$maxLength

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

  # Check if 'enum' property exists and is specified
  if (!is.null(prop$enum)) {
    # Check if each item is one of the allowed values in the enum
    if (required) {
      in_enum <- item %in% unlist(prop$enum)
    } else {
      # Allow NA if not required
      in_enum <- (item %in% unlist(prop$enum)) | is.na(item)
    }

    if (!all(in_enum)) {
      invalid_values <- item[!in_enum]
      cli_alert_danger(
        "{.field {field}} has {sum(!in_enum)} item{?s} that are not in the allowed values:
      {.val {glue::glue_collapse(prop$enum, sep = ', ')}}. Invalid value{?s}:
      {.val {glue::glue_collapse(unique(invalid_values), sep = ', ')}}"
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


#' @noRd
validate_gldp_fields_match <- function(
  schema_fields,
  data_fields,
  fields_match
) {
  if (is.null(fields_match)) {
    fields_match <- "exact"
  }

  valid <- TRUE
  # Perform the check based on the fieldsMatch value
  if (fields_match == "exact") {
    if (!identical(schema_fields, data_fields)) {
      cli_alert_danger(
        "Exact match failed. Schema fields: {schema_fields}, Data fields:
                       {data_fields}"
      )
      valid <- FALSE
    }
  } else if (fields_match == "equal") {
    if (!setequal(schema_fields, data_fields)) {
      cli_alert_danger(
        "Equal match failed. Schema fields: {schema_fields}, Data fields:
                       {data_fields}"
      )
      valid <- FALSE
    }
  } else if (fields_match == "subset") {
    if (!all(schema_fields %in% data_fields)) {
      missing_fields <- schema_fields[!schema_fields %in% data_fields]
      cli_alert_danger("Subset match failed. Missing fields: {missing_fields}")
      valid <- FALSE
    }
  } else if (fields_match == "superset") {
    if (!all(data_fields %in% schema_fields)) {
      extra_fields <- data_fields[!data_fields %in% schema_fields]
      cli_alert_danger("Superset match failed. Extra fields: {extra_fields}")
      valid <- FALSE
    }
  } else if (fields_match == "partial") {
    if (!any(schema_fields %in% data_fields)) {
      cli_alert_danger("Partial match failed. No schema fields found in data.")
      valid <- FALSE
    }
  }

  invisible(valid)
}


#' @noRd
check_format <- function(value, format, field) {
  if (is.null(format)) {
    return(TRUE)
  }

  format_map <- list(
    `date-time` = function(v) {
      all(is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", v))
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
    # valid the value using the corresponding validation function
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

#' @noRd
get_type_map <- function() {
  list(
    string = function(v) is.character(v),
    number = function(v) is.numeric(v) && !is.logical(v),
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
          grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", v),
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
        # Extract coordinates and validate ranges using base R
        regex_matches <- regexec(pattern, valid_vals[matches])
        coords <- regmatches(valid_vals[matches], regex_matches)
        lat <- as.numeric(sapply(coords, function(x) {
          if (length(x) > 1) x[2] else NA
        }))
        lon <- as.numeric(sapply(coords, function(x) {
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

#' @noRd
check_type <- function(value, type, field) {
  if (is.null(type)) {
    return(TRUE)
  }

  type_map <- get_type_map()

  valid <- TRUE
  # Check if the expected type is in the type_map
  if (type %in% names(type_map)) {
    type_check_result <- type_map[[type]](value)
    if (!all(type_check_result)) {
      cli_alert_danger(
        "Type mismatch for {.field {field}}: Expected `{type}`, but got
                       `{typeof(value)}`."
      )
      valid <- FALSE
    }
  } else {
    cli_alert_danger("Unknown expected type `{type}` for {.field {field}}.")
    valid <- FALSE
  }

  valid
}

