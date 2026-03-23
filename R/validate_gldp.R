#' Validate a GeoLocator Data Package
#'
#' @description
#' `validate_gldp()` runs the standard quality-control workflow for a GeoLocator Data Package.
#' It provides an automated check that a package is structurally valid and internally coherent
#' before publication or reuse.
#'
#' @details
#' Validation is organized in two complementary layers:
#' 1. **Technical conformance**: checks compliance with the GeoLocator Data Package specification,
#'    including required resources, schema conformity, required fields, and controlled vocabularies.
#' 2. **Coherence and metadata quality**: checks consistency of identifiers and relationships across
#'    key tables (e.g. `tags`, `observations`, and `measurements`) and reports metadata best-practice
#'    mismatches that should be justified during curation.
#'
#' See the [GeoLocator-DP curation guide](https://raphaelnussbaumer.com/GeoLocator-DP/curation/) for
#' more informations.
#'
#' @param pkg A GeoLocator Data Package object to be validated.
#'
#' @return A silent logical value indicating whether the package validation was successful (`TRUE`) or
#' failed (`FALSE`).
#'
#' @export
validate_gldp <- function(pkg) {
  check_gldp(pkg)

  valid <- validate_gldp_profile(pkg)

  valid <- valid & validate_gldp_resources(pkg)

  valid <- valid & validate_gldp_coherence(pkg)

  validate_gldp_meta(pkg)

  cli_h1("Final:")
  if (valid) {
    cli_alert_success("Package validation succeeded.")
  } else {
    cli_alert_danger("Package validation failed. Review messages above.")
  }

  invisible(valid)
}

#' Validate GeoLocator Data Package profile
#'
#' Internal helper function to validate that a GeoLocator Data Package conforms
#' to the expected profile schema.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Logical indicating whether the profile validation passed
#' @noRd
validate_gldp_profile <- function(pkg) {
  cli_h3("Check Profile")
  version <- gldp_version(pkg)

  pkg_schema <- gldp_profile_schema(version)

  required <- unlist(pkg_schema$allOf[[2]]$required)
  properties <- pkg_schema$allOf[[2]]$properties

  # Skip resource validation at the profile level (handled separately)
  required <- setdiff(required, "resources")
  properties$resources <- NULL

  # Ignore all additional field not present in the schema
  ignore_fields <- setdiff(names(pkg), names(properties))

  valid <- validate_gldp_object(
    pkg,
    required,
    properties,
    defs = pkg_schema$`$defs`,
    ignore_fields = ignore_fields
  )

  invisible(valid)
}

#' Validate GeoLocator Data Package resources
#'
#' Internal helper function to validate all resources within a GeoLocator Data Package
#' against their respective schemas.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Logical indicating whether all resource validations passed
#' @noRd
validate_gldp_resources <- function(pkg) {
  pkg <- update_gldp_order_resources(pkg)

  valid <- TRUE

  if (length(pkg$resources) == 0) {
    cli_alert_danger("Package must contain at least one resource.")
    valid <- FALSE
  }

  for (i in seq_along(pkg$resources)) {
    resource <- pkg$resources[[i]]

    is_tabular_resource <- identical(resource$profile, "tabular-data-resource") &&
      is.list(resource$schema) &&
      !is.null(resource$schema$fields)

    if (is_tabular_resource) {
      valid <- valid & validate_gldp_table(resource$data, resource$schema)
    } else {
      cli_h3("Check Resources {.field {resource$name}}")
      cli::cli_alert_warning("Could not check {.field {resource$name}}")
      valid <- FALSE
    }
  }

  invisible(valid)
}

#' @noRd
validate_gldp_table <- function(data, schema) {
  cli_h3("Check Resources {.field {schema$name}}")

  if (is.null(data)) {
    cli_alert_danger("data is not available.")
    return(FALSE)
  }

  schema_fields <- stats::setNames(
    schema$fields,
    sapply(schema$fields, \(x) x$name)
  )

  valid <- validate_gldp_fields_match(
    names(schema_fields),
    names(data),
    schema$fieldsMatch[[1]]
  )

  fields <- intersect(names(schema_fields), names(data))

  for (field in fields) {
    prop <- schema_fields[[field]]
    if (!is.null(prop$constraints)) {
      prop <- c(prop, prop$constraints)
      prop$constraints <- NULL # Remove the 'constraints' field after merging
    }

    field_name <- glue::glue("{schema$name}${field}")
    prop <- resolve_prop(data[[field]], prop, defs = NULL, field = field_name)

    valid <- valid & validate_gldp_item(data[[field]], prop, field_name)
  }

  if (valid) {
    cli_alert_success(
      "Table {.field {schema$name}} is consistent with the schema."
    )
  }

  invisible(valid)
}

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


#' @noRd
validate_gldp_coherence <- function(pkg) {
  cli_h3("Check Coherence")

  valid_pkg <- validate_gldp_pkg_coherence(pkg)
  valid_obs <- validate_gldp_observations(observations(pkg))
  valid <- valid_pkg & valid_obs

  if (valid) {
    cli_alert_success("Coherence checks passed.")
  } else {
    cli_alert_danger("Coherence checks failed.")
  }

  invisible(valid)
}

#' @noRd
validate_gldp_pkg_coherence <- function(pkg) {
  valid <- TRUE

  min_res_required <- c("tags", "observations", "measurements")
  res_missing <- min_res_required[
    !(min_res_required %in% sapply(pkg$resources, \(x) x$name))
  ]
  if (length(res_missing) > 0) {
    cli::cli_alert_warning(
      "{.pkg pkg} is missing {.val {res_missing}}. \\
                      We could not check package coherence."
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
      "{.field tags} is missing {.field tag_id}={.val {midmissing}} which are present in \\
    {.field measurements}."
    )
    cli_alert_info(
      "All {.field tag_id} presents in the resource {.field measurements} need to also be present \\
    in the resource {.field tags}."
    )
    valid <- FALSE
  }

  # Check for ring_number present in tags but missing from observations.
  # All birds with a ring_number in tags should have at least one corresponding observation.
  tringmissing <- unique(t$ring_number[!(t$ring_number %in% o$ring_number)])
  if (length(tringmissing) > 1) {
    cli_alert_danger(
      "{.field observations} is missing {.field ring_number} {.val {tringmissing}} which are \\
      present in {.field tags}."
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
      "The following {.field tag_id} and {.field ring_number} combinations in \\
      {.field observation} are not present in {.field tags}:"
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
      "No equipment found for {.val {tidmissingequip}} in in {.field observations} while \\
      data present in {.field measurements}."
    )
    valid <- FALSE
  }
  tidmissingret <- setdiff(
    unique(m$tag_id),
    unique(o$tag_id[o$observation_type == "retrieval"])
  )
  if (length(tidmissingret) > 0) {
    cli_alert_danger(
      "No retrieval found for {.val {tidmissingret}} in in {.field observations} while \\
      data present in {.field measurements}."
    )
    valid <- FALSE
  }

  invisible(valid)
}


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
      "{nrow(inconsistent_tag_ids)} tag_id{?s} {?is/are} associated with multiple \\
                     ring_numbers. Check: {.field {inconsistent_tag_ids$tag_id}}"
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
      "{length(error_tag)} equipment or retrieval observation{?s} {?is/are} \\
                          missing a tag_id. Check: {.field {error_tag}}"
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
      "{length(error_tag)} equipment or retrieval observation{?s} don't have a  \\
                    device status 'present'. Check: {.field {error_tag}}"
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
      "{length(error_ring_number)} {.var ring_number} with a device status {.val missing} or \\
      {.val present} {?is/are} missing a {.var tag_id}. Check: {.field {error_ring_number}}"
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
      "{length(error_ring_number)} ring{?s} where a second tag is attached without a prior \\
    retrieval or capture-missing. Check: {.field {error_ring_number}}"
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
      "{length(error_tag)} tag{?s} {?was/were} recorded without a preceding equipment event. \\
      Check: {.field {error_tag}}"
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
      "{length(error_ring_number)} invalid device_status transitions found. \\
      Check: {.field {error_ring_number}}"
    )
    valid <- FALSE
  }

  invisible(valid)
}


#' Validate GeoLocator Data Package metadata recommendations
#'
#' Internal helper function to report soft metadata recommendations for a GeoLocator
#' Data Package. These checks are advisory but included in global validation.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Always returns `TRUE` and reports metadata recommendations as messages.
#' @noRd
validate_gldp_meta <- function(pkg) {
  cli_h3("Check Metadata")

  # Summary of metadata recommendations checked below:
  # - title prefix convention
  # - contributor role completeness
  # - record_type should be "dataset"
  # - related identifiers presence
  # - community selection
  # - GitHub codeRepository URL
  # - embargo metadata quality (date, duration, justification text)
  # - allowed tag manufacturer names in tags metadata
  # - taxonomic names present in eBird taxonomy
  has_warnings <- FALSE

  # 1) Title should follow the shared "GeoLocator Data Package: " prefix.
  title <- pkg$title %||% ""

  if (!nzchar(title)) {
    cli::cli_alert_warning(
      "{.field pkg$title} is missing or empty."
    )
    has_warnings <- TRUE
  } else if (!startsWith(title, "GeoLocator Data Package: ")) {
    cli::cli_alert_warning(
      "Missing expected prefix {.val GeoLocator Data Package: } in {.field pkg$title}."
    )
    has_warnings <- TRUE
  }

  # 2) Contributors should include roles when possible.
  contributors <- pkg$contributors %||% list()
  contributors_with_role <- contributors |>
    purrr::map_lgl(\(x) {
      roles <- x$roles %||% character(0)
      any(!is.na(roles) & nzchar(as.character(roles)))
    }) |>
    sum()

  if (contributors_with_role == 0) {
    cli::cli_alert_warning(
      "Missing {.field role} in {.field pkg$contributors}."
    )
    has_warnings <- TRUE
  }

  # 3) Zenodo record type should be "dataset".
  record_type <- pkg$record_type %||% NA_character_
  is_dataset_record <- is.character(record_type) &&
    length(record_type) == 1 &&
    identical(trimws(tolower(record_type)), "dataset")
  if (!is_dataset_record) {
    cli::cli_alert_warning(
      "Expected {.field pkg$record_type} to be {.val dataset}; got {.val {record_type}}."
    )
    has_warnings <- TRUE
  }

  # 4) tags$manufacturer should use one of the expected institution names.
  allowed_manufacturers <- c(
    "Swiss Ornithological Institute",
    "Migrate Technology Limited",
    "Lund University",
    "British Antarctic Survey"
  )
  if ("tags" %in% frictionless::resources(pkg)) {
    t <- tags(pkg)
    manufacturers <- unique(t$manufacturer)
    invalid_manufacturers <- manufacturers[
      !is.na(manufacturers) &
        nzchar(trimws(manufacturers)) &
        !(manufacturers %in% allowed_manufacturers)
    ]
    if (length(invalid_manufacturers) > 0) {
      cli::cli_alert_warning(
        "Unusual {.field tags$manufacturer}: {.val {invalid_manufacturers}}. Please, try to use {.val {allowed_manufacturers}} if possible."
      )
      has_warnings <- TRUE
    }
  }

  # 5) Data-related publications/resources should be linked in related identifiers.
  related <- pkg$relatedIdentifiers %||% list()
  if (length(related) == 0) {
    cli::cli_alert_warning(
      "No {.field relatedIdentifiers} present. Add any resources related to the data (e.g. papers, project pages, derived datasets, etc.)."
    )
    has_warnings <- TRUE
  }

  # 6) At least one community should be selected for the record.
  communities <- pkg$communities %||% list()
  allowed_communities <- c(
    "b7c70316-310b-435e-9a8b-84188d60a3cc",
    "6e9c24fb-954a-4087-ae9b-71fcba41a624" # sandbox community
  )
  has_expected_community <- purrr::some(
    communities,
    \(x) x %in% allowed_communities
  )
  if (!has_expected_community) {
    cli::cli_alert_warning(
      "Missing selected community {.url https://zenodo.org/communities/geolocator-dp/}."
    )
    has_warnings <- TRUE
  }

  # 7) Software/repository URL should be a GitHub repository URL.
  code_repo <- pkg$codeRepository %||% NA_character_

  is_github_repo <- is.character(code_repo) &&
    length(code_repo) == 1 &&
    grepl(
      "^https://github\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(\\.git)?/?$",
      trimws(code_repo)
    )

  if (!is_github_repo) {
    cli::cli_alert_warning(
      "Missing or invalid {.field pkg$codeRepository}; expected a GitHub repository URL."
    )
    has_warnings <- TRUE
  }
  has_warnings <- has_warnings | validate_gldp_taxonomic(pkg)

  if (!has_warnings) {
    cli_alert_success("Metadata recommendations passed.")
  }

  invisible(has_warnings)
}

#' @noRd
validate_gldp_taxonomic <- function(pkg) {
  species <- pkg$taxonomic %||% character(0)
  species <- unique(trimws(as.character(species)))
  species <- species[!is.na(species) & nzchar(species)]
  if (length(species) == 0) {
    return(FALSE)
  }

  req <- httr2::request("https://api.ebird.org/v2/ref/taxonomy/ebird?fmt=json") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(20)

  ebird <- tryCatch(
    req |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE),
    error = function(e) {
      cli::cli_alert_warning(
        "Could not validate {.field pkg$taxonomic} against eBird taxonomy: {e$message}."
      )
      NULL
    }
  )
  if (is.null(ebird)) {
    return(TRUE)
  }

  ebird_names <- if (is.data.frame(ebird) && "sciName" %in% names(ebird)) {
    ebird$sciName
  } else {
    purrr::map_chr(ebird, \(x) x$sciName %||% NA_character_)
  }
  ebird_names <- unique(trimws(as.character(ebird_names)))
  ebird_names <- ebird_names[!is.na(ebird_names) & nzchar(ebird_names)]

  invalid <- setdiff(species, ebird_names)
  if (length(invalid) > 0) {
    cli::cli_alert_warning(
      "Scientific names in {.field pkg$taxonomic} not matching any entry in eBird taxonomy: {.val {invalid}}."
    )
    return(TRUE)
  }

  FALSE
}
