#' Validate GeoLocator Data Package profile
#'
#' Internal helper that validates package-level metadata against the GeoLocator-DP
#' profile schema. Resource descriptors are excluded and not checked; resource data
#' are validated by `validate_gldp_resources()`.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Invisibly returns `TRUE` when the package-level profile validation passes,
#'   `FALSE` otherwise.
#' @noRd
validate_gldp_profile <- function(pkg) {
  cli_h3("Check Profile")
  version <- gldp_version(pkg)

  pkg_schema <- gldp_profile_schema(version)

  required <- unlist(pkg_schema$allOf[[2]]$required)
  properties <- pkg_schema$allOf[[2]]$properties

  # The profile's rules on resource descriptors are not checked; resource data are
  # validated by validate_gldp_resources().
  required <- setdiff(required, "resources")
  properties$resources <- NULL

  # Ignore all additional fields not present in the schema
  ignore_fields <- setdiff(names(pkg), names(properties))

  valid <- validate_gldp_object(
    pkg,
    required,
    properties,
    defs = pkg_schema$`$defs`,
    ignore_fields = ignore_fields
  )

  if (valid) {
    cli_alert_success("Profile checks passed.")
  }

  invisible(valid)
}

#' Validate object against schema properties
#'
#' Internal helper function to validate an object against a set of required fields
#' and property definitions from a schema. It resolves local schema references,
#' validates scalar fields, and recurses into object and array properties when
#' nested schemas are available.
#'
#' @param obj The object to validate
#' @param required Vector of required field names
#' @param properties List of property definitions from schema
#' @param name Optional name prefix for error messages
#' @param defs Optional schema definitions used to resolve local `"$ref"`
#'   entries.
#' @param ignore_fields Field names to skip during validation.
#' @return Invisibly returns `TRUE` when the object validation passes, `FALSE`
#'   otherwise.
#' @noRd
validate_gldp_object <- function(
  obj,
  required,
  properties,
  name = "",
  defs = NULL,
  ignore_fields = c("directory")
) {
  name <- glue::glue("{name}{if (name == '') '' else '$'}")

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
          # External schemas rely on anyOf, allOf etc., which are not implemented.
          cli::cli_alert_warning(
            "{.field {field}} cannot be validated (external schema)."
          )
        } else if (!is.null(prop$properties)) {
          valid <- valid &
            validate_gldp_object(
              obj[[field]],
              prop$required,
              prop$properties,
              field_name,
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
  valid
}

#' Resolve a local schema reference
#'
#' Internal helper that replaces a local `"$ref"` pointing to `#/$defs/...`
#' with the corresponding schema definition, then merges any field-specific
#' overrides.
#'
#' @param prop A schema property definition.
#' @param defs Schema definitions used to resolve local references.
#'
#' @return A schema property with the local reference resolved when possible.
#' @noRd
resolve_local_ref <- function(prop, defs) {
  if (is.null(prop$`$ref`) || is.null(defs)) {
    return(prop)
  }

  ref <- prop$`$ref`
  if (!startsWith(ref, "#/$defs/")) {
    return(prop)
  }

  key <- sub("^#/\\$defs/", "", ref)
  if (is.null(defs[[key]])) {
    return(prop)
  }

  base <- defs[[key]]
  prop_no_ref <- prop
  prop_no_ref$`$ref` <- NULL

  utils::modifyList(base, prop_no_ref)
}

#' Infer a schema property type
#'
#' Internal helper that fills in an implicit schema type for object and array
#' properties when `type` is omitted but structural fields are present.
#'
#' @param prop A schema property definition.
#'
#' @return The schema property with `type` added when it can be inferred.
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

#' Resolve a `oneOf` schema branch
#'
#' Internal helper that selects the first `oneOf` alternative whose type matches
#' the current value, then merges it with the shared base property definition.
#'
#' @param value The value being validated.
#' @param prop A schema property definition containing `oneOf`.
#' @param defs Schema definitions used to resolve local references.
#' @param field Field name used in validation messages.
#'
#' @return A concrete schema property to use for validation.
#' @noRd
resolve_oneof <- function(value, prop, defs, field) {
  if (is.null(prop$oneOf)) {
    return(prop)
  }

  base <- prop
  base$oneOf <- NULL
  base <- resolve_local_ref(base, defs)

  alternatives <- purrr::map(prop$oneOf, function(alt) {
    alt <- resolve_local_ref(alt, defs)
    infer_type(alt)
  })

  matches <- purrr::map_lgl(
    alternatives,
    function(alt) check_type_silent(value, alt$type)
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

#' Resolve a schema property before validation
#'
#' Internal helper that resolves local references, selects a `oneOf` branch when
#' needed, and infers a missing `type`.
#'
#' @param value The value being validated.
#' @param prop A schema property definition.
#' @param defs Schema definitions used to resolve local references.
#' @param field Field name used in validation messages.
#'
#' @return A schema property ready to be passed to the validation helpers.
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
