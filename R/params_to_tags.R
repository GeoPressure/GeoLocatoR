#' Generate `tags` from GeoPressureR parameters
#'
#' @description
#' Convert a list of GeoPressureR parameter objects into a `tags` tibble
#' following the GeoLocator Data Package structure.
#'
#' @param params A list of GeoPressureR parameter objects. These parameters should have been
#' generated during the GeoPressure workflow. See [`GeoPressureR::param_create()`
#' ](https://geopressure.org/GeoPressureR/reference/param_create.html) for more information.
#'
#' @return A [tibble::tibble()] data frame with columns `tag_id` and `ring_number` (always
#' missing), plus `manufacturer`, `scientific_name`, `model` and `firmware` when available.
#'
#' @details
#' Manufacturer names are normalised for common GeoPressure sources. Model and
#' firmware are read from `param$tag_create$settings` (GeoPressureR >= 3.8.0),
#' else from the older `param$soi_settings` and `param$migratec_model`.
#'
#' @seealso [params_to_observations()] and [tags_to_measurements()] for related
#'   conversions from GeoPressureR objects to GeoLocator-DP resources.
#'
#' @export
params_to_tags <- function(params) {
  if (length(params) == 0) {
    return(tibble::tibble(
      tag_id = character(),
      manufacturer = character(),
      scientific_name = character(),
      ring_number = character(),
      model = character(),
      firmware = character()
    ))
  }

  tag_list <- list()
  for (i in seq_along(params)) {
    param <- params[[i]]
    t <- tryCatch(
      {
        tibble::tibble(
          tag_id = param$id,
          manufacturer = param$tag_create$manufacturer,
          scientific_name = param$bird_create$scientific_name %||%
            param$graph_set_movement$bird$scientific_name,
          ring_number = NA_character_
        )
      },
      error = function(e) {
        cli_abort(
          c(
            glue::glue(
              "Error in {{.code params_to_tags}} for param {{.val {i}}}:"
            ),
            "x" = e$message
          )
        )
      }
    )

    # Standardize manufacturer names (only if manufacturer column exists)
    if ("manufacturer" %in% names(t)) {
      t <- dplyr::mutate(
        t,
        manufacturer = case_when(
          .data$manufacturer == "soi" ~ "Swiss Ornithological Institute",
          .data$manufacturer == "migratetech" ~ "Migrate Technology Limited",
          .data$manufacturer == "lund" ~ "Lund University",
          .data$manufacturer == "bas" ~ "British Antarctic Survey",
          .data$manufacturer == "prestag" ~ "PresTag",
          .default = .data$manufacturer
        )
      )
    }

    # GeoPressureR >= 3.8.0 stores model and firmware in tag_create$settings; older
    # versions in soi_settings (HWVersion/SWVersion in older SOI files) or migratec_model.
    settings <- param$tag_create$settings
    soi <- param$soi_settings
    model <- settings$model %||% soi[["HW Version"]] %||% soi$HWVersion %||% param$migratec_model
    firmware <- settings$firmware %||% soi[["FW Version"]] %||% soi$SWVersion
    if (!is.null(model)) {
      t$model <- model
    }
    if (!is.null(firmware)) {
      t$firmware <- firmware
    }

    tag_list[[i]] <- t
  }

  # Combine all tags, with error reporting for type mismatches
  tags_df <- tryCatch(
    {
      dplyr::bind_rows(tag_list)
    },
    error = function(e) {
      col_types <- sapply(tag_list, function(x) {
        paste(sapply(x, class), collapse = ", ")
      })
      cli_abort(
        c(
          "Type mismatch when combining tags in {.code params_to_tags}:",
          "i" = glue::glue(
            "Column types: {glue::glue_collapse(col_types, sep = ' | ')}"
          ),
          "x" = e$message
        )
      )
    }
  )
  tags_df
}
