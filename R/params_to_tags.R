#' Generate `tags` from GeoPressureR parameters
#'
#' This function converts a list of parameters into a single `tags` tibble according to the
#' [GeoLocator Data Package specification](https://geopressure.org/GeoLocator-DP/core/tags/).
#'
#' @param params A list of GeoPressureR parameter objects. These parameters should have been
#' generated during the GeoPressure workflow. See [`GeoPressureR::param_create()`
#' ](https://geopressure.org/GeoPressureR/reference/param_create.html) for more information.
#'
#' @return A [tibble::tibble()] data frame with columns `tag_id`, `manufacturer`, `scientific_name`,
#' `ring_number`, `model`, and `firmware` (if `soi_settings` is present).
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
          scientific_name = param$bird_create$scientific_name,
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
          .data$manufacturer == "migratetech" ~ "Migrate Technology",
          .data$manufacturer == "lund" ~ "Lund CAnMove",
          .default = .data$manufacturer
        )
      )
    }

    # Add model/firmware if soi_settings present
    if ("soi_settings" %in% names(param)) {
      s <- param$soi_settings
      t <- tryCatch(
        {
          dplyr::mutate(t, model = s$`HW Version`, firmware = s$`FW Version`)
        },
        error = function(e) {
          cli_abort(
            c(
              glue::glue(
                "Error in {{.code params_to_tags}} for param {{.val {i}}} (soi_settings):"
              ),
              "x" = e$message
            )
          )
        }
      )
    }

    # Add model if migratec_model present
    if ("migratec_model" %in% names(param)) {
      t <- tryCatch(
        {
          dplyr::mutate(t, model = param$migratec_model)
        },
        error = function(e) {
          cli_abort(
            c(
              glue::glue(
                "Error in {{.code params_to_tags}} for param {{.val {i}}} (migratec_model):"
              ),
              "x" = e$message
            )
          )
        }
      )
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
