#' Generate `observations` from GeoPressureR parameters
#'
#' @description
#' Convert a list of GeoPressureR parameter objects into a minimal
#' `observations` tibble following the GeoLocator Data Package structure.
#'
#' One `equipment` row and one `retrieval` row are generated per parameter
#' object. When available, crop dates are used as observation datetimes and
#' known stationary locations are used to populate the first and last locations.
#'
#' @param params A list of GeoPressureR parameter objects. These parameters should have been
#' generated during the GeoPressure workflow. See [`GeoPressureR::param_create()`
#' ](https://geopressure.org/GeoPressureR/reference/param_create.html) for more information.
#'
#' @return A [tibble::tibble()] with one equipment and one retrieval observation
#'   per parameter object.
#'
#' @seealso [params_to_tags()] and [tags_to_measurements()] for related
#'   conversions from GeoPressureR objects to GeoLocator-DP resources.
#'
#' @export
params_to_observations <- function(params) {
  params |>
    purrr::map(\(param) {
      o0 <- tibble::tibble(
        ring_number = NA_character_,
        tag_id = param$id,
        datetime = as.POSIXct(NA, tz = "UTC"),
        location_name = NA_character_,
        longitude = NA_real_,
        latitude = NA_real_,
        age_class = "0",
        sex = "U",
        observation_comments = "Automatically computed with `params_to_observations()`"
      )

      if (!is.null(param$tag_create$crop_start)) {
        oe <- o0 |>
          mutate(
            datetime = as.POSIXct(param$tag_create$crop_start, tz = "UTC"),
            observation_type = "equipment"
          )
      } else {
        oe <- o0 |>
          mutate(
            observation_type = "equipment"
          )
      }

      if (!is.null(param$tag_create$crop_end)) {
        or <- o0 |>
          mutate(
            datetime = as.POSIXct(param$tag_create$crop_end, tz = "UTC"),
            observation_type = "retrieval"
          )
      } else {
        or <- o0 |>
          mutate(
            observation_type = "retrieval"
          )
      }

      if ("known" %in% names(param$tag_set_map)) {
        known <- param$tag_set_map$known

        id <- known$stap_id == 1
        if (any(id)) {
          oe <- oe |>
            mutate(
              longitude = known$known_lon[id],
              latitude = known$known_lat[id],
              observation_comments = glue::glue(
                "Automatically computed from `known$stap_id==1` with `params_to_observations()`"
              )
            )
        }

        id <- known$stap_id == -1
        if (any(id)) {
          or <- or |>
            mutate(
              longitude = known$known_lon[id],
              latitude = known$known_lat[id],
              observation_comments = glue::glue(
                "Automatically computed from `known$stap_id==-1` with `params_to_observations()`"
              )
            )
        }
      }

      o <- bind_rows(oe, or)

      o
    }) |>
    purrr::list_rbind()
}
