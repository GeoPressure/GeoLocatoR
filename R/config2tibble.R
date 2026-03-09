#' Convert GeoPressureTemplate config.yml to tibble
#'
#' @description
#' Reads a GeoPressureTemplate `config.yml` file and converts it into a tibble format with
#' one row per tag and columns for each configuration parameter. This is useful for comparing
#' configurations across multiple tags or for programmatic analysis of tag settings.
#'
#' @param file Character string specifying the path to the `config.yml` file.
#'   Defaults to `"config.yml"` in the current directory, or the value of the
#'   `R_CONFIG_FILE` environment variable if set.
#' @param filter_return Logical. If `TRUE` (default), only columns that vary across tags
#'   are returned. If `FALSE`, all configuration columns are included even if they have
#'   the same value for all tags.
#'
#' @return A [tibble::tibble()] with one row per tag ID and columns for each configuration
#'   parameter. The exact columns returned depend on the configuration file structure and
#'   the `filter_return` parameter.
#'
#' @details
#' This function uses [`GeoPressureR::geopressuretemplate_config()`] internally to parse
#' each tag's configuration. It then flattens the nested list structure and applies
#' appropriate type conversions for each parameter (e.g., converting dates to strings,
#' functions to their deparsed representation).
#'
#' The function handles various GeoPressureR configuration parameters including:
#' - Tag creation settings (manufacturer, file paths, crop dates)
#' - Labeling parameters (flight/stap duration warnings)
#' - Map settings (extent, known locations, pressure/light thresholds)
#' - Graph creation and movement model parameters
#' - Bird characteristics (mass, wing measurements)
#' - Wind and environmental data settings
#'
#' @examples
#' \dontrun{
#' # Read config from default location
#' cfg <- config2tibble()
#'
#' # Read from specific file
#' cfg <- config2tibble("path/to/config.yml")
#'
#' # Include all columns even if they don't vary
#' cfg_full <- config2tibble(filter_return = FALSE)
#'
#' # View which parameters differ across tags
#' View(cfg)
#' }
#'
#' @seealso
#' - [`GeoPressureR::geopressuretemplate_config()`] for the underlying config parser
#' - [GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate) for the
#'   template structure
#'
#' @export
config2tibble <- function(
  file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
  filter_return = TRUE
) {
  parse_fun <- \(x) {
    if (is.null(x) || (length(x) == 1 && is.na(x))) {
      return(NA_character_)
    }
    if (is.function(x) || is.call(x)) {
      x <- deparse(x)
    }
    paste(x, collapse = ", ")
  }

  col_transforms <- list(
    "id" = as.character,
    "tag_create.manufacturer" = as.character, # NULL -> keep flexible character
    "tag_create.directory" = parse_fun,
    "tag_create.pressure_file" = as.character,
    "tag_create.light_file" = as.character,
    "tag_create.acceleration_file" = as.character,
    "tag_create.temperature_external_file" = as.character,
    "tag_create.temperature_internal_file" = as.character,
    "tag_create.magnetic_file" = as.character,
    "tag_create.assert_pressure" = as.logical,
    "tag_create.crop_start" = as.character, # date stored as string
    "tag_create.crop_end" = as.character, # datetime stored as string
    "tag_label.file" = parse_fun,
    "tag_label.warning_flight_duration" = as.numeric,
    "tag_label.warning_stap_duration" = as.numeric,
    "tag_set_map.scale" = as.numeric,
    "tag_set_map.include_stap_id" = as.character, # flexible
    "tag_set_map.include_min_duration" = as.numeric,
    "tag_set_map.extent" = \(x) list(x),
    "tag_set_map.known" = \(x) {
      paste0("data.frame(", paste(names(x), collapse = ","), ")")
    },
    "geopressure_map.compute_known" = as.logical,
    "geopressure_map.max_sample" = as.numeric,
    "geopressure_map.margin" = as.numeric,
    "geopressure_map.sd" = as.numeric,
    "geopressure_map.thr_mask" = as.numeric,
    "geopressure_map.log_linear_pooling_weight" = parse_fun,
    "twilight_create.twl_thr" = as.numeric,
    "twilight_create.transform_light" = as.logical,
    "twilight_create.twl_offset" = as.numeric,
    "twilight_label_read.file" = parse_fun,
    "geolight_map.compute_known" = as.logical,
    "geolight_map.twl_calib_adjust" = as.numeric,
    "geolight_map.twl_llp" = parse_fun,
    "graph_create.likelihood" = as.character,
    "graph_create.thr_likelihood" = as.numeric,
    "graph_create.thr_gs" = as.numeric,
    "graph_set_movement.method" = parse_fun,
    "graph_set_movement.shape" = as.numeric,
    "graph_set_movement.scale" = as.numeric,
    "graph_set_movement.location" = as.numeric,
    "graph_set_movement.power2prob" = parse_fun,
    "graph_set_movement.low_speed_fix" = as.numeric,
    "graph_set_movement.type" = parse_fun,
    "graph_set_movement.zero_speed_ratio" = as.numeric,
    "bird_create.mass" = as.numeric,
    "bird_create.wing_span" = as.numeric,
    "bird_create.wing_aspect" = as.numeric,
    "bird_create.wing_area" = as.numeric,
    "bird_create.body_frontal_area" = as.numeric,
    "bird_create.scientific_name" = as.character,
    "graph_add_wind.file" = parse_fun,
    "graph_add_wind.variable" = parse_fun,
    "graph_add_wind.rounding_interval" = as.numeric,
    "graph_add_wind.interp_spatial_linear" = as.logical,
    "graph_add_wind.thr_as" = as.numeric,
    "graph_simulation.nj" = as.numeric,
    "pressurepath_create.solar_dep" = as.numeric,
    "pressurepath_create.era5_dataset" = as.character,
    "pressurepath_create.variable" = parse_fun,
    "GeoPressureR_version" = as.character,
    "geopressuretemplate.likelihood" = \(x) paste(x, collapse = ", "),
    "geopressuretemplate.outputs" = \(x) paste(x, collapse = ", "),
    "geopressuretemplate.pressurepath" = \(x) paste(x, collapse = ", "),
    "tag_comments" = as.character
  )

  list_id <- utils::tail(
    names(yaml::yaml.load_file(file, eval.expr = FALSE)),
    -1
  )

  cfg <- purrr::map_df(list_id, \(id) {
    c <- GeoPressureR::geopressuretemplate_config(
      id,
      config = file,
      assert_tag = FALSE,
      assert_graph = FALSE
    )

    class(c) <- NULL

    c <- purrr::list_flatten(
      c,
      name_spec = "{outer}.{inner}",
      name_repair = "minimal"
    )

    c <- purrr::imap(
      c,
      ~ {
        v <- .x
        nm <- .y

        if (is.null(v)) {
          v <- NA
        }

        if (nm %in% names(col_transforms)) {
          col_transforms[[nm]](v)
        } else if (is.function(v) || is.call(v)) {
          parse_fun(v)
        } else {
          v
        }
      }
    )

    # detect problematic elements
    idx <- lengths(c) > 1

    if (any(idx)) {
      cli::cli_warn(
        "The following fields had length > 1 and were truncated: {paste(names(c)[idx], collapse = ', ')}"
      )

      # correction: keep only first element
      c[idx] <- lapply(c[idx], `[`, 1)
    }

    as_tibble(c)
  })

  if (filter_return) {
    # filter for column that dont contain the same value
    cfg <- cfg %>%
      select(where(~ n_distinct(.) > 1))
  }
  cfg
}
