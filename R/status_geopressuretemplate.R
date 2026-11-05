#' Inspect GeoPressureTemplate files
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarize which tags are discoverable in a GeoPressureTemplate project from
#' `config.yml`, `data/interim`, and `data/raw-tag`. Use this before importing
#' a project to see which tags are available from each source and, optionally,
#' which workflow objects are present in interim files.
#'
#' During automatic discovery, names starting with `"_"` are treated as private
#' project entries and skipped. This applies to config keys, interim files,
#' raw-tag folders, and files inside raw-tag folders. Empty raw-tag folders are
#' also skipped, so `in_raw_tag = TRUE` means the folder contains at least one
#' non-ignored file.
#'
#' @param directory Path to the GeoPressureTemplate directory.
#' @param read_interim Logical. If `TRUE`, read interim `.RData` files and add
#'   columns for selected saved objects and tag status. If `FALSE`, only file
#'   discovery is performed.
#'
#' @return A [tibble::tibble()] with one row per tag and columns:
#' - `tag_id`: tag identifier;
#' - `in_config`, `in_interim`, `in_raw_tag`: whether the tag was found in each
#'   project source;
#' - when `read_interim = TRUE`, `interim_*` columns indicating which workflow
#'   objects are saved in the interim file, plus `tag_status` from
#'   [`GeoPressureR::tag_status()`] when a saved `tag` object is available.
#'
#' @examples
#' \dontrun{
#' status_geopressuretemplate()
#' status_geopressuretemplate(read_interim = FALSE)
#' }
#'
#' @export
status_geopressuretemplate <- function(directory = ".", read_interim = TRUE) {
  # Check user inputs at the project boundary.
  if (!dir.exists(directory)) {
    cli_abort(c(
      "x" = "The specified directory does not exist: {.file {directory}}."
    ))
  }

  withr::with_dir(directory, {
    # List tag IDs without loading RData files.
    project_files <- geopressuretemplate_project_files()
    tag_ids <- project_files$tag_ids

    out <- tibble::tibble(
      tag_id = tag_ids,
      in_config = tag_ids %in% project_files$config_ids,
      in_interim = tag_ids %in% names(project_files$interim_files),
      in_raw_tag = tag_ids %in% names(project_files$raw_tag_files)
    )

    if (read_interim) {
      # Loading interim files is optional because it can be slow on large projects.
      interim_status <- if (length(tag_ids) == 0) {
        tibble::tibble(
          interim_tag = logical(),
          interim_param = logical(),
          interim_marginal = logical(),
          interim_path_most_likely = logical(),
          interim_path_simulation = logical(),
          interim_pressurepath_most_likely = logical(),
          tag_status = character()
        )
      } else {
        purrr::map_dfr(tag_ids, \(tag_id) {
          file <- unname(project_files$interim_files[tag_id])
          saved <- character()
          tag_status <- NA_character_

          if (!is.na(file)) {
            env <- new.env(parent = emptyenv())
            saved <- load(file, envir = env)

            if ("tag" %in% saved) {
              status <- GeoPressureR::tag_status(get("tag", envir = env, inherits = FALSE))
              tag_status <- if (length(status) == 0) {
                NA_character_
              } else {
                # tag_status() repeats "read" for each sensor.
                as.character(glue::glue_collapse(unique(status), sep = ", "))
              }
            }
          }

          tibble::tibble(
            interim_tag = "tag" %in% saved,
            interim_param = "param" %in% saved,
            interim_marginal = "marginal" %in% saved,
            interim_path_most_likely = "path_most_likely" %in% saved,
            interim_path_simulation = "path_simulation" %in% saved,
            interim_pressurepath_most_likely = "pressurepath_most_likely" %in% saved,
            tag_status = tag_status
          )
        })
      }
      out <- dplyr::bind_cols(
        out,
        interim_status
      )
    }

    out
  })
}

#' @noRd
geopressuretemplate_config_ids <- function(file = "config.yml") {
  # Config IDs are top-level YAML keys except defaults and private keys.
  if (!file.exists(file)) {
    return(character())
  }

  ids <- names(yaml::yaml.load_file(file, eval.expr = FALSE))
  ids <- setdiff(ids, "default")
  ids[!startsWith(ids, "_")]
}

#' @noRd
geopressuretemplate_project_files <- function() {
  # Shared project inventory for status and read_geopressuretemplate().
  config_ids <- geopressuretemplate_config_ids("config.yml")

  interim_files <- character()
  if (dir.exists("./data/interim")) {
    interim_files <- list.files("./data/interim", pattern = "\\.[Rr][Dd]ata$", full.names = TRUE)
    interim_files <- interim_files[!startsWith(basename(interim_files), "_")]
    names(interim_files) <- tools::file_path_sans_ext(basename(interim_files))
  }

  raw_tag_ids <- character()
  raw_tag_files <- list()
  if (dir.exists("./data/raw-tag")) {
    raw_tag_ids <- list.dirs("./data/raw-tag", recursive = FALSE, full.names = FALSE)
    raw_tag_ids <- raw_tag_ids[!startsWith(raw_tag_ids, "_")]
    raw_tag_files <- purrr::map(
      raw_tag_ids,
      \(id) {
        files <- list.files(file.path("./data/raw-tag", id), full.names = TRUE)
        files[!startsWith(basename(files), "_")]
      }
    )
    names(raw_tag_files) <- raw_tag_ids

    empty_raw_tag_ids <- names(raw_tag_files)[lengths(raw_tag_files) == 0]
    if (length(empty_raw_tag_ids) > 0) {
      cli_warn(c(
        "!" = "Ignoring empty raw-tag folder{?s}: {.file {empty_raw_tag_ids}}."
      ))
      raw_tag_files <- raw_tag_files[lengths(raw_tag_files) > 0]
    }
  }

  list(
    config_ids = config_ids,
    interim_files = interim_files,
    raw_tag_files = raw_tag_files,
    tag_ids = sort(unique(c(
      config_ids,
      names(interim_files),
      names(raw_tag_files)
    )))
  )
}
