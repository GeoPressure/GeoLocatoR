#' Add Geolocator DP resources from a GeoPressureTemplate
#'
#' @description
#' Read a GeoPressureTemplate project and populate a GeoLocator Data Package
#' with the resources that can be derived from it.
#'
#' The import can combine two sources:
#' 1. `"interim"`: read `.RData` outputs from `data/interim/`, convert them to
#'    package resources, and store raw parameter objects in `pkg$params`.
#' 2. `"raw-tag"`: create tag objects from `data/raw-tag/` and `config.yml`,
#'    then derive `measurements`, `tags`, `observations`, and `params`.
#'
#' Manual data files take precedence over generated metadata:
#' - `data/tags.csv` or `data/tags.xlsx` replaces generated `tags`;
#' - `data/observations.csv` or `data/observations.xlsx` replaces generated
#'   `observations`.
#'
#' Files or directories whose names start with `"_"` are ignored.
#'
#' See the
#' [GeoPressureManual](https://geopressure.org/GeoPressureManual/geolocator-create.html)
#' for a full workflow example.
#'
#' @param directory Path to the GeoPressureTemplate directory.
#' @param from Character vector specifying which sources to import. Supported
#'   values are `"raw-tag"` and/or `"interim"`.
#' @param pkg Optional GeoLocator Data Package object to update. Defaults to a
#'   new package created with [create_gldp()].
#'
#' @return The updated `geolocatordp` object.
#'
#' @seealso [create_geopressuretemplate()] to create a project from a package,
#'   [params_to_tags()], [params_to_observations()], and
#'   [tags_to_measurements()] for the underlying conversions.
#' @export
read_geopressuretemplate <- function(
  directory = ".",
  from = c("raw-tag", "interim"),
  pkg = create_gldp()
) {
  # Check input
  check_gldp(pkg)

  # Check if the directory exists
  if (!dir.exists(directory)) {
    cli_abort(c(
      "x" = "The specified directory does not exist: {.file {directory}}."
    ))
  }
  if (!any(from %in% c("interim", "raw-tag"))) {
    cli_abort(c(
      "x" = "{.arg from} must contain at least one of {.val {'interim'}} or {.val {'raw-tag'}}."
    ))
  }

  # pkg has already data
  if (length(frictionless::resources(pkg)) > 0) {
    cli_bullets(
      c(
        "!" = "The {.pkg pkg} has already resources
        {.field {purrr::map_vec(pkg$resources, ~.x$name)}}."
      )
    )
    res <- utils::askYesNo(
      "Do you want to continue and overwrite the existing resources?",
      default = FALSE,
      yes = "Yes, overwrite",
      no = "No, keep existing resources"
    )
    if (is.na(res) || !res) {
      return(pkg)
    }
    for (r in frictionless::resources(pkg)) {
      pkg <- frictionless::remove_resource(pkg, r)
    }
  }

  # Initiate empty resources to be able to merge interim and raw-tag as necessary
  t <- NULL
  o <- NULL
  m <- NULL
  params <- list()

  # Change working directory to the specified directory so that GeoPressureR can work with default
  # value: setwd(directory)
  pkg <- withr::with_dir(directory, {
    # STEP 1: Read all interim file available
    if ("interim" %in% from) {
      all_files <- list.files(
        path = "./data/interim/",
        pattern = "\\.RData$",
        full.names = TRUE
      )
      # Exclude folder starting with _
      all_files <- all_files[!grepl("^_", basename(all_files))]

      # List of variable names to be processed
      var_names_required <- c("tag", "param")
      var_names_path <- c(
        "path_simulation",
        "path_tag",
        "path_most_likely",
        "path_geopressureviz"
      )
      var_names_edges <- c(
        "edge_most_likely",
        "edge_simulation",
        "edge_geopressureviz",
        "edge_tag"
      )
      var_names_pressurepath <- c(
        "pressurepath_most_likely",
        "pressurepath_geopressureviz",
        "pressurepath_geopressureviz"
      )
      var_names <- c(
        var_names_required,
        var_names_path,
        var_names_edges,
        var_names_pressurepath
      )

      # Initialize lists dynamically
      interim <- stats::setNames(vector("list", length(var_names)), var_names)
      interim <- lapply(interim, function(x) vector("list", length(all_files)))

      # Loop through files and populate the result lists
      all_files |>
        purrr::iwalk(
          \(f, idx) {
            save_list <- load(f)
            for (var in var_names) {
              if (var %in% save_list) {
                interim[[var]][[idx]] <<- get(var)
              }
            }
          },
          .progress = list(
            type = "custom",
            format = "{cli::pb_spin} Reading {cli::pb_current}/{cli::pb_total} interim tag{?s}.",
            format_done = "{cli::col_green(cli::symbol$tick)} Read {.val {cli::pb_total}} interim \\
            tag{?s}.",
            clear = FALSE
          )
        )

      # Check for required variables
      for (var in var_names_required) {
        is_null_var <- sapply(interim[[var]], is.null)
        if (any(is_null_var)) {
          cli_abort(c(
            "x" = "File {.file {basename(all_files)[is_null_var]}} missing variable {.var {var}}."
          ))
        }
      }

      # Crete tag resource
      t <- params_to_tags(interim$param)

      # Create observations resource
      o <- params_to_observations(interim$param)

      # Create measurements resource
      m <- tags_to_measurements(interim$tag)

      # Collect GeoPressureR param objects
      params <- c(params, interim$param)

      # Add twilights
      twl <- interim$tag |>
        purrr::map(function(tag) {
          if ("twilight" %in% names(tag)) {
            tag$twilight |>
              mutate(tag_id = tag$param$id)
          } else {
            NULL
          }
        }) |>
        purrr::compact() |> # Remove NULLs
        purrr::list_rbind()

      if (nrow(twl) > 0) {
        pkg <- add_gldp_resource(pkg, "twilights", twl)
      }

      # Add stap
      staps <- interim$tag |>
        purrr::map(function(tag) {
          if ("stap" %in% names(tag)) {
            tag$stap |>
              mutate(tag_id = tag$param$id)
          } else {
            NULL
          }
        }) |>
        purrr::compact() |> # Remove NULLs
        purrr::list_rbind() |>
        select(-any_of(c("duration", "nb_sample")))

      if (nrow(staps) > 0) {
        pkg <- add_gldp_resource(pkg, "staps", staps)
      }

      # Add Path
      paths <- var_names_path |>
        purrr::map(\(x) {
          interim[[x]] |>
            purrr::imap(function(p, i) {
              if (!is.null(p)) {
                p |>
                  select(-any_of(c("start", "end", "include"))) |>
                  mutate(
                    type = sub("path_", "", x, fixed = TRUE),
                    tag_id = interim$tag[[i]]$param$id
                  )
              }
            }) |>
            purrr::list_rbind()
        }) |>
        purrr::list_rbind() |>
        tibble::tibble()

      if (nrow(paths) > 0) {
        pkg <- add_gldp_resource(pkg, "paths", paths)
      }

      # Add Edge
      edges <- var_names_edges |>
        purrr::map(\(x) {
          interim[[x]] |>
            purrr::imap(function(e, i) {
              if (!is.null(e)) {
                e |>
                  mutate(
                    type = sub("edge_", "", x, fixed = TRUE),
                    tag_id = interim$tag[[i]]$param$id
                  )
              }
            }) |>
            purrr::list_rbind()
        }) |>
        purrr::list_rbind() |>
        tibble::tibble()

      if ("gs" %in% names(edges)) {
        edges <- edges |>
          mutate(
            gs_u = Re(.data$gs),
            gs_v = Im(.data$gs),
          ) |>
          select(-c("gs"))
      }

      if ("ws" %in% names(edges)) {
        edges <- edges |>
          mutate(
            ws_u = Re(.data$ws),
            ws_v = Im(.data$ws),
          ) |>
          select(-c("ws"))
      }

      if (nrow(edges) > 0) {
        pkg <- add_gldp_resource(pkg, "edges", edges)
      }

      # Add Pressurepath
      pressurepaths <- var_names_pressurepath |>
        purrr::map(\(x) {
          interim[[x]] |>
            purrr::imap(function(p, i) {
              if (!is.null(p)) {
                p |>
                  mutate(
                    type = sub("pressurepath_", "", x, fixed = TRUE),
                    tag_id = interim$tag[[i]]$param$id,
                    datetime = as.POSIXct(date, tz = "UTC")
                  ) |>
                  select(-any_of("date"))
              }
            }) |>
            purrr::list_rbind()
        }) |>
        purrr::list_rbind() |>
        tibble::tibble() |>
        select(-any_of(c("known", "include")))

      if (nrow(pressurepaths) > 0) {
        pkg <- add_gldp_resource(pkg, "pressurepaths", pressurepaths)
      }
    }

    # STEP 2: Read raw tag data for the file not in interim
    if ("raw-tag" %in% from) {
      # Read tag data
      all_dirs <- list.dirs(path = "data/raw-tag", recursive = FALSE)
      # Exclude folder starting with _
      all_dirs <- all_dirs[!grepl("^_", basename(all_dirs))]
      # Get the list of tag_id
      list_id <- basename(all_dirs)

      # Remove tag_id already present in t
      list_id <- list_id[!(list_id %in% t$tag_id)]

      if (length(list_id) > 0) {
        # Read raw tag data with raw_tag_id_to_tag function
        dtags <- list_id |>
          purrr::map(
            purrr::possibly(raw_tag_id_to_tag, NULL),
            .progress = list(
              type = "custom",
              format = "{cli::pb_spin} Reading {cli::pb_current}/{cli::pb_total} raw tag{?s}.",
              format_done = "{cli::col_green(cli::symbol$tick)} Read {.val {cli::pb_total}} raw \\
            tag{?s}.",
              clear = FALSE
            )
          )

        # Check for failed tag reads and warn
        failed_tags <- list_id[sapply(dtags, is.null)]
        if (length(failed_tags) > 0) {
          cli_warn(c(
            "!" = "Failed to read {.val {length(failed_tags)}} {?tag/tags}:",
            "i" = "{.field {failed_tags}}",
            ">" = "These tags will be skipped."
          ))
        }

        # Remove NULL entries
        dtags <- purrr::compact(dtags)

        # Collect GeoPressureR param objects
        params <- c(params, purrr::map(dtags, ~ .x$param))

        # Adding measurements resource
        m <- bind_rows(m, tags_to_measurements(dtags))

        # Adding tag resource
        t <- bind_rows(
          t,
          dtags |>
            purrr::map(~ .x$param) |>
            params_to_tags()
        )

        # Adding observations resource
        o <- bind_rows(
          o,
          dtags |>
            purrr::map(~ .x$param) |>
            params_to_observations()
        )
      }
    }

    # STEP 3: Overwrite tags and observations if csv/xlsx files present
    if (file.exists("data/tags.xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        cli_abort("The {.pkg readxl} package is required to read {.file data/tags.xlsx}.")
      }
      file <- "data/tags.xlsx"
      tf <- readxl::read_excel(
        file,
        col_types = c(
          tag_id = "text",
          ring_number = "text",
          scientific_name = "text",
          manufacturer = "text",
          model = "text",
          firmware = "text",
          weight = "numeric",
          attachment_type = "text",
          readout_method = "text",
          tag_comments = "text"
        )
      )
      cli_alert_success(
        "Reading tags from {.file {file.path(directory, file)}}."
      )
    } else if (file.exists("data/tags.csv")) {
      file <- "data/tags.csv"
      tf <- readr::read_delim(
        file,
        col_types = readr::cols(
          tag_id = "c",
          ring_number = "c",
          scientific_name = "c",
          manufacturer = "c",
          model = "c",
          firmware = "c",
          weight = "d",
          attachment_type = "c",
          readout_method = "c",
          tag_comments = "c"
        )
      )
      cli_alert_success(
        "Reading tags from {.file {file.path(directory, file)}}."
      )
    } else {
      tf <- t
    }

    # Check that all tag_id are in tf
    if (!all(t$tag_id %in% tf$tag_id)) {
      missing_tag_ids <- setdiff(t$tag_id, tf$tag_id)
      cli_warn(c(
        "!" = "The following tag_id from interim/raw data are missing in {.file {file}}: \\
        {missing_tag_ids}",
        "i" = "We will proceed with a merge of the two.",
        ">" = "Please, fix {.file {file}}"
      ))
      t <- bind_rows(
        filter(t, !(.data$tag_id %in% tf$tag_id)),
        filter(tf, .data$tag_id %in% t$tag_id)
      )
    } else {
      t <- tf
    }

    if (file.exists("./data/observations.xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        cli_abort(
          "The {.pkg readxl} package is required to read {.file data/observations.xlsx}."
        )
      }
      o <- readxl::read_excel(
        "./data/observations.xlsx",
        col_types = c(
          ring_number = "text", # ring_number: character
          tag_id = "text", # tag_id: character
          observation_type = "text", # observation_type: character
          datetime = "date", # datetime: date-time (ISO 8601 format)
          latitude = "numeric", # latitude: numeric
          longitude = "numeric", # longitude: numeric
          location_name = "text", # location_name: character
          device_status = "text", # device_status: character
          observer = "text", # observer: character
          catching_method = "text", # catching_method: character
          age_class = "text", # age_class: character
          sex = "text", # sex: character
          condition = "text", # condition: character
          mass = "numeric", # mass: numeric
          wing_length = "numeric", # wing_length: numeric
          additional_metric = "text", # additional_metric: character
          observation_comments = "text" # observation_comments: character
        )
      )
      cli_alert_success(
        "Reading observations from {.file {file.path(directory, 'data/observations.xlsx')}}."
      )
    } else if (file.exists("./data/observations.csv")) {
      o <- readr::read_delim(
        "./data/observations.csv",
        col_types = readr::cols(
          ring_number = "c",
          tag_id = "c",
          observation_type = "c",
          datetime = "T", # ISO 8601 format
          latitude = "d",
          longitude = "d",
          location_name = "c",
          device_status = "c",
          observer = "c",
          catching_method = "c",
          age_class = "c",
          sex = "c",
          condition = "c",
          mass = "d",
          wing_length = "d",
          additional_metric = "c",
          observation_comments = "c"
        )
      )
      cli_alert_success(
        "Reading observations from {.file {file.path(directory, 'data/observations.csv')}}."
      )
    }

    # Use add_gldp_resource instead of tags() <- to avoid update
    added_any <- FALSE
    if (is.data.frame(t) && nrow(t) > 0) {
      pkg <- add_gldp_resource(pkg, "tags", t)
      added_any <- TRUE
    }
    if (is.data.frame(o) && nrow(o) > 0) {
      pkg <- add_gldp_resource(pkg, "observations", o)
      added_any <- TRUE
    }
    if (is.data.frame(m) && nrow(m) > 0) {
      pkg <- add_gldp_resource(pkg, "measurements", m)
      added_any <- TRUE
    }

    # Store params as a top-level pkg property (serialized to params.json when writing package).
    pkg$params <- normalize_gldp_params(params)

    if (added_any) {
      pkg <- pkg |>
        update_gldp_taxonomic() |>
        update_gldp_number_tags() |>
        update_gldp_temporal()
    } else {
      cli_inform("No tag/observation/measurement data found to add.")
    }

    return(pkg)
  })

  return(pkg)
}

#' Convert raw tag ID to tag object
#'
#' Internal helper function to convert a raw tag identifier to a complete tag object
#' by reading the configuration and data files.
#'
#' @param id Character string of the tag identifier
#' @param display_config_error Logical indicating whether to display configuration errors
#' @return A tag object with parameter and data information
#' @noRd
raw_tag_id_to_tag <- function(id, display_config_error = TRUE) {
  config <- tryCatch(
    {
      GeoPressureR::geopressuretemplate_config(
        id,
        tag_create = list(assert_pressure = FALSE)
      )
    },
    error = function(e) {
      if (display_config_error) {
        # Warn that the configuration file could not be read and display the error
        cli_warn(c(
          "i" = "Configuration file {.file config.yml} could not be read for {.field {id}}}.",
          ">" = "Create the tag with default value with {.fun GeoPressureR::param_create}.",
          "!" = "Error: {e$message}"
        ))
        display_config_error <- FALSE
      }
      GeoPressureR::param_create(
        id,
        default = TRUE,
        tag_create = list(assert_pressure = FALSE)
      )
    }
  )

  tag <- do.call(
    GeoPressureR::tag_create,
    c(
      list(id = id, quiet = TRUE),
      config$tag_create
    )
  )

  tag <- tryCatch(
    {
      GeoPressureR::tag_label_read(
        tag = tag,
        file = config$tag_label$file
      )

      GeoPressureR::tag_label_stap(
        tag = tag,
        quiet = TRUE,
        file = config$tag_label$file
      )

      tag <- do.call(
        GeoPressureR::tag_set_map,
        c(
          list(tag = tag),
          config$tag_set_map
        )
      )

      tag # return the value
    },
    error = function(e) {
      # Cheat to still keep tag_set_map information even without label and tag_set_map
      tag$param$tag_set_map <- config$tag_set_map
      tag # return the value
    }
  )

  tag$param$bird_create <- config$bird_create

  return(tag)
}


normalize_gldp_params <- function(params) {
  # Drop NULL entries and return early when there is no param payload.
  params <- purrr::compact(params %||% list())
  if (length(params) == 0) {
    return(list())
  }

  # Params from GeoPressure should always expose a non-empty scalar id.
  param_ids <- vapply(
    params,
    \(param) {
      id <- as.character(param$id %||% NA_character_)[1]
      id <- trimws(id)
      id
    },
    character(1)
  )
  if (any(is.na(param_ids) | !nzchar(param_ids))) {
    cli_abort(
      "All entries in {.field params} must contain a non-empty scalar {.field id}."
    )
  }

  params
}
