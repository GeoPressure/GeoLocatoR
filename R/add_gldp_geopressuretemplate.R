#' Add Geolocator DP resources from a GeoPressureTemplate
#'
#' @description
#' This function adds all possible resources to a GeolocatoR Data Package by reading data from
#' a [GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate) directories and files.
#'
#' The function performs the following steps:
#' 1. If `"interim"` in `from`,  reads all interim ".RData" files, extract all variables and add
#' corresponding resources to the package (`measurements`, `twilights`, `staps`, `paths`, `edges`,
#' and `pressurepaths`). Generated automatically temporary `tags` and `observations` tables from
#' the `param` in the interim file.
#' 2. If `"raw-data"` in `from`, search for all tag_id in `data/raw-data/` which were not included
#' from `interim`. It will use at least `tag_create` and if possible `tag_label` and `tag_set_map`
#' using `config.yml` .It will also generate or update `tags` and `observations` table from the the
#' tag data.
#' 3. Reads the `tags.csv` (or `tags.xlsx` if present) and `observations.csv` (or
#' `observations.xlsx` if present) from the `./data` directory if they exist and overwrite the
#' previously generated `tags` and `observations`.
#'
#' You can exclude interim file or raw-tag folder to be included in the package by starting the
#' file name with an `"_"`.
#'
#' It is possible to do a mix of some `tag` read from `"interim"` and other from `"raw-data"`
#' simultaneously.
#'
#' You can find more information on the use of this function in the [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/geolocator-create.html)
#'
#' @param pkg A GeoLocator Data Package object.
#' @param directory A character string specifying the geopressuretemplate directory.
#' @param from A character vector specifying the source of the data files. Either or both of
#' `"raw-tag"` (for creating `tag` based on the data in `data/raw-tag/`) and `"interim"` for data
#' in `data/interim`.
#'
#' @return The updated GLDP package object with new resources
#' @export
add_gldp_geopressuretemplate <- function(
  pkg,
  directory = ".",
  from = c("raw-tag", "interim")
) {
  # Check input
  check_gldp(pkg)

  # Check if the directory exists
  if (!dir.exists(directory)) {
    cli_abort(c(
      "x" = "The specified directory does not exist: {.file {directory}}."
    ))
  }
  assertthat::assert_that(any(from %in% c("interim", "raw-tag")))

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
      all_files %>%
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

      # Add twilights
      twl <- interim$tag %>%
        purrr::map(function(tag) {
          if ("twilight" %in% names(tag)) {
            tag$twilight %>%
              mutate(tag_id = tag$param$id)
          } else {
            NULL
          }
        }) %>%
        purrr::compact() %>% # Remove NULLs
        purrr::list_rbind()

      if (nrow(twl) > 0) {
        pkg <- add_gldp_resource(pkg, "twilights", twl)
      }

      # Add stap
      staps <- interim$tag %>%
        purrr::map(function(tag) {
          if ("stap" %in% names(tag)) {
            tag$stap %>%
              mutate(tag_id = tag$param$id)
          } else {
            NULL
          }
        }) %>%
        purrr::compact() %>% # Remove NULLs
        purrr::list_rbind() %>%
        select(-any_of(c("duration", "nb_sample")))

      if (nrow(staps) > 0) {
        pkg <- add_gldp_resource(pkg, "staps", staps)
      }

      # Add Path
      paths <- var_names_path %>%
        purrr::map(\(x) {
          interim[[x]] %>%
            purrr::imap(function(p, i) {
              if (!is.null(p)) {
                p %>%
                  select(-any_of(c("start", "end", "include"))) %>%
                  mutate(
                    type = sub("path_", "", x, fixed = TRUE),
                    tag_id = interim$tag[[i]]$param$id
                  )
              }
            }) %>%
            purrr::list_rbind()
        }) %>%
        purrr::list_rbind() %>%
        tibble::tibble()

      if (nrow(paths) > 0) {
        pkg <- add_gldp_resource(pkg, "paths", paths)
      }

      # Add Edge
      edges <- var_names_edges %>%
        purrr::map(\(x) {
          interim[[x]] %>%
            purrr::imap(function(e, i) {
              if (!is.null(e)) {
                e %>%
                  mutate(
                    type = sub("edge_", "", x, fixed = TRUE),
                    tag_id = interim$tag[[i]]$param$id
                  )
              }
            }) %>%
            purrr::list_rbind()
        }) %>%
        purrr::list_rbind() %>%
        tibble::tibble()

      if ("gs" %in% names(edges)) {
        edges <- edges %>%
          mutate(
            gs_u = Re(.data$gs),
            gs_v = Im(.data$gs),
          ) %>%
          select(-c("gs"))
      }

      if ("ws" %in% names(edges)) {
        edges <- edges %>%
          mutate(
            ws_u = Re(.data$ws),
            ws_v = Im(.data$ws),
          ) %>%
          select(-c("ws"))
      }

      if (nrow(edges) > 0) {
        pkg <- add_gldp_resource(pkg, "edges", edges)
      }

      # Add Pressurepath
      pressurepaths <- var_names_pressurepath %>%
        purrr::map(\(x) {
          interim[[x]] %>%
            purrr::imap(function(p, i) {
              if (!is.null(p)) {
                p %>%
                  mutate(
                    type = sub("pressurepath_", "", x, fixed = TRUE),
                    tag_id = interim$tag[[i]]$param$id,
                    datetime = as.POSIXct(date, tz = "UTC")
                  ) %>%
                  select(-any_of("date"))
              }
            }) %>%
            purrr::list_rbind()
        }) %>%
        purrr::list_rbind() %>%
        tibble::tibble() %>%
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
        # Read raw tag data with rawtagid_to_tag function
        dtags <- list_id %>%
          purrr::map(
            purrr::possibly(rawtagid_to_tag, NULL),
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

        # Adding measurements resource
        m <- bind_rows(m, tags_to_measurements(dtags))

        # Adding tag resource
        t <- bind_rows(
          t,
          dtags %>%
            purrr::map(~ .x$param) %>%
            params_to_tags()
        )

        # Adding observations resource
        o <- bind_rows(
          o,
          dtags %>%
            purrr::map(~ .x$param) %>%
            params_to_observations()
        )
      }
    }

    # STEP 3: Overwrite tags and observations if csv/xlsx files present
    if (file.exists("data/tags.xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        cli::cli_abort("The {.pkg readxl} package is required to read {.file data/tags.xlsx}.")
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
        cli::cli_abort(
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
    pkg <- add_gldp_resource(pkg, "tags", t)
    pkg <- add_gldp_resource(pkg, "observations", o)
    pkg <- add_gldp_resource(pkg, "measurements", m)

    pkg <- pkg %>%
      update_gldp_taxonomic() %>%
      update_gldp_number_tags() %>%
      update_gldp_spatial() %>%
      update_gldp_temporal()

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
rawtagid_to_tag <- function(id, display_config_error = TRUE) {
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
