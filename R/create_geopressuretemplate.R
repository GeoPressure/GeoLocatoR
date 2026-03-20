#' Create a GeoPressureTemplate Project
#' @description
#' Initializes a geopressure template project by creating a specified directory structure and
#' populating it with essential files, including a DESCRIPTION file, README, license, and data.
#'
#' This function sets up the project directory and ensures that all necessary components are in
#' place for a geopressure analysis project.
#'
#' @param path A character string specifying the destination directory where the project will
#' be created. The last folder will give the name to the project.
#' @param pkg A GeoLocatoR Datapackage object (optional)
#' @param open If `TRUE`, the package is opened in a new RStudio session.
#'
#' @return The path to the created project directory.
#'
#' @details Upon execution, the function performs the following steps:
#' \itemize{
#'   \item Creates the project directory.
#'   \item Generates the DESCRIPTION file using metadata from `pkg`.
#'   \item Creates a README file that outlines project details.
#'   \item Generates a LICENSE file based on the specified licenses.
#'   \item Writes relevant data files into the project structure.
#' }
#'
#' @export
create_geopressuretemplate <- function(path, pkg = NULL, open = interactive()) {
  # Expand `~` and similar user paths so git and file checks
  # operate on the intended directory.
  path <- path.expand(path)

  if (dir.exists(path)) {
    existing <- list.files(path, all.files = TRUE, no.. = TRUE)

    if (length(existing) > 0) {
      cli_abort(c(
        "x" = "Directory {.path {path}} already exists and is not empty.",
        "i" = "Please choose a new path or empty the directory before creating a GeoPressureTemplate project."
      ))
    } else {
      # `git clone` cannot clone into an existing directory, even if empty
      unlink(path, recursive = TRUE)
    }
  }

  clone_status <- system2(
    command = "git",
    args = c(
      "clone",
      "--quiet",
      "--depth",
      "1",
      "https://github.com/Rafnuss/GeoPressureTemplate",
      path
    ),
    stdout = FALSE,
    stderr = FALSE
  )
  if (!identical(clone_status, 0L)) {
    cli_abort(
      "Failed to clone {.url https://github.com/Rafnuss/GeoPressureTemplate/} into {.path {path}}."
    )
  }
  cli_alert_success(
    "Cloning repo from {.url https://github.com/Rafnuss/GeoPressureTemplate/} into {.path {path}}."
  )
  project_name <- basename(path)

  file.rename(
    from = file.path(path, "GeoPressureTemplate.Rproj"),
    to = file.path(path, glue::glue("{project_name}.Rproj"))
  )

  # setwd(path)
  withr::with_dir(path, {
    # setwd(path)
    d <- desc::desc()
    d$set("Package", gsub("[^a-zA-Z0-9\\.]", "", project_name))
    d$write()

    # delete existing data
    unlink(list.files("./data/raw-tag/", full.names = TRUE), recursive = TRUE)
    unlink(list.files("./data/tag-label/", full.names = TRUE), recursive = TRUE)
    unlink(
      list.files("./data/twilight-label/", full.names = TRUE),
      recursive = TRUE
    )

    # Remove example in config
    default_yaml <- readLines("config.yml")
    writeLines(
      default_yaml[1:(grep("18LX:", default_yaml)[1] - 1)],
      "config.yml"
    )

    if (!is.null(pkg)) {
      check_gldp(pkg)
      pkg <- update_gldp(pkg)

      steps <- list(
        "DESCRIPTION file" = \(p) create_geopressuretemplate_desc(p),
        "README file" = \(p) create_geopressuretemplate_readme(p),
        "LICENSE file" = \(p) create_geopressuretemplate_licenses(p),
        "data files" = \(p) create_geopressuretemplate_data(p),
        "config file" = \(p) create_geopressuretemplate_config(p)
      )

      purrr::iwalk(steps, \(step_fun, step_name) {
        tryCatch(
          step_fun(pkg),
          error = function(e) {
            cli_warn(c(
              "!" = "Failed to generate {.val {step_name}}.",
              ">" = conditionMessage(e)
            ))
            invisible(NULL)
          }
        )
      })
    }
  })

  if (open) {
    if (!requireNamespace("rstudioapi", quietly = TRUE)) {
      cli_abort("The {.pkg rstudioapi} package is required to open the project in RStudio.")
    }
    rstudioapi::openProject(path, newSession = TRUE)
  }

  path
}

#' Create DESCRIPTION file for GeoPressure template
#'
#' Internal helper function to generate a DESCRIPTION file based on a
#' GeoLocator Data Package's metadata.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Nothing (side effect: writes DESCRIPTION file)
#' @noRd
create_geopressuretemplate_desc <- function(pkg) {
  contributors <- pkg$contributors %||% list()
  licenses <- pkg$licenses %||% list()

  if (!is.list(licenses)) {
    licenses <- list(licenses)
  }
  if (length(licenses) > 0 && !is.list(licenses[[1]])) {
    licenses <- list(licenses)
  }

  if (is.list(contributors) && length(contributors) > 0) {
    contributors <- purrr::map(contributors, \(c) {
      if (!is.list(c)) {
        return(list(title = as.character(c)[1]))
      }
      c
    })
  }

  d <- desc::description$new()

  d$set("Title", pkg$title %||% "GeoPressureTemplate project", check = FALSE)

  if (length(licenses) > 0) {
    license_text <- purrr::map_chr(licenses, \(lic) {
      if (is.list(lic)) {
        lic$name %||% lic$title %||% ""
      } else {
        as.character(lic)[1]
      }
    })
    license_text <- license_text[nzchar(license_text)]
    if (length(license_text) > 0) {
      d$set("License", paste(license_text, collapse = ", "), check = FALSE)
    }
  }

  if (length(contributors) > 0) {
    d$set_authors(contributors_to_persons(contributors))
  }

  # Optional fields
  if (!is.null(pkg$description)) {
    description_text <- as.character(pkg$description)[1] |>
      rvest::read_html() |>
      rvest::html_text2()
    if (nzchar(description_text)) {
      d$set("Description", description_text, check = FALSE)
    }
  }

  if (!is.null(pkg$version)) {
    d$set_version(gsub("^v", "", as.character(pkg$version)[1]))
  }

  d$normalize()
  d$write()
}

#' Create README file for GeoPressure template
#'
#' Internal helper function to generate a README.md file based on a
#' GeoLocator Data Package's metadata.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Nothing (side effect: writes README.md file)
#' @noRd
create_geopressuretemplate_readme <- function(pkg) {
  badge_part <- if (!is.null(pkg$name) && nzchar(pkg$name)) {
    glue::glue(
      "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.{pkg$name}.svg)](https://doi.org/10.5281/zenodo.{pkg$name})"
    )
  } else {
    ""
  }
  description_text <- if (!is.null(pkg$description)) {
    as.character(pkg$description)[1] |>
      rvest::read_html() |>
      rvest::html_text2()
  } else {
    ""
  }

  content <- glue::glue(
    "# {pkg$title %||% 'GeoPressureTemplate project'}\n\n",
    "{badge_part}\n\n",
    "{description_text}\n"
  )

  writeLines(content, "README.md")
}

#' Create LICENSE file for GeoPressure template
#'
#' Internal helper function to generate appropriate LICENSE files based on
#' the license specifications in a GeoLocator Data Package.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Nothing (side effect: creates LICENSE files)
#' @noRd
create_geopressuretemplate_licenses <- function(pkg) {
  licenses <- pkg$licenses %||% list()

  if (!is.list(licenses)) {
    licenses <- list(licenses)
  }
  if (length(licenses) > 0 && !is.list(licenses[[1]])) {
    licenses <- list(licenses)
  }

  if (length(licenses) == 0) {
    cli_inform("No license information provided; skipping LICENSE file setup.")
    return(invisible(NULL))
  }

  # 1. If more than one license is provided, warn and keep the first
  if (length(licenses) > 1) {
    cli_warn(c(
      "!" = "Multiple licenses detected.",
      ">" = "Only the first license will be used."
    ))
  }
  lic <- licenses[[1]]

  # 2. Delete existing LICENSE.md file if it exists
  if (file.exists("LICENSE.md")) {
    file.remove("LICENSE.md")
  }

  # 3. Force usethis to recognize current directory as project root
  usethis::proj_set(getwd(), force = TRUE)

  # 4. Fallback to license path if name is missing or empty
  license_name <- if (is.list(lic)) {
    lic$name %||% lic$path
  } else {
    as.character(lic)[1]
  }
  # 5. Abort if no usable license identifier is available
  if (is.null(license_name) || is.na(license_name) || !nzchar(license_name)) {
    cli_warn(c(
      "!" = "No license name or path provided.",
      ">" = "License file not created."
    ))
    return(invisible(NULL))
  }
  license_name <- tolower(license_name)

  rules <- list(
    agpl = list("agpl", usethis::use_agpl_license),
    lgpl = list("lgpl", usethis::use_lgpl_license),
    gpl = list("\\bgpl\\b", usethis::use_gpl_license),
    mit = list("\\bmit\\b", usethis::use_mit_license),
    apache = list("apache", usethis::use_apache_license),
    cc0 = list("cc[- ]?0", usethis::use_cc0_license),
    ccby = list("cc[- ]?by", usethis::use_ccby_license),
    proprietary = list("proprietary", usethis::use_proprietary_license)
  )

  # 6. Identify the first matching rule
  hit <- vapply(
    rules,
    function(r) grepl(r[[1]], license_name),
    logical(1)
  )

  # 7. Apply the corresponding usethis license function
  if (any(hit)) {
    rules[[which(hit)[1]]][[2]]()
  } else {
    cli_warn(c(
      "!" = "No matching license found in {.pkg usethis}.",
      ">" = "License file not created."
    ))
  }
}


#' @noRd
create_geopressuretemplate_data <- function(pkg) {
  check_gldp(pkg)

  # Observations
  readr::write_csv(tags(pkg), "./data/tags.csv")
  readr::write_csv(observations(pkg), "./data/observations.csv")

  m <- measurements(pkg)

  tag_ids <- unique(tags(pkg)$tag_id)
  tags_export <- gldp_to_tag(pkg, tag_id = tag_ids)
  if (inherits(tags_export, "tag")) {
    tags_export <- stats::setNames(list(tags_export), tag_ids[1])
  }

  # Create the tag-label directory if it doesn't exist
  path_label <- glue::glue("./data/tag-label/")
  if (!dir.exists(path_label)) {
    dir.create(path_label, recursive = TRUE)
  }
  path_twl <- glue::glue("./data/twilight-label/")
  if (!dir.exists(path_twl)) {
    dir.create(path_twl, recursive = TRUE)
  }

  sensor_names <- c(
    "pressure",
    "light",
    "acceleration",
    "temperature_external",
    "temperature_internal",
    "magnetic"
  )

  purrr::iwalk(
    tags_export,
    \(tag, tag_id) {
      dir_path <- glue::glue("./data/raw-tag/{tag_id}")
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }

      purrr::walk(sensor_names, \(sensor) {
        if (!sensor %in% names(tag)) {
          return(invisible(NULL))
        }
        tag[[sensor]] |>
          dplyr::rename(datetime = "date") |>
          readr::write_csv(file = glue::glue("{dir_path}/{sensor}.csv"))
      })

      labels_pressure <- m |>
        dplyr::filter(.data$tag_id == !!tag_id, .data$sensor == "pressure") |>
        dplyr::transmute(
          date = .data$datetime,
          label = ifelse(is.na(.data$label), "", .data$label)
        ) |>
        dplyr::group_by(.data$date) |>
        dplyr::summarize(
          label = dplyr::first(.data$label[.data$label != ""], default = ""),
          .groups = "drop"
        )

      labels_acceleration <- m |>
        dplyr::filter(
          .data$tag_id == !!tag_id,
          .data$sensor %in% c("acceleration", "activity")
        ) |>
        dplyr::transmute(
          date = .data$datetime,
          label = ifelse(is.na(.data$label), "", .data$label)
        ) |>
        dplyr::group_by(.data$date) |>
        dplyr::summarize(
          label = dplyr::first(.data$label[.data$label != ""], default = ""),
          .groups = "drop"
        )

      if ("pressure" %in% names(tag)) {
        tag$pressure <- tag$pressure |>
          dplyr::left_join(labels_pressure, by = "date") |>
          dplyr::mutate(label = dplyr::coalesce(.data$label, ""))
      }
      if ("acceleration" %in% names(tag)) {
        tag$acceleration <- tag$acceleration |>
          dplyr::left_join(labels_acceleration, by = "date") |>
          dplyr::mutate(label = dplyr::coalesce(.data$label, ""))
      }

      if ("pressure" %in% names(tag)) {
        GeoPressureR::tag_label_write(
          tag = tag,
          file = glue::glue("{path_label}/{tag_id}-labeled.csv"),
          quiet = TRUE
        )
      }
    },
    .progress = list(type = "tasks")
  )

  purrr::iwalk(
    tags_export,
    \(tag, tag_id) {
      if ("twilight" %in% names(tag)) {
        if (
          is.null(tag$param$twilight_create$twl_offset) ||
            length(tag$param$twilight_create$twl_offset) == 0
        ) {
          tag$param$twilight_create$twl_offset <- 0
        }
        GeoPressureR::twilight_label_write(
          tag = tag,
          file = glue::glue("{path_twl}/{tag_id}-labeled.csv"),
          quiet = TRUE
        )
      }
    },
    .progress = list(type = "tasks")
  )
}

#' @noRd
create_geopressuretemplate_config <- function(pkg) {
  check_gldp(pkg)

  t <- tags(pkg)
  o <- observations(pkg)

  t_config <- t |>
    purrr::pmap(\(tag_id, ring_number, scientific_name, ...) {
      # Create the basic config from tag tibble
      co <- list(
        bird_create = list(
          scientific_name = scientific_name
        ),
        ring_number = ring_number
      )

      # Construct the known data.frame from observation
      k <- o |>
        filter(.data$tag_id == !!tag_id) |>
        arrange(.data$datetime) |>
        transmute(
          stap_id = ifelse(
            .data$observation_type == "equipment",
            1,
            ifelse(.data$observation_type == "retrieval", -1, 0)
          ),
          datetime = as.POSIXct(.data$datetime, tz = "UTC"),
          known_lon = .data$longitude,
          known_lat = .data$latitude,
          location_name = .data$location_name,
          device_status = .data$device_status,
          condition = .data$condition,
          age_class = .data$age_class,
          sex = .data$sex,
          observation_comments = .data$observation_comments
        )

      # Add sex if unique and defined (i.e., not U)
      usex <- unique(k$sex)
      usex <- usex[!is.na(usex)]
      if (length(usex) == 1 && usex != "U") {
        co$sex <- usex
      }

      # Conditionally remove the column
      rm_col <- c("sex")
      if (all(is.na(k$location_name) | k$location_name == "")) {
        rm_col <- c(rm_col, "location_name")
      }
      if (
        all(
          is.na(k$device_status) |
            k$device_status == "" |
            k$device_status == "unknown"
        )
      ) {
        rm_col <- c(rm_col, "device_status")
      }
      if (all(is.na(k$condition) | k$condition == "" | k$condition == "unknown")) {
        rm_col <- c(rm_col, "condition")
      }
      if (
        all(
          is.na(k$age_class) |
            k$age_class == "" |
            k$age_class == 0 |
            k$age_class == "0"
        )
      ) {
        rm_col <- c(rm_col, "age_class")
      }
      if (all(is.na(k$observation_comments) | k$observation_comments == "")) {
        rm_col <- c(rm_col, "observation_comments")
      }
      k <- k |> select(any_of(names(k)[!names(k) %in% rm_col]))

      # Add crop date from equipement and retrieval
      co$tag_create <- list(
        crop_start = k |>
          filter(.data$stap_id == 1) |>
          mutate(
            dt = format(
              .data$datetime + as.difftime(1, units = "days"),
              "%Y-%m-%d"
            )
          ) |>
          pull(.data$dt),
        # we start one day after equipment (at 00:00)
        crop_end = k |>
          filter(.data$stap_id == -1) |>
          mutate(dt = format(.data$datetime, "%Y-%m-%d")) |>
          pull(.data$dt)
      )

      # Add known
      co$tag_set_map <- list(
        known = k |>
          mutate(
            datetime = format(.data$datetime, "%Y-%m-%d")
          )
      )

      co
    })

  # Add tag_id as name
  names(t_config) <- t$tag_id

  # convert to yaml
  t_yaml <- yaml::as.yaml(
    t_config,
    handlers = list(
      data.frame = \(k) {
        tmp <- lapply(names(k), function(name) {
          x <- k[[name]]
          if (name == "stap_id" && any(x == 0)) {
            add_text <- " # Modify the `stap_id` of `0` to the correct stap_id"
          } else {
            add_text <- ""
          }
          if (!is.numeric(x)) {
            x <- glue::glue('"{x}"')
          }
          tmp <- glue::glue("[{glue::glue_collapse(x, sep = ', ')}]")

          # class(tmp) <- "verbatim" # does not work
          glue::glue("{tmp}{add_text}")
        })
        names(tmp) <- names(k)
        tmp
      }
    )
  )

  # Manually fix issue with tibble export
  t_yaml <- gsub("\\]\\'", "]", gsub("\\'\\[", "[", t_yaml))

  # Combine default config.yml with trim_yaml
  combined_yaml <- c(readLines("config.yml"), t_yaml)

  # Write the combined output to config.yml
  writeLines(combined_yaml, "config.yml")
}
