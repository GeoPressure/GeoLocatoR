#' Read SOI data into a GeoLocator Data Package
#'
#' @description
#' Import Swiss Ornithological Institute (SOI) geolocator exports into a new
#' GeoLocator Data Package. The function reads available raw tag data from
#' `directory_data`, derives measurements, and builds `tags` and optionally
#' prefilled `observations` from the supplied SOI summary table.
#'
#' See an example of use [with this tutorial](https://rpubs.com/GeoPressure/Geolocator_create_from_soi).
#'
#' @param gdl A data frame of SOI export metadata, typically returned by
#'   [read_soi_gld()]. It must contain at least `GDL_ID`, `OrderName` unless it
#'   has a `directory` column, and the fields needed to derive package
#'   metadata. `tags$attachment_type` is built from the
#'   harness columns (`Harness_data`, `HarnessMaterial_data`,
#'   `HarnessAttachement_data`, `HarnessThickness`, `LegHarnessDiameter`,
#'   `BreastHarnessDiameterHead` and `BreastHarnessDiameterTail`), which
#'   `read_soi_gld(filter_col = TRUE)` does not keep: use `filter_col = FALSE`
#'   to include them.
#' @param directory_data Path to the directory containing SOI raw tag files or
#'   folders, in one sub-folder per `OrderName`. Within it, a tag's data is the
#'   folder, or else the `.glf` file, whose name is the `GDL_ID` alone or
#'   followed by a non-alphanumeric separator (e.g. `A1` or `A1_2020`, but not
#'   `A10_2020`). When several match, the alphabetically last is used, with a
#'   warning naming the candidates. Not used when `gdl` already has
#'   a `directory` column, which gives the data path of each tag directly.
#' @param generate_observations A logical value. Equipment and retrieval
#'   `observations`, with `device_status = "present"`, are always created from
#'   the dates in `gdl`. If `TRUE`, rows
#'   are also created without a datetime, as placeholders to fill in; if `FALSE`,
#'   only rows with a known datetime are kept.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Retrieves the data directory for each `GDL_ID` when not already present.
#'   \item Creates GeoPressureR tag objects for tags with available data.
#'   \item Extracts measurements and adds them as a package resource.
#'   \item Derives `tags` and `observations` from `gdl` and adds them as resources.
#' }
#'
#' @return A `geolocatordp` object.
#'
#' @seealso [read_soi_gld()] to prepare the SOI input table and
#'   [tags_to_measurements()] for the underlying conversion of raw tag objects
#'   to the `measurements` resource.
#'
#' @export
# nocov start
read_soi <- function(
  gdl,
  directory_data,
  generate_observations = TRUE
) {
  pkg <- create_gldp()
  if (!is.data.frame(gdl)) {
    cli_abort(c(
      "x" = "{.arg gdl} must be a data frame."
    ))
  }

  # Error for duplicate
  duplicates <- gdl$GDL_ID[duplicated(gdl$GDL_ID)]
  if (length(duplicates) > 0) {
    cli_abort(c(
      "x" = "Duplicate {.var GDL_ID} found in {.arg gdl}: {unique(duplicates)}",
      "i" = "{.var GDL_ID} (or {.var tag_id}) needs to be unique."
    ))
  }

  # Retrieve directory of all data and display warning message if absent
  if (!("directory" %in% names(gdl))) {
    cli_progress_step("Retrieving data directories for SOI tags")
    gdl <- read_soi_directory(gdl, directory_data)
  }

  # Read tag data
  n_tags_with_data <- sum(!is.na(gdl$directory))
  cli_progress_step(
    glue::glue(
      "Loading tag data for {n_tags_with_data} tags with available data"
    )
  )
  dtags <- gdl |>
    filter(!is.na(.data$directory)) |>
    select("GDL_ID", "directory") |>
    purrr::pmap(
      \(GDL_ID, directory) {
        tryCatch(
          {
            if (grepl("\\.glf$", directory)) {
              # The folder can hold other tags' files, which GeoPressureR would pick by
              # extension, so pass the full path of each sensor file of this tag: every
              # GeoPressureR version uses an existing path as is and skips a sensor given NA
              tag_files <- list.files(dirname(directory), full.names = TRUE)
              tag_files <- tag_files[soi_id_match(basename(tag_files), GDL_ID)]
              sensor_file <- \(ext) {
                tag_files[grepl(glue::glue("\\.{ext}$"), tag_files, ignore.case = TRUE)][1]
              }
              GeoPressureR::tag_create(
                id = GDL_ID,
                directory = dirname(directory),
                light_file = directory,
                pressure_file = sensor_file("pressure"),
                acceleration_file = sensor_file("acceleration"),
                temperature_external_file = sensor_file("(air)?temperature"),
                temperature_internal_file = sensor_file("bodytemperature"),
                magnetic_file = sensor_file("magnetic"),
                assert_pressure = FALSE, # Allow tag to not have pressure data
                quiet = TRUE
              )
            } else {
              GeoPressureR::tag_create(
                id = GDL_ID,
                directory = directory,
                assert_pressure = FALSE, # Allow tag to not have pressure data
                quiet = TRUE
              )
            }
          },
          error = function(e) {
            cli_warn(c(
              "!" = "Could not read the data of tag {.val {GDL_ID}} in {.file {directory}}. Its measurements are skipped.",
              "x" = conditionMessage(e)
            ))
            list() # Return empty list on error
          }
        )
      },
      .progress = list(
        type = "tasks",
        name = "Loading tags from SOI data"
      )
    )

  # Adding measurement resource, without the tags that could not be read
  m <- tags_to_measurements(purrr::compact(dtags))

  if (nrow(m) > 0) {
    cli_progress_step("Add {.field measurements} to {.pkg pkg}")
    pkg <- add_gldp_resource(
      pkg,
      "measurements",
      m,
      replace = TRUE
    )
  }

  # Compute tags.csv from gdl table
  if (nrow(gdl) == 0) {
    t_gdl <- tibble(
      tag_id = character(),
      manufacturer = character(),
      model = character(),
      firmware = character(),
      weight = numeric(),
      attachment_type = character(),
      scientific_name = character(),
      ring_number = character(),
      tag_comments = character()
    )
  } else {
    t_gdl <- gdl |>
      rowwise() |>
      mutate(
        attachment_type = {
          vars <- pick(everything())
          glue::glue_collapse(
            c(
              if ("Harness_data" %in% names(vars) && !is.na(.data$Harness_data)) {
                .data$Harness_data
              },
              if (
                "HarnessMaterial_data" %in%
                  names(vars) &&
                  !is.na(.data$HarnessMaterial_data)
              ) {
                glue::glue("material:{HarnessMaterial_data}")
              },
              if (
                "HarnessAttachement_data" %in%
                  names(vars) &&
                  !is.na(.data$HarnessAttachement_data)
              ) {
                glue::glue("attachment:{HarnessAttachement_data}")
              },
              if (
                "HarnessThickness" %in%
                  names(vars) &&
                  !is.na(.data$HarnessThickness)
              ) {
                glue::glue("thickness:{HarnessThickness}")
              },
              if (
                "LegHarnessDiameter" %in%
                  names(vars) &&
                  !is.na(.data$LegHarnessDiameter)
              ) {
                glue::glue("legDiameter:{LegHarnessDiameter}")
              },
              if (
                "BreastHarnessDiameterHead" %in%
                  names(vars) &&
                  !is.na(.data$BreastHarnessDiameterHead)
              ) {
                glue::glue("BreastDiameterHead:{BreastHarnessDiameterHead}")
              },
              if (
                "BreastHarnessDiameterTail" %in%
                  names(vars) &&
                  !is.na(.data$BreastHarnessDiameterTail)
              ) {
                glue::glue("BreastDiameterTail:{BreastHarnessDiameterTail}")
              }
            ),
            sep = "|"
          )
        }
      ) |>
      ungroup() |>
      # NA rather than an empty string when no harness detail is known
      mutate(attachment_type = na_if(as.character(.data$attachment_type), ""))

    t_gdl <- t_gdl |>
      transmute(
        tag_id = .data$GDL_ID,
        manufacturer = "Swiss Ornithological Institute",
        model = if (all(c("GDL_Type", "HardwareVersion") %in% names(t_gdl))) {
          # Without a hardware version, the type alone names the model
          coalesce(
            as.character(glue::glue("{GDL_Type}-{HardwareVersion}", .na = NULL)),
            .data$GDL_Type
          )
        } else {
          NA_character_
        },
        firmware = if ("FirmwareVersion" %in% names(t_gdl)) {
          .data$FirmwareVersion
        } else {
          NA_character_
        },
        weight = if ("TotalWeight" %in% names(t_gdl)) {
          .data$TotalWeight
        } else {
          NA_real_
        },
        attachment_type = if ("attachment_type" %in% names(t_gdl)) {
          .data$attachment_type
        } else {
          NA_character_
        },
        scientific_name = if ("Species" %in% names(t_gdl)) {
          .data$Species
        } else {
          NA_character_
        },
        ring_number = if ("RingNumber" %in% names(t_gdl)) {
          .data$RingNumber
        } else {
          NA_character_
        },
        tag_comments = if ("Remarks" %in% names(t_gdl)) {
          .data$Remarks
        } else {
          NA_character_
        }
      )
  }

  if (nrow(t_gdl) > 0) {
    cli_progress_step("Add {.field tags} to {.pkg pkg}")
    pkg <- add_gldp_resource(
      pkg,
      "tags",
      t_gdl,
      replace = TRUE
    )
  }

  # Equipment and retrieval observations from the attachment and removal records
  o_gdl <- bind_rows(
    gdl |>
      transmute(
        ring_number = if ("RingNumber" %in% names(gdl)) {
          .data$RingNumber
        } else {
          NA_character_
        },
        tag_id = .data$GDL_ID,
        datetime = if ("UTC_Attached" %in% names(gdl)) {
          .data$UTC_Attached
        } else {
          as.POSIXct(NA)
        },
        location_name = if ("SiteAttached" %in% names(gdl)) {
          .data$SiteAttached
        } else {
          NA_character_
        },
        longitude = if ("LongitudeAttached" %in% names(gdl)) {
          .data$LongitudeAttached
        } else {
          NA_real_
        },
        latitude = if ("LatitudeAttached" %in% names(gdl)) {
          .data$LatitudeAttached
        } else {
          NA_real_
        },
        observation_type = "equipment",
        device_status = "present",
        age_class = "0",
        sex = "U"
      ),
    gdl |>
      transmute(
        ring_number = if ("RingNumber" %in% names(gdl)) {
          .data$RingNumber
        } else {
          NA_character_
        },
        tag_id = .data$GDL_ID,
        datetime = if ("UTC_Removed" %in% names(gdl)) {
          .data$UTC_Removed
        } else {
          as.POSIXct(NA)
        },
        longitude = if ("LongitudeRemoved" %in% names(gdl)) {
          .data$LongitudeRemoved
        } else {
          NA_real_
        },
        latitude = if ("LatitudeRemoved" %in% names(gdl)) {
          .data$LatitudeRemoved
        } else {
          NA_real_
        },
        observation_type = "retrieval",
        device_status = "present",
        age_class = "0",
        sex = "U"
      )
  )

  if (!generate_observations) {
    o_gdl <- o_gdl |> filter(!is.na(.data$datetime))
  }

  if (nrow(o_gdl) > 0) {
    cli_progress_step("Add {.field observations} to {.pkg pkg}")
    pkg <- add_gldp_resource(
      pkg,
      "observations",
      o_gdl,
      replace = TRUE
    )
  }

  # read_soi() keeps no params, so the SOI axis frame is that of the installed
  # GeoPressureR: forward = raw y from 3.8.0, the hardware-based mapping before.
  if (utils::packageVersion("GeoPressureR") < "3.8.0") {
    pkg <- gldp_fix_soi_axes(pkg, quiet = TRUE)
  } else {
    pkg <- gldp_params_set_mag_axis(pkg, names(gldp_soi_mag_axis(pkg)))
  }

  # Update metadata
  pkg <- pkg |>
    gldp_update_taxonomic() |>
    gldp_update_number_tags() |>
    gldp_update_temporal()

  cli_progress_done()

  pkg
}


#' Add Swiss Ornithological Institute directory information
#'
#' Internal helper function to add directory information for SOI data files
#' to a GDL (Geolocator Data List) object.
#'
#' @param gdl A GDL data frame object
#' @param directory_data Path to the directory containing one folder per
#'   `OrderName`
#' @return Updated GDL object with directory information
#' @noRd
read_soi_directory <- function(gdl, directory_data) {
  # Check if the required columns are present
  if (!all(c("OrderName", "GDL_ID") %in% colnames(gdl))) {
    cli_abort(c(
      "x" = "{.arg gdl} must contain {.field OrderName} and {.field GDL_ID} columns."
    ))
  }
  if (!dir.exists(directory_data)) {
    cli_abort(c(
      "x" = "{.arg directory_data} must be an existing directory."
    ))
  }

  # Function to retrieve data directory path and an optional issue description
  check_folder_exists <- function(order_name, gdl_id, base_dir) {
    if (length(order_name) != 1 || is.na(order_name) || order_name == "") {
      return(list(
        directory = NA_character_,
        reason = "OrderName is missing or empty."
      ))
    }
    if (length(gdl_id) != 1 || is.na(gdl_id) || gdl_id == "") {
      return(list(
        directory = NA_character_,
        reason = "GDL_ID is missing or empty."
      ))
    }

    order_dir <- file.path(base_dir, order_name)
    if (!dir.exists(order_dir)) {
      return(list(
        directory = NA_character_,
        reason = glue::glue("Order directory not found: {order_dir}")
      ))
    }

    # 1 find folder named after the tag, else 2 its .glf file: outside a tag folder only
    # a .glf file can be read, as read_soi() passes any other file as a directory
    folders <- list.dirs(order_dir, recursive = FALSE, full.names = TRUE)
    files <- setdiff(list.files(order_dir, full.names = TRUE), folders)
    candidates <- folders[soi_id_match(basename(folders), gdl_id)]
    if (length(candidates) == 0) {
      candidates <- files[soi_id_match(basename(files), gdl_id) & grepl("\\.glf$", files)]
    }
    if (length(candidates) == 0) {
      return(list(
        directory = NA_character_,
        reason = glue::glue(
          "No folder or .glf file named after GDL_ID {gdl_id} was found in {order_name}."
        )
      ))
    }
    candidates <- sort(candidates, decreasing = TRUE)
    if (length(candidates) > 1) {
      cli_warn(c(
        "!" = "Several data paths match GDL_ID {.val {gdl_id}}; using {.path {candidates[1]}}.",
        "i" = "Candidates: {.path {candidates}}."
      ))
    }
    list(directory = candidates[1], reason = NA_character_)
  }

  # Apply directory lookup row-wise while preserving table shape
  directory_lookup <- purrr::map2(
    gdl$OrderName,
    gdl$GDL_ID,
    check_folder_exists,
    base_dir = directory_data
  )
  gdl$directory <- purrr::map_chr(directory_lookup, "directory")
  directory_reason <- purrr::map_chr(directory_lookup, "reason")

  # Display warning for missing directories with reasons
  missing_directory <- gdl |>
    mutate(directory_reason = directory_reason) |>
    filter(is.na(.data$directory))

  if (nrow(missing_directory) > 0) {
    details <- paste0(
      "GDL_ID=",
      missing_directory$GDL_ID,
      " | OrderName=",
      missing_directory$OrderName,
      " | reason=",
      missing_directory$directory_reason
    )
    cli_warn(c(
      "!" = paste0(
        "Could not find data directory for ",
        nrow(missing_directory),
        " tags (out of ",
        nrow(gdl),
        "). Rows are kept with {.field directory} = NA."
      ),
      stats::setNames(as.list(details), rep("x", length(details)))
    ))
  }
  gdl
}

#' Match file or folder names to a SOI tag ID
#'
#' A name matches when it is the ID alone or the ID followed by a non-alphanumeric
#' separator, so that `A1` does not match `A10_2020`.
#'
#' @param x Character vector of file or folder names.
#' @param gdl_id A single `GDL_ID`.
#' @return A logical vector.
#' @noRd
soi_id_match <- function(x, gdl_id) {
  gdl_id <- as.character(gdl_id)
  startsWith(x, gdl_id) & grepl("^([^[:alnum:]]|$)", substring(x, nchar(gdl_id) + 1))
}
# nocov end
