#' Read SOI data into a GeoLocator Data Package
#'
#' @description
#' This function reads data from the Swiss Ornithological Institute (SOI) into
#' a new package created with [create_gldp()]. It includes
#' tags, measurements, and observations based on the provided data frame and directory of data. The
#' function also handles missing directories and updates the package accordingly.
#'
#' See an example of use [with this tutorial](https://rpubs.com/GeoPressure/Geolocator_create_from_soi).
#'
#' @param gdl A data frame containing the SOI data. Must include columns like `OrderName`,
#' `GDL_ID`, and other relevant fields for tags, measurements, and observations. See `read_soi_gld()`
#' for more information.
#' @param directory_data A character string specifying the path to the directory where data files
#' are located. This directory is used to locate and match GDL_IDs to their corresponding
#' directories.
#' @param generate_observations A logical value indicating whether to create pre-filled
#' observations with missing values (date, locations, etc... assuming equipment and retrieval.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Checks and retrieves the directory information for each GDL_ID/tag_id.
#'   \item Creates GeoPressureR tag data for each of them when possible
#'   \item Extract measurements and add them as resources to pkg
#'   \item Compute tags.csv and observations.csv from `gdl` and add them as resources too.
#' }
#'
#' @return A `geolocatordp` object.
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

  # Retrieve directory of all data and display warning message if absent
  if (!("directory" %in% names(gdl))) {
    cli_progress_step("Retrieving data directories for SOI tags")
    gdl <- read_soi_directory(gdl, directory_data)
  }

  # Do not add any data if same id already presents in measurements
  if ("measurements" %in% frictionless::resources(pkg)) {
    m <- measurements(pkg)
    gdl <- gdl |> filter(!(.data$GDL_ID %in% unique(m$tag_id)))
  } else {
    m <- NULL
  }

  # Error for duplicate
  duplicates <- gdl$GDL_ID[duplicated(gdl$GDL_ID)]
  if (length(duplicates) > 0) {
    cli_abort(c(
      "x" = "Duplicate {.var GDL_ID} found in {.arg gdl}: {unique(duplicates)}",
      "i" = "{.var GDL_ID} (or {.var tag_id}) needs to be unique."
    ))
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
              GeoPressureR::tag_create(
                id = GDL_ID,
                directory = dirname(directory),
                light_file = basename(directory),
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
            list() # Return empty list on error
          }
        )
      },
      .progress = list(
        type = "tasks",
        name = "Loading tags from SOI data"
      )
    )

  # Adding measurement resource
  m <- bind_rows(m, tags_to_measurements(dtags))

  if (nrow(m) > 0) {
    cli_progress_step("Add {.field measurements} to {.pkg pkg}")
    pkg <- add_gldp_resource(
      pkg,
      "measurements",
      m,
      replace = "measurements" %in% frictionless::resources(pkg)
    )
  }

  # Only add tags and observations data to the tag_id not yet present in tag
  if ("tags" %in% frictionless::resources(pkg)) {
    t <- tags(pkg)
    gdl_to <- gdl |> filter(!(.data$GDL_ID %in% t$tag_id))
  } else {
    t <- NULL
    gdl_to <- gdl
  }

  # Compute tags.csv from gdl table
  if (nrow(gdl_to) == 0) {
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
    t_gdl <- gdl_to |>
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
                glue::glue("attachement:{HarnessAttachement_data}")
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
      ungroup()

    t_gdl <- t_gdl |>
      transmute(
        tag_id = .data$GDL_ID,
        manufacturer = "Swiss Ornithological Institute",
        model = if (all(c("GDL_Type", "HardwareVersion") %in% names(t_gdl))) {
          glue::glue("{GDL_Type}-{HardwareVersion}")
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

  t <- bind_rows(t, t_gdl)

  if (nrow(t) > 0) {
    cli_progress_step("Add {.field tags} to {.pkg pkg}")
    pkg <- add_gldp_resource(
      pkg,
      "tags",
      t,
      replace = "tags" %in% frictionless::resources(pkg)
    )
  }

  if ("observations" %in% frictionless::resources(pkg)) {
    o <- observations(pkg)
  } else {
    o <- NULL
  }

  # Adding sensor resource
  o_gdl <- bind_rows(
    gdl_to |>
      transmute(
        ring_number = if ("RingNumber" %in% names(gdl_to)) {
          .data$RingNumber
        } else {
          NA_character_
        },
        tag_id = if ("GDL_ID" %in% names(gdl_to)) {
          .data$GDL_ID
        } else {
          NA_character_
        },
        datetime = if ("UTC_Attached" %in% names(gdl_to)) {
          .data$UTC_Attached
        } else {
          as.POSIXct(NA)
        },
        location_name = if ("SiteAttached" %in% names(gdl_to)) {
          .data$SiteAttached
        } else {
          NA_character_
        },
        longitude = if ("LongitudeAttached" %in% names(gdl_to)) {
          .data$LongitudeAttached
        } else {
          NA_real_
        },
        latitude = if ("LatitudeAttached" %in% names(gdl_to)) {
          .data$LatitudeAttached
        } else {
          NA_real_
        },
        observation_type = "equipment",
        age_class = "0",
        sex = "U"
      ),
    gdl_to |>
      transmute(
        ring_number = if ("RingNumber" %in% names(gdl_to)) {
          .data$RingNumber
        } else {
          NA_character_
        },
        tag_id = if ("GDL_ID" %in% names(gdl_to)) {
          .data$GDL_ID
        } else {
          NA_character_
        },
        datetime = if ("UTC_Removed" %in% names(gdl_to)) {
          .data$UTC_Removed
        } else {
          as.POSIXct(NA)
        },
        longitude = if ("LongitudeRemoved" %in% names(gdl_to)) {
          .data$LongitudeRemoved
        } else {
          NA_real_
        },
        latitude = if ("LatitudeRemoved" %in% names(gdl_to)) {
          .data$LatitudeRemoved
        } else {
          NA_real_
        },
        observation_type = "retrieval",
        age_class = "0",
        sex = "U"
      )
  )

  if (!generate_observations) {
    o_gdl <- o_gdl |> filter(!is.na(.data$datetime))
  }

  o <- bind_rows(o, o_gdl)

  if (nrow(o) > 0) {
    cli_progress_step("Add {.field observations} to {.pkg pkg}")
    pkg <- add_gldp_resource(
      pkg,
      "observations",
      o,
      replace = "observations" %in% frictionless::resources(pkg)
    )
  }

  # Update metadata
  pkg <- pkg |>
    update_gldp_taxonomic() |>
    update_gldp_number_tags() |>
    update_gldp_temporal()

  cli_progress_done()

  pkg
}


#' Add Swiss Ornithological Institute directory information
#'
#' Internal helper function to add directory information for SOI data files
#' to a GDL (Geolocator Data List) object.
#'
#' @param gdl A GDL data frame object
#' @param directory_data A data frame containing directory information
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
    if (!dir.exists(order_dir) && order_name != "Wallis") {
      return(list(
        directory = NA_character_,
        reason = glue::glue("Order directory not found: {order_dir}")
      ))
    }

    # 1 find folder with tag_id name
    folders <- if (dir.exists(order_dir)) {
      list.dirs(order_dir, recursive = FALSE, full.names = FALSE)
    } else {
      character()
    }
    matching_folders <- sort(
      folders[startsWith(folders, as.character(gdl_id))],
      decreasing = TRUE
    )
    if (length(matching_folders) == 1) {
      return(list(
        directory = file.path(order_dir, matching_folders[1]),
        reason = NA_character_
      ))
    } else if (length(matching_folders) > 1) {
      return(list(
        directory = file.path(order_dir, matching_folders[1]),
        reason = NA_character_
      ))
    }

    # 2 find file with tag_id name
    if (order_name == "Wallis") {
      files <- c(
        list.files(
          file.path(base_dir, "UpuEpoCH09/glf"),
          recursive = FALSE,
          full.names = TRUE
        ),
        list.files(
          file.path(base_dir, "UpuEpoCH10/"),
          recursive = FALSE,
          full.names = TRUE
        )
      )
    } else {
      files <- list.files(order_dir, recursive = FALSE, full.names = TRUE)
    }

    matching_files <- files[startsWith(basename(files), as.character(gdl_id))]
    # Check if there are matching files and prioritize .glf files
    if (length(matching_files) > 0) {
      # Look for a .glf file first
      glf_files <- matching_files[grepl("\\.glf$", matching_files)]

      # If there are .glf files, return the first one, otherwise return the first matching file
      if (length(glf_files) > 0) {
        return(list(
          directory = glf_files[1],
          reason = NA_character_
        ))
      } else {
        return(list(
          directory = matching_files[1],
          reason = NA_character_
        ))
      }
    }
    list(
      directory = NA_character_,
      reason = glue::glue(
        "No folder or file starting with GDL_ID {gdl_id} was found in {order_name}."
      )
    )
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
      setNames(as.list(details), rep("x", length(details)))
    ))
  }
  gdl
}
# nocov end
