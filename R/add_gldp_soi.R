#' Add GLDP SOI Data to a Package
#'
#' @description
#' This function adds data from the Swiss Ornithological Institute (SOI) to a package. It includes
#' tags, measurements, and observations based on the provided data frame and directory of data. The
#' function also handles missing directories and updates the package accordingly.
#'
#' See an example of use [with this tutorial](https://rpubs.com/rafnuss/geolocator_create_from_soi).
#'
#' @param pkg The package object to which the data will be added.
#' @param gdl A data frame containing the SOI data. Must include columns like `OrderName`,
#' `GDL_ID`, and other relevant fields for tags, measurements, and observations. See [`read_gdl`]
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
#' @return The updated package object with the added resources.
#'
#' @export
add_gldp_soi <- function(
  pkg,
  gdl,
  directory_data,
  generate_observations = TRUE
) {
  check_gldp(pkg)
  assertthat::assert_that(is.data.frame(gdl))

  # Retrieve directory of all data and display warning message if absent
  if (!("directory" %in% names(gdl))) {
    cli_progress_step("Retrieving data directories for SOI tags")
    gdl <- add_gldp_soi_directory(gdl, directory_data)
  }

  # Do not add any data if same id already presents in measurements
  if ("measurements" %in% frictionless::resources(pkg)) {
    m <- measurements(pkg)
    gdl <- gdl %>% filter(!(.data$GDL_ID %in% unique(m$tag_id)))
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
  dtags <- gdl %>%
    filter(!is.na(.data$directory)) %>%
    select("GDL_ID", "directory") %>%
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
    gdl_to <- gdl %>% filter(!(.data$GDL_ID %in% t$tag_id))
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
    t_gdl <- gdl_to %>%
      rowwise() %>%
      mutate(
        attachment_type = {
          vars <- pick(everything())
          paste0(
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
            collapse = "|"
          )
        }
      ) %>%
      ungroup()

    t_gdl <- t_gdl %>%
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
    gdl_to %>%
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
    gdl_to %>%
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
    o_gdl <- o_gdl %>% filter(!is.na(.data$datetime))
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
  pkg <- pkg %>%
    update_gldp_taxonomic() %>%
    update_gldp_number_tags() %>%
    update_gldp_spatial() %>%
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
add_gldp_soi_directory <- function(gdl, directory_data) {
  # Check if the required columns are present
  assertthat::assert_that(
    all(c("OrderName", "GDL_ID") %in% colnames(gdl)),
    msg = "The input tibble must contain {.field OrderName} and {.field GDL_ID} columns."
  )
  assertthat::assert_that(
    !any(is.na(gdl$OrderName) | gdl$OrderName == ""),
    msg = "The 'OrderName' column contains empty values."
  )
  assertthat::assert_that(
    !any(is.na(gdl$GDL_ID) | gdl$GDL_ID == ""),
    msg = "The 'GDL_ID' column contains empty values."
  )
  assertthat::assert_that(
    file.exists(directory_data) && file.info(directory_data)$isdir
  )

  # Function to get the highest alphabetical directory path matching the GDL_ID pattern
  check_folder_exists <- function(order_name, gdl_id, base_dir) {
    order_dir <- file.path(base_dir, order_name)

    # 1 find folder with tag_id name
    folders <- list.dirs(order_dir, recursive = FALSE, full.names = FALSE)
    matching_folders <- sort(
      folders[grepl(glue::glue("^{gdl_id}"), folders)],
      decreasing = TRUE
    )
    if (length(matching_folders) == 1) {
      return(file.path(order_dir, matching_folders[1]))
    } else if (length(matching_folders) > 1) {
      print(matching_folders)
      return(file.path(order_dir, matching_folders[1]))
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

    matching_files <- files[grepl(glue::glue("^{gdl_id}"), basename(files))]
    # Check if there are matching files and prioritize .glf files
    if (length(matching_files) > 0) {
      # Look for a .glf file first
      glf_files <- matching_files[grepl("\\.glf$", matching_files)]

      # If there are .glf files, return the first one, otherwise return the first matching file
      if (length(glf_files) > 0) {
        return(glf_files[1])
      } else {
        return(matching_files[1])
      }
    }
    NA
  }

  # Apply the check_folder_exists function to each row and add directory and folder_exists columns
  gdl <- gdl %>%
    rowwise() %>%
    mutate(
      directory = check_folder_exists(
        .data$OrderName,
        .data$GDL_ID,
        directory_data
      )
    ) %>%
    ungroup()

  # Display warning for any missing directories
  gdl_id_na_dir <- gdl %>%
    filter(is.na(.data$directory)) %>%
    pull(.data$GDL_ID)

  if (length(gdl_id_na_dir) > 0 && FALSE) {
    cli_warn(c(
      "!" = "Could not find data directory for {length(gdl_id_na_dir)} tags (out of {nrow(gdl)}).",
      ">" = "GDL_IDs: {.field {gdl_id_na_dir}}. These will not be imported."
    ))
  }
  gdl
}
