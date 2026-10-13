library(testthat)
library(dplyr)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("gldp_to_dwc writes occurrence.csv and returns occurrence data", {
  pkg <- pkg_shared
  tmp <- withr::local_tempdir()

  suppressMessages({
    ids <- data.frame(
      scientific_name = unique(tags(pkg)$scientific_name),
      scientificNameID = NA_character_
    )
    occurrence <- gldp_to_dwc(pkg, directory = tmp, scientific_name_ids = ids)
  })

  # File is written
  expect_true(file.exists(file.path(tmp, "occurrence.csv")))

  # Return value is a data frame with rows
  expect_s3_class(occurrence, "data.frame")
  expect_gt(nrow(occurrence), 0)

  # Required Darwin Core columns are present
  required_cols <- c(
    "type",
    "license",
    "rightsHolder",
    "datasetID",
    "institutionCode",
    "collectionCode",
    "datasetName",
    "basisOfRecord",
    "dataGeneralizations",
    "dynamicProperties",
    "occurrenceID",
    "sex",
    "lifeStage",
    "occurrenceStatus",
    "organismID",
    "organismName",
    "eventID",
    "parentEventID",
    "eventType",
    "eventDate",
    "samplingProtocol",
    "samplingEffort",
    "eventRemarks",
    "minimumElevationInMeters",
    "maximumElevationInMeters",
    "locationRemarks",
    "decimalLatitude",
    "decimalLongitude",
    "geodeticDatum",
    "coordinateUncertaintyInMeters",
    "georeferenceSources",
    "identificationVerificationStatus",
    "scientificNameID",
    "scientificName",
    "kingdom"
  )
  expect_identical(names(occurrence), required_cols)

  # Key fields are set consistently from package metadata
  expect_true(all(occurrence$datasetName == pkg$title))
  expect_true(all(occurrence$datasetID == pkg$id))
  expect_true(all(occurrence$basisOfRecord == "MachineObservation"))
  expect_true(all(occurrence$geodeticDatum == "EPSG:4326"))
  expect_true(all(occurrence$samplingProtocol == "geolocator"))
  expect_true(all(occurrence$occurrenceStatus == "present"))

  # Coordinates look numeric
  expect_type(occurrence$decimalLatitude, "double")
  expect_type(occurrence$decimalLongitude, "double")
})

test_that("supplied taxonomy identifiers bypass automatic lookups", {
  tmp <- tempfile()
  dir.create(tmp)
  pkg <- pkg_shared
  ids <- data.frame(
    scientific_name = unique(tags(pkg)$scientific_name),
    scientificNameID = "avibase-7A2734EB"
  )
  occurrence <- suppressMessages(gldp_to_dwc(pkg, tmp, scientific_name_ids = ids))
  expect_true(all(occurrence$scientificNameID == "avibase-7A2734EB"))
  ids$scientificNameID <- NA_character_
  occurrence <- suppressMessages(gldp_to_dwc(pkg, tmp, scientific_name_ids = ids))
  expect_true(all(is.na(occurrence$scientificNameID)))
  unlink(tmp, recursive = TRUE)
})

test_that("a NULL directory returns the table without writing files", {
  ids <- data.frame(
    scientific_name = unique(tags(pkg_shared)$scientific_name),
    scientificNameID = "avibase-7A2734EB"
  )
  occurrence <- gldp_to_dwc(pkg_shared, NULL, scientific_name_ids = ids)
  expect_s3_class(occurrence, "data.frame")
  expect_gt(nrow(occurrence), 0)
})

test_that("known-location uncertainty uses the tag grid or ERA5 default", {
  pkg <- pkg_shared
  path_data <- paths(pkg)
  stap_data <- staps(pkg)
  stap_data$known_lat <- 0
  stap_data$known_lon <- 0
  staps(pkg) <- stap_data
  path_data$lat <- 0
  paths(pkg) <- path_data
  pkg$params <- lapply(unique(path_data$tag_id), \(id) list(id = id, tag_set_map = list(scale = 2)))
  ids <- data.frame(
    scientific_name = unique(tags(pkg)$scientific_name),
    scientificNameID = NA_character_
  )
  occurrence <- gldp_to_dwc(pkg, NULL, scientific_name_ids = ids)
  expected <- ceiling(6371008.8 * acos(cos(pi / 720)^2))
  expect_true(all(occurrence$coordinateUncertaintyInMeters == expected))
  expect_true(all(grepl("analysis parameters", occurrence$eventRemarks)))

  pkg$params <- list()
  default <- gldp_to_dwc(pkg, NULL, scientific_name_ids = ids)
  expected <- ceiling(6371008.8 * acos(cos(pi / 1440)^2))
  expect_true(all(default$coordinateUncertaintyInMeters == expected))
  expect_true(all(grepl("ERA5 default", default$eventRemarks)))
  path_data$lat <- 45
  paths(pkg) <- path_data
  higher_latitude <- gldp_to_dwc(pkg, NULL, scientific_name_ids = ids)
  expect_true(all(
    higher_latitude$coordinateUncertaintyInMeters < default$coordinateUncertaintyInMeters
  ))
})

test_that("simulation uncertainty handles sample size, grid floor and missing simulations", {
  pkg <- pkg_shared
  stap_data <- staps(pkg)
  stap_data$known_lat <- NA_real_
  stap_data$known_lon <- NA_real_
  staps(pkg) <- stap_data
  centre <- paths(pkg) |> filter(type == "most_likely") |> slice(1)
  centre$lat <- 0
  centre$lon <- 0
  pkg$params <- list()
  ids <- data.frame(
    scientific_name = unique(tags(pkg)$scientific_name),
    scientificNameID = NA_character_
  )

  for (n_simulations in c(10, 99, 100)) {
    simulations <- centre[rep(1, n_simulations), ]
    simulations$type <- "simulation"
    simulations$j <- seq_len(n_simulations)
    simulations$lon <- seq_len(n_simulations) / 10
    paths(pkg) <- bind_rows(centre, simulations)
    occurrence <- gldp_to_dwc(pkg, NULL, scientific_name_ids = ids)
    expected <- if (n_simulations == 100) 1055055 else n_simulations * 11100
    expect_equal(occurrence$coordinateUncertaintyInMeters, expected)
    expect_match(
      occurrence$eventRemarks,
      if (n_simulations == 100) "95th percentile" else "maximum"
    )
  }

  # Invalid simulated positions do not count towards the 100-simulation threshold.
  simulations$lon[1] <- NA_real_
  paths(pkg) <- bind_rows(centre, simulations)
  occurrence <- gldp_to_dwc(pkg, NULL, scientific_name_ids = ids)
  expect_equal(occurrence$coordinateUncertaintyInMeters, 1110000)
  expect_match(occurrence$eventRemarks, "maximum distance of 99")

  # Coincident simulations still retain the grid resolution; absent simulations do not.
  simulations <- centre
  simulations$type <- "simulation"
  simulations$j <- 1
  paths(pkg) <- bind_rows(centre, simulations)
  occurrence <- gldp_to_dwc(pkg, NULL, scientific_name_ids = ids)
  expect_equal(
    occurrence$coordinateUncertaintyInMeters,
    ceiling(6371008.8 * acos(cos(pi / 1440)^2))
  )
  paths(pkg) <- centre
  occurrence <- gldp_to_dwc(pkg, NULL, scientific_name_ids = ids)
  expect_true(is.na(occurrence$coordinateUncertaintyInMeters))
  expect_match(occurrence$eventRemarks, "no finite simulation distances")
})
