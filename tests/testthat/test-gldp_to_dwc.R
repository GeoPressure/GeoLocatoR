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

test_that("default identifiers are blank and a NULL directory returns the table", {
  occurrence <- gldp_to_dwc(pkg_shared, NULL)
  expect_s3_class(occurrence, "data.frame")
  expect_gt(nrow(occurrence), 0)
  expect_true(all(is.na(occurrence$scientificNameID)))
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

test_that("gldp_to_dwc keeps datasetID for a package without id", {
  pkg <- create_gldp(title = "Example package")
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      scientific_name = "Upupa epops",
      manufacturer = "soi",
      model = "GDL3"
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "observations",
    data.frame(
      ring_number = "R1",
      tag_id = "AA",
      observation_type = "equipment",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      latitude = 46,
      longitude = 7,
      age_class = "4",
      sex = "M"
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "staps",
    data.frame(
      tag_id = "AA",
      stap_id = 1,
      start = as.POSIXct("2020-01-01", tz = "UTC"),
      end = as.POSIXct("2020-01-05", tz = "UTC")
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "paths",
    data.frame(tag_id = "AA", type = "most_likely", stap_id = 1, lat = 46, lon = 7, j = 1L)
  )

  occurrence <- gldp_to_dwc(pkg, NULL)
  expect_true("datasetID" %in% names(occurrence))
  expect_true(is.na(occurrence$datasetID))
})

# Minimal offline package with one stationary period and the given observations.
dwc_offline_pkg <- function(observations) {
  pkg <- create_gldp(title = "Example package")
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      scientific_name = "Upupa epops",
      manufacturer = "soi",
      model = "GDL3"
    )
  )
  pkg <- add_gldp_resource(pkg, "observations", observations)
  pkg <- add_gldp_resource(
    pkg,
    "staps",
    data.frame(
      tag_id = "AA",
      stap_id = c(1, 2),
      start = as.POSIXct(c("2020-01-01", "2020-01-06"), tz = "UTC"),
      end = as.POSIXct(c("2020-01-05", "2020-03-01"), tz = "UTC")
    )
  )
  add_gldp_resource(
    pkg,
    "paths",
    data.frame(tag_id = "AA", type = "most_likely", stap_id = c(1, 2), lat = 46, lon = 7, j = 1L)
  )
}

test_that("gldp_to_dwc leaves observation and uncertainty fields empty without matches", {
  # The only observation is more than 14 days from every stationary period.
  pkg <- dwc_offline_pkg(data.frame(
    ring_number = "R1",
    tag_id = "AA",
    observation_type = "equipment",
    datetime = as.POSIXct("2019-01-01", tz = "UTC"),
    latitude = 46,
    longitude = 7,
    age_class = "4",
    sex = "M"
  ))
  occurrence <- gldp_to_dwc(pkg, NULL)
  expect_equal(nrow(occurrence), 2)
  expect_true(all(is.na(occurrence$sex)))
  expect_true(all(is.na(occurrence$lifeStage)))
  expect_true(all(is.na(occurrence$coordinateUncertaintyInMeters)))
  expect_match(occurrence$eventRemarks, "^Location reconstructed")
  expect_equal(
    occurrence$dynamicProperties[1],
    '{"pathType":"most_likely","locationSource":"reconstructed"}'
  )
})

test_that("gldp_to_dwc maps sex and age class to Darwin Core vocabularies", {
  pkg <- dwc_offline_pkg(data.frame(
    ring_number = "R1",
    tag_id = "AA",
    observation_type = c("equipment", "retrieval"),
    datetime = as.POSIXct(c("2020-01-02", "2020-02-01"), tz = "UTC"),
    latitude = 46,
    longitude = 7,
    age_class = c("1", "4"),
    sex = c("U", "F")
  ))
  occurrence <- gldp_to_dwc(pkg, NULL)
  expect_equal(occurrence$sex, c(NA, "female"))
  expect_equal(occurrence$lifeStage, c("juvenile", "adult"))
  expect_equal(
    occurrence$dynamicProperties[2],
    glue::glue(
      '{{"pathType":"most_likely","locationSource":"reconstructed","euringAgeClass":"4",',
      '"observation":{{"observation_type":"retrieval"}}}}'
    )
  )

  # Unknown and undetermined full-grown age classes stay empty.
  observations <- observations(pkg)
  observations$age_class <- c("0", "2")
  observations$sex <- "M"
  observations(pkg) <- observations
  occurrence <- gldp_to_dwc(pkg, NULL)
  expect_equal(occurrence$sex, c("male", "male"))
  expect_equal(occurrence$lifeStage, c(NA_character_, NA_character_))
})
