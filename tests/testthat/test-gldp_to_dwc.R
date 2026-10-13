library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("gldp_to_dwc writes occurrence.csv and returns occurrence data", {
  pkg <- pkg_shared
  tmp <- withr::local_tempdir()

  suppressMessages({
    occurrence <- gldp_to_dwc(pkg, directory = tmp)
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
