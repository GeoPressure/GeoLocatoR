library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("gldp_to_dwc writes occurrence.csv and returns occurrence data", {
  pkg <- pkg_shared
  tmp <- withr::local_tempdir()

  suppressMessages({
    occurrence <- gldp_to_dwc(pkg, directory = tmp, path_type = "most_likely")
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
    "datasetName",
    "basisOfRecord",
    "eventDate",
    "decimalLatitude",
    "decimalLongitude",
    "geodeticDatum",
    "scientificName",
    "organismID",
    "occurrenceID",
    "samplingProtocol",
    "samplingEffort",
    "individualCount",
    "occurrenceStatus",
    "dynamicProperties",
    "coordinateUncertaintyInMeters",
    "sex",
    "lifeStage"
  )
  expect_true(all(required_cols %in% names(occurrence)))

  # Key fields are set consistently from package metadata
  expect_true(all(occurrence$datasetName == pkg$title))
  expect_true(all(occurrence$datasetID == pkg$id))
  expect_true(all(occurrence$basisOfRecord == "MachineObservation"))
  expect_true(all(occurrence$geodeticDatum == "EPSG:4326"))
  expect_true(all(occurrence$samplingProtocol == "geolocator"))
  expect_true(all(occurrence$individualCount == 1L))
  expect_true(all(occurrence$occurrenceStatus == "present"))

  # Coordinates look numeric
  expect_type(occurrence$decimalLatitude, "double")
  expect_type(occurrence$decimalLongitude, "double")
})

test_that("gldp_to_dwc works for all valid path_type values", {
  suppressMessages({
    occ_mean <- expect_no_error(
      gldp_to_dwc(pkg_shared, directory = withr::local_tempdir(), path_type = "mean_simulation")
    )
  })
  suppressMessages({
    occ_median <- expect_no_error(
      gldp_to_dwc(pkg_shared, directory = withr::local_tempdir(), path_type = "median_simulation")
    )
  })

  expect_error(
    gldp_to_dwc(pkg_shared, directory = tmp, path_type = "invalid_type"),
    "path_type"
  )
})
