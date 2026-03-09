library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("tags getter and setter work correctly", {
  pkg <- pkg_shared

  # Get tags
  tag_data <- tags(pkg)
  expect_true(is.data.frame(tag_data))
  expect_true("tag_id" %in% names(tag_data))

  # Set tags (should not error)
  expect_no_error({
    tags(pkg) <- tag_data
  })
})

test_that("observations getter and setter work correctly", {
  pkg <- pkg_shared

  # Skip if no observations
  skip_if_not(
    "observations" %in% frictionless::resources(pkg),
    "Package has no observations resource"
  )

  # Get observations
  obs_data <- observations(pkg)
  expect_true(is.data.frame(obs_data))

  # Set observations (should not error)
  expect_no_error({
    observations(pkg) <- obs_data
  })
})

test_that("measurements getter and setter work correctly", {
  pkg <- pkg_shared

  # Skip if no measurements
  skip_if_not(
    "measurements" %in% frictionless::resources(pkg),
    "Package has no measurements resource"
  )

  # Get measurements
  meas_data <- measurements(pkg)
  expect_true(is.data.frame(meas_data))

  # Set measurements (should not error)
  expect_no_error({
    measurements(pkg) <- meas_data
  })
})

test_that("staps getter and setter work correctly", {
  pkg <- pkg_shared

  # Skip if no staps
  skip_if_not(
    "staps" %in% frictionless::resources(pkg),
    "Package has no staps resource"
  )

  # Get staps
  staps_data <- staps(pkg)
  expect_true(is.data.frame(staps_data))

  # Set staps (should not error)
  expect_no_error({
    staps(pkg) <- staps_data
  })
})

test_that("twilights getter and setter work correctly", {
  pkg <- pkg_shared

  # Skip if no twilights
  skip_if_not(
    "twilights" %in% frictionless::resources(pkg),
    "Package has no twilights resource"
  )

  # Get twilights
  twl_data <- twilights(pkg)
  expect_true(is.data.frame(twl_data))

  # Set twilights (should not error)
  expect_no_error({
    twilights(pkg) <- twl_data
  })
})

test_that("paths getter and setter work correctly", {
  pkg <- pkg_shared

  # Skip if no paths
  skip_if_not(
    "paths" %in% frictionless::resources(pkg),
    "Package has no paths resource"
  )

  # Get paths
  paths_data <- paths(pkg)
  expect_true(is.data.frame(paths_data))

  # Set paths (should not error)
  expect_no_error({
    paths(pkg) <- paths_data
  })
})

test_that("edges getter and setter work correctly", {
  pkg <- pkg_shared

  # Skip if no edges
  skip_if_not(
    "edges" %in% frictionless::resources(pkg),
    "Package has no edges resource"
  )

  # Get edges
  edges_data <- edges(pkg)
  expect_true(is.data.frame(edges_data))

  # Set edges (should not error)
  expect_no_error({
    edges(pkg) <- edges_data
  })
})

test_that("pressurepaths getter and setter work correctly", {
  pkg <- pkg_shared

  # Skip if no pressurepaths
  skip_if_not(
    "pressurepaths" %in% frictionless::resources(pkg),
    "Package has no pressurepaths resource"
  )

  # Get pressurepaths
  pp_data <- pressurepaths(pkg)
  expect_true(is.data.frame(pp_data))
  expect_s3_class(pp_data$datetime, "POSIXct")

  # Set pressurepaths (should not error)
  expect_no_error({
    pressurepaths(pkg) <- pp_data
  })

  pp_data_roundtrip <- pressurepaths(pkg)
  expect_s3_class(pp_data_roundtrip$datetime, "POSIXct")
  expect_equal(attr(pp_data_roundtrip$datetime, "tzone"), "UTC")
})
