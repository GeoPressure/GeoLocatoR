library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("tags getter and setter work correctly", {
  pkg <- pkg_shared

  tag_data <- tags(pkg)
  expect_true(is.data.frame(tag_data))
  expect_true("tag_id" %in% names(tag_data))

  expect_no_error({
    tags(pkg) <- tag_data
  })
})

test_that("observations getter and setter work correctly", {
  pkg <- pkg_shared

  skip_if_not(
    "observations" %in% frictionless::resources(pkg),
    "Package has no observations resource"
  )

  obs_data <- observations(pkg)
  expect_true(is.data.frame(obs_data))

  expect_no_error({
    observations(pkg) <- obs_data
  })
})

test_that("measurements getter and setter work correctly", {
  pkg <- pkg_shared

  skip_if_not(
    "measurements" %in% frictionless::resources(pkg),
    "Package has no measurements resource"
  )

  meas_data <- measurements(pkg)
  expect_true(is.data.frame(meas_data))

  expect_no_error({
    measurements(pkg) <- meas_data
  })
})

test_that("staps getter and setter work correctly", {
  pkg <- pkg_shared

  skip_if_not(
    "staps" %in% frictionless::resources(pkg),
    "Package has no staps resource"
  )

  staps_data <- staps(pkg)
  expect_true(is.data.frame(staps_data))

  expect_no_error({
    staps(pkg) <- staps_data
  })
})

test_that("twilights getter and setter work correctly", {
  pkg <- pkg_shared

  skip_if_not(
    "twilights" %in% frictionless::resources(pkg),
    "Package has no twilights resource"
  )

  twl_data <- twilights(pkg)
  expect_true(is.data.frame(twl_data))

  expect_no_error({
    twilights(pkg) <- twl_data
  })
})

test_that("paths getter and setter work correctly", {
  pkg <- pkg_shared

  skip_if_not(
    "paths" %in% frictionless::resources(pkg),
    "Package has no paths resource"
  )

  paths_data <- paths(pkg)
  expect_true(is.data.frame(paths_data))

  expect_no_error({
    paths(pkg) <- paths_data
  })
})

test_that("edges getter and setter work correctly", {
  pkg <- pkg_shared

  skip_if_not(
    "edges" %in% frictionless::resources(pkg),
    "Package has no edges resource"
  )

  edges_data <- edges(pkg)
  expect_true(is.data.frame(edges_data))

  expect_no_error({
    edges(pkg) <- edges_data
  })
})

test_that("pressurepaths getter and setter work correctly", {
  pkg <- pkg_shared

  skip_if_not(
    "pressurepaths" %in% frictionless::resources(pkg),
    "Package has no pressurepaths resource"
  )

  pp_data <- pressurepaths(pkg)
  expect_true(is.data.frame(pp_data))
  expect_s3_class(pp_data$datetime, "POSIXct")

  expect_no_error({
    pressurepaths(pkg) <- pp_data
  })

  pp_data_roundtrip <- pressurepaths(pkg)
  expect_s3_class(pp_data_roundtrip$datetime, "POSIXct")
  expect_equal(attr(pp_data_roundtrip$datetime, "tzone"), "UTC")
})

test_that("params is handled as a plain pkg property", {
  pkg <- create_gldp()
  param_data <- list(list(id = "TEST-PARAM", method = "GeoPressureR"))

  expect_no_error({
    pkg$params <- param_data
  })
  expect_true(is.list(pkg$params))
  expect_equal(pkg$params[[1]]$id, "TEST-PARAM")
})
