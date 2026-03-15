library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("validate_gldp validates a strict geolocatordp", {
  expect_no_error(suppressMessages({
    validate_gldp(pkg_shared)
  }))
})
