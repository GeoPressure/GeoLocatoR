library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("check_gldp validates a valid geolocatordp", {
  expect_no_error(check_gldp(pkg_shared))
})

test_that("check_gldp errors on invalid input", {
  # Should error on non-gldp object
  expect_error(check_gldp(list()))
  expect_error(check_gldp(NULL))
  expect_error(check_gldp("not a package"))
})
