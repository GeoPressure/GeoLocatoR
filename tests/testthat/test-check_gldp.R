library(testthat)
library(GeoLocatoR)

# shared_pkg() is defined in setup.R

test_that("check_gldp validates a valid geolocatordp", {
  expect_no_error(check_gldp(shared_pkg()))
})

test_that("check_gldp errors on invalid input", {
  # Should error on non-gldp object
  expect_error(check_gldp(list()))
  expect_error(check_gldp(NULL))
  expect_error(check_gldp("not a package"))
})
