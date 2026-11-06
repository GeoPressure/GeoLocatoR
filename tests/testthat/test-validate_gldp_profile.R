library(testthat)
library(GeoLocatoR)

test_that("validate_gldp_profile reports a passing profile", {
  expect_message(validate_gldp_profile(create_gldp()), "Profile checks passed")
})
