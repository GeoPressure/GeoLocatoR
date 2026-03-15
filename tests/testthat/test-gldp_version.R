library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("gldp_version returns package version", {
  v <- gldp_version(pkg_shared)

  expect_type(v, "character")
  expect_length(v, 1)
})
