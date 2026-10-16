library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("gldp_version returns package version", {
  v <- gldp_version(pkg_shared)

  expect_type(v, "character")
  expect_length(v, 1)
})

test_that("unsupported versions report the requested and supported versions", {
  pkg <- create_gldp()
  pkg[["$schema"]] <- "https://example.org/GeoLocator-DP/v99/geolocator-dp-profile.json"

  expect_error(gldp_version(pkg), "Unsupported GeoLocator-DP version:.*v99")
  expect_error(gldp_version(pkg), "Supported versions are:.*v1.1")
  expect_error(gldp_schema_url("v99"), "Allowed versions are.*v1.1")
  pkg[["$schema"]] <- gldp_schema_url("v1.1")
  expect_error(upgrade_gldp(pkg, to_version = "v99"), "Allowed versions are.*v1.1")
})
