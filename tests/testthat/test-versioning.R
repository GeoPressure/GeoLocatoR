library(testthat)
library(GeoLocatoR)

# shared_pkg() is defined in setup.R

test_that("gldp_version returns package version", {
  v <- gldp_version(shared_pkg())

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

test_that("gldp_version reads a schema URL that uses a tag ref", {
  # The GeoLocator-DP profile accepts `refs/tags/<version>` as well as `<version>`.
  pkg <- create_gldp()
  pkg[["$schema"]] <-
    "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/refs/tags/v1.0/geolocator-dp-profile.json"

  expect_identical(gldp_version(pkg), "v1.0")
})

test_that("gldp_version rejects development schemas", {
  pkg <- create_gldp()
  pkg[["$schema"]] <-
    "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/dev/geolocator-dp-profile.json"

  expect_error(gldp_version(pkg), "Development schemas are not supported")
})

test_that("main is readable as a legacy alias but not written", {
  pkg <- create_gldp()
  pkg[["$schema"]] <-
    "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/main/geolocator-dp-profile.json"

  expect_identical(gldp_version(pkg), "main")
  expect_error(gldp_schema_url("main"), "Unsupported GeoLocator-DP version")
  expect_error(create_gldp(version = "main"), "Unsupported GeoLocator-DP version")
})
