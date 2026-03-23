library(testthat)
library(glue)
library(GeoLocatoR)

# Test for create_gldp function
test_that("create_gldp creates a valid GeoLocator Data Package", {
  # Test minimal input
  package <- create_gldp()
  expect_s3_class(package, "geolocatordp")
  expect_true("$schema" %in% names(package))

  # Test with additional descriptor metadata
  package_full <- create_gldp(
    title = "Geolocator Data Package example",
    id = "https://doi.org/10.5281/zenodo.13829929",
    description = "test"
  )
  expect_equal(package_full$id, "https://doi.org/10.5281/zenodo.13829929")
  expect_equal(package_full$description, "test")
})

test_that("create_gldp handles invalid inputs", {
  # Numeric id is accepted as descriptor content and converted by frictionless
  expect_s3_class(create_gldp(id = 123), "geolocatordp")

  # Unknown fields are preserved in descriptor
  pkg_custom <- create_gldp(schema = "invalid_schema_url")
  expect_equal(pkg_custom$schema, "invalid_schema_url")
})
