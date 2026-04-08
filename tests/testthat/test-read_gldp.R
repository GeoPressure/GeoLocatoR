library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("read_gldp reads a local datapackage.json", {
  path <- file.path(withr::local_tempdir(), "datapackage.json")

  jsonlite::write_json(
    list(
      `$schema` = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v1.0/geolocator-dp-profile.json",
      id = "unit-test",
      resources = list()
    ),
    path,
    auto_unbox = TRUE
  )

  pkg <- suppressWarnings(read_gldp(path))

  expect_s3_class(pkg, "geolocatordp")
  expect_true("resources" %in% names(pkg))
  expect_equal(gldp_version(pkg), "v1.0")
})

test_that("read_gldp handles invalid input", {
  # Test with non-existent file
  expect_error(read_gldp("nonexistent.json"))

  # Test with non-json content
  tmp <- withr::local_tempdir()
  txt_path <- file.path(tmp, "not-json.txt")
  writeLines("not json", txt_path)
  expect_error(read_gldp(txt_path))
})

test_that("read_gldp schema version extraction supports short and explicit refs", {
  pkg <- function(schema) {
    p <- create_gldp()
    p$`$schema` <- schema
    p
  }

  expect_equal(
    gldp_version(pkg(
      "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v0.4/geolocator-dp-profile.json"
    )),
    "v0.4"
  )

  expect_error(gldp_version(pkg("https://example.com/schema.json")))
})

test_that("read_gldp canonicalizes legacy schema owners", {
  path <- file.path(withr::local_tempdir(), "datapackage.json")

  jsonlite::write_json(
    list(
      `$schema` = "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/v1.0/geolocator-dp-profile.json",
      id = "unit-test",
      resources = list(
        list(
          name = "tags",
          path = "tags.csv",
          `$schema` = "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/v1.0/tags-table-schema.json"
        )
      )
    ),
    path,
    auto_unbox = TRUE
  )

  writeLines("tag_id\nid-1", file.path(dirname(path), "tags.csv"))

  pkg <- suppressWarnings(read_gldp(path, force_read = FALSE))

  expect_equal(
    pkg$`$schema`,
    "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v1.0/geolocator-dp-profile.json"
  )
})

test_that("write_gldp writes a datapackage readable by read_gldp", {
  pkg <- pkg_shared

  out_dir <- withr::local_tempdir()
  expect_no_error(write_gldp(pkg, directory = out_dir))
  expect_true(file.exists(file.path(out_dir, "datapackage.json")))

  pkg_back <- suppressWarnings(read_gldp(file.path(out_dir, "datapackage.json")))
  expect_s3_class(pkg_back, "geolocatordp")
  expect_equal(pkg_back$id, pkg$id)
})

test_that("write_gldp/read_gldp roundtrip keeps pkg$params via params.json", {
  pkg <- pkg_shared
  param <- GeoPressureR::param_create("TEST-PARAM", default = TRUE)
  pkg[["params"]] <- list(param)

  out_dir <- withr::local_tempdir()
  expect_no_error(write_gldp(pkg, directory = out_dir))
  expect_true(file.exists(file.path(out_dir, "params.json")))

  pkg_back <- suppressWarnings(read_gldp(file.path(out_dir, "datapackage.json"), force_read = TRUE))
  expect_true(is.list(pkg_back$params))
  expect_equal(pkg_back$params[[1]]$id, "TEST-PARAM")
})
