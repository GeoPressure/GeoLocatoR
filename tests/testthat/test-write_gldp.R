library(testthat)
library(GeoLocatoR)
library(frictionless)
library(jsonlite)
library(withr)

# Write `params` with a minimal package and read them back.
params_round_trip <- function(params) {
  directory <- local_tempdir(.local_envir = parent.frame())
  pkg <- add_gldp_resource(create_gldp(), "tags", data.frame(tag_id = "A", ring_number = "R1"))
  pkg[["params"]] <- params
  write_gldp(pkg, directory)
  read_gldp(directory)
}

test_that("write_gldp writes params as plain JSON in a declared params resource", {
  pkg <- params_round_trip(list(list(id = "A", tag_create = list(manufacturer = "soi"))))
  directory <- local_tempdir()
  write_gldp(pkg, directory)

  json <- read_json(file.path(directory, "params.json"))
  expect_identical(json, list(list(id = "A", tag_create = list(manufacturer = "soi"))))
  descriptor <- read_json(file.path(directory, "datapackage.json"))
  expect_identical(
    Find(\(r) r$name == "params", descriptor$resources),
    list(name = "params", path = "params.json")
  )
  # The resource is declared once, and kept out of the package in memory.
  expect_identical(resource_names(pkg), "tags")
  write_gldp(read_gldp(directory), directory)
  descriptor <- read_json(file.path(directory, "datapackage.json"))
  expect_identical(sum(vapply(descriptor$resources, \(r) r$name, "") == "params"), 1L)
})

test_that("declaring the params resource leaves the rest of datapackage.json unchanged", {
  pkg <- add_gldp_resource(create_gldp(), "tags", data.frame(tag_id = "A", ring_number = "R1"))
  without <- local_tempdir()
  with <- local_tempdir()
  write_gldp(pkg, without)
  pkg[["params"]] <- list(list(id = "A"))
  write_gldp(pkg, with)

  descriptor <- read_json(file.path(with, "datapackage.json"))
  descriptor$resources <- Filter(\(r) r$name != "params", descriptor$resources)
  expect_identical(descriptor, read_json(file.path(without, "datapackage.json")))
})

test_that("params of a GeoPressureR interim file survive a write/read round trip", {
  interim <- new.env()
  load(
    system.file("extdata/data/interim/18LX.RData", package = "GeoPressureR"),
    envir = interim
  )
  param <- interim$param
  back <- params_round_trip(list(param))$params[[1]]

  # Data frames, NULL values, integer and double types and vector lengths are kept.
  expect_identical(back$tag_set_map$known, param$tag_set_map$known)
  expect_identical(back$tag_set_map$extent, param$tag_set_map$extent)
  expect_identical(back$tag_set_map$scale, param$tag_set_map$scale)
  expect_true("time_reference" %in% names(back$tag_create))
  expect_null(back$tag_create$time_reference)
  expect_identical(back$graph_create, param$graph_create)
  expect_identical(back$tag_create$crop_start, param$tag_create$crop_start)
  expect_identical(names(back), names(unclass(param)))
  # The twilight calibration GeoPressureR uses again is kept to 15 significant digits.
  expect_equal(back$geolight_map$twl_calib$x, param$geolight_map$twl_calib$x, tolerance = 1e-14)
  expect_equal(back$geolight_map$twl_calib$y, param$geolight_map$twl_calib$y, tolerance = 1e-14)
  expect_equal(back$twilight_create, param$twilight_create, tolerance = 1e-14)

  # Lossy by design: JSON has no version, function or time; Inf is restored.
  expect_identical(back$GeoPressureR_version, as.character(param$GeoPressureR_version))
  expect_type(back$geolight_map$twl_llp, "character")
  expect_identical(back$geolight_map$fitted_location_duration, Inf)
  expect_type(back$geolight_map$twl_calib$calib_stap$start, "character")

  # The tag GeoPressureR gets back holds the same parameters.
  pkg <- add_gldp_resource(
    create_gldp(),
    "tags",
    data.frame(tag_id = "18LX", ring_number = "R1")
  )
  pkg <- add_gldp_resource(
    pkg,
    "measurements",
    data.frame(
      tag_id = "18LX",
      sensor = "pressure",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      value = 1000
    )
  )
  pkg[["params"]] <- list(back)
  tag <- gldp_to_tag(pkg, "18LX")
  expect_s3_class(tag$param, "param")
  expect_identical(tag$param$tag_set_map$known, param$tag_set_map$known)
})

test_that("read_gldp reads params.json written by GeoLocatoR < 1.5.0", {
  directory <- local_tempdir()
  param <- list(id = "A", GeoPressureR_version = package_version("3.6.3"), x = c(a = 1))
  pkg <- add_gldp_resource(create_gldp(), "tags", data.frame(tag_id = "A", ring_number = "R1"))
  write_gldp(pkg, directory)
  writeLines(serializeJSON(list(param)), file.path(directory, "params.json"))

  expect_identical(read_gldp(directory)$params, list(param))
})

test_that("params of a Zenodo package survive a write/read round trip", {
  skip_on_cran()
  skip_if_offline()
  # Tawny Pipits from Czechia: GeoPressureR 3.5.4 params, written with serializeJSON().
  pkg <- suppressMessages(read_zenodo("21376505"))
  directory <- local_tempdir()
  write_gldp(pkg, directory)
  back <- read_gldp(directory)

  expect_identical(length(back$params), length(pkg$params))
  Map(back$params, pkg$params, f = \(b, p) {
    expect_identical(names(b), names(unclass(p)))
    expect_identical(b$tag_set_map$known, p$tag_set_map$known)
    expect_identical(b$tag_create$mag_axis, p$tag_create$mag_axis)
    expect_identical(b$tag_create$settings, p$tag_create$settings)
  })
  expect_equal(measurements(back), measurements(pkg))
  tag_id <- back$params[[1]]$id
  expect_identical(
    gldp_to_tag(back, tag_id)$param$tag_set_map,
    unclass(gldp_to_tag(pkg, tag_id)$param)$tag_set_map
  )
})
