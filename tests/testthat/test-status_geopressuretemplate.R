library(testthat)
library(GeoLocatoR)

test_that("status_geopressuretemplate reports interim objects and tag status", {
  directory <- tempfile("template-status-")
  dir.create(file.path(directory, "data", "interim"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  tag <- structure(
    list(
      param = list(id = "AA"),
      pressure = data.frame(date = as.POSIXct("2020-01-01", tz = "UTC"), value = 1000),
      light = data.frame(date = as.POSIXct("2020-01-01", tz = "UTC"), value = 1)
    ),
    class = "tag"
  )
  pressurepath_most_likely <- data.frame()
  save(tag, pressurepath_most_likely, file = file.path(directory, "data", "interim", "AA.RData"))

  status <- status_geopressuretemplate(directory)
  expect_true(status$interim_pressurepath_most_likely)
  expect_identical(status$tag_status, "tag, read, pressure, light")
})

test_that("an empty config.yml gives no config ids", {
  file <- tempfile(fileext = ".yml")
  on.exit(unlink(file))
  file.create(file)
  expect_identical(geopressuretemplate_config_ids(file), character())
})
