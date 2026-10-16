library(testthat)
library(GeoLocatoR)

test_that("manual tag metadata is retained when an interim tag is absent from it", {
  directory <- tempfile("template-metadata-")
  dir.create(file.path(directory, "data", "interim"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  param <- list(
    id = "AA",
    tag_create = list(manufacturer = "soi"),
    bird_create = list(scientific_name = "Upupa epops")
  )
  tag <- list(
    param = param,
    pressure = data.frame(date = as.POSIXct("2020-01-01", tz = "UTC"), value = 1000)
  )
  tag$twilight <- data.frame(
    twilight = as.POSIXct("2020-01-01 06:00:00", tz = "UTC"),
    rise = TRUE,
    label = NA_character_
  )
  tag$stap <- data.frame(
    stap_id = 1,
    start = as.POSIXct("2020-01-01", tz = "UTC"),
    end = as.POSIXct("2020-01-02", tz = "UTC")
  )
  save(param, tag, file = file.path(directory, "data", "interim", "AA.RData"))
  param$id <- "CC"
  tag$param <- param
  save(param, tag, file = file.path(directory, "data", "interim", "CC.RData"))
  write.csv(
    data.frame(
      tag_id = c("AA", "BB"),
      ring_number = c("R1", "R2"),
      scientific_name = "Upupa epops",
      manufacturer = "Swiss Ornithological Institute",
      model = "M1",
      firmware = "1",
      weight = 1,
      attachment_type = "leg harness",
      readout_method = "unknown",
      tag_comments = NA_character_
    ),
    file.path(directory, "data", "tags.csv"),
    row.names = FALSE
  )
  expect_warning(pkg <- read_geopressuretemplate(directory, from = "interim"), "missing")
  expect_setequal(tags(pkg)$tag_id, c("AA", "BB", "CC"))
  expect_identical(tags(pkg)$ring_number[tags(pkg)$tag_id == "AA"], "R1")
  expect_identical(tags(pkg)$ring_number[tags(pkg)$tag_id == "BB"], "R2")
})

test_that("incomplete interim files identify the missing variable", {
  directory <- tempfile("template-incomplete-")
  dir.create(file.path(directory, "data", "interim"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  tag <- list()
  save(tag, file = file.path(directory, "data", "interim", "AA.RData"))
  expect_error(read_geopressuretemplate(directory, from = "interim"), "missing variable.*param")
})
