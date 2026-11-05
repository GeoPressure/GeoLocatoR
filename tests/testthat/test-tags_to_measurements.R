library(testthat)
library(GeoLocatoR)

test_that("tags_to_measurements returns an empty table for tags without sensors", {
  m <- tags_to_measurements(list(list(param = list(id = "A"))))
  expect_identical(nrow(m), 0L)
  expect_named(m, c("tag_id", "sensor", "datetime", "value", "label"))
})

test_that("tags_to_measurements still accepts the deprecated tags argument", {
  tag_list <- list(list(param = list(id = "A")))
  expect_warning(m <- tags_to_measurements(tags = tag_list), class = "lifecycle_warning_deprecated")
  expect_identical(m, tags_to_measurements(tag_list = tag_list))
})

test_that("tags_to_measurements reads the temperature of older GeoPressureR tags as external", {
  d <- as.POSIXct("2020-01-01", tz = "UTC") + 3600 * (0:1)
  tag <- list(param = list(id = "A"), temperature = data.frame(date = d, value = c(20, 21)))
  m <- tags_to_measurements(list(tag))
  expect_identical(unique(m$sensor), "temperature_external")
  expect_identical(m$value, c(20, 21))
})
