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
