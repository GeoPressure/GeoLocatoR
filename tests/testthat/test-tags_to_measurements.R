library(testthat)
library(GeoLocatoR)

test_that("tags_to_measurements returns an empty table for tags without sensors", {
  m <- tags_to_measurements(list(list(param = list(id = "A"))))
  expect_identical(nrow(m), 0L)
  expect_named(m, c("tag_id", "sensor", "datetime", "value", "label"))
})
