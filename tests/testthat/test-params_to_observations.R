library(testthat)
library(GeoLocatoR)
library(GeoPressureR)

test_that("params_to_observations recycles known positions read from config.yml", {
  param <- list(
    id = "A",
    tag_set_map = list(known = list(stap_id = c(1, -1), known_lon = 17, known_lat = 48))
  )
  o <- params_to_observations(list(param))
  expect_equal(o$longitude, c(17, 17))
  expect_equal(o$latitude, c(48, 48))
})

test_that("params_to_observations accepts the default known of GeoPressureR", {
  param <- list(id = "A", tag_set_map = list(known = formals(tag_set_map)$known))
  o <- params_to_observations(list(param))
  expect_equal(o$longitude, c(NA_real_, NA_real_))
})
