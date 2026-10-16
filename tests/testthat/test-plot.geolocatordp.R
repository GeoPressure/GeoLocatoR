library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("plot.geolocatordp creates coverage plot", {
  pkg <- pkg_shared

  # Coverage plot
  expect_no_error({
    p <- plot(pkg, "coverage")
  })
  expect_s3_class(p, "ggplot")
})

test_that("plot.geolocatordp creates ring plot", {
  pkg <- pkg_shared

  # Ring plot
  expect_no_error({
    p <- plot(pkg, "ring")
  })
  expect_s3_class(p, "ggplot")
})

test_that("plot.geolocatordp creates map plot", {
  pkg <- pkg_shared

  # Map plot
  expect_no_error({
    p <- plot(pkg, "map")
  })
  # Map plot might be leaflet or ggplot depending on implementation
})

test_that("plot.geolocatordp uses default type when not specified", {
  pkg <- pkg_shared

  # Default plot
  expect_no_error(plot(pkg))
})

test_that("plot.geolocatordp explains which resources a plot type needs", {
  pkg <- pkg_shared
  pkg$resources <- purrr::keep(pkg$resources, \(r) r$name != "measurements")

  expect_error(plot(pkg, "coverage"), "needs .*measurements")
  expect_error(plot(pkg, "ring"), "needs .*measurements")
  expect_error(plot(pkg, "unknown"), "Unsupported plot type")
})
