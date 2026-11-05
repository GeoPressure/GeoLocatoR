library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

# A small offline package with tags, observations and measurements
local_plot_pkg <- function() {
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    tibble(
      tag_id = c("T1", "T2"),
      ring_number = c("R1", "R2"),
      scientific_name = "Upupa epops",
      manufacturer = "Swiss Ornithological Institute",
      model = "GDL3"
    ),
    cast_type = TRUE
  )
  pkg <- add_gldp_resource(
    pkg,
    "observations",
    tibble(
      ring_number = c("R1", "R2"),
      tag_id = c("T1", "T2"),
      observation_type = "equipment",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      latitude = 46,
      longitude = 7,
      age_class = "0",
      sex = "U"
    ),
    cast_type = TRUE
  )
  add_gldp_resource(
    pkg,
    "measurements",
    tibble(
      tag_id = rep(c("T1", "T2"), each = 4),
      sensor = rep(c("pressure", "activity", "mean_acceleration_z", "magnetic_x"), 2),
      datetime = as.POSIXct("2020-02-01", tz = "UTC") + rep(0:3, 2) * 86400,
      value = 1
    ),
    cast_type = TRUE
  )
}

drop_resource <- function(pkg, name) {
  pkg$resources <- purrr::keep(pkg$resources, \(r) r$name != name)
  pkg
}

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

test_that("plot.geolocatordp plots coverage without observations, and asks tags for a ring plot", {
  pkg <- local_plot_pkg()

  p <- plot(drop_resource(pkg, "observations"))
  expect_no_warning(ggplot2::ggplot_build(p))
  expect_error(plot(drop_resource(pkg, "tags"), "ring"), "needs .*tags")
})

test_that("plot.geolocatordp shows the mean z acceleration with activity", {
  p <- plot(local_plot_pkg(), "coverage")

  expect_setequal(unique(p$data$sensor), c("pressure", "activity", "magnetic"))
  activity_dates <- p$data$date[p$data$tag_id == "T1" & p$data$sensor == "activity"]
  expect_identical(sort(activity_dates), as.Date(c("2020-02-02", "2020-02-03")))
})

test_that("plot.geolocatordp colours days with data alike with and without empty days", {
  pkg <- local_plot_pkg()
  m <- measurements(pkg)
  fill_with_data <- function(pkg) {
    b <- ggplot2::ggplot_build(plot(pkg, "coverage"))
    unique(b$data[[1]]$fill[b$plot$data$has_data])
  }

  expect_identical(fill_with_data(pkg), "black")
  m$value[1] <- NA
  pkg <- add_gldp_resource(pkg, "measurements", m, replace = TRUE)
  expect_identical(fill_with_data(pkg), "black")
})

test_that("plot.geolocatordp draws the ring plot measurement periods in black", {
  b <- ggplot2::ggplot_build(plot(local_plot_pkg(), "ring"))

  expect_identical(unique(b$data[[1]]$colour), "black")
  expect_identical(b$data[[1]]$yend, b$data[[1]]$y)
})
