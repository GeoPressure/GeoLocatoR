library(testthat)
library(GeoLocatoR)
library(GeoPressureR)

# shared_pkg() is defined in setup.R

# Offline package with one tag "AA" and the given measurements.
local_tag_pkg <- function(measurements) {
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      scientific_name = "Upupa epops",
      manufacturer = "soi",
      model = "GDL3"
    )
  )
  add_gldp_resource(pkg, "measurements", measurements)
}

test_that("gldp_to_tag converts temperature sensors", {
  pkg <- local_tag_pkg(data.frame(
    tag_id = "AA",
    sensor = rep(c("temperature_external", "temperature_internal"), each = 2),
    datetime = as.POSIXct("2020-01-01", tz = "UTC") + 3600 * 0:1,
    value = c(10, 11, 40, 41)
  ))
  tag <- gldp_to_tag(pkg, "AA")
  expect_equal(tag$temperature_external$value, c(10, 11))
  expect_equal(tag$temperature_internal$value, c(40, 41))
})

test_that("gldp_to_tag accepts measurements without a label column", {
  directory <- tempfile("gldp-to-tag-")
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- local_tag_pkg(data.frame(
    tag_id = "AA",
    sensor = rep(c("pressure", "activity"), each = 2),
    datetime = as.POSIXct("2020-01-01", tz = "UTC") + 3600 * 0:1,
    value = c(1000, 1001, 3, 4)
  ))
  write_gldp(pkg, directory)
  file <- file.path(directory, "measurements.csv")
  data <- read.csv(file)
  write.csv(data[names(data) != "label"], file, row.names = FALSE)

  tag <- gldp_to_tag(read_gldp(directory, force_read = FALSE), "AA")
  expect_equal(tag$pressure$value, c(1000, 1001))
  expect_equal(tag$acceleration$value, c(3, 4))
  expect_false("label" %in% names(tag$pressure))
})

test_that("gldp_to_tag keeps pressure labels when only acceleration is labelled", {
  datetime <- as.POSIXct("2020-01-01", tz = "UTC") + 3600 * 0:5
  pkg <- local_tag_pkg(rbind(
    data.frame(tag_id = "AA", sensor = "pressure", datetime = datetime, value = 1000, label = NA),
    # Acceleration starts after pressure, as in tags switched on at different times.
    data.frame(
      tag_id = "AA",
      sensor = "activity",
      datetime = datetime[2:5],
      value = 1,
      label = c("", "flight", "flight", "")
    )
  ))

  tag <- gldp_to_tag(pkg, "AA")
  expect_equal(tag$pressure$label, rep("", 6))
  expect_equal(tag$stap$stap_id, c(1, 2))
  expect_true(tag_assert(tag, "label", "logical"))
})

test_that("gldp_to_tag turns missing twilight labels into empty strings", {
  datetime <- as.POSIXct("2020-01-01", tz = "UTC") + 3600 * 0:1
  pkg <- local_tag_pkg(data.frame(tag_id = "AA", sensor = "light", datetime = datetime, value = 1))
  pkg <- add_gldp_resource(
    pkg,
    "twilights",
    data.frame(tag_id = "AA", twilight = datetime, rise = c(TRUE, FALSE), label = c(NA, "discard"))
  )

  tag <- gldp_to_tag(pkg, "AA")
  expect_equal(tag$twilight$label, c("", "discard"))
})

test_that("gldp_to_tag converts single tag correctly", {
  pkg <- shared_pkg()
  # Get the first tag_id
  tag_ids <- unique(tags(pkg)$tag_id)
  first_tag_id <- tag_ids[1]

  # Convert single tag
  expect_no_warning(tag <- gldp_to_tag(pkg, tag_id = first_tag_id))

  # Check that result is a tag object
  expect_s3_class(tag, "tag")

  # Check that param exists and has correct id
  expect_true("param" %in% names(tag))
  expect_equal(tag$param$id, first_tag_id)
  expect_equal(tag$param$tag_create$manufacturer, "datapackage")

  # The shared record has pressure, light, acceleration, staps and twilights for every tag
  expect_true(is.data.frame(tag$pressure))
  expect_true(all(c("date", "value") %in% names(tag$pressure)))
  expect_s3_class(tag$pressure$date, "POSIXct")
  expect_equal(attr(tag$pressure$date, "tzone"), "UTC")

  expect_true(is.data.frame(tag$light))
  expect_true(all(c("date", "value") %in% names(tag$light)))
  expect_s3_class(tag$light$date, "POSIXct")

  expect_true(is.data.frame(tag$acceleration))
  expect_true(all(c("date", "value") %in% names(tag$acceleration)))
  expect_s3_class(tag$acceleration$date, "POSIXct")

  expect_true(is.data.frame(tag$stap))
  expect_true(all(c("stap_id", "start", "end") %in% names(tag$stap)))
  expect_s3_class(tag$stap$start, "POSIXct")
  expect_s3_class(tag$stap$end, "POSIXct")
  expect_equal(attr(tag$stap$start, "tzone"), "UTC")
  expect_equal(attr(tag$stap$end, "tzone"), "UTC")

  expect_true(is.data.frame(tag$twilight))
  expect_true(all(c("twilight", "rise") %in% names(tag$twilight)))
  expect_s3_class(tag$twilight$twilight, "POSIXct")
  expect_equal(attr(tag$twilight$twilight, "tzone"), "UTC")
  expect_type(tag$twilight$rise, "logical")
})

test_that("gldp_to_tag converts all tags when tag_id is NULL", {
  pkg <- shared_pkg()
  # Get all tag_ids
  tag_ids <- unique(tags(pkg)$tag_id)

  # Convert all tags
  result <- gldp_to_tag(pkg, tag_id = NULL)

  # The shared record has two tags, so a named list is returned
  expect_type(result, "list")
  expect_equal(length(result), length(tag_ids))
  expect_equal(names(result), tag_ids)
  for (i in seq_along(result)) {
    expect_s3_class(result[[i]], "tag")
    expect_equal(result[[i]]$param$id, tag_ids[i])
  }
})

test_that("gldp_to_tag converts multiple specific tags", {
  pkg <- shared_pkg()
  # Get all tag_ids
  tag_ids <- unique(tags(pkg)$tag_id)

  # Skip if only one tag
  skip_if(length(tag_ids) < 2, "Package has less than 2 tags")

  # Select first two tags
  selected_ids <- tag_ids[1:2]

  # Convert selected tags
  result <- gldp_to_tag(pkg, tag_id = selected_ids)

  # Should return list
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_equal(names(result), selected_ids)

  # Check each tag
  expect_s3_class(result[[1]], "tag")
  expect_s3_class(result[[2]], "tag")
})

test_that("gldp_to_tag errors on invalid tag_id", {
  pkg <- shared_pkg()
  # Try to convert non-existent tag
  expect_error(
    gldp_to_tag(pkg, tag_id = "INVALID_TAG_ID"),
    "Tag ID\\(s\\) not found in package"
  )
})

test_that("gldp_to_tag works with GeoPressureR functions", {
  pkg <- shared_pkg()
  # Get the first tag_id
  tag_ids <- unique(tags(pkg)$tag_id)
  first_tag_id <- tag_ids[1]

  # Convert to tag
  tag <- gldp_to_tag(pkg, tag_id = first_tag_id)

  # Test that tag_assert works (from GeoPressureR)
  expect_no_error(GeoPressureR::tag_assert(tag, "tag"))

  # Test print method
  suppressMessages({
    expect_no_error(print(tag))
  })
})

test_that("gldp_to_tag reuses saved GeoPressureR parameters", {
  pkg <- shared_pkg()
  tag_id <- unique(tags(pkg)$tag_id)[1]
  param <- param_create(tag_id, default = TRUE)
  param$tag_create$manufacturer <- "migratetech"
  param$tag_set_map$scale <- 1.5
  pkg$params <- list(param)

  tag <- gldp_to_tag(pkg, tag_id)

  expect_identical(tag$param, param)
  expect_equal(tag$param$tag_create$manufacturer, "migratetech")
  expect_equal(tag$param$tag_set_map$scale, 1.5)
})

test_that("gldp_to_tag extracts stap and twilight data correctly", {
  pkg <- shared_pkg()

  first_tag_id <- unique(tags(pkg)$tag_id)[1]
  tag <- gldp_to_tag(pkg, tag_id = first_tag_id)

  # Staps of the tag, without tag_id
  stap_for_tag <- staps(pkg) |> dplyr::filter(.data$tag_id == first_tag_id)
  expect_gt(nrow(stap_for_tag), 0)
  expect_equal(nrow(tag$stap), nrow(stap_for_tag))
  expect_false("tag_id" %in% names(tag$stap))
  expect_true(all(c("stap_id", "start", "end") %in% names(tag$stap)))
  expect_equal(sort(tag$stap$stap_id), sort(stap_for_tag$stap_id))

  # Twilights of the tag, without tag_id
  twl_for_tag <- twilights(pkg) |> dplyr::filter(.data$tag_id == first_tag_id)
  expect_gt(nrow(twl_for_tag), 0)
  expect_equal(nrow(tag$twilight), nrow(twl_for_tag))
  expect_false("tag_id" %in% names(tag$twilight))
  expect_true(all(c("twilight", "rise") %in% names(tag$twilight)))
  expect_equal(sort(tag$twilight$rise), sort(twl_for_tag$rise))
})

test_that("gldp_to_tag keeps mean_acceleration_z in acceleration", {
  pkg <- shared_pkg()

  resource_idx <- which(vapply(pkg$resources, \(r) identical(r$name, "measurements"), logical(1)))
  skip_if(length(resource_idx) == 0, "Package has no measurements resource")

  m <- pkg$resources[[resource_idx[1]]]$data
  skip_if(!is.data.frame(m) || nrow(m) == 0, "Measurements table is empty")
  skip_if(
    !all(c("tag_id", "sensor", "value", "datetime") %in% names(m)),
    "Measurements table is missing required columns"
  )

  target_tag_id <- unique(tags(pkg)$tag_id)[1]
  marker <- 987654.321

  row <- m[1, , drop = FALSE]
  row$tag_id <- target_tag_id
  row$sensor <- "mean_acceleration_z"
  row$value <- marker
  datetimes <- m$datetime[!is.na(m$datetime)]
  row$datetime <- if (length(datetimes) > 0) {
    max(datetimes) + 1
  } else {
    as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  }
  if ("label" %in% names(row)) {
    row$label <- NA_character_
  }

  pkg$resources[[resource_idx[1]]]$data <- dplyr::bind_rows(m, row)

  tag <- gldp_to_tag(pkg, tag_id = target_tag_id)
  expect_true("acceleration" %in% names(tag))
  expect_true("mean_acceleration_z" %in% names(tag$acceleration))
  expect_true(any(tag$acceleration$mean_acceleration_z == marker, na.rm = TRUE))
})

test_that("gldp_to_tag combines magnetic and acceleration axes", {
  pkg <- shared_pkg()
  resource_idx <- which(vapply(pkg$resources, \(r) identical(r$name, "measurements"), logical(1)))
  m <- pkg$resources[[resource_idx[1]]]$data
  tag_id <- unique(tags(pkg)$tag_id)[1]
  datetime <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  axes <- c(
    "magnetic_x",
    "magnetic_y",
    "magnetic_z",
    "acceleration_x",
    "acceleration_y",
    "acceleration_z"
  )
  template <- m[1, , drop = FALSE]

  axis_data <- purrr::imap_dfr(axes, \(sensor, i) {
    row <- template
    row$tag_id <- tag_id
    row$sensor <- sensor
    row$datetime <- datetime
    row$value <- i
    if ("label" %in% names(row)) {
      row$label <- NA_character_
    }
    row
  })
  pkg$resources[[resource_idx[1]]]$data <- dplyr::bind_rows(m, axis_data)

  tag <- gldp_to_tag(pkg, tag_id)
  expect_true("magnetic" %in% names(tag))
  expect_true(all(c("date", axes) %in% names(tag$magnetic)))
  expect_equal(unname(unlist(tag$magnetic[1, axes])), seq_along(axes))
})

test_that("gldp_to_tag preserves pressure and acceleration labels", {
  pkg <- shared_pkg()
  resource_idx <- which(vapply(pkg$resources, \(r) identical(r$name, "measurements"), logical(1)))
  skip_if(length(resource_idx) == 0, "Package has no measurements resource")

  m <- pkg$resources[[resource_idx[1]]]$data
  tag_id <- unique(tags(pkg)$tag_id)[1]
  datetime <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  template <- m[1, , drop = FALSE]

  pressure <- template
  pressure$tag_id <- tag_id
  pressure$sensor <- "pressure"
  pressure$datetime <- datetime
  pressure$value <- 1000
  pressure$label <- "flight"

  activity <- template
  activity$tag_id <- tag_id
  activity$sensor <- "activity"
  activity$datetime <- datetime
  activity$value <- 1
  activity$label <- "discard"

  mean_acceleration <- template
  mean_acceleration$tag_id <- tag_id
  mean_acceleration$sensor <- "mean_acceleration_z"
  mean_acceleration$datetime <- datetime
  mean_acceleration$value <- 2
  mean_acceleration$label <- "flight"

  unlabelled_pressure <- pressure
  unlabelled_pressure$datetime <- datetime + 60
  unlabelled_pressure$label <- NA_character_

  unlabelled_activity <- activity
  unlabelled_activity$datetime <- datetime + 60
  unlabelled_activity$label <- NA_character_

  pkg$resources[[resource_idx[1]]]$data <- dplyr::bind_rows(
    m,
    pressure,
    activity,
    mean_acceleration,
    unlabelled_pressure,
    unlabelled_activity
  )
  tag <- gldp_to_tag(pkg, tag_id)

  expect_equal(tag$pressure$label[tag$pressure$date == datetime], "flight")
  expect_equal(tag$acceleration$label[tag$acceleration$date == datetime], "discard")
  expect_equal(tag$pressure$label[tag$pressure$date == datetime + 60], "")
  expect_equal(tag$acceleration$label[tag$acceleration$date == datetime + 60], "")
  expect_true("stap_id" %in% names(tag$pressure))
  expect_true("stap_id" %in% names(tag$acceleration))
})

test_that("gldp_to_tag omits empty label columns", {
  pkg <- local_tag_pkg(data.frame(
    tag_id = "AA",
    sensor = rep(c("pressure", "activity"), each = 2),
    datetime = as.POSIXct("2020-01-01", tz = "UTC") + 3600 * 0:1,
    value = c(1000, 1001, 3, 4),
    label = NA_character_
  ))
  tag <- gldp_to_tag(pkg, "AA")
  expect_false("label" %in% names(tag$pressure))
  expect_false("label" %in% names(tag$acceleration))
})
