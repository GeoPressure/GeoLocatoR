library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("gldp_to_tag converts single tag correctly", {
  pkg <- pkg_shared
  # Get the first tag_id
  tag_ids <- unique(tags(pkg)$tag_id)
  first_tag_id <- tag_ids[1]

  # Convert single tag
  tag <- gldp_to_tag(pkg, tag_id = first_tag_id)

  # Check that result is a tag object
  expect_s3_class(tag, "tag")

  # Check that param exists and has correct id
  expect_true("param" %in% names(tag))
  expect_equal(tag$param$id, first_tag_id)
  expect_equal(tag$param$tag_create$manufacturer, "datapackage")

  # Check that at least one sensor data exists
  sensor_names <- c(
    "pressure",
    "light",
    "acceleration",
    "temperature_external",
    "temperature_internal"
  )
  has_sensor <- any(sensor_names %in% names(tag))
  expect_true(has_sensor)

  # If pressure data exists, check structure
  if ("pressure" %in% names(tag)) {
    expect_true(is.data.frame(tag$pressure))
    expect_true(all(c("date", "value") %in% names(tag$pressure)))
    expect_s3_class(tag$pressure$date, "POSIXct")
    expect_equal(attr(tag$pressure$date, "tzone"), "UTC")
  }

  # If light data exists, check structure
  if ("light" %in% names(tag)) {
    expect_true(is.data.frame(tag$light))
    expect_true(all(c("date", "value") %in% names(tag$light)))
    expect_s3_class(tag$light$date, "POSIXct")
  }

  # If acceleration data exists, check structure
  if ("acceleration" %in% names(tag)) {
    expect_true(is.data.frame(tag$acceleration))
    expect_true(all(c("date", "value") %in% names(tag$acceleration)))
    expect_s3_class(tag$acceleration$date, "POSIXct")
  }

  # If stap data exists, check structure
  if ("stap" %in% names(tag)) {
    expect_true(is.data.frame(tag$stap))
    expect_true(all(c("stap_id", "start", "end") %in% names(tag$stap)))
    expect_s3_class(tag$stap$start, "POSIXct")
    expect_s3_class(tag$stap$end, "POSIXct")
    expect_equal(attr(tag$stap$start, "tzone"), "UTC")
    expect_equal(attr(tag$stap$end, "tzone"), "UTC")
  }

  # If twilight data exists, check structure
  if ("twilight" %in% names(tag)) {
    expect_true(is.data.frame(tag$twilight))
    expect_true(all(c("twilight", "rise") %in% names(tag$twilight)))
    expect_s3_class(tag$twilight$twilight, "POSIXct")
    expect_equal(attr(tag$twilight$twilight, "tzone"), "UTC")
    expect_type(tag$twilight$rise, "logical")
  }
})

test_that("gldp_to_tag converts all tags when tag_id is NULL", {
  pkg <- pkg_shared
  # Get all tag_ids
  tag_ids <- unique(tags(pkg)$tag_id)

  # Convert all tags
  result <- gldp_to_tag(pkg, tag_id = NULL)

  # If only one tag, should return single tag object
  if (length(tag_ids) == 1) {
    expect_s3_class(result, "tag")
  } else {
    # If multiple tags, should return list
    expect_type(result, "list")
    expect_equal(length(result), length(tag_ids))
    expect_equal(names(result), tag_ids)

    # Check each tag in list
    for (i in seq_along(result)) {
      expect_s3_class(result[[i]], "tag")
      expect_equal(result[[i]]$param$id, tag_ids[i])
    }
  }
})

test_that("gldp_to_tag converts multiple specific tags", {
  pkg <- pkg_shared
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
  pkg <- pkg_shared
  # Try to convert non-existent tag
  expect_error(
    gldp_to_tag(pkg, tag_id = "INVALID_TAG_ID"),
    "Tag ID\\(s\\) not found in package"
  )
})

test_that("gldp_to_tag works with GeoPressureR functions", {
  pkg <- pkg_shared
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

test_that("gldp_to_tag extracts stap and twilight data correctly", {
  pkg <- pkg_shared

  # Check if package has staps and twilights resources
  has_staps <- "staps" %in% frictionless::resources(pkg)
  has_twilights <- "twilights" %in% frictionless::resources(pkg)

  # Get the first tag_id
  tag_ids <- unique(tags(pkg)$tag_id)
  first_tag_id <- tag_ids[1]

  # Convert to tag
  tag <- gldp_to_tag(pkg, tag_id = first_tag_id)

  # If package has staps, check they're in tag object
  if (has_staps) {
    stap_for_tag <- staps(pkg) |> dplyr::filter(.data$tag_id == first_tag_id)
    if (nrow(stap_for_tag) > 0) {
      expect_true("stap" %in% names(tag))
      expect_equal(nrow(tag$stap), nrow(stap_for_tag))

      # Check that tag_id column was removed
      expect_false("tag_id" %in% names(tag$stap))

      # Check required columns exist
      expect_true(all(c("stap_id", "start", "end") %in% names(tag$stap)))

      # Verify stap_id matches
      expect_equal(sort(tag$stap$stap_id), sort(stap_for_tag$stap_id))
    }
  }

  # If package has twilights, check they're in tag object
  if (has_twilights) {
    twl_for_tag <- twilights(pkg) |> dplyr::filter(.data$tag_id == first_tag_id)
    if (nrow(twl_for_tag) > 0) {
      expect_true("twilight" %in% names(tag))
      expect_equal(nrow(tag$twilight), nrow(twl_for_tag))

      # Check that tag_id column was removed
      expect_false("tag_id" %in% names(tag$twilight))

      # Check required columns exist
      expect_true(all(c("twilight", "rise") %in% names(tag$twilight)))

      # Verify rise values match
      expect_equal(sort(tag$twilight$rise), sort(twl_for_tag$rise))
    }
  }
})

test_that("gldp_to_tag keeps mean_acceleration_z in acceleration", {
  pkg <- pkg_shared

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
