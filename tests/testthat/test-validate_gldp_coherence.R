library(testthat)
library(GeoLocatoR)

test_that("unknown tag_id/ring_number combinations are reported as a message", {
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      scientific_name = "Upupa epops",
      manufacturer = "test",
      model = "test"
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "observations",
    data.frame(
      tag_id = "AA",
      ring_number = c("R1", "R2"),
      observation_type = "equipment",
      device_status = "present",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      latitude = 47,
      longitude = 8
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "measurements",
    data.frame(
      tag_id = "AA",
      sensor = "pressure",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      value = 1000
    )
  )
  # The combination is part of the message, not printed to the console.
  expect_silent(
    messages <- capture_messages({
      valid <- validate_gldp_core_coherence(pkg)
    })
  )
  expect_false(valid)
  expect_true(any(grepl("AA/R2", messages, fixed = TRUE)))
})

test_that("validate_gldp_geopressure_coherence accepts realistic values", {
  pkg <- make_geopressure_test_pkg()
  expect_true(suppressMessages(validate_gldp_geopressure_coherence(pkg)))
})

test_that("validate_gldp_geopressure_coherence ignores discarded pressurepath altitudes", {
  pkg <- make_geopressure_test_pkg()
  pressurepath_idx <- which(vapply(
    pkg$resources,
    \(r) identical(r$name, "pressurepaths"),
    logical(1)
  ))
  pkg[["resources"]][[pressurepath_idx[1]]]$data$altitude[1] <- 8000
  pkg[["resources"]][[pressurepath_idx[1]]]$data$label <- "discard"

  expect_true(suppressMessages(validate_gldp_geopressure_coherence(pkg)))
})

test_that("validate_gldp_geopressure_coherence only warns on unusual altitudes", {
  pkg <- make_geopressure_test_pkg()
  pressurepath_idx <- which(vapply(
    pkg$resources,
    \(r) identical(r$name, "pressurepaths"),
    logical(1)
  ))
  pkg[["resources"]][[pressurepath_idx[1]]]$data$altitude[1] <- 12000

  expect_message(
    expect_true(validate_gldp_geopressure_coherence(pkg)),
    "outside the usual"
  )
})

test_that("validate_gldp_geopressure_coherence warns on unusual distances", {
  pkg <- make_geopressure_test_pkg()
  edge_idx <- which(vapply(pkg$resources, \(r) identical(r$name, "edges"), logical(1)))
  pkg[["resources"]][[edge_idx[1]]]$data$distance[1] <- 4000

  expect_message(
    expect_true(validate_gldp_geopressure_coherence(pkg)),
    "unusual distance"
  )
})

test_that("validate_gldp_geopressure_coherence only warns on implausible years, stap_id and j", {
  pkg <- make_geopressure_test_pkg()
  # Data from 2000 and the 1990s are accepted without a warning.
  staps <- gldp_resource(pkg, "staps")
  staps$data$start <- as.POSIXct("2000-01-01", tz = "UTC")
  staps$data$end <- as.POSIXct("2000-01-02", tz = "UTC")
  gldp_resource(pkg, "staps") <- staps
  twilights <- gldp_resource(pkg, "twilights")
  twilights$data$twilight <- as.POSIXct("1995-06-01", tz = "UTC")
  gldp_resource(pkg, "twilights") <- twilights
  expect_no_message(
    expect_true(validate_gldp_geopressure_coherence(pkg)),
    message = "unusual"
  )

  twilights$data$twilight <- as.POSIXct("1985-01-01", tz = "UTC")
  gldp_resource(pkg, "twilights") <- twilights
  expect_message(expect_true(validate_gldp_geopressure_coherence(pkg)), "unusual year")

  pkg <- make_geopressure_test_pkg()
  for (name in c("staps", "paths", "edges")) {
    resource <- gldp_resource(pkg, name)
    resource$data[intersect(c("stap_id", "stap_s", "stap_t"), names(resource$data))] <- 300
    gldp_resource(pkg, name) <- resource
  }
  paths <- gldp_resource(pkg, "paths")
  paths$data$j <- 500L
  gldp_resource(pkg, "paths") <- paths
  msgs <- capture_messages(
    expect_true(validate_gldp_geopressure_coherence(pkg))
  )
  expect_match(msgs, "staps\\$stap_id.*unusually large", all = FALSE)
  expect_match(msgs, "paths\\$j.*unusually large", all = FALSE)
})

test_that("one orphan measurement tag and one unobserved ring each invalidate core coherence", {
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      scientific_name = "Upupa epops",
      manufacturer = "test",
      model = "test"
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "observations",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      observation_type = c("equipment", "retrieval"),
      device_status = "present",
      datetime = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"),
      latitude = 47,
      longitude = 8
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "measurements",
    data.frame(
      tag_id = "AA",
      sensor = "pressure",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      value = 1000,
      label = NA_character_
    )
  )
  expect_true(suppressMessages(validate_gldp_core_coherence(pkg)))
  invalid <- pkg
  invalid[["resources"]][[match(
    "measurements",
    vapply(invalid$resources, function(r) r$name, character(1))
  )]]$data$tag_id <- "orphan"
  expect_false(suppressMessages(validate_gldp_core_coherence(invalid)))
  invalid <- pkg
  invalid[["resources"]][[match(
    "tags",
    vapply(invalid$resources, function(r) r$name, character(1))
  )]]$data$ring_number <- "unobserved"
  expect_false(suppressMessages(validate_gldp_core_coherence(invalid)))
})

test_that("observation histories report invalid device states and duplicate events", {
  observations <- data.frame(
    tag_id = c("AA", "AA"),
    ring_number = "R1",
    datetime = as.POSIXct(c("2024-01-01", "2024-02-01"), tz = "UTC"),
    observation_type = c("equipment", "retrieval"),
    device_status = "present"
  )
  expect_true(suppressMessages(validate_gldp_observations(observations)))

  invalid <- observations
  invalid$device_status[1] <- "none"
  expect_message(expect_false(validate_gldp_observations(invalid)), "does not have a device status")

  invalid <- rbind(observations, observations[1, ])
  invalid$observation_type[3] <- "capture"
  invalid$datetime[3] <- invalid$datetime[1] + 86400
  invalid$tag_id[3] <- NA_character_
  expect_message(expect_false(validate_gldp_observations(invalid)), "missing a")

  invalid <- rbind(observations, observations[1, ])
  expect_message(expect_false(validate_gldp_observations(invalid)), "duplicate observation")

  # A heavy bird is a guideline warning, not a failure.
  heavy <- observations
  heavy$mass <- c(12000, NA)
  expect_message(expect_true(validate_gldp_observations(heavy)), "above 10,000 g")

  invalid <- rbind(observations, observations[1, ])
  invalid$observation_type[3] <- "capture"
  invalid$datetime[3] <- invalid$datetime[1] + 86400
  invalid$device_status[3] <- "none"
  expect_message(
    expect_false(validate_gldp_observations(invalid)),
    "invalid device_status transition"
  )
})

test_that("observation device_status must suit the observation_type", {
  observations <- data.frame(
    tag_id = c("AA", "AA", NA),
    ring_number = "R1",
    datetime = as.POSIXct(c("2024-01-01", "2024-02-01", "2024-03-01"), tz = "UTC"),
    observation_type = c("equipment", "retrieval", "sighting"),
    device_status = c("present", "present", "unknown")
  )
  expect_true(suppressMessages(validate_gldp_observations(observations)))

  # A missing device_status on equipment or retrieval fails.
  invalid <- observations
  invalid$device_status[2] <- NA
  expect_message(expect_false(validate_gldp_observations(invalid)), "does not have a device status")

  # `unknown` is only for a sighting.
  invalid <- rbind(observations, observations[3, ])
  invalid$observation_type[4] <- "capture"
  invalid$datetime[4] <- invalid$datetime[4] + 86400
  expect_message(expect_false(validate_gldp_observations(invalid)), "not allowed")

  # `none` and `missing` are only for a capture.
  for (status in c("none", "missing")) {
    invalid <- observations
    invalid$device_status[3] <- status
    invalid$tag_id[3] <- "AA"
    expect_message(expect_false(validate_gldp_observations(invalid)), "not allowed")
  }
})

test_that("coherence checks accept observations without optional columns", {
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      scientific_name = "Upupa epops",
      manufacturer = "test",
      model = "test"
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "observations",
    data.frame(
      ring_number = "R1",
      observation_type = "capture",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      latitude = 47,
      longitude = 8,
      age_class = "0",
      sex = "U"
    )
  )
  expect_no_error(suppressMessages(validate_gldp_coherence(pkg)))
  expect_true(suppressMessages(validate_gldp_observations(observations(pkg))))
  expect_true(suppressMessages(validate_gldp_geopressure_coherence(pkg)))
  expect_true(suppressMessages(validate_gldp_foreign_keys(pkg)))
})

test_that("coherence checks report a missing observations resource without erroring", {
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      scientific_name = "Upupa epops",
      manufacturer = "test",
      model = "test"
    )
  )
  expect_false(suppressMessages(validate_gldp_coherence(pkg)))
})
