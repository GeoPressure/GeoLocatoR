library(testthat)
library(GeoLocatoR)

test_that("upgrade_gldp migrates v0.1 package fields to v0.6", {
  pkg <- list(
    "$schema" = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v0.1/geolocator-dp-profile.json",
    citation = "A citation",
    reference_location = list(latitude = 46.8, longitude = 8.3),
    id = "https://example.org/id",
    resources = list(
      list(
        name = "tags",
        data = data.frame(
          tag_id = "tag-1",
          firwmare = "1.2",
          stringsAsFactors = FALSE
        )
      ),
      list(
        name = "observations",
        data = data.frame(
          life_stage = "adult",
          device_status = "broken_damage",
          stringsAsFactors = FALSE
        )
      ),
      list(
        name = "paths",
        data = data.frame(
          tag_id = "tag-1",
          stap_id = "stap-1",
          type = "tag",
          stringsAsFactors = FALSE
        )
      ),
      list(
        name = "edges",
        data = data.frame(
          tag_id = "tag-1",
          stap_s = "stap-1",
          stap_t = "stap-2",
          stringsAsFactors = FALSE
        )
      ),
      list(
        name = "pressurepaths",
        data = data.frame(
          tag_id = "tag-1",
          ind = 1L,
          j = 2L,
          stringsAsFactors = FALSE
        )
      )
    ),
    directory = "."
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")

  upgraded <- suppressMessages(upgrade_gldp(pkg, to_version = "v0.6"))

  expect_equal(
    upgraded$`$schema`,
    "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v0.6/geolocator-dp-profile.json"
  )
  expect_equal(gldp_version(upgraded), "v0.6")

  expect_true(!is.null(upgraded$bibliographicCitation))
  expect_null(upgraded$citation)
  expect_true(!is.null(upgraded$referenceLocation))
  expect_null(upgraded$reference_location)

  tags_data <- purrr::detect(upgraded$resources, \(r) r$name == "tags")$data
  expect_true("firmware" %in% names(tags_data))
  expect_false("firwmare" %in% names(tags_data))
  expect_true("datapackage_id" %in% names(tags_data))

  obs_data <- purrr::detect(upgraded$resources, \(r) r$name == "observations")$data
  expect_true("age_class" %in% names(obs_data))
  expect_false("life_stage" %in% names(obs_data))
  expect_equal(obs_data$device_status[[1]], "missing")

  edges_data <- purrr::detect(upgraded$resources, \(r) r$name == "edges")$data
  expect_true("type" %in% names(edges_data))
  expect_equal(edges_data$type[[1]], "tag")

  pressurepaths_data <- purrr::detect(upgraded$resources, \(r) r$name == "pressurepaths")$data
  expect_false("ind" %in% names(pressurepaths_data))
})

test_that("step v0.4 -> v0.5 reconstructs missing edges$type from j", {
  pkg <- list(
    "$schema" = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v0.4/geolocator-dp-profile.json",
    id = "https://example.org/id",
    resources = list(
      list(
        name = "edges",
        data = data.frame(
          tag_id = c("tag-1", "tag-1", "tag-1", "tag-1", "tag-1", "tag-2", "tag-2"),
          stap_s = c("A", "B", "C", "C", "D", "X", "X"),
          stap_t = c("B", "C", "D", "D", "E", "Y", "Y"),
          j = c(1L, 1L, 1L, 2L, 1L, 1L, 1L),
          stringsAsFactors = FALSE
        )
      )
    ),
    directory = "."
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")

  upgraded <- suppressMessages(upgrade_gldp(pkg, to_version = "v0.5"))
  edges_data <- purrr::detect(upgraded$resources, \(r) r$name == "edges")$data

  expect_equal(
    edges_data$type,
    c(
      "most_likely",
      "most_likely",
      "most_likely",
      "simulation",
      "most_likely",
      "most_likely",
      "simulation"
    )
  )
})

test_that("step v0.5 -> v0.6 preserves pressurepaths extra schema fields", {
  pkg <- list(
    "$schema" = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v0.5/geolocator-dp-profile.json",
    resources = list(
      list(
        name = "pressurepaths",
        data = data.frame(
          tag_id = "tag-1",
          datetime = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
          stap_id = 1,
          type = "most_likely",
          lat = 46.1,
          lon = 8.1,
          ind = 10L,
          j = 1L,
          sunrise = 0.123,
          stringsAsFactors = FALSE
        )
      )
    ),
    directory = "."
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")

  upgraded <- suppressMessages(upgrade_gldp(pkg, to_version = "v0.6"))
  pressurepaths_res <- upgraded$resources[[1]]
  schema_fields <- vapply(pressurepaths_res$schema$fields, \(f) f$name, character(1))

  expect_false("ind" %in% names(pressurepaths_res$data))
  expect_false("ind" %in% schema_fields)
  expect_true("sunrise" %in% names(pressurepaths_res$data))
  expect_true("sunrise" %in% schema_fields)
})

test_that("upgrade_gldp migrates v0.6 package to v1.0 schema refs", {
  pkg <- list(
    "$schema" = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v0.6/geolocator-dp-profile.json",
    id = "https://example.org/id",
    title = "Legacy title",
    description = "Legacy description",
    created = "2024-01-01T00:00:00Z",
    keywords = c("geolocator", "bird"),
    grants = list(
      list(
        funderName = "SNSF",
        awardNumber = "123"
      )
    ),
    temporal = list(start = "2024-01-01", end = "2024-01-31"),
    spatial = list(type = "Point", coordinates = c(8, 46)),
    taxonomic = c("Hirundo rustica"),
    numberTags = list(tags = 2),
    bibliographicCitation = "Legacy citation",
    meta = list(source = "zenodo"),
    custom_field = "should-stay",
    resources = list(
      list(
        name = "tags",
        path = "tags.csv",
        data = data.frame(
          tag_id = c("tag-1", "tag-2"),
          ring_number = c("A-123", NA),
          scientific_name = c("Hirundo rustica", "Hirundo rustica"),
          stringsAsFactors = FALSE
        )
      ),
      list(
        name = "measurements",
        path = "measurements.csv",
        data = data.frame(
          tag_id = c("tag-1", "tag-1"),
          sensor = c("pitch", "light"),
          value = c(0.2, 15),
          stringsAsFactors = FALSE
        )
      ),
      list(
        name = "paths",
        data = data.frame(
          tag_id = "tag-1",
          stap_id = "stap-1",
          latitude = 46.1,
          longitude = 8.1,
          j = 1L,
          type = "most_likely",
          ind = 42L,
          interp = TRUE,
          known = TRUE,
          stringsAsFactors = FALSE
        )
      ),
      list(
        name = "staps",
        data = data.frame(
          tag_id = "tag-1",
          stap_id = "stap-1",
          start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
          end = as.POSIXct("2024-01-02 00:00:00", tz = "UTC"),
          known_lat = 46.1,
          known_lon = 8.1,
          include = TRUE,
          stringsAsFactors = FALSE
        )
      ),
      list(
        name = "edges",
        data = data.frame(
          tag_id = "tag-1",
          stap_s = "stap-1",
          stap_t = "stap-2",
          latitude_s = 46.1,
          longitude_s = 8.1,
          latitude_t = 46.2,
          longitude_t = 8.2,
          j = 1L,
          type = "most_likely",
          s = 11L,
          t = 12L,
          stringsAsFactors = FALSE
        )
      )
    ),
    directory = "."
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")

  msgs <- testthat::capture_messages({
    upgraded <- upgrade_gldp(pkg, to_version = "v1.0")
  })
  expect_true(any(grepl("Removed 1 non-deployed tag row", msgs, fixed = TRUE)))

  expect_equal(
    upgraded$`$schema`,
    "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v1.0/geolocator-dp-profile.json"
  )
  expect_equal(gldp_version(upgraded), "v1.0")

  tags_res <- purrr::detect(upgraded$resources, \(r) r$name == "tags")
  meas_res <- purrr::detect(upgraded$resources, \(r) r$name == "measurements")
  paths_res <- purrr::detect(upgraded$resources, \(r) r$name == "paths")
  staps_res <- purrr::detect(upgraded$resources, \(r) r$name == "staps")
  edges_res <- purrr::detect(upgraded$resources, \(r) r$name == "edges")

  expect_type(tags_res$schema, "list")
  expect_type(meas_res$schema, "list")
  expect_equal(tags_res$schema$name, "tags")
  expect_equal(meas_res$schema$name, "measurements")
  expect_true(!is.null(tags_res$schema$fields))
  expect_true(!is.null(meas_res$schema$fields))
  expect_equal(nrow(tags_res$data), 1)
  expect_equal(tags_res$data$tag_id[[1]], "tag-1")

  expect_true("mean_acceleration_z" %in% meas_res$data$sensor)
  expect_false("pitch" %in% meas_res$data$sensor)
  expect_false("ind" %in% names(paths_res$data))
  expect_false("interp" %in% names(paths_res$data))
  expect_false("known" %in% names(paths_res$data))
  expect_false("include" %in% names(staps_res$data))
  expect_false("s" %in% names(edges_res$data))
  expect_false("t" %in% names(edges_res$data))
})
