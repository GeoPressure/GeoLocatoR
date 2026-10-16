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
    )
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")
  attr(pkg, "directory") <- "."

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
    )
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")
  attr(pkg, "directory") <- "."

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
    )
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")
  attr(pkg, "directory") <- "."

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
        data = data.frame(
          tag_id = c("tag-1", "tag-2"),
          ring_number = c("A-123", NA),
          scientific_name = c("Hirundo rustica", "Hirundo rustica"),
          stringsAsFactors = FALSE
        )
      ),
      list(
        name = "measurements",
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
          lat = 46.1,
          lon = 8.1,
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
          lat_s = 46.1,
          lon_s = 8.1,
          lat_t = 46.2,
          lon_t = 8.2,
          j = 1L,
          type = "most_likely",
          s = 11L,
          t = 12L,
          stringsAsFactors = FALSE
        )
      )
    )
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")
  attr(pkg, "directory") <- "."

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

test_that("upgrade_gldp rejects a resource holding both path and data", {
  # The Data Package standard allows `path` or `data`, never both. Going through
  # gldp_resource() in mutate_resource() surfaces such a descriptor instead of
  # silently carrying it into the upgraded package.
  pkg <- list(
    "$schema" = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v0.6/geolocator-dp-profile.json",
    resources = list(
      list(
        name = "measurements",
        path = "measurements.csv",
        data = data.frame(
          tag_id = "tag-1",
          sensor = "pitch",
          value = 0.2,
          stringsAsFactors = FALSE
        )
      )
    )
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")
  attr(pkg, "directory") <- "."

  expect_error(
    suppressMessages(upgrade_gldp(pkg, to_version = "v1.0")),
    class = "gldp_error_resource_both_path_data"
  )
})

test_that("upgrade_gldp migrates a v1.0 package to v1.1", {
  pkg <- list(
    "$schema" = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v1.0/geolocator-dp-profile.json",
    resources = list(
      list(
        name = "tags",
        data = data.frame(
          tag_id = "tag-1",
          ring_number = "A-123",
          scientific_name = "Hirundo rustica",
          stringsAsFactors = FALSE
        )
      )
    )
  )
  class(pkg) <- c("geolocatordp", "datapackage", "list")
  attr(pkg, "directory") <- "."

  upgraded <- suppressMessages(suppressWarnings(upgrade_gldp(pkg, to_version = "v1.1")))

  expect_equal(gldp_version(upgraded), "v1.1")
  expect_equal(upgraded$`$schema`, gldp_schema_url("v1.1"))

  # v1.1 requires `type` and the table schema in `schema`, not in `$schema`.
  tags_res <- purrr::detect(upgraded$resources, \(r) r$name == "tags")
  expect_equal(tags_res$type, "table")
  expect_type(tags_res$schema, "list")
  expect_false(is.null(tags_res$schema$fields))
})

test_that("file-backed legacy upgrades transform values and match loaded upgrades", {
  directory <- tempfile("upgrade-lazy-")
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- create_gldp()
  pkg[["$schema"]] <- gldp_schema_url("v0.6")
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = c("AA", "unused"),
      ring_number = c("R1", NA),
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
      observation_type = "equipment",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      latitude = 47,
      longitude = 8,
      age_class = "0",
      sex = "U"
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "measurements",
    data.frame(
      tag_id = "AA",
      sensor = "pitch",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      value = 0.2,
      label = NA_character_
    )
  )
  # Exercise the existing v0.6 migration, including its tag-removal policy.
  write_gldp(pkg, directory)
  lazy <- suppressMessages(read_gldp(directory, force_read = FALSE))
  loaded <- suppressMessages(read_gldp(directory))
  expect_equal(tags(lazy), tags(loaded))
  expect_equal(measurements(lazy), measurements(loaded))
  expect_identical(as.character(measurements(lazy)$sensor), "mean_acceleration_z")
  expect_identical(tags(lazy)$tag_id, "AA")
  expect_equal(gldp_version(lazy), .gldp_default_version)
  expect_true(all(vapply(lazy$resources, function(r) is.null(r$path), logical(1))))
})

test_that("mixed-version merges preserve declared extra numeric fields in lazy and loaded reads", {
  directory <- tempfile("upgrade-extra-fields-")
  on.exit(unlink(directory, recursive = TRUE))
  for (version in c("v1.0", "v1.1")) {
    pkg <- create_gldp()
    pkg[["id"]] <- paste0("source-", version)
    pkg[["$schema"]] <- gldp_schema_url(version)
    pkg <- add_gldp_resource(
      pkg,
      "tags",
      data.frame(
        tag_id = version,
        ring_number = version,
        scientific_name = "Upupa epops",
        manufacturer = "test",
        model = "test"
      )
    )
    pkg <- add_gldp_resource(
      pkg,
      "pressurepaths",
      data.frame(
        tag_id = version,
        datetime = as.POSIXct("2020-01-01", tz = "UTC"),
        stap_id = 1,
        type = "most_likely",
        j = 1L,
        lat = 47,
        lon = 8,
        pressure_tag = 812L
      )
    )
    resource <- gldp_resource(pkg, "pressurepaths")
    i <- match("pressure_tag", schema_field_names(resource$schema))
    resource$schema$fields[[i]]$unit <- "hPa"
    resource$schema$fields[[i]]$description <- "Recorded tag pressure"
    gldp_resource(pkg, "pressurepaths") <- resource
    write_gldp(pkg, file.path(directory, version))
  }
  for (force in c(FALSE, TRUE)) {
    old <- suppressMessages(read_gldp(file.path(directory, "v1.0"), force_read = force))
    current <- read_gldp(file.path(directory, "v1.1"), force_read = force)
    fields <- gldp_resource(old, "pressurepaths")$schema$fields
    field <- fields[[match("pressure_tag", schema_field_names(list(fields = fields)))]]
    expect_identical(field$type, "integer")
    expect_identical(field$unit, "hPa")
    expect_identical(field$description, "Recorded tag pressure")
    expect_true(is.numeric(pressurepaths(old)$pressure_tag))
    expect_equal(pressurepaths(old)$pressure_tag, 812)
    # The v1.2 upgrade loads pressurepaths to fill `j`, even in a lazy read.
    expect_false(is.null(gldp_resource(old, "pressurepaths")$data))
    merged <- merge_gldp(old, current)
    expect_true(is.numeric(pressurepaths(merged)$pressure_tag))
    expect_equal(pressurepaths(merged)$pressure_tag, c(812, 812))
    write_gldp(old, file.path(directory, "copy"))
    reopened <- read_gldp(file.path(directory, "copy"))
    expect_equal(pressurepaths(reopened)$pressure_tag, 812)
    reopened_fields <- gldp_resource(reopened, "pressurepaths")$schema$fields
    expect_identical(
      reopened_fields[[match(
        "pressure_tag",
        schema_field_names(list(fields = reopened_fields))
      )]]$unit,
      "hPa"
    )
  }
})

test_that("gldp_fill_pressurepaths_j fills flight rows with the trajectory's j", {
  pp <- tibble::tibble(
    tag_id = c("A", "A", "A", "B", "B", "C", "C", "C"),
    type = "most_likely",
    stap_id = c(1, 1.5, 2, 1, 1.5, 1, 1.5, 1),
    j = c(1L, NA, 1L, NA, NA, 1L, NA, 2L)
  )
  out <- gldp_fill_pressurepaths_j(pp)

  expect_identical(out$j[pp$tag_id == "A"], c(1L, 1L, 1L))
  # No known j: left missing.
  expect_true(all(is.na(out$j[pp$tag_id == "B"])))
  # Ambiguous j: left missing for validate_gldp() to report.
  expect_true(is.na(out$j[pp$tag_id == "C" & pp$stap_id == 1.5]))
  expect_identical(gldp_fill_pressurepaths_j(out), out)
})

test_that("upgrade_gldp fills pressurepaths j for packages up to v1.1 only", {
  pp <- tibble::tibble(
    tag_id = "A",
    datetime = as.POSIXct("2020-01-01", tz = "UTC") + 0:2 * 3600,
    stap_id = c(1, 1.5, 2),
    type = "most_likely",
    j = c(1L, NA, 1L),
    pressure_tag = 900
  )
  pkg <- create_gldp(version = "v1.1")
  pkg <- add_gldp_resource(pkg, "pressurepaths", pp, cast_type = TRUE)

  out <- suppressMessages(upgrade_gldp(pkg))
  expect_identical(gldp_version(out), "v1.2")
  expect_identical(pressurepaths(out)$j, c(1L, 1L, 1L))

  # A file-backed table is loaded so that its gaps can be filled.
  directory <- tempfile("upgrade-j-")
  on.exit(unlink(directory, recursive = TRUE))
  write_gldp(pkg, directory)
  lazy <- suppressMessages(read_gldp(file.path(directory, "datapackage.json"), force_read = FALSE))
  expect_identical(pressurepaths(lazy)$j, c(1L, 1L, 1L))
})
