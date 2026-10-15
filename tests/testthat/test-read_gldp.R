library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("read_gldp reads a local datapackage.json", {
  path <- file.path(withr::local_tempdir(), "datapackage.json")

  jsonlite::write_json(
    list(
      `$schema` = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v1.0/geolocator-dp-profile.json",
      id = "unit-test",
      resources = list()
    ),
    path,
    auto_unbox = TRUE
  )

  pkg <- suppressWarnings(read_gldp(path))

  expect_s3_class(pkg, "geolocatordp")
  expect_true("resources" %in% names(pkg))
  # read_gldp() upgrades to the latest supported version, so track it rather
  # than a literal that goes stale at every GeoLocator-DP release.
  expect_equal(gldp_version(pkg), .gldp_default_version)
})

test_that("read_gldp reads a local package directory", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, "datapackage.json")

  jsonlite::write_json(
    list(
      `$schema` = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v1.0/geolocator-dp-profile.json",
      id = "unit-test",
      resources = list()
    ),
    path,
    auto_unbox = TRUE
  )

  pkg <- suppressWarnings(read_gldp(directory))

  expect_s3_class(pkg, "geolocatordp")
})

test_that("read_gldp reports a missing descriptor in a package directory", {
  directory <- withr::local_tempdir()

  expect_error(
    read_gldp(directory),
    "does not contain.*datapackage.json"
  )
})

test_that("read_gldp identifies an unreadable resource", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, "datapackage.json")

  jsonlite::write_json(
    list(
      `$schema` = "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v1.0/geolocator-dp-profile.json",
      id = "unit-test",
      resources = list(list(
        name = "tags",
        path = "tags.csv",
        schema = list()
      ))
    ),
    path,
    auto_unbox = TRUE
  )

  expect_error(
    read_gldp(path),
    "Could not read resource tags"
  )
})

test_that("read_gldp handles invalid input", {
  # Test with non-existent file
  expect_error(read_gldp("nonexistent.json"))

  # Test with non-json content
  tmp <- withr::local_tempdir()
  txt_path <- file.path(tmp, "not-json.txt")
  writeLines("not json", txt_path)
  expect_error(read_gldp(txt_path))
})

test_that("read_gldp schema version extraction supports short and explicit refs", {
  pkg <- function(schema) {
    p <- create_gldp()
    p$`$schema` <- schema
    p
  }

  expect_equal(
    gldp_version(pkg(
      "https://raw.githubusercontent.com/GeoPressure/GeoLocator-DP/v0.4/geolocator-dp-profile.json"
    )),
    "v0.4"
  )

  expect_error(gldp_version(pkg("https://example.com/schema.json")))
})

test_that("read_gldp canonicalizes legacy schema owners", {
  path <- file.path(withr::local_tempdir(), "datapackage.json")

  jsonlite::write_json(
    list(
      `$schema` = "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/v1.0/geolocator-dp-profile.json",
      id = "unit-test",
      resources = list(
        list(
          name = "tags",
          profile = "tabular-data-resource",
          path = "tags.csv",
          schema = list(
            fields = list(
              list(name = "tag_id", type = "string")
            )
          ),
          `$schema` = "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/v1.0/tags-table-schema.json"
        )
      )
    ),
    path,
    auto_unbox = TRUE
  )

  writeLines("tag_id\nid-1", file.path(dirname(path), "tags.csv"))

  pkg <- expect_no_warning(read_gldp(path, force_read = FALSE))

  expect_equal(pkg$`$schema`, gldp_schema_url(.gldp_default_version))
})

test_that("write_gldp writes a datapackage readable by read_gldp", {
  pkg <- pkg_shared

  out_dir <- withr::local_tempdir()
  expect_no_error(write_gldp(pkg, directory = out_dir))
  expect_true(file.exists(file.path(out_dir, "datapackage.json")))

  pkg_back <- suppressWarnings(read_gldp(file.path(out_dir, "datapackage.json")))
  expect_s3_class(pkg_back, "geolocatordp")
  expect_equal(pkg_back$id, pkg$id)
})

test_that("write_gldp/read_gldp roundtrip keeps pkg$params via params.json", {
  pkg <- pkg_shared
  param <- GeoPressureR::param_create("TEST-PARAM", default = TRUE)
  pkg[["params"]] <- list(param)

  out_dir <- withr::local_tempdir()
  expect_no_error(write_gldp(pkg, directory = out_dir))
  expect_true(file.exists(file.path(out_dir, "params.json")))

  pkg_back <- suppressWarnings(read_gldp(file.path(out_dir, "datapackage.json"), force_read = TRUE))
  expect_true(is.list(pkg_back$params))
  expect_equal(pkg_back$params[[1]]$id, "TEST-PARAM")
})

test_that("read_gldp restores the provenance table after a JSON round trip", {
  pkg <- add_gldp_resource(create_gldp(), "tags", data.frame(tag_id = "tag-1"))
  pkg[["datapackages"]] <- data.frame(
    datapackage_id = c("source-1", "source-2"),
    title = c("First source", "Second source"),
    is_published = c(TRUE, NA),
    numberTags_tags = c(2L, 3L),
    embargo = c(NA_character_, NA_character_)
  )
  directory <- withr::local_tempdir()
  suppressMessages(write_gldp(pkg, directory))
  restored <- suppressWarnings(read_gldp(directory))

  expect_s3_class(restored$datapackages, "tbl_df")
  expect_equal(names(restored$datapackages), names(pkg$datapackages))
  expect_equal(restored$datapackages$datapackage_id, pkg$datapackages$datapackage_id)
  expect_equal(restored$datapackages$title, pkg$datapackages$title)
  expect_equal(restored$datapackages$is_published, c(TRUE, NA))
  expect_equal(restored$datapackages$numberTags_tags, c(2L, 3L))
  expect_true(all(is.na(restored$datapackages$embargo)))
})

test_that("file-backed metadata, merging and writing match loaded packages", {
  directory <- tempfile("read-lazy-workflows-")
  on.exit(unlink(directory, recursive = TRUE))
  loaded <- list()
  lazy <- list()
  for (id in c("AA", "BB")) {
    pkg <- create_gldp()
    pkg[["id"]] <- paste0("source-", id)
    pkg <- add_gldp_resource(
      pkg,
      "tags",
      data.frame(
        tag_id = id,
        ring_number = paste0("ring-", id),
        scientific_name = "Upupa epops",
        manufacturer = "test",
        model = "test"
      )
    )
    pkg <- add_gldp_resource(
      pkg,
      "measurements",
      data.frame(
        tag_id = id,
        sensor = "pressure",
        datetime = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"),
        value = c(1000, 1001),
        label = NA_character_
      )
    )
    write_gldp(pkg, file.path(directory, id))
    loaded[[id]] <- read_gldp(file.path(directory, id))
    lazy[[id]] <- read_gldp(file.path(directory, id), force_read = FALSE)
    expect_equal(lazy[[id]]$numberTags, loaded[[id]]$numberTags)
    expect_equal(lazy[[id]]$temporal, loaded[[id]]$temporal)
    expect_equal(lazy[[id]]$taxonomic, loaded[[id]]$taxonomic)
  }
  merged_lazy <- merge_gldp(lazy)
  merged_loaded <- merge_gldp(loaded)
  expect_equal(tags(merged_lazy), tags(merged_loaded))
  expect_equal(measurements(merged_lazy), measurements(merged_loaded))
  write_gldp(lazy[["AA"]], file.path(directory, "copy"))
  reopened <- read_gldp(file.path(directory, "copy"))
  expect_equal(tags(reopened), tags(loaded[["AA"]]))
  expect_equal(measurements(reopened), measurements(loaded[["AA"]]))
  expect_true(all(vapply(lazy[["AA"]]$resources, function(r) !is.null(r$path), logical(1))))
})
