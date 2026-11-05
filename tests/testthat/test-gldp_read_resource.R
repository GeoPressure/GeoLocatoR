library(testthat)
library(GeoLocatoR)

# Write a one-resource package to disk, then replace its CSV, so the file no
# longer has the columns its schema declares.
local_package_with_csv <- function(resource_name, data, csv_lines, env = parent.frame()) {
  directory <- withr::local_tempdir(.local_envir = env)
  pkg <- suppressWarnings(add_gldp_resource(create_gldp(), resource_name, data))
  suppressMessages(frictionless::write_package(pkg, directory))
  writeLines(csv_lines, file.path(directory, paste0(resource_name, ".csv")))
  suppressWarnings(frictionless::read_package(file.path(directory, "datapackage.json")))
}

test_that("a resource that leaves out a column keeps its remaining names", {
  # frictionless maps schema fields to columns by position, so without matching
  # by name `ring_number` is read as `datapackage_id`, the second declared
  # field (frictionlessdata/frictionless-r#216).
  pkg <- local_package_with_csv(
    "tags",
    tibble::tibble(tag_id = "a", ring_number = "r1"),
    c("tag_id,ring_number", "a,r1")
  )

  data <- gldp_read_resource(pkg, "tags")

  expect_identical(names(data), c("tag_id", "ring_number"))
  expect_identical(data$ring_number, "r1")
})

test_that("a resource read out of order is matched by name", {
  pkg <- local_package_with_csv(
    "tags",
    tibble::tibble(tag_id = "a", ring_number = "r1"),
    c("ring_number,tag_id", "r1,a")
  )

  data <- gldp_read_resource(pkg, "tags")

  expect_identical(data$tag_id, "a")
  expect_identical(data$ring_number, "r1")
})

test_that("undeclared columns are kept when fieldsMatch allows them", {
  # `tags` declares `fieldsMatch: partial`.
  pkg <- local_package_with_csv(
    "tags",
    tibble::tibble(tag_id = "a", ring_number = "r1"),
    c("tag_id,ring_number,movebank_tag_id", "a,r1,12345")
  )

  data <- gldp_read_resource(pkg, "tags")

  expect_true("movebank_tag_id" %in% names(data))
  expect_identical(data$tag_id, "a")
})

test_that("undeclared columns are reported when fieldsMatch forbids them", {
  # `staps` declares `fieldsMatch: superset`.
  pkg <- local_package_with_csv(
    "staps",
    tibble::tibble(tag_id = "a", stap_id = 1),
    c("tag_id,stap_id,my_extra", "a,1,9")
  )

  expect_error(
    gldp_read_resource(pkg, "staps"),
    class = "gldp_error_undeclared_columns"
  )
})

test_that("a file sharing no column with its schema is reported", {
  pkg <- local_package_with_csv(
    "tags",
    tibble::tibble(tag_id = "a", ring_number = "r1"),
    c("alpha,beta", "1,2")
  )

  expect_error(
    gldp_read_resource(pkg, "tags"),
    class = "gldp_error_header_unrelated"
  )
})

test_that("a remote package reads the header of a relative path from its URL", {
  # The relative path resolves to a URL, which `file.exists()` cannot check.
  requested <- NULL
  local_mocked_bindings(
    read_delim = function(file, ...) {
      requested <<- file
      data.frame(tag_id = character(0))
    },
    .package = "readr"
  )
  pkg <- create_gldp()
  attr(pkg, "directory") <- "https://example.org/package"

  header <- gldp_resource_header(pkg, list(path = "tags.csv"))

  expect_identical(requested, "https://example.org/package/tags.csv")
  expect_identical(header, "tag_id")
})

test_that("file-backed and loaded reads report one named parsing warning", {
  # The example measurements use `acceleration`, which is not a v1.2 sensor.
  directory <- test_path("fixtures", "geolocator-dp-example")
  for (force_read in c(TRUE, FALSE)) {
    warnings <- capture_warnings(read_gldp(directory, force_read = force_read))
    expect_length(warnings, 1)
    expect_match(warnings, "resource measurements", fixed = TRUE)
  }
  pkg <- suppressWarnings(read_gldp(directory, force_read = FALSE))
  expect_warning(measurements(pkg), class = "gldp_warning_parsing_issues")
})
