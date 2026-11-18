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

test_that("gldp_resource_header lets an unreadable file fail", {
  # Returning NULL would skip the alignment and fall back to positional matching.
  directory <- withr::local_tempdir()
  dir.create(file.path(directory, "tags.csv"))
  pkg <- create_gldp()
  attr(pkg, "directory") <- directory

  expect_error(suppressWarnings(gldp_resource_header(pkg, list(path = "tags.csv"))))
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

test_that("gldp_cast_table leaves untyped and any fields as they are", {
  schema <- list(
    fields = list(
      list(name = "value", type = "number"),
      list(name = "note"),
      list(name = "extra", type = "any")
    )
  )
  data <- data.frame(value = "1.5", note = "a", extra = 2L)

  expect_no_warning(out <- gldp_cast_table(data, schema))
  expect_identical(out, data.frame(value = 1.5, note = "a", extra = 2L))
})

test_that("gldp_cast_table warns when a cast turns values into NA", {
  schema <- list(
    fields = list(
      list(name = "value", type = "number"),
      list(name = "count", type = "integer"),
      list(name = "day", type = "date"),
      list(name = "time", type = "datetime")
    )
  )
  data <- data.frame(
    value = c("1.5", "a", NA, ""),
    count = c("1", "2", "3", "4"),
    day = c("2020-01-01", "x", NA, NA),
    time = c("2020-01-01T00:00:00Z", NA, NA, NA)
  )

  warnings <- capture_warnings(gldp_cast_table(data, schema))
  # Missing and empty values are not counted; fully cast fields stay quiet.
  expect_length(warnings, 2)
  expect_match(warnings[1], "1 non-missing value of value became NA when cast to \"number\"")
  expect_match(warnings[2], "day")
  expect_warning(gldp_cast_table(data["value"], schema), class = "gldp_warning_cast_loss")
  expect_identical(suppressWarnings(gldp_cast_table(data, schema))$count, 1:4)
})

test_that("gldp_cast_table turns an unparsable first date or datetime into NA", {
  schema <- list(
    fields = list(
      list(name = "day", type = "date"),
      list(name = "time", type = "datetime")
    )
  )
  data <- data.frame(
    day = c("x", "2020-01-02", "12-05-2020"),
    time = c("x", "2020-01-02T03:04:05Z", "2020-01-02 03:04")
  )

  expect_length(capture_warnings(gldp_cast_table(data, schema)), 2)
  out <- suppressWarnings(gldp_cast_table(data, schema))
  expect_identical(out$day, as.Date(c(NA, "2020-01-02", NA)))
  expect_identical(
    out$time,
    as.POSIXct(c(NA, "2020-01-02 03:04:05", "2020-01-02 03:04:00"), tz = "UTC")
  )
})

test_that("gldp_cast_table converts a datetime with a UTC offset to UTC", {
  schema <- list(fields = list(list(name = "time", type = "datetime")))
  data <- data.frame(
    time = c("2020-01-01T12:00:00+02:00", "2020-01-01T12:00:00-0530", "2020-01-01T12:00Z")
  )

  expect_no_warning(out <- gldp_cast_table(data, schema))
  expect_identical(
    out$time,
    as.POSIXct(c("2020-01-01 10:00:00", "2020-01-01 17:30:00", "2020-01-01 12:00:00"), tz = "UTC")
  )
})

test_that("gldp_cast_table makes non-integer values NA with a warning", {
  schema <- list(
    fields = list(list(name = "count", type = "integer"), list(name = "year", type = "year"))
  )
  data <- data.frame(count = c("1.7", "2", NA), year = c(2020.5, 2021, NA))

  expect_length(capture_warnings(gldp_cast_table(data, schema)), 2)
  out <- suppressWarnings(gldp_cast_table(data, schema))
  expect_identical(out$count, c(NA, 2L, NA))
  expect_identical(out$year, c(NA, 2021L, NA))
})

test_that("gldp_cast_table casts booleans with trueValues and falseValues", {
  schema <- list(
    fields = list(
      list(name = "a", type = "boolean"),
      list(name = "b", type = "boolean", trueValues = "yes", falseValues = "no"),
      list(name = "c", type = "boolean", trueValues = "yes")
    )
  )
  data <- data.frame(
    a = c("1", "0", "true", "FALSE", "yes", NA),
    b = c("yes", "no", "yes", "no", "1", NA),
    c = c(TRUE, FALSE, TRUE, FALSE, TRUE, NA)
  )

  warnings <- capture_warnings(gldp_cast_table(data, schema))
  expect_length(warnings, 2)
  expect_match(warnings, "became NA when cast to \"boolean\"")
  out <- suppressWarnings(gldp_cast_table(data, schema))
  expect_identical(out$a, c(TRUE, FALSE, TRUE, FALSE, NA, NA))
  expect_identical(out$b, c(TRUE, FALSE, TRUE, FALSE, NA, NA))
  expect_identical(out$c, data$c)
})
