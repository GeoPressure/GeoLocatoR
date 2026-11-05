library(testthat)
library(GeoLocatoR)

test_that("add_gldp_resource warns when a superset resource drops columns", {
  # `staps` declares `fieldsMatch: superset`, so a column its schema does not
  # define cannot be kept. Warn rather than lose it silently.
  data <- tibble::tibble(
    tag_id = "a",
    stap_id = 1,
    my_custom_note = "hello"
  )

  expect_warning(
    pkg <- add_gldp_resource(create_gldp(), "staps", data),
    class = "gldp_warning_undeclared_columns_dropped"
  )
  expect_false("my_custom_note" %in% names(pkg$resources[[1]]$data))
})

test_that("add_gldp_resource keeps undeclared columns on tags", {
  # `tags` declares `fieldsMatch: partial`, so extra attributes a user carries
  # alongside the standard columns are kept and described in the schema.
  data <- tibble::tibble(
    tag_id = "a",
    ring_number = "r1",
    movebank_tag_id = 12345
  )

  expect_no_warning(add_gldp_resource(create_gldp(), "tags", data))

  pkg <- add_gldp_resource(create_gldp(), "tags", data)
  resource <- pkg$resources[[1]]
  expect_true("movebank_tag_id" %in% names(resource$data))
  expect_true(
    "movebank_tag_id" %in% vapply(resource$schema$fields, \(f) f$name, character(1))
  )
})

test_that("add_gldp_resource keeps undeclared columns for a partial resource", {
  # `pressurepaths` declares `fieldsMatch: partial`, so extra columns are kept
  # and described in the schema.
  data <- tibble::tibble(
    tag_id = "a",
    date = as.Date("2024-01-01"),
    my_extra = 1.5
  )

  expect_no_warning(add_gldp_resource(create_gldp(), "pressurepaths", data))

  pkg <- add_gldp_resource(create_gldp(), "pressurepaths", data)
  resource <- pkg$resources[[1]]
  expect_true("my_extra" %in% names(resource$data))
  expect_true("my_extra" %in% vapply(resource$schema$fields, \(f) f$name, character(1)))
})

test_that("add_gldp_resource fills declared but absent columns with typed NA", {
  data <- tibble::tibble(tag_id = "a", ring_number = "r1")
  pkg <- suppressWarnings(add_gldp_resource(create_gldp(), "tags", data))

  schema_fields <- vapply(pkg$resources[[1]]$schema$fields, \(f) f$name, character(1))
  expect_identical(names(pkg$resources[[1]]$data), schema_fields)
  expect_true(all(is.na(pkg$resources[[1]]$data$scientific_name)))
})

test_that("add_gldp_resource marks resources as tabular", {
  # GeoLocator-DP requires `type: "table"` from v1.1, and frictionless only sets
  # it from version 2.0.
  data <- tibble::tibble(tag_id = "a", ring_number = "r1")
  pkg <- suppressWarnings(add_gldp_resource(create_gldp(), "tags", data))

  expect_equal(pkg$resources[[1]]$type, "table")
})

test_that("add_gldp_resource sets the data resource profile", {
  # GeoLocator-DP requires `$schema` from v1.1, and frictionless only sets it
  # from version 2.0. Implementations read it to tell which version of the
  # Data Package standard a resource follows.
  data <- tibble::tibble(tag_id = "a", ring_number = "r1")
  pkg <- suppressWarnings(add_gldp_resource(create_gldp(), "tags", data))
  resource <- pkg$resources[[1]]

  expect_equal(
    resource[["$schema"]],
    "https://datapackage.org/profiles/2.0/dataresource.json"
  )
  # `$schema` first, as frictionless 2.0 writes it.
  expect_equal(names(resource)[1], "$schema")
})

test_that("add_gldp_resource fills absent columns of a base data.frame by row count", {
  # A base data.frame does not recycle a single NA to 0 rows, so the fill used to
  # fail before frictionless could report that the table is empty.
  expect_error(
    add_gldp_resource(create_gldp(), "tags", data.frame(tag_id = character(0))),
    class = "frictionless_error_data_invalid"
  )
})

test_that("add_gldp_resource casts factors and ISO 8601 datetimes", {
  # Factors are cast from their labels, not their level codes, and the time of
  # an ISO 8601 datetime is kept.
  data <- data.frame(
    tag_id = "A",
    sensor = "pressure",
    datetime = c("2020-01-01T12:30:00Z", "2020-01-02 06:00:00"),
    value = factor(c("1008.4", "1003.5"))
  )

  pkg <- add_gldp_resource(create_gldp(), "measurements", data, cast_type = TRUE)
  out <- pkg$resources[[1]]$data

  expect_identical(out$value, c(1008.4, 1003.5))
  expect_identical(
    out$datetime,
    as.POSIXct(c("2020-01-01 12:30:00", "2020-01-02 06:00:00"), tz = "UTC")
  )
})
