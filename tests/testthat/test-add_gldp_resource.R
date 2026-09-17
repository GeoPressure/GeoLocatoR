library(testthat)
library(GeoLocatoR)

test_that("add_gldp_resource warns when a superset resource drops columns", {
  # `tags` declares `fieldsMatch: superset`, so a column its schema does not
  # define cannot be kept. Warn rather than lose it silently.
  data <- tibble::tibble(
    tag_id = "a",
    ring_number = "r1",
    scientific_name = "Turdus merula",
    my_custom_note = "hello"
  )

  expect_warning(
    pkg <- add_gldp_resource(create_gldp(), "tags", data),
    class = "gldp_warning_undeclared_columns_dropped"
  )
  expect_false("my_custom_note" %in% names(pkg$resources[[1]]$data))
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
