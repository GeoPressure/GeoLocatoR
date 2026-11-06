library(testthat)
library(GeoLocatoR)

test_that("validate_gldp_table reads Table Schema fields, not JSON-Schema properties", {
  # A Table Schema `format` says how a column is parsed, and a field without
  # `type` is `any`: neither should fail a valid table.
  schema <- list(
    name = "custom",
    fieldsMatch = "equal",
    fields = list(
      list(name = "date", type = "date", format = "default"),
      list(name = "note", constraints = list(required = FALSE))
    )
  )
  expect_true(suppressMessages(validate_gldp_table(
    data.frame(date = as.Date("2020-01-01"), note = "a"),
    schema
  )))
})

test_that("validate_gldp_table matches a pattern against the whole value", {
  # Table Schema patterns are XML Schema regular expressions, implicitly anchored.
  schema <- list(
    name = "custom",
    fieldsMatch = "equal",
    fields = list(list(name = "code", type = "string", constraints = list(pattern = "[A-Z]{2}")))
  )
  expect_true(suppressMessages(validate_gldp_table(data.frame(code = c("AB", NA)), schema)))
  expect_message(
    expect_false(validate_gldp_table(data.frame(code = "xAB1"), schema)),
    "do not match"
  )
})

test_that("validate_gldp_foreign_keys checks foreign keys", {
  pkg <- make_geopressure_test_pkg()
  expect_true(suppressMessages(validate_gldp_foreign_keys(pkg)))
  path_idx <- which(vapply(pkg$resources, \(r) identical(r$name, "paths"), logical(1)))
  pkg[["resources"]][[path_idx[1]]]$data$stap_id[1] <- 999

  expect_false(suppressMessages(validate_gldp_foreign_keys(pkg)))
})

test_that("file-backed foreign keys fail for a single orphan tag", {
  directory <- tempfile("validate-fk-")
  on.exit(unlink(directory, recursive = TRUE))
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
      tag_id = "orphan",
      ring_number = "R1",
      observation_type = "equipment",
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      latitude = 47,
      longitude = 8,
      age_class = "0",
      sex = "U"
    )
  )
  write_gldp(pkg, directory)
  lazy <- read_gldp(directory, force_read = FALSE)
  messages <- capture_messages({
    valid <- validate_gldp_foreign_keys(lazy)
  })
  expect_false(valid)
  expect_true(any(grepl("foreign key", messages, ignore.case = TRUE)))
  expect_false(suppressMessages(validate_gldp_foreign_keys(read_gldp(directory))))
})

test_that("foreign-key validation reports missing child and parent columns", {
  pkg <- make_geopressure_test_pkg()
  resource <- gldp_resource(pkg, "paths")
  resource$data$stap_id <- NULL
  gldp_resource(pkg, "paths") <- resource
  expect_message(expect_false(validate_gldp_foreign_keys(pkg)), "missing child fields")

  pkg <- make_geopressure_test_pkg()
  resource <- gldp_resource(pkg, "staps")
  resource$data$stap_id <- NULL
  gldp_resource(pkg, "staps") <- resource
  expect_message(expect_false(validate_gldp_foreign_keys(pkg)), "missing parent fields")
})

test_that("a foreign key without a reference resource points at its own resource", {
  # Table Schema v2 omits `reference.resource` for a self-reference; v1 writes "".
  make_pkg <- function(parent, resource) {
    pkg <- create_gldp()
    pkg[["resources"]] <- list(list(
      name = "custom",
      data = data.frame(id = c("a", "b"), parent = parent),
      schema = list(
        foreignKeys = list(list(fields = "parent", reference = resource))
      )
    ))
    pkg
  }

  expect_true(suppressMessages(validate_gldp_foreign_keys(
    make_pkg(c(NA, "a"), list(fields = "id"))
  )))
  expect_true(suppressMessages(validate_gldp_foreign_keys(
    make_pkg(c(NA, "a"), list(resource = "", fields = "id"))
  )))
  expect_message(
    expect_false(validate_gldp_foreign_keys(
      make_pkg(c(NA, "orphan"), list(fields = "id"))
    )),
    "missing in parent"
  )
})

test_that("schema validation reads allowed values from categories", {
  # GeoLocator-DP v1.1 declares `categories` alongside the `enum` constraint.
  # A schema that dropped `enum` must still constrain its values.
  schema <- list(
    name = "observations",
    fields = list(
      list(
        name = "sex",
        type = "string",
        categories = list(
          list(value = "U", label = "Unknown"),
          list(value = "M", label = "Male"),
          list(value = "F", label = "Female")
        ),
        constraints = list(required = TRUE)
      )
    )
  )

  expect_true(suppressMessages(validate_gldp_table(
    tibble::tibble(sex = c("M", "F")),
    schema
  )))
  expect_false(suppressMessages(validate_gldp_table(
    tibble::tibble(sex = c("M", "female")),
    schema
  )))
})

test_that("schema validation flags a missing required column", {
  # `fieldsMatch` says nothing about which columns are required, so every mode
  # GeoLocator-DP uses would otherwise accept a table without its primary key
  # (frictionlessdata/datapackage#1126).
  schema <- list(
    name = "tags",
    fieldsMatch = "partial",
    fields = list(
      list(name = "tag_id", type = "string", constraints = list(required = TRUE)),
      list(name = "ring_number", type = "string", constraints = list(required = TRUE)),
      list(name = "tag_comments", type = "string")
    )
  )

  # An optional column may be left out.
  expect_true(suppressMessages(validate_gldp_table(
    tibble::tibble(tag_id = "a", ring_number = "r1"),
    schema
  )))

  # A required one may not.
  expect_false(suppressMessages(validate_gldp_table(
    tibble::tibble(ring_number = "r1"),
    schema
  )))
})

test_that("schema validation rejects duplicated primary keys", {
  schema <- list(
    name = "paths",
    fieldsMatch = "partial",
    primaryKey = list("tag_id", "type", "j", "stap_id"),
    fields = list(
      list(name = "tag_id", type = "string"),
      list(name = "type", type = "string"),
      list(name = "j", type = "integer"),
      list(name = "stap_id", type = "number")
    )
  )
  paths <- tibble::tibble(tag_id = "a", type = "simulation", j = c(1L, 2L), stap_id = 1)
  expect_true(suppressMessages(validate_gldp_table(paths, schema)))
  expect_message(
    expect_false(validate_gldp_table(paths[c(1, 1, 2), ], schema)),
    "1 duplicated row for its primary key"
  )
})
