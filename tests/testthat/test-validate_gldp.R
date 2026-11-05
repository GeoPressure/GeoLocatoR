library(testthat)
library(GeoLocatoR)

# shared_pkg() is defined in setup.R

test_that("validate_gldp validates a strict geolocatordp", {
  pkg <- shared_pkg()
  expect_no_error(suppressMessages({
    valid <- validate_gldp(pkg)
  }))
  expect_true(valid)
})

test_that("validate_gldp_resources accepts v1 and v2 tabular resources", {
  # frictionless < 2.0 marks tabular resources with `profile`, >= 2.0 with
  # `type`. Both must validate.
  make_pkg <- function(...) {
    pkg <- create_gldp()
    pkg[["resources"]] <- list(
      c(
        list(
          name = "tags",
          data = data.frame(tag_id = "tag-1", stringsAsFactors = FALSE),
          schema = list(
            name = "tags",
            fieldsMatch = "superset",
            fields = list(list(name = "tag_id", type = "string"))
          )
        ),
        list(...)
      )
    )
    pkg
  }

  expect_true(suppressMessages(
    validate_gldp_resources(make_pkg(profile = "tabular-data-resource"))
  ))
  expect_true(suppressMessages(
    validate_gldp_resources(make_pkg(type = "table"))
  ))
  expect_false(suppressMessages(
    validate_gldp_resources(make_pkg())
  ))
})

test_that("validate_gldp_resources reports custom and params resources without failing", {
  # The profile allows `params` and custom resources to be any file. Only the
  # tables GeoLocator-DP defines must be tabular.
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
  pkg[["resources"]] <- c(
    pkg$resources,
    list(
      list(name = "params", path = "params.json"),
      list(name = "readme", path = "readme.pdf")
    )
  )
  expect_message(expect_true(validate_gldp_resources(pkg)), "is not checked")

  pkg[["resources"]][[1]]$schema <- NULL
  expect_message(expect_false(validate_gldp_resources(pkg)), "not a tabular resource")
})

test_that("resource validation reads file-backed tables and reports missing identity metadata", {
  directory <- tempfile("validate-lazy-")
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
  write_gldp(pkg, directory)
  lazy <- read_gldp(directory, force_read = FALSE)
  expect_true(suppressMessages(validate_gldp_resources(lazy)))
  # Missing ring numbers remain a validation failure; they are not repaired on read.
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = NA_character_,
      scientific_name = "Upupa epops",
      manufacturer = "test",
      model = "test"
    ),
    replace = TRUE
  )
  write_gldp(pkg, directory)
  lazy <- read_gldp(directory, force_read = FALSE)
  messages <- capture_messages({
    valid <- validate_gldp_resources(lazy)
  })
  expect_false(valid)
  expect_true(any(grepl("ring_number", messages, fixed = TRUE)))
})
