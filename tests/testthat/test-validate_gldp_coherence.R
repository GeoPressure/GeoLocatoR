library(testthat)
library(GeoLocatoR)

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

  expect_true(suppressMessages(validate_gldp_geopressure_coherence(
    make_pkg(c(NA, "a"), list(fields = "id"))
  )))
  expect_true(suppressMessages(validate_gldp_geopressure_coherence(
    make_pkg(c(NA, "a"), list(resource = "", fields = "id"))
  )))
  expect_message(
    expect_false(validate_gldp_geopressure_coherence(
      make_pkg(c(NA, "orphan"), list(fields = "id"))
    )),
    "missing in parent"
  )
})

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
