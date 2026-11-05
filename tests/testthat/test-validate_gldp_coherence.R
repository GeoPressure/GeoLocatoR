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
