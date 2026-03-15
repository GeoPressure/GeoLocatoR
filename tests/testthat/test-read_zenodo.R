library(testthat)
library(GeoLocatoR)

test_that("parse_zenodo_id supports Zenodo Sandbox record URLs", {
  expect_equal(
    parse_zenodo_id("https://zenodo.org/records/470406"),
    "470406"
  )

  expect_equal(
    parse_zenodo_id("https://sandbox.zenodo.org/records/470406"),
    "470406"
  )
})

test_that("read_zenodo can read from Zenodo Sandbox", {
  skip_on_cran()
  skip_if_offline()

  run_sandbox <- tolower(Sys.getenv("RUN_ZENODO_SANDBOX_TESTS", unset = "false"))
  if (!identical(run_sandbox, "true")) {
    skip("Set RUN_ZENODO_SANDBOX_TESTS=true to run Zenodo Sandbox integration tests.")
  }

  token <- Sys.getenv("ZENODO_SANDBOX_TOKEN", unset = "")
  if (!nzchar(token)) {
    token <- NULL
  }

  pkg <- read_zenodo(
    "470406",
    sandbox = TRUE,
    token = token
  )

  expect_s3_class(pkg, "geolocatordp")
  expect_true("resources" %in% names(pkg))
  expect_true("title" %in% names(pkg))
})
