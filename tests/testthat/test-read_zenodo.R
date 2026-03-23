library(testthat)
library(GeoLocatoR)

test_that("parse_zenodo_id supports Zenodo Sandbox record URLs", {
  expect_equal(
    parse_zenodo_id("10.5072/zenodo.470406"),
    "470406"
  )

  expect_equal(
    parse_zenodo_id("https://doi.org/10.5072/zenodo.470406"),
    "470406"
  )

  expect_equal(
    parse_zenodo_id("https://zenodo.org/records/470406"),
    "470406"
  )

  expect_equal(
    parse_zenodo_id("https://sandbox.zenodo.org/records/470406"),
    "470406"
  )

  expect_equal(
    parse_zenodo_id("https://zenodo.org/records/470406/"),
    "470406"
  )

  expect_equal(
    parse_zenodo_id("https://sandbox.zenodo.org/records/470406?x=1"),
    "470406"
  )
})

test_that("read_zenodo_fetch_record does not require token for request construction", {
  withr::local_envvar(c(ZENODO_TOKEN = "", ZENODO_TOKEN_SANDBOX = ""))
  local_mocked_bindings(
    resolve_zenodo_token = function(token = NULL, sandbox = FALSE) NULL,
    .package = "GeoLocatoR"
  )

  auth_called <- FALSE
  expected <- list(id = "470406")
  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_headers = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_auth_bearer_token = function(req, token) {
      auth_called <<- TRUE
      req
    },
    req_error = function(req, ...) req,
    req_perform = function(req) req,
    resp_body_json = function(resp, simplifyVector = FALSE) expected,
    .package = "httr2"
  )

  expect_equal(
    read_zenodo_fetch_record("470406", token = NULL, sandbox = TRUE),
    expected
  )
  expect_false(auth_called)
})

test_that("resolve_zenodo_token prefers sandbox env token when sandbox = TRUE", {
  withr::local_envvar(c(
    ZENODO_TOKEN_SANDBOX = "sandbox-token",
    ZENODO_TOKEN = "default-token"
  ))

  expect_equal(
    resolve_zenodo_token(token = NULL, sandbox = TRUE),
    "sandbox-token"
  )
})

test_that("resolve_zenodo_token falls back to default token in sandbox mode", {
  withr::local_envvar(c(
    ZENODO_TOKEN_SANDBOX = "",
    ZENODO_TOKEN = "default-token"
  ))

  expect_equal(
    resolve_zenodo_token(token = NULL, sandbox = TRUE),
    "default-token"
  )
})

test_that("read_zenodo reuses sandbox-resolved token for fetch and download", {
  withr::local_envvar(c(
    ZENODO_TOKEN_SANDBOX = "sandbox-token",
    ZENODO_TOKEN = ""
  ))

  fetch_token <- NULL
  download_token <- NULL

  local_mocked_bindings(
    read_zenodo_fetch_record = function(id, token, draft, sandbox) {
      fetch_token <<- token
      list()
    },
    read_zenodo_download_files = function(zenodo_record, token, sandbox) {
      download_token <<- token
      NULL
    },
    create_gldp = function(...) {
      structure(list(resources = list()), class = c("geolocatordp", "datapackage", "list"))
    },
    read_zenodo_attach_metadata = function(pkg, zenodo_record) pkg,
    update_gldp = function(pkg) pkg,
    .package = "GeoLocatoR"
  )

  suppressMessages({
    expect_no_error(read_zenodo("470406", sandbox = TRUE))
  })
  expect_equal(fetch_token, "sandbox-token")
  expect_equal(download_token, "sandbox-token")
})

test_that("read_zenodo_download_files aborts when parallel downloads fail", {
  local_mocked_bindings(
    req_perform_parallel = function(...) {
      list(simpleError("boom"))
    },
    .package = "httr2"
  )

  zenodo_record <- list(
    links = list(
      self_html = "https://zenodo.org/records/1",
      self = "https://zenodo.org/api/records/1"
    ),
    files = list(
      entries = list(
        list(
          key = "datapackage.json",
          filename = "datapackage.json",
          links = list(content = "https://example.org/datapackage.json")
        )
      )
    )
  )

  expect_error(
    read_zenodo_download_files(zenodo_record, token = "dummy"),
    "Failed to download 1 file"
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

test_that("read_zenodo_attach_metadata falls back to review community", {
  pkg <- structure(list(), class = c("geolocatordp", "datapackage", "list"))

  record_with_community <- list(
    id = "1",
    metadata = list(),
    parent = list(
      communities = list(ids = c("community-main")),
      review = list(receiver = list(community = "community-fallback"))
    ),
    links = list()
  )
  out <- read_zenodo_attach_metadata(pkg, record_with_community)
  expect_identical(out$communities, c("community-main"))

  record_with_review_only <- list(
    id = "2",
    metadata = list(),
    parent = list(
      communities = list(),
      review = list(receiver = list(community = "community-fallback"))
    ),
    links = list()
  )
  out <- read_zenodo_attach_metadata(pkg, record_with_review_only)
  expect_identical(out$communities, "community-fallback")
})

test_that("read_zenodo_attach_metadata uses version DOI as pkg$id", {
  pkg <- structure(list(), class = c("geolocatordp", "datapackage", "list"))
  record <- list(
    id = "3",
    doi = "10.5281/zenodo.1234567",
    metadata = list(),
    parent = list(id = "1234566", communities = list(), review = list()),
    links = list()
  )
  out <- read_zenodo_attach_metadata(pkg, record)
  expect_identical(out$id, "https://doi.org/10.5281/zenodo.1234567")
  expect_identical(out$conceptid, "https://doi.org/10.5281/zenodo.1234566")
})

test_that("read_zenodo_attach_metadata computes access status and embargo", {
  pkg <- structure(list(), class = c("geolocatordp", "datapackage", "list"))

  record_open <- list(
    id = "3",
    metadata = list(),
    access = list(status = "open", embargo = list(active = FALSE, until = "2030-01-01")),
    parent = list(communities = list(), review = list()),
    links = list()
  )
  out_open <- read_zenodo_attach_metadata(pkg, record_open)
  expect_identical(out_open$access_status, "open")
  expect_null(out_open$embargo)

  record_embargoed <- list(
    id = "4",
    metadata = list(),
    access = list(status = "embargoed", embargo = list(active = TRUE, until = "2030-01-01")),
    parent = list(communities = list(), review = list()),
    links = list()
  )
  out_embargoed <- read_zenodo_attach_metadata(pkg, record_embargoed)
  expect_identical(out_embargoed$access_status, "embargoed")
  expect_identical(out_embargoed$embargo, "2030-01-01")

  record_restricted <- list(
    id = "5",
    metadata = list(),
    access = list(status = "restricted", embargo = list(active = FALSE, until = "2030-01-01")),
    parent = list(communities = list(), review = list()),
    links = list()
  )
  out_restricted <- read_zenodo_attach_metadata(pkg, record_restricted)
  expect_identical(out_restricted$access_status, "restricted")
  expect_null(out_restricted$embargo)
})
