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

  # Upper-case DOIs, dx.doi.org, a trailing slash, www. and legacy /record/ URLs
  ids <- c(
    "10.5281/ZENODO.470406",
    "https://dx.doi.org/10.5281/zenodo.470406",
    "https://doi.org/10.5281/zenodo.470406/",
    "https://www.zenodo.org/records/470406",
    "https://zenodo.org/record/470406"
  )
  expect_identical(purrr::map_chr(ids, parse_zenodo_id), rep("470406", length(ids)))
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

test_that("read_zenodo_fetch_record reports an HTTP error without a JSON body", {
  httr2::local_mocked_responses(\(req) {
    httr2::response(502, headers = "Content-Type: text/html", body = charToRaw("<html></html>"))
  })

  expect_error(read_zenodo_fetch_record("1", token = "dummy"), "HTTP 502 Bad Gateway")
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
  download_timeout <- NULL

  local_mocked_bindings(
    read_zenodo_fetch_record = function(id, token, draft, sandbox) {
      fetch_token <<- token
      list()
    },
    read_zenodo_download_files = function(zenodo_record, token, sandbox, timeout) {
      download_token <<- token
      download_timeout <<- timeout
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
    expect_no_error(read_zenodo("470406", sandbox = TRUE, timeout = 1200))
  })
  expect_equal(fetch_token, "sandbox-token")
  expect_equal(download_token, "sandbox-token")
  expect_equal(download_timeout, 1200)
})

test_that("read_zenodo reads a Sandbox URL or DOI from Zenodo Sandbox", {
  fetch_sandbox <- logical()
  local_mocked_bindings(
    resolve_zenodo_token = function(token = NULL, sandbox = FALSE) NULL,
    read_zenodo_fetch_record = function(id, token, draft, sandbox) {
      fetch_sandbox <<- c(fetch_sandbox, sandbox)
      list()
    },
    read_zenodo_download_files = function(zenodo_record, token, sandbox, timeout) NULL,
    read_zenodo_attach_metadata = function(pkg, zenodo_record) pkg,
    update_gldp = function(pkg) pkg,
    .package = "GeoLocatoR"
  )

  read_zenodo("https://sandbox.zenodo.org/records/5", quiet = TRUE)
  read_zenodo("10.5072/zenodo.5", quiet = TRUE)
  read_zenodo("https://zenodo.org/records/5", quiet = TRUE)
  expect_identical(fetch_sandbox, c(TRUE, TRUE, FALSE))
})

# A Zenodo record serving `contents`, a named list of file contents by key
zenodo_test_record <- function(contents) {
  entries <- purrr::imap(contents, \(content, key) {
    path <- tempfile()
    writeLines(content, path)
    list(
      key = key,
      checksum = as.character(glue::glue("md5:{tools::md5sum(path)}")),
      links = list(content = glue::glue("https://example.org/{key}"))
    )
  })
  list(
    links = list(self_html = "https://zenodo.org/records/1"),
    files = list(enabled = TRUE, entries = unname(entries))
  )
}

# Serve the record files by writing `contents` to the requested paths
local_zenodo_downloads <- function(contents, env = parent.frame()) {
  requested <- character()
  local_mocked_bindings(
    req_perform_parallel = function(reqs, paths, ...) {
      purrr::map2(reqs, paths, \(req, path) {
        key <- basename(req$url)
        requested <<- c(requested, key)
        writeLines(contents[[key]], path)
        key
      })
    },
    .package = "httr2",
    .env = env
  )
  \() requested
}

datapackage_json <- '{"resources": [{"name": "tags", "path": "tags.csv"}]}'

test_that("read_zenodo_download_files aborts when parallel downloads fail", {
  local_mocked_bindings(
    req_perform_parallel = function(...) {
      list(simpleError("boom"))
    },
    .package = "httr2"
  )

  zenodo_record <- zenodo_test_record(list(datapackage.json = datapackage_json))

  expect_error(
    read_zenodo_download_files(zenodo_record, token = "dummy"),
    "Failed to download 1 file"
  )
})

test_that("read_zenodo_download_files counts every failed download", {
  contents <- list(
    datapackage.json = '{"resources": [{"name": "tags", "path": "tags.csv"},
      {"name": "observations", "path": "observations.csv"}]}',
    tags.csv = "tag_id",
    observations.csv = "tag_id"
  )
  zenodo_record <- zenodo_test_record(contents)
  local_mocked_bindings(
    req_perform_parallel = function(reqs, paths, ...) {
      purrr::map2(reqs, paths, \(req, path) {
        if (basename(req$url) == "datapackage.json") {
          writeLines(contents$datapackage.json, path)
          return("ok")
        }
        simpleError("HTTP 404")
      })
    },
    .package = "httr2"
  )

  expect_error(
    read_zenodo_download_files(zenodo_record, token = "dummy"),
    "Failed to download 2 files"
  )
})

test_that("read_zenodo_download_files downloads each call to its own folder", {
  contents <- list(datapackage.json = datapackage_json, tags.csv = "tag_id")
  local_zenodo_downloads(contents)
  zenodo_record <- zenodo_test_record(contents)

  dirs <- replicate(2, read_zenodo_download_files(zenodo_record, token = "dummy"))
  expect_false(dirs[1] == dirs[2])
})

test_that("read_zenodo_download_files downloads only the files the package uses", {
  contents <- list(
    datapackage.json = datapackage_json,
    tags.csv = "tag_id",
    params.json = "{}",
    README.md = "Not used"
  )
  requested <- local_zenodo_downloads(contents)

  dir <- read_zenodo_download_files(zenodo_test_record(contents), token = "dummy")

  expect_setequal(requested(), c("datapackage.json", "tags.csv", "params.json"))
  expect_false(file.exists(file.path(dir, "README.md")))
})

test_that("read_zenodo_download_files aborts on a file missing from the record", {
  contents <- list(datapackage.json = datapackage_json)
  local_zenodo_downloads(contents)

  expect_error(
    read_zenodo_download_files(zenodo_test_record(contents), token = "dummy"),
    "does not contain.*tags.csv"
  )
})

test_that("read_zenodo_download_files checks the MD5 checksum", {
  contents <- list(datapackage.json = datapackage_json, tags.csv = "tag_id")
  local_zenodo_downloads(contents)
  zenodo_record <- zenodo_test_record(contents)
  zenodo_record$files$entries[[2]]$checksum <- "md5:00000000000000000000000000000000"

  before <- list.files(tempdir(), "^geolocator-zenodo-")
  expect_error(
    read_zenodo_download_files(zenodo_record, token = "dummy"),
    "tags.csv: its MD5 checksum does not match"
  )
  expect_identical(list.files(tempdir(), "^geolocator-zenodo-"), before)
})

test_that("read_zenodo_download_files explains a record without files", {
  record <- \(enabled) {
    list(links = list(self_html = "https://zenodo.org/records/1"), files = list(enabled = enabled))
  }

  expect_warning(
    expect_null(read_zenodo_download_files(record(FALSE))),
    "metadata-only"
  )
  expect_warning(
    expect_null(read_zenodo_download_files(record(TRUE))),
    "restricted or embargoed"
  )
})

test_that("read_zenodo deletes the downloaded files after reading", {
  download_dir <- tempfile("zenodo-test-")
  dir.create(download_dir)
  read_args <- NULL
  local_mocked_bindings(
    resolve_zenodo_token = function(token = NULL, sandbox = FALSE) NULL,
    read_zenodo_fetch_record = function(id, token, draft, sandbox) list(),
    read_zenodo_download_files = function(zenodo_record, token, sandbox, timeout) download_dir,
    read_gldp = function(x, force_read) {
      read_args <<- list(x = x, force_read = force_read)
      create_gldp()
    },
    read_zenodo_attach_metadata = function(pkg, zenodo_record) pkg,
    update_gldp = function(pkg) pkg,
    .package = "GeoLocatoR"
  )

  read_zenodo("1", quiet = TRUE)

  expect_true(read_args$force_read)
  expect_false(dir.exists(download_dir))
})

test_that("read_zenodo can read from Zenodo Sandbox", {
  skip_on_cran()
  skip_if_offline()

  run_sandbox <- tolower(Sys.getenv("RUN_ZENODO_SANDBOX_TESTS", unset = "false"))
  if (!identical(run_sandbox, "true")) {
    skip("Set RUN_ZENODO_SANDBOX_TESTS=true to run Zenodo Sandbox integration tests.")
  }

  token <- Sys.getenv("ZENODO_TOKEN_SANDBOX", unset = "")
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

test_that("read_zenodo_attach_metadata keeps contributor roles as a list", {
  pkg <- structure(list(), class = c("geolocatordp", "datapackage", "list"))
  creator <- \(role) {
    list(person_or_org = list(name = "Doe, Jane"), role = role, affiliations = list())
  }
  record <- list(
    id = "1",
    metadata = list(creators = list(creator(list(id = "datacollector")), creator(NULL))),
    links = list()
  )

  out <- read_zenodo_attach_metadata(pkg, record)
  expect_identical(out$contributors[[1]]$roles, list("datacollector"))
  expect_null(out$contributors[[2]]$roles)
  expect_null(out$contributors[[1]]$organization)
})

test_that("read_zenodo_attach_metadata drops absent licence, keyword and grant values", {
  pkg <- structure(list(), class = c("geolocatordp", "datapackage", "list"))
  record <- list(
    id = "1",
    metadata = list(
      rights = list(list(title = list(en = "Custom licence"))),
      funding = list(list(funder = list(id = "00k4n6c32")), list(funder = list(name = "SNSF")))
    ),
    links = list()
  )

  out <- read_zenodo_attach_metadata(pkg, record)
  expect_identical(out$licenses, list(list(title = "Custom licence")))
  expect_identical(out$grants, "SNSF")
  expect_false("keywords" %in% names(out))

  out <- read_zenodo_attach_metadata(pkg, list(id = "1", metadata = list(), links = list()))
  expect_false(any(c("licenses", "grants") %in% names(out)))
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
