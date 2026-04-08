library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

make_geopressure_test_pkg <- function() {
  pkg <- create_gldp()
  start <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  end <- as.POSIXct("2024-01-01 06:00:00", tz = "UTC")

  # Minimal in-memory resources to exercise range and FK coherence checks.
  pkg[["resources"]] <- list(
    list(
      name = "tags",
      data = data.frame(tag_id = "tag-1", stringsAsFactors = FALSE),
      schema = list()
    ),
    list(
      name = "staps",
      data = data.frame(
        tag_id = "tag-1",
        stap_id = 1,
        start = start,
        end = end,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          )
        )
      )
    ),
    list(
      name = "twilights",
      data = data.frame(
        tag_id = "tag-1",
        twilight = start + 1800,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          )
        )
      )
    ),
    list(
      name = "paths",
      data = data.frame(
        tag_id = "tag-1",
        type = "most_likely",
        stap_id = 1,
        lat = 45,
        lon = 8,
        j = 1L,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          ),
          list(
            fields = c("tag_id", "stap_id"),
            reference = list(resource = "staps", fields = c("tag_id", "stap_id"))
          )
        )
      )
    ),
    list(
      name = "edges",
      data = data.frame(
        tag_id = "tag-1",
        type = "most_likely",
        stap_s = 1,
        stap_t = 1,
        lat_s = 45,
        lon_s = 8,
        lat_t = 46,
        lon_t = 9,
        j = 1L,
        start = start,
        end = start + 3600,
        n = 1L,
        distance = 100,
        gs_u = 5,
        gs_v = 5,
        ws_u = 2,
        ws_v = 2,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          ),
          list(
            fields = c("tag_id", "stap_s"),
            reference = list(resource = "staps", fields = c("tag_id", "stap_id"))
          ),
          list(
            fields = c("tag_id", "stap_t"),
            reference = list(resource = "staps", fields = c("tag_id", "stap_id"))
          )
        )
      )
    ),
    list(
      name = "pressurepaths",
      data = data.frame(
        tag_id = "tag-1",
        datetime = start + 600,
        stap_id = 1,
        type = "most_likely",
        lat = 45,
        lon = 8,
        j = 1L,
        altitude = 100,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          )
        )
      )
    )
  )

  pkg
}

test_that("validate_gldp validates a strict geolocatordp", {
  expect_no_error(suppressMessages({
    validate_gldp(pkg_shared)
  }))
})

test_that("validate_gldp_meta checks missing title", {
  local_mocked_bindings(
    resources = function(x) character(0),
    .package = "frictionless"
  )

  pkg <- pkg_shared
  pkg[["title"]] <- ""
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- character(0)
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_meta checks title prefix", {
  local_mocked_bindings(
    resources = function(x) character(0),
    .package = "frictionless"
  )

  pkg <- pkg_shared
  pkg[["title"]] <- "Wrong title"
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- character(0)
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_meta checks contributor role completeness", {
  local_mocked_bindings(
    resources = function(x) character(0),
    .package = "frictionless"
  )

  pkg <- pkg_shared
  pkg[["title"]] <- "GeoLocator Data Package: Example"
  pkg[["contributors"]] <- list(list(roles = character(0)))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- character(0)
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_meta checks record_type is dataset", {
  local_mocked_bindings(
    resources = function(x) character(0),
    .package = "frictionless"
  )

  pkg <- pkg_shared
  pkg[["title"]] <- "GeoLocator Data Package: Example"
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "software"
  pkg[["taxonomic"]] <- character(0)
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_meta checks allowed manufacturers", {
  local_mocked_bindings(
    resources = function(x) "tags",
    .package = "frictionless"
  )
  local_mocked_bindings(
    tags = function(x) {
      data.frame(manufacturer = "Unknown Manufacturer", stringsAsFactors = FALSE)
    },
    .package = "GeoLocatoR"
  )

  pkg <- pkg_shared
  pkg[["title"]] <- "GeoLocator Data Package: Example"
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- character(0)
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_meta checks relatedIdentifiers presence", {
  local_mocked_bindings(
    resources = function(x) character(0),
    .package = "frictionless"
  )

  pkg <- pkg_shared
  pkg[["title"]] <- "GeoLocator Data Package: Example"
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list()
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- character(0)
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_meta checks selected community", {
  local_mocked_bindings(
    resources = function(x) character(0),
    .package = "frictionless"
  )

  pkg <- pkg_shared
  pkg[["title"]] <- "GeoLocator Data Package: Example"
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("not-allowed-community")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- character(0)
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_meta checks codeRepository URL", {
  local_mocked_bindings(
    resources = function(x) character(0),
    .package = "frictionless"
  )

  pkg <- pkg_shared
  pkg[["title"]] <- "GeoLocator Data Package: Example"
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://gitlab.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- character(0)
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_meta checks taxonomic names against eBird taxonomy", {
  local_mocked_bindings(
    resources = function(x) character(0),
    .package = "frictionless"
  )
  local_mocked_bindings(
    request = function(url) list(url = url),
    req_headers = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_perform = function(req) req,
    resp_body_json = function(resp, simplifyVector = TRUE) {
      data.frame(sciName = c("Hirundo rustica", "Fringilla coelebs"))
    },
    .package = "httr2"
  )

  pkg <- pkg_shared
  pkg[["title"]] <- "GeoLocator Data Package: Example"
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- c("Hirundo rustica", "Not a species")
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_geopressure_coherence accepts realistic values", {
  pkg <- make_geopressure_test_pkg()
  expect_true(suppressMessages(validate_gldp_geopressure_coherence(pkg)))
})

test_that("validate_gldp_geopressure_coherence warns on unusual distances", {
  pkg <- make_geopressure_test_pkg()
  edge_idx <- which(vapply(pkg$resources, \(r) identical(r$name, "edges"), logical(1)))
  pkg[["resources"]][[edge_idx[1]]]$data$distance[1] <- 4000

  expect_message(
    expect_true(validate_gldp_geopressure_coherence(pkg)),
    "unusual distance"
  )
})

test_that("validate_gldp_geopressure_coherence checks foreign keys", {
  pkg <- make_geopressure_test_pkg()
  path_idx <- which(vapply(pkg$resources, \(r) identical(r$name, "paths"), logical(1)))
  pkg[["resources"]][[path_idx[1]]]$data$stap_id[1] <- 999

  expect_false(suppressMessages(validate_gldp_geopressure_coherence(pkg)))
})
