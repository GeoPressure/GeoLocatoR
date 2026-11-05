library(testthat)
library(GeoLocatoR)

test_that("validate_gldp_meta checks missing title", {
  pkg <- shared_pkg()
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
  pkg <- shared_pkg()
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
  pkg <- shared_pkg()
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
  pkg <- shared_pkg()
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
    tags = function(x) {
      data.frame(manufacturer = "Unknown Manufacturer", stringsAsFactors = FALSE)
    },
    .package = "GeoLocatoR"
  )

  pkg <- shared_pkg()
  pkg[["title"]] <- "GeoLocator Data Package: Example"
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- character(0)
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})

test_that("validate_gldp_meta accepts every manufacturer GeoPressureR imports", {
  local_mocked_bindings(
    tags = function(x) {
      data.frame(
        manufacturer = c(
          "Swiss Ornithological Institute",
          "Migrate Technology Limited",
          "Lund University",
          "British Antarctic Survey",
          "PresTag"
        )
      )
    },
    .package = "GeoLocatoR"
  )
  pkg <- create_gldp()
  pkg[["resources"]] <- list(list(name = "tags"))
  expect_no_message(validate_gldp_meta(pkg), message = "manufacturer")
})

test_that("validate_gldp_meta words the title prefix as a community expectation", {
  pkg <- create_gldp(title = "Wrong title")
  expect_message(validate_gldp_meta(pkg), "Zenodo")
})

test_that("validate_gldp_meta checks relatedIdentifiers presence", {
  pkg <- shared_pkg()
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
  pkg <- shared_pkg()
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
  pkg <- shared_pkg()
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
    request = function(url) list(url = url),
    req_headers = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_perform = function(req) req,
    resp_body_json = function(resp, simplifyVector = TRUE) {
      data.frame(sciName = c("Hirundo rustica", "Fringilla coelebs"))
    },
    .package = "httr2"
  )

  pkg <- shared_pkg()
  pkg[["title"]] <- "GeoLocator Data Package: Example"
  pkg[["contributors"]] <- list(list(roles = "author"))
  pkg[["relatedIdentifiers"]] <- list(list(relatedIdentifier = "https://doi.org/10.1/abc"))
  pkg[["communities"]] <- list("b7c70316-310b-435e-9a8b-84188d60a3cc")
  pkg[["codeRepository"]] <- "https://github.com/example/repo"
  pkg[["record_type"]] <- "dataset"
  pkg[["taxonomic"]] <- c("Hirundo rustica", "Not a species")
  suppressMessages(expect_false(validate_gldp_meta(pkg)))
})
