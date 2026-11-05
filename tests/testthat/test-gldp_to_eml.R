library(testthat)
library(GeoLocatoR)

# shared_pkg() is defined in setup.R

test_that("gldp_to_eml writes eml.xml and returns EML-like object", {
  skip_if_not_installed("EML")

  pkg <- shared_pkg()

  # Allow for HTML entities like &nbsp; in free-text fields
  pkg$description <- glue::glue("{pkg$description} with non-breaking space: &nbsp;")
  tmp <- withr::local_tempdir()

  suppressMessages({
    eml <- gldp_to_eml(pkg, directory = tmp)
  })

  # File is written
  expect_true(file.exists(file.path(tmp, "eml.xml")))
  # EML::eml_validate(file.path(tmp, "eml.xml"))
  expect_true(isTRUE(EML::eml_validate(file.path(tmp, "eml.xml"))))

  # Key metadata fields are transferred
  expect_equal(eml$dataset$title, pkg$title)
})

test_that("gldp_to_eml handles lowercase roles and title-only contributors", {
  skip_if_not_installed("EML")

  pkg <- create_gldp(title = "Example package")
  pkg$contributors <- list(
    list(title = "Org Contact", roles = "contactperson"),
    list(title = "Data Research Team", roles = "researcher")
  )

  tmp <- withr::local_tempdir()
  suppressMessages({
    eml <- gldp_to_eml(pkg, directory = tmp)
  })

  expect_true(length(eml$dataset$contact) >= 1)
  expect_true(length(eml$dataset$creator) >= 1)
})

test_that("gldp_to_eml writes valid EML without a creator or contact role", {
  skip_if_not_installed("EML")

  tmp <- tempfile("gldp-to-eml-")
  on.exit(unlink(tmp, recursive = TRUE))
  pkg <- create_gldp(title = "Example package")
  pkg$contributors <- list(
    list(title = "Doe, Jane", givenName = "Jane", familyName = "Doe", roles = "datacollector"),
    list(title = "Roe, Rick", givenName = "Rick", familyName = "Roe", roles = "supervisor")
  )

  eml <- suppressMessages(gldp_to_eml(pkg, tmp))
  expect_length(eml$dataset$creator, 2)
  expect_equal(eml$dataset$contact[[1]]$individualName$surName, "Doe")
})

test_that("gldp_to_eml adds a rights holder once", {
  skip_if_not_installed("EML")

  tmp <- tempfile("gldp-to-eml-")
  on.exit(unlink(tmp, recursive = TRUE))
  pkg <- create_gldp(title = "Example package")
  pkg$contributors <- list(
    list(title = "Doe, Jane", roles = "researcher"),
    list(title = "Bird Institute", roles = "rightsholder"),
    list(title = "Roe, Rick", roles = c("rightsholder", "datacollector"))
  )

  parties <- suppressMessages(gldp_to_eml(pkg, tmp))$dataset$associatedParty
  roles <- vapply(parties, \(party) party$role, character(1))
  names <- vapply(parties, \(party) party$organizationName, character(1))
  expect_equal(roles, c("rightsHolder", "datacollector", "rightsHolder"))
  expect_equal(names, c("Bird Institute", "Roe, Rick", "Roe, Rick"))
})

test_that("gldp_to_eml does not use a person's name as organization", {
  skip_if_not_installed("EML")

  tmp <- tempfile("gldp-to-eml-")
  on.exit(unlink(tmp, recursive = TRUE))
  pkg <- create_gldp(title = "Example package")
  pkg$contributors <- list(
    list(title = "Doe, Jane", givenName = "Jane", familyName = "Doe", roles = "researcher"),
    list(title = "Bird Institute", roles = "researcher")
  )

  creators <- suppressMessages(gldp_to_eml(pkg, tmp))$dataset$creator
  expect_null(creators[[1]]$organizationName)
  expect_equal(creators[[1]]$individualName$surName, "Doe")
  expect_equal(creators[[2]]$organizationName, "Bird Institute")
})

test_that("gldp_to_eml reads the publication year of a partial date", {
  skip_if_not_installed("EML")

  tmp <- tempfile("gldp-to-eml-")
  on.exit(unlink(tmp, recursive = TRUE))
  pkg <- create_gldp(title = "Example package", created = "2024-06")
  pkg$contributors <- list(list(title = "Doe, Jane", roles = "researcher"))

  expect_equal(suppressMessages(gldp_to_eml(pkg, tmp))$dataset$pubDate, "2024")
})

test_that("gldp_to_eml writes the bounding box of a Point and a Polygon", {
  skip_if_not_installed("EML")

  tmp <- tempfile("gldp-to-eml-")
  on.exit(unlink(tmp, recursive = TRUE))
  pkg <- create_gldp(title = "Example package")
  pkg$contributors <- list(list(title = "Doe, Jane", roles = "researcher"))

  # upgrade_gldp() builds a Point from the former referenceLocation.
  pkg$spatial <- list(type = "Point", coordinates = c(8, 46))
  box <- suppressMessages(gldp_to_eml(pkg, tmp))$dataset$coverage$geographicCoverage
  expect_equal(
    unlist(box$boundingCoordinates),
    c(
      westBoundingCoordinate = 8,
      eastBoundingCoordinate = 8,
      northBoundingCoordinate = 46,
      southBoundingCoordinate = 46
    )
  )

  pkg$spatial <- list(
    type = "Polygon",
    coordinates = list(list(list(7, 45), list(9, 45), list(9, 47), list(7, 47), list(7, 45)))
  )
  box <- suppressMessages(gldp_to_eml(pkg, tmp))$dataset$coverage$geographicCoverage
  expect_equal(
    unlist(box$boundingCoordinates),
    c(
      westBoundingCoordinate = 7,
      eastBoundingCoordinate = 9,
      northBoundingCoordinate = 47,
      southBoundingCoordinate = 45
    )
  )
})

test_that("gldp_to_eml sets the packageId system from the identifier type", {
  skip_if_not_installed("EML")

  tmp <- withr::local_tempdir()
  pkg <- create_gldp(title = "Example package")
  pkg$contributors <- list(list(title = "Doe, Jane", givenName = "Jane", familyName = "Doe"))

  pkg$id <- "https://doi.org/10.5281/zenodo.17367320"
  eml <- suppressMessages(gldp_to_eml(pkg, tmp))
  expect_equal(eml$system, "https://doi.org")

  pkg$id <- "10.5281/zenodo.17367320"
  eml <- suppressMessages(gldp_to_eml(pkg, tmp))
  expect_equal(eml$system, "https://doi.org")

  pkg$id <- "my-package"
  eml <- suppressMessages(gldp_to_eml(pkg, tmp))
  expect_equal(eml$system, "uuid")
})

test_that("gldp_to_eml returns the EML list without writing for a NULL directory", {
  skip_if_not_installed("EML")

  pkg <- create_gldp(title = "Example package")
  pkg$contributors <- list(list(title = "Doe, Jane", givenName = "Jane", familyName = "Doe"))
  withr::local_dir(withr::local_tempdir())

  expect_invisible(gldp_to_eml(pkg, NULL))
  eml <- gldp_to_eml(pkg, NULL)
  expect_equal(eml$dataset$title, "Example package")
  expect_length(list.files(), 0)
})

test_that("gldp_to_eml decodes HTML entities in text without tags", {
  skip_if_not_installed("EML")

  pkg <- create_gldp(title = "Hoopoe &amp; friends")
  pkg$contributors <- list(list(title = "Doe, Jane", givenName = "Jane", familyName = "Doe"))
  eml <- gldp_to_eml(pkg, NULL)
  expect_equal(eml$dataset$title, "Hoopoe & friends")
})
