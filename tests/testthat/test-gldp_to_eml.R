library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("gldp_to_eml writes eml.xml and returns EML-like object", {
  skip_if_not_installed("EML")

  pkg <- pkg_shared

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
