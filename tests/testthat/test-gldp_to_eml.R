library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("gldp_to_eml writes eml.xml and returns EML-like object", {
  skip_if_not_installed("EML")

  pkg <- pkg_shared

  # Allow for HTML entities like &nbsp; in free-text fields
  pkg$description <- paste0(pkg$description, " with non-breaking space: &nbsp;")
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
