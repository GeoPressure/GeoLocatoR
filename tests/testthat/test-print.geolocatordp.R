library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("print.geolocatordp displays package information", {
  # Should print without error
  suppressMessages({
    expect_output(print(pkg_shared))
  })
})

test_that("print.geolocatordp renders HTML description as plain text", {
  pkg <- pkg_shared
  pkg[["description"]] <- "<p>This is <strong>HTML</strong> &amp; text.</p>"

  suppressMessages({
    expect_output(print(pkg))
  })
})
