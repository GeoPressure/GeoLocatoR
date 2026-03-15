library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("print.geolocatordp displays package information", {
  # Should print without error
  suppressMessages({
    expect_output(print(pkg_shared))
  })
})
