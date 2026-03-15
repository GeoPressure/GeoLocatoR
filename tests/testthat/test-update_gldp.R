library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("update_gldp stores derived values directly in pkg", {
  pkg <- pkg_shared

  # Update derived values
  pkg_updated <- update_gldp(pkg)

  # Should still be a valid geolocatordp
  expect_s3_class(pkg_updated, "geolocatordp")

  # Should have derived values at top-level
  expect_true(!is.null(pkg_updated$bibliographicCitation))
  expect_true(grepl("10\\.5281/zenodo\\.", pkg_updated$bibliographicCitation))
})
