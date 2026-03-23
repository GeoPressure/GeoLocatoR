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

test_that("update_gldp_bibliographic_citation prefers conceptid", {
  pkg <- create_gldp(
    title = "GeoLocator Data Package: Example",
    id = "https://zenodo.org/records/15259676",
    conceptid = "https://doi.org/10.5281/zenodo.15259676",
    homepage = "https://zenodo.org/records/15259676",
    created = "2025-01-01T00:00:00Z",
    publisher = "Zenodo"
  )
  pkg$contributors <- list(list(title = "Doe, Jane"))

  pkg <- update_gldp_bibliographic_citation(pkg)

  expect_true(grepl("doi:10\\.5281/zenodo\\.15259676", pkg$bibliographicCitation))
  expect_false(grepl("doi:https://", pkg$bibliographicCitation, fixed = TRUE))
})

test_that("update_gldp_order_resources enforces schema resource order", {
  pkg <- pkg_shared
  skip_if(length(pkg$resources) < 2, "Package has fewer than two resources")

  pkg$resources <- rev(pkg$resources)
  ordered_pkg <- update_gldp_order_resources(pkg)

  input_names <- purrr::map_chr(pkg$resources, "name")
  output_names <- purrr::map_chr(ordered_pkg$resources, "name")

  expect_false(identical(output_names, input_names))
})

test_that("update_gldp_number_tags counts mean_acceleration_z as activity", {
  pkg <- pkg_shared

  resource_idx <- which(vapply(pkg$resources, \(r) identical(r$name, "measurements"), logical(1)))
  skip_if(length(resource_idx) == 0, "Package has no measurements resource")

  m <- pkg$resources[[resource_idx[1]]]$data
  skip_if(!is.data.frame(m) || nrow(m) == 0, "Measurements table is empty")
  skip_if(
    !all(c("tag_id", "sensor", "value", "datetime") %in% names(m)),
    "Measurements table is missing required columns"
  )

  row <- m[1, , drop = FALSE]
  row$tag_id <- "tag-mean-acc-test"
  row$sensor <- "mean_acceleration_z"
  row$value <- 1
  datetimes <- m$datetime[!is.na(m$datetime)]
  row$datetime <- if (length(datetimes) > 0) {
    max(datetimes) + 1
  } else {
    as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  }
  if ("label" %in% names(row)) {
    row$label <- NA_character_
  }

  m2 <- dplyr::bind_rows(m, row)
  pkg$resources[[resource_idx[1]]]$data <- m2

  m_expected <- if ("label" %in% names(m2)) {
    dplyr::filter(m2, .data$label != "discard" | is.na(.data$label))
  } else {
    m2
  }
  expected_activity <- length(unique(m_expected$tag_id[
    m_expected$sensor %in% c("activity", "mean_acceleration_z")
  ]))

  out <- update_gldp_number_tags(pkg)
  expect_equal(out$numberTags$activity, expected_activity)
})
