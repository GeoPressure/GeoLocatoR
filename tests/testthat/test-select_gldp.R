library(testthat)
library(GeoLocatoR)

test_that("file-backed selection materialises filtered resources without changing files", {
  directory <- tempfile("select-gldp-")
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(tag_id = c("AA", "BB"), ring_number = c("R1", "R2"), scientific_name = "Upupa epops")
  )
  pkg <- add_gldp_resource(
    pkg,
    "paths",
    data.frame(
      tag_id = c("AA", "BB"),
      type = "most_likely",
      stap_id = 1,
      j = 1L,
      lat = c(45, 46),
      lon = c(7, 8)
    )
  )
  write_gldp(pkg, directory)
  lazy <- read_gldp(directory, force_read = FALSE)
  selected <- select_gldp(lazy, "AA")
  expect_identical(tags(selected)$tag_id, "AA")
  expect_identical(paths(selected)$tag_id, "AA")
  loaded <- select_gldp(read_gldp(directory), "AA")
  expect_equal(tags(selected), tags(loaded))
  expect_equal(paths(selected), paths(loaded))
  expect_identical(tags(lazy)$tag_id, c("AA", "BB"))
  expect_true(all(vapply(selected$resources, function(r) is.null(r$path), logical(1))))
  expect_identical(paths(select_gldp(lazy, -"BB")), paths(selected))
})
