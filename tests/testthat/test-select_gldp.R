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

test_that("observations without tag_id follow the ring of a kept tag", {
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = c("AA", "BB"),
      ring_number = c("R1", "R2"),
      scientific_name = "Upupa epops",
      manufacturer = "soi",
      model = "GDL3"
    )
  )
  pkg <- add_gldp_resource(
    pkg,
    "observations",
    data.frame(
      ring_number = c("R1", "R1", "R2", "R2"),
      tag_id = c("AA", NA, "BB", NA),
      observation_type = c("equipment", "capture", "equipment", "capture"),
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      latitude = 46,
      longitude = 7,
      age_class = "4",
      sex = "M"
    )
  )

  selected <- observations(select_gldp(pkg, -"BB"))
  expect_identical(selected$ring_number, c("R1", "R1"))
  expect_identical(selected$tag_id, c("AA", NA))
})

test_that("select_gldp drops the params of removed tags", {
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = c("AA", "BB"),
      ring_number = c("R1", "R2"),
      scientific_name = "Upupa epops",
      manufacturer = "soi",
      model = "GDL3"
    )
  )
  pkg$params <- list(list(id = "AA"), list(id = "BB"))

  selected <- select_gldp(pkg, "AA")
  expect_identical(vapply(selected$params, \(param) param$id, character(1)), "AA")
  expect_null(select_gldp(create_gldp() |> add_gldp_resource("tags", tags(pkg)), "AA")$params)
})
