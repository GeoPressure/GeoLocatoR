library(testthat)
library(GeoLocatoR)

test_that("config_to_tibble reads inherited and tag-specific configuration", {
  file <- tempfile(fileext = ".yml")
  writeLines(
    c(
      "default:",
      "  use: true",
      "  geopressuretemplate:",
      "    likelihood: [map_pressure]",
      "  geolight_map:",
      "    fitted_location_duration: Inf",
      "  graph_create:",
      "    likelihood: [map_pressure, map_light]",
      "  tag_set_map:",
      "    include_stap_id: [1, 2]",
      "tag1:",
      "  inherits: default",
      "tag2:",
      "  inherits: default",
      "  geopressuretemplate:",
      "    likelihood: [map_light]",
      "  geolight_map:",
      "    fitted_location_duration: 5"
    ),
    file
  )

  cfg <- config_to_tibble(file, filter_return = FALSE)
  expect_equal(cfg$geopressuretemplate.likelihood, c("map_pressure", "map_light"))
  expect_equal(cfg$geolight_map.fitted_location_duration, c(Inf, 5))
  expect_equal(cfg$graph_create.likelihood, rep("map_pressure, map_light", 2))
  expect_equal(cfg$tag_set_map.include_stap_id, rep("1, 2", 2))
  expect_equal(cfg$id[cfg$use & cfg$geopressuretemplate.likelihood != "map_light"], "tag1")
})

test_that("config_to_tibble keeps id for a single tag", {
  file <- tempfile(fileext = ".yml")
  writeLines(c("default:", "  use: true", "tag1:", "  inherits: default"), file)
  expect_identical(config_to_tibble(file)$id, "tag1")
})

test_that("config_to_tibble keeps top-level tag fields", {
  file <- tempfile(fileext = ".yml")
  writeLines(
    c("default:", "  use: true", "tag1:", "  ring_number: 12345", "  tag_comments: note"),
    file
  )
  cfg <- config_to_tibble(file, filter_return = FALSE)
  expect_identical(cfg$ring_number, "12345")
  expect_identical(cfg$tag_comments, "note")
})
