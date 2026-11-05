library(testthat)
library(GeoLocatoR)
library(GeoPressureR)
library(yaml)

# pkg_shared is loaded from setup.R

test_that("create_geopressuretemplate creates a project shell", {
  pkg <- pkg_shared
  tag_id <- unique(tags(pkg)$tag_id)[1]
  param <- param_create(tag_id, default = TRUE)
  param$tag_set_map$extent <- c(-20, 30, -5, 55)
  param$geopressure_map$margin <- 42
  param$twilight_create$twl_calib <- "not-used"
  pkg$params <- list(param)

  # Create project
  suppressMessages({
    project_dir <- create_geopressuretemplate(
      path = tempfile(),
      pkg = pkg,
      open = FALSE
    )
  })

  t <- config_to_tibble(file = file.path(project_dir, "config.yml"))
  t <- config_to_tibble(file = file.path(project_dir, "config.yml"), filter_return = FALSE)

  expect_true(all(c("id", "tag_create.manufacturer") %in% names(t)))

  config <- withr::with_dir(
    project_dir,
    geopressuretemplate_config(tag_id, config::get(config = tag_id))
  )
  expect_equal(config$tag_set_map$extent, c(-20, 30, -5, 55))
  expect_equal(config$geopressure_map$margin, 42)
  expect_equal(config$tag_create$manufacturer, "tabular")
  expect_equal(config$tag_create$time_shift, 0)

  yaml <- readLines(file.path(project_dir, "config.yml"))
  expect_true(any(grepl("extent: \\[-20.0, 30.0, -5.0, 55.0\\]", yaml)))
  expect_false(any(grepl("twl_calib:|directory:|twilight-label", yaml)))
})

test_that("generated GeoPressureTemplate runs the full workflow", {
  skip_if(
    Sys.getenv("RUN_GEOPRESSURETEMPLATE_TEST") != "true",
    "Set RUN_GEOPRESSURETEMPLATE_TEST=true to run the external integration test."
  )
  glp_available <- tryCatch(
    {
      httr2::req_perform(httr2::request("https://glp.mgravey.com"))
      TRUE
    },
    error = \(e) FALSE
  )
  skip_if(!glp_available, "The GeoPressure pressure-data service is unavailable.")

  pkg <- pkg_shared
  tag_id <- measurements(pkg) |>
    dplyr::filter(.data$sensor == "pressure") |>
    dplyr::pull(.data$tag_id) |>
    unique() |>
    dplyr::first()
  skip_if(is.na(tag_id), "The shared package has no pressure data")
  pkg$resources <- purrr::map(pkg$resources, \(resource) {
    if ("tag_id" %in% names(resource$data)) {
      resource$data <- dplyr::filter(resource$data, .data$tag_id == tag_id)
    }
    resource
  })

  project_dir <- create_geopressuretemplate(
    path = tempfile(),
    pkg = pkg,
    open = FALSE
  )
  interim_file <- withr::with_dir(project_dir, {
    geopressuretemplate(tag_id, quiet = TRUE)
    glue::glue("./data/interim/{tag_id}.RData")
  })

  expect_true(file.exists(file.path(project_dir, interim_file)))
})

# Two tags with different sensors, written to a project holding only a default config.
template_config_pkg <- function() {
  d <- as.POSIXct("2020-01-01", tz = "UTC") + 3600 * (0:3)
  m <- rbind(
    data.frame(
      tag_id = "A",
      sensor = "pressure",
      datetime = d,
      value = 1000,
      label = NA_character_
    ),
    data.frame(tag_id = "A", sensor = "light", datetime = d, value = 1, label = NA_character_),
    data.frame(
      tag_id = "A",
      sensor = "temperature_external",
      datetime = d,
      value = 20,
      label = NA_character_
    ),
    data.frame(tag_id = "B", sensor = "pressure", datetime = d, value = 1000, label = NA_character_)
  )
  t <- data.frame(
    tag_id = c("A", "B"),
    ring_number = c("R1", "R2"),
    scientific_name = "Upupa epops",
    manufacturer = "Swiss Ornithological Institute",
    model = "M"
  )
  o <- data.frame(
    ring_number = rep(c("R1", "R2"), each = 2),
    tag_id = rep(c("A", "B"), each = 2),
    observation_type = c("equipment", "retrieval"),
    datetime = as.POSIXct(c("2019-12-31", "2020-01-02"), tz = "UTC"),
    latitude = 46,
    longitude = 7,
    age_class = "0",
    sex = "U"
  )
  create_gldp() |>
    add_gldp_resource("tags", t) |>
    add_gldp_resource("observations", o) |>
    add_gldp_resource("measurements", m)
}

template_config_write <- function(pkg) {
  directory <- tempfile("template-config-")
  dir.create(directory)
  old <- setwd(directory)
  on.exit({
    setwd(old)
    unlink(directory, recursive = TRUE)
  })
  writeLines(c("default:", "  tag_set_map:", "    extent: [-16, 23, 0, 50]"), "config.yml")
  create_geopressuretemplate_config(pkg)
  yaml.load_file("config.yml")
}

test_that("generated config only references the sensor files of each tag", {
  config <- template_config_write(template_config_pkg())
  expect_identical(config$A$tag_create$light_file, "light.csv")
  expect_null(config$B$tag_create$light_file)
})

test_that("generated config references temperature files", {
  config <- template_config_write(template_config_pkg())
  expect_identical(config$A$tag_create$temperature_external_file, "temperature_external.csv")
})
