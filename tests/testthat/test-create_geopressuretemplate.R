library(testthat)
library(GeoLocatoR)
library(GeoPressureR)

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
