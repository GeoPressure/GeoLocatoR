library(testthat)
library(GeoLocatoR)
library(GeoPressureR)
library(yaml)
library(usethis)

# shared_pkg() is defined in setup.R

test_that("create_geopressuretemplate creates a project shell", {
  pkg <- shared_pkg()
  tag_id <- unique(tags(pkg)$tag_id)[1]
  param <- param_create(tag_id, default = TRUE)
  param$tag_set_map$extent <- c(-20, 30, -5, 55)
  param$geopressure_map$margin <- 42
  param$twilight_create$twl_calib <- "not-used"
  pkg[["params"]] <- list(param)

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

  pkg <- shared_pkg()
  tag_id <- measurements(pkg) |>
    dplyr::filter(.data$sensor == "pressure") |>
    dplyr::pull(.data$tag_id) |>
    unique() |>
    dplyr::first()
  skip_if(is.na(tag_id), "The shared package has no pressure data")
  pkg$resources <- purrr::map(pkg$resources, \(resource) {
    if ("tag_id" %in% names(resource$data)) {
      resource$data <- dplyr::filter(resource$data, .data$tag_id == .env$tag_id)
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

test_that("generated config rebuilds known from observations rather than params", {
  pkg <- template_config_pkg()
  param <- param_create("A", default = TRUE)
  param$tag_set_map$known <- data.frame(stap_id = 1, known_lat = 46, known_lon = 7)
  pkg[["params"]] <- list(param)
  config <- template_config_write(pkg)
  expect_equal(unlist(config$A$tag_set_map$known$stap_id), c(1, -1))
})

test_that("generated config writes functions of params read from JSON as R expressions", {
  pkg <- template_config_pkg()
  param <- param_create("A", default = TRUE)
  param$geolight_map$twl_llp <- \(n) log(n) / n^2
  param$geolight_map$fitted_location_duration <- Inf
  param$graph_create$thr_gs <- 120
  pkg[["params"]] <- list(param)
  directory <- tempfile("template-json-")
  on.exit(unlink(directory, recursive = TRUE))
  write_gldp(pkg, directory)
  pkg <- read_gldp(directory)
  expect_type(pkg$params[[1]]$geolight_map$twl_llp, "character")

  template <- file.path(directory, "template")
  dir.create(template)
  old <- setwd(template)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  writeLines(c("default:", "  tag_set_map:", "    extent: [-16, 23, 0, 50]"), "config.yml")
  create_geopressuretemplate_config(pkg)
  config <- yaml.load_file("config.yml", handlers = list(expr = \(x) paste("!expr", x)))
  expect_match(config$A$geolight_map$twl_llp, "^!expr function \\(n\\)")
  expect_match(config$A$geolight_map$twl_llp, "log(n)/n^2", fixed = TRUE)
  # Inf is read back as a number, equal to the default, so it is left out.
  expect_null(config$A$geolight_map$fitted_location_duration)
  expect_equal(config$A$graph_create$thr_gs, 120)
})

test_that("generated config does not apply the clock drift a second time", {
  pkg <- template_config_pkg()
  param <- param_create("A", default = TRUE)
  param$tag_create$time_drift <- 0.5
  param$tag_create$time_reference <- as.POSIXct("2020-01-01", tz = "UTC")
  pkg[["params"]] <- list(param)
  config <- template_config_write(pkg)
  expect_null(config$A$tag_create$time_drift)
  expect_null(config$A$tag_create$time_reference)
})

test_that("generated config skips known positions without a location", {
  pkg <- template_config_pkg()
  o <- observations(pkg)
  o$latitude[o$tag_id == "B" & o$observation_type == "retrieval"] <- NA
  pkg <- add_gldp_resource(pkg, "observations", o, replace = TRUE)
  config <- template_config_write(pkg)
  expect_equal(config$B$tag_set_map$known$stap_id, 1)
  expect_equal(config$B$tag_set_map$known$known_lon, 7)
  expect_identical(config$B$tag_create$crop_end, "2020-01-02")
})

test_that("generated known positions only hold the columns tag_set_map() reads", {
  config <- template_config_write(template_config_pkg())
  expect_named(config$A$tag_set_map$known, c("stap_id", "known_lat", "known_lon"))
})

test_that("generated config parses with unassigned and long known positions", {
  pkg <- template_config_pkg()
  o <- observations(pkg)
  sightings <- o[rep(which(o$tag_id == "A")[1], 30), ]
  sightings$observation_type <- "sighting"
  sightings$datetime <- as.POSIXct("2020-01-01 12:00", tz = "UTC") + seq_len(30)
  sightings$latitude <- 46.123456 + seq_len(30) / 1000
  pkg <- add_gldp_resource(pkg, "observations", rbind(o, sightings), replace = TRUE)
  config <- template_config_write(pkg)
  expect_equal(unlist(config$A$tag_set_map$known$stap_id), c(1, rep(0, 30), -1))
  expect_length(unlist(config$A$tag_set_map$known$known_lat), 32)
})

test_that("generated config keeps the param crop dates without equipment or retrieval date", {
  pkg <- template_config_pkg()
  o <- observations(pkg)
  o$datetime[o$tag_id == "A" & o$observation_type == "equipment"] <- NA
  o$observation_type[o$tag_id == "A" & o$observation_type == "retrieval"] <- "capture"
  pkg <- add_gldp_resource(pkg, "observations", o, replace = TRUE)
  param <- param_create("A", default = TRUE)
  param$tag_create$crop_start <- "2020-01-05"
  param$tag_create$crop_end <- "2020-02-01"
  pkg[["params"]] <- list(param)
  config <- template_config_write(pkg)
  expect_identical(config$A$tag_create$crop_start, "2020-01-05")
  expect_identical(config$A$tag_create$crop_end, "2020-02-01")
})

test_that("generated config uses the earliest equipment and the latest retrieval", {
  pkg <- template_config_pkg()
  o <- observations(pkg)
  a <- o[o$tag_id == "A" & o$observation_type %in% c("equipment", "retrieval"), ]
  a$datetime <- a$datetime + ifelse(a$observation_type == "equipment", 86400 * 3, -86400 * 3)
  pkg <- add_gldp_resource(pkg, "observations", rbind(o, a), replace = TRUE)
  expect_warning(config <- template_config_write(pkg), "earliest equipment")
  first <- min(o$datetime[o$tag_id == "A" & o$observation_type == "equipment"])
  last <- max(o$datetime[o$tag_id == "A" & o$observation_type == "retrieval"])
  expect_identical(config$A$tag_create$crop_start, format(first + 86400, "%Y-%m-%d"))
  expect_identical(config$A$tag_create$crop_end, format(last, "%Y-%m-%d"))
  expect_identical(sum(unlist(config$A$tag_set_map$known$stap_id) %in% c(1, -1)), 2L)
})

test_that("README is written from a plain-text description", {
  directory <- tempfile("template-readme-")
  dir.create(directory)
  old <- setwd(directory)
  on.exit({
    setwd(old)
    unlink(directory, recursive = TRUE)
  })
  create_geopressuretemplate_readme(create_gldp(description = "Tracking data of hoopoes"))
  expect_true("Tracking data of hoopoes" %in% readLines("README.md"))
})

test_that("CC-BY variants are not written as a plain CC-BY licence", {
  directory <- tempfile("template-license-")
  dir.create(directory)
  old <- setwd(directory)
  on.exit({
    setwd(old)
    unlink(directory, recursive = TRUE)
  })
  pkg <- create_gldp(licenses = list(list(name = "CC-BY-SA-4.0")))
  expect_warning(
    suppressMessages(create_geopressuretemplate_licenses(pkg)),
    "No matching license"
  )
  expect_false(file.exists("LICENSE.md"))
})

test_that("README DOI badge uses the package DOI", {
  directory <- tempfile("template-badge-")
  dir.create(directory)
  old <- setwd(directory)
  on.exit({
    setwd(old)
    unlink(directory, recursive = TRUE)
  })
  create_geopressuretemplate_readme(create_gldp(name = "my-package"))
  expect_false(any(grepl("zenodo", readLines("README.md"))))
  create_geopressuretemplate_readme(create_gldp(id = "https://doi.org/10.5281/zenodo.123"))
  expect_true(any(grepl(
    "(https://doi.org/10.5281/zenodo.123)",
    readLines("README.md"),
    fixed = TRUE
  )))
})

test_that("unlabelled tags are exported for labelling, not as final labels", {
  directory <- tempfile("template-data-")
  dir.create(file.path(directory, "data"), recursive = TRUE)
  old <- setwd(directory)
  on.exit({
    setwd(old)
    unlink(directory, recursive = TRUE)
  })
  pkg <- template_config_pkg()
  m <- measurements(pkg)
  m$label[m$tag_id == "A" & m$sensor == "pressure"][2] <- "flight"
  pkg <- add_gldp_resource(pkg, "measurements", m, replace = TRUE)
  suppressMessages(create_geopressuretemplate_data(pkg))
  expect_true(file.exists("data/tag-label/A-labeled.csv"))
  expect_true(file.exists("data/tag-label/B.csv"))
  expect_false(file.exists("data/tag-label/B-labeled.csv"))
})

test_that("crop dates survive a read-create-read cycle", {
  pkg <- template_config_pkg()
  config <- template_config_write(pkg)
  expect_identical(config$A$tag_create$crop_start, "2020-01-01")

  # Observations read back from the config give the same crop dates again
  o <- params_to_observations(list(list(id = "A", tag_create = config$A$tag_create)))
  expect_equal(
    o$datetime,
    observations(pkg)$datetime[observations(pkg)$tag_id == "A"]
  )
  o_pkg <- observations(pkg)
  o_pkg$datetime[o_pkg$tag_id == "A"] <- o$datetime
  pkg <- add_gldp_resource(pkg, "observations", o_pkg, replace = TRUE)
  config2 <- template_config_write(pkg)
  expect_identical(
    config2$A$tag_create[c("crop_start", "crop_end")],
    config$A$tag_create[c("crop_start", "crop_end")]
  )
})

test_that("an invalid package leaves no project folder", {
  path <- tempfile("template-invalid-")
  pkg <- template_config_pkg()
  pkg[["resources"]] <- NULL
  expect_error(create_geopressuretemplate(path, pkg = pkg, open = FALSE))
  expect_false(dir.exists(path))
})

test_that("the example tags are removed from config.yml whatever their names", {
  skip_if(!nzchar(Sys.which("git")), "git is not available")
  # Serve a minimal template from a local repository instead of GitHub
  repo <- withr::local_tempdir()
  dir.create(file.path(repo, "data", "raw-tag", "X"), recursive = TRUE)
  file.create(file.path(repo, "data", "raw-tag", "X", "pressure.csv"))
  writeLines(c("Package: GeoPressureTemplate", "Version: 1.0"), file.path(repo, "DESCRIPTION"))
  file.create(file.path(repo, "GeoPressureTemplate.Rproj"))
  writeLines(
    c("# comment", "default:", "  tag_set_map:", "    scale: 4", "", "ABC:", "  tag_create:"),
    file.path(repo, "config.yml")
  )
  withr::with_dir(repo, {
    system2("git", c("init", "--quiet"))
    system2("git", c("add", "."))
    system2(
      "git",
      c("-c", "user.name=t", "-c", "user.email=t@t", "commit", "--quiet", "-m", "init")
    )
  })
  withr::local_envvar(
    GIT_CONFIG_COUNT = "1",
    GIT_CONFIG_KEY_0 = glue::glue("url.file://{repo}.insteadOf"),
    GIT_CONFIG_VALUE_0 = "https://github.com/GeoPressure/GeoPressureTemplate"
  )

  project_dir <- suppressMessages(create_geopressuretemplate(tempfile(), open = FALSE))
  expect_identical(
    readLines(file.path(project_dir, "config.yml")),
    c("# comment", "default:", "  tag_set_map:", "    scale: 4", "")
  )
})

test_that("writing the licence leaves the active usethis project unchanged", {
  active <- withr::local_tempdir()
  local_project(active, force = TRUE, setwd = FALSE, quiet = TRUE)
  directory <- withr::local_tempdir()
  withr::with_dir(directory, {
    suppressMessages(create_geopressuretemplate_licenses(create_gldp(
      licenses = list(list(name = "CC0-1.0"))
    )))
  })
  expect_true(file.exists(file.path(directory, "LICENSE.md")))
  expect_identical(normalizePath(proj_get()), normalizePath(active))
})
