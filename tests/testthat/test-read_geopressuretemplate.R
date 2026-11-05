library(testthat)
library(GeoLocatoR)

test_that("manual tag metadata is retained when an interim tag is absent from it", {
  directory <- tempfile("template-metadata-")
  dir.create(file.path(directory, "data", "interim"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  param <- list(
    id = "AA",
    tag_create = list(manufacturer = "soi"),
    bird_create = list(scientific_name = "Upupa epops")
  )
  tag <- list(
    param = param,
    pressure = data.frame(date = as.POSIXct("2020-01-01", tz = "UTC"), value = 1000)
  )
  tag$twilight <- data.frame(
    twilight = as.POSIXct("2020-01-01 06:00:00", tz = "UTC"),
    rise = TRUE,
    label = NA_character_,
    stap_id = 1
  )
  tag$stap <- data.frame(
    stap_id = 1,
    include = TRUE,
    start = as.POSIXct("2020-01-01", tz = "UTC"),
    end = as.POSIXct("2020-01-02", tz = "UTC")
  )
  path_tag <- data.frame(stap_id = 1, lat = 0, lon = 0, j = 1, ind = 1, known = TRUE, nb_sample = 1)
  edge_tag <- data.frame(stap_s = 1, stap_t = 2, distance = 100, s = 1, t = 2, duration = 24)
  save(param, tag, path_tag, edge_tag, file = file.path(directory, "data", "interim", "AA.RData"))
  param$id <- "CC"
  tag$param <- param
  save(param, tag, file = file.path(directory, "data", "interim", "CC.RData"))
  write.csv(
    data.frame(
      tag_id = c("AA", "BB"),
      ring_number = c("R1", "R2"),
      scientific_name = "Upupa epops",
      manufacturer = "Swiss Ornithological Institute",
      model = "M1",
      firmware = "1",
      weight = 1,
      attachment_type = "leg harness",
      readout_method = "unknown",
      tag_comments = NA_character_
    ),
    file.path(directory, "data", "tags.csv"),
    row.names = FALSE
  )
  expect_no_warning(
    expect_warning(pkg <- read_geopressuretemplate(directory, from = "interim"), "missing")
  )
  expect_equal(paths(pkg)$lat, 0)
  expect_equal(edges(pkg)$distance, 100)
  expect_setequal(tags(pkg)$tag_id, c("AA", "BB", "CC"))
  expect_identical(tags(pkg)$ring_number[tags(pkg)$tag_id == "AA"], "R1")
  expect_identical(tags(pkg)$ring_number[tags(pkg)$tag_id == "BB"], "R2")
})

test_that("incomplete interim files identify the missing variable", {
  directory <- tempfile("template-incomplete-")
  dir.create(file.path(directory, "data", "interim"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  tag <- list()
  save(tag, file = file.path(directory, "data", "interim", "AA.RData"))
  expect_error(read_geopressuretemplate(directory, from = "interim"), "missing variable.*param")
})


test_that("CSV metadata omits optional parsers and preserves declared column types", {
  directory <- tempfile("template-optional-columns-")
  dir.create(file.path(directory, "data"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))

  # Omit multiple optional fields, including readout_method, from both tables.
  write.csv(
    data.frame(
      tag_id = "001",
      datapackage_id = NA_character_,
      scientific_name = "Upupa epops",
      firmware = "002",
      weight = 1.2
    ),
    file.path(directory, "data", "tags.csv"),
    row.names = FALSE
  )
  obs <- data.frame(
    tag_id = "001",
    ring_number = "003",
    observation_type = "equipment",
    datetime = "2020-01-01T06:00:00Z",
    latitude = -1.5,
    longitude = 36.5,
    age_class = "3",
    sex = NA_character_,
    mass = 30,
    wing_length = 99
  )
  write.csv(obs, file.path(directory, "data", "observations.csv"), row.names = FALSE)

  expect_no_warning(pkg <- read_geopressuretemplate(directory, from = "interim"))
  expect_identical(tags(pkg)$tag_id, "001")
  expect_identical(tags(pkg)$datapackage_id, NA_character_)
  expect_identical(tags(pkg)$firmware, "002")
  expect_identical(tags(pkg)$weight, 1.2)
  expect_identical(observations(pkg)$tag_id, "001")
  expect_identical(observations(pkg)$ring_number, "003")
  expect_identical(observations(pkg)$age_class, "3")
  expect_identical(observations(pkg)$sex, NA_character_)
  expect_equal(observations(pkg)$datetime, as.POSIXct("2020-01-01 06:00:00", tz = "UTC"))
  expect_identical(observations(pkg)$latitude, -1.5)
  expect_identical(observations(pkg)$longitude, 36.5)
  expect_identical(observations(pkg)$mass, 30)
  expect_identical(observations(pkg)$wing_length, 99)

  # Invalid values in present numeric columns must still produce parsing warnings.
  obs$mass <- "-"
  write.csv(obs, file.path(directory, "data", "observations.csv"), row.names = FALSE)
  expect_no_warning(
    expect_warning(
      pkg <- read_geopressuretemplate(directory, from = "interim"),
      "parsing issues"
    )
  )
  expect_true(is.na(observations(pkg)$mass))
})

test_that("normalize_gldp_params drops NULL entries", {
  normalize_params <- getFromNamespace("normalize_gldp_params", "GeoLocatoR")

  out <- normalize_params(list(NULL, list(id = "A"), NULL))
  expect_length(out, 1)
  expect_equal(as.character(out[[1]]$id)[1], "A")
})

test_that("normalize_gldp_params errors when id is missing or empty", {
  normalize_params <- getFromNamespace("normalize_gldp_params", "GeoLocatoR")

  expect_error(
    normalize_params(list(list(id = ""), list(label = "missing-id"))),
    "must contain a non-empty"
  )
})

test_that("normalize_gldp_params preserves order for valid params", {
  normalize_params <- getFromNamespace("normalize_gldp_params", "GeoLocatoR")

  params <- list(
    list(id = "tag-1", order = 1),
    list(id = "tag-2", order = 2),
    list(id = "tag-2", order = 3)
  )

  out <- normalize_params(params)
  expect_equal(vapply(out, function(param) param$order, numeric(1)), c(1, 2, 3))
})

test_that("raw-tag import applies the labelled file and known positions", {
  directory <- tempfile("template-raw-tag-")
  extdata <- system.file("extdata", package = "GeoPressureR")
  dir.create(file.path(directory, "data", "raw-tag"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  file.copy(file.path(extdata, "config.yml"), directory)
  file.copy(
    file.path(extdata, "data", "raw-tag", "18LX"),
    file.path(directory, "data", "raw-tag"),
    recursive = TRUE
  )
  file.copy(file.path(extdata, "data", "tag-label"), file.path(directory, "data"), recursive = TRUE)

  pkg <- suppressMessages(read_geopressuretemplate(directory, from = "raw-tag"))
  expect_true("flight" %in% measurements(pkg)$label)
  expect_s3_class(pkg$params[[1]]$tag_set_map$known, "data.frame")
})

test_that("interim import keeps simulated pressurepaths", {
  directory <- tempfile("template-pressurepath-")
  dir.create(file.path(directory, "data", "interim"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  param <- list(id = "AA", tag_create = list(manufacturer = "soi"))
  tag <- list(
    param = param,
    pressure = data.frame(date = as.POSIXct("2020-01-01", tz = "UTC"), value = 1000)
  )
  pressurepath_simulation <- data.frame(
    date = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 3600),
    stap_id = 1,
    lat = 0,
    lon = 0,
    j = c(1, 2)
  )
  save(
    param,
    tag,
    pressurepath_simulation,
    file = file.path(directory, "data", "interim", "AA.RData")
  )
  pkg <- suppressMessages(read_geopressuretemplate(directory, from = "interim"))
  expect_identical(unique(pressurepaths(pkg)$type), "simulation")
  expect_equal(pressurepaths(pkg)$j, c(1, 2))
})

# Minimal interim project for tag AA, with an optional config.yml.
template_interim <- function(config = NULL) {
  directory <- tempfile("template-interim-")
  dir.create(file.path(directory, "data", "interim"), recursive = TRUE)
  param <- list(id = "AA", tag_create = list(manufacturer = "soi", crop_start = "2020-01-01"))
  tag <- list(
    param = param,
    pressure = data.frame(date = as.POSIXct("2020-01-01", tz = "UTC"), value = 1000)
  )
  save(param, tag, file = file.path(directory, "data", "interim", "AA.RData"))
  if (!is.null(config)) {
    writeLines(config, file.path(directory, "config.yml"))
  }
  directory
}

test_that("numeric ring numbers in config.yml are read as text", {
  directory <- template_interim(c("AA:", "  ring_number: 12345"))
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- suppressMessages(read_geopressuretemplate(directory, from = "interim"))
  expect_identical(tags(pkg)$ring_number, "12345")
})
