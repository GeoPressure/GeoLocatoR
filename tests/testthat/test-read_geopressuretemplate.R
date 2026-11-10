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

test_that("raw-tag import warns when the label file cannot be applied", {
  directory <- tempfile("template-raw-tag-")
  extdata <- system.file("extdata", package = "GeoPressureR")
  dir.create(file.path(directory, "data", "raw-tag"), recursive = TRUE)
  dir.create(file.path(directory, "data", "tag-label"))
  on.exit(unlink(directory, recursive = TRUE))
  file.copy(file.path(extdata, "config.yml"), directory)
  file.copy(
    file.path(extdata, "data", "raw-tag", "18LX"),
    file.path(directory, "data", "raw-tag"),
    recursive = TRUE
  )
  writeLines("not,a,label,file", file.path(directory, "data", "tag-label", "18LX-labeled.csv"))

  expect_warning(
    pkg <- suppressMessages(read_geopressuretemplate(directory, from = "raw-tag")),
    "Labelling 18LX"
  )
  expect_identical(tags(pkg)$tag_id, "18LX")
  expect_false("flight" %in% measurements(pkg)$label)
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

test_that("interim tags of older GeoPressureR are repaired from the evidence in their params", {
  directory <- tempfile("template-repair-")
  dir.create(file.path(directory, "data", "interim"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  # GeoPressureR 3.6.3 stored hw <= 2.3 axes as (-raw y, -raw x, raw z).
  param <- list(
    id = "AA",
    GeoPressureR_version = package_version("3.6.3"),
    tag_create = list(manufacturer = "soi"),
    mag_axis = c("left", "backward", "down")
  )
  tag <- list(
    param = param,
    pressure = data.frame(date = as.POSIXct("2020-01-01", tz = "UTC"), value = 1000),
    magnetic = data.frame(
      date = as.POSIXct("2020-01-01", tz = "UTC"),
      magnetic_x = -2,
      magnetic_y = -1,
      magnetic_z = 3,
      acceleration_x = -20,
      acceleration_y = -10,
      acceleration_z = 30
    )
  )
  # The second flight carries the groundspeed of another one.
  start <- as.POSIXct(c("2020-01-01 00:00", "2020-01-05 00:00"), tz = "UTC")
  edge_simulation <- data.frame(
    stap_s = 1,
    stap_t = 2,
    s = 1,
    t = 2,
    lat_s = 0,
    lon_s = 0,
    lat_t = 0,
    lon_t = 1,
    start = start,
    end = start + 10 * 3600,
    duration = 10,
    n = 1,
    distance = 100,
    bearing = 90,
    gs = complex(real = c(10, 30)),
    ws = complex(real = 1, imaginary = 2),
    j = 1:2
  )
  save(param, tag, edge_simulation, file = file.path(directory, "data", "interim", "AA.RData"))
  # A tag without version or axis record is left as stored.
  param <- list(id = "BB", tag_create = list(manufacturer = "soi"))
  tag$param <- param
  save(param, tag, file = file.path(directory, "data", "interim", "BB.RData"))

  expect_warning(
    expect_message(
      pkg <- read_geopressuretemplate(directory, from = "interim"),
      "Recomputed"
    ),
    "unknown"
  )
  m <- measurements(pkg)
  mag <- m[m$sensor != "pressure", ]
  value <- \(id) {
    setNames(mag$value[mag$tag_id == id], mag$sensor[mag$tag_id == id])[c(
      "magnetic_x",
      "magnetic_y",
      "magnetic_z",
      "acceleration_x",
      "acceleration_y",
      "acceleration_z"
    )]
  }
  expect_equal(unname(value("AA")), c(2, 1, 3, 20, 10, 30))
  expect_equal(unname(value("BB")), c(-2, -1, 3, -20, -10, 30))
  expect_equal(pkg$params[[1]]$tag_create$mag_axis, c("right", "forward", "down"))
  expect_null(pkg$params[[1]]$mag_axis)
  expect_null(pkg$params[[2]]$tag_create$mag_axis)
  expect_equal(edges(pkg)$gs_u, c(10, 10))
  expect_equal(edges(pkg)$ws_u, c(1, NA))
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

  # YAML reads an unquoted leading zero as an octal number.
  directory <- template_interim(c("AA:", "  ring_number: 0012345"))
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  pkg <- suppressMessages(read_geopressuretemplate(directory, from = "interim"))
  expect_identical(tags(pkg)$ring_number, "0012345")
})

test_that("config.yml fields of a tag inherit from the default entry", {
  directory <- template_interim(c(
    "default:",
    "  tag_comments: shared comment",
    "  bird_create:",
    "    scientific_name: Upupa epops",
    "AA:",
    "  tag_comments: own comment"
  ))
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- suppressMessages(read_geopressuretemplate(directory, from = "interim"))
  expect_identical(tags(pkg)$scientific_name, "Upupa epops")
  expect_identical(tags(pkg)$tag_comments, "own comment")
})

test_that("interim trajectories are found by prefix, with their type from the suffix", {
  directory <- template_interim()
  on.exit(unlink(directory, recursive = TRUE))
  path <- file.path(directory, "data", "interim", "AA.RData")
  load(path)
  pressurepath <- data.frame(
    date = as.POSIXct("2020-01-01", tz = "UTC"),
    stap_id = 1,
    lat = 0,
    lon = 0,
    j = 1
  )
  pressurepath_geopressureviz <- pressurepath
  pressurepath_tag <- pressurepath
  path_unknown <- data.frame(stap_id = 1, lat = 0, lon = 0, j = 1)
  marginal <- list()
  save(
    param,
    tag,
    pressurepath_geopressureviz,
    pressurepath_tag,
    path_unknown,
    marginal,
    file = path
  )

  expect_warning(
    pkg <- suppressMessages(read_geopressuretemplate(directory, from = "interim")),
    "path_unknown"
  )
  expect_setequal(pressurepaths(pkg)$type, c("geopressureviz", "tag"))
  expect_null(gldp_resource(pkg, "paths"))
})

test_that("interim tags take their species from config.yml", {
  directory <- template_interim(c("AA:", "  bird_create:", "    scientific_name: Upupa epops"))
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- suppressMessages(read_geopressuretemplate(directory, from = "interim"))
  expect_identical(tags(pkg)$scientific_name, "Upupa epops")
})

test_that("generated observations take the ring number of their tag", {
  directory <- template_interim(c("AA:", "  ring_number: R1"))
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- suppressMessages(read_geopressuretemplate(directory, from = "interim"))
  expect_identical(observations(pkg)$ring_number, c("R1", "R1"))
})

test_that("xlsx metadata is parsed by column name", {
  skip_if_not_installed("readxl")
  directory <- tempfile("template-xlsx-")
  dir.create(file.path(directory, "data"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  file.copy(
    test_path("fixtures", "geopressuretemplate", c("tags.xlsx", "observations.xlsx")),
    file.path(directory, "data")
  )
  pkg <- suppressMessages(read_geopressuretemplate(directory, from = "interim"))
  expect_identical(tags(pkg)$ring_number, "002")
  expect_identical(tags(pkg)$weight, 1.2)
  expect_identical(observations(pkg)$mass, 30)
  expect_equal(observations(pkg)$datetime, as.POSIXct("2020-01-01 06:00:00", tz = "UTC"))
})

test_that("a pkg with resources is not overwritten without confirmation", {
  directory <- template_interim()
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- add_gldp_resource(create_gldp(), "tags", data.frame(tag_id = "BB"))
  expect_error(
    read_geopressuretemplate(directory, from = "interim", pkg = pkg),
    "interactive session"
  )
})

test_that("raw-tag read failures are reported with their error message", {
  directory <- tempfile("template-raw-tag-error-")
  dir.create(file.path(directory, "data", "raw-tag", "ZZ"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  file.copy(file.path(system.file("extdata", package = "GeoPressureR"), "config.yml"), directory)
  writeLines("not a tag", file.path(directory, "data", "raw-tag", "ZZ", "notes.txt"))
  expect_warning(
    suppressMessages(read_geopressuretemplate(directory, from = "raw-tag")),
    "ZZ.*manufacturer"
  )
})
