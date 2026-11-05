library(testthat)
library(GeoLocatoR)

# Two simulated flights of 10 h and 100 km: the second has the groundspeed of
# another flight, as written by GeoPressureR 3.2.0 to 3.7.0.
make_edges <- function(type = "simulation", n = 1L) {
  start <- as.POSIXct(c("2020-01-01 00:00", "2020-01-05 00:00"), tz = "UTC")
  data.frame(
    tag_id = "AA",
    type = type,
    j = 1:2,
    stap_s = 1L,
    stap_t = 2L,
    start = start,
    end = start + 10 * 3600,
    n = n,
    distance = 100,
    bearing = 90,
    gs_u = c(10, 30),
    gs_v = 0,
    ws_u = 1,
    ws_v = 2
  )
}

# One SOI tag "AA" with magnetic and acceleration x/y/z and pressure.
make_mag_pkg <- function(
  params = list(),
  value = c(1, 2, 3, 4, 5, 6),
  version = "v1.3",
  manufacturer = "Swiss Ornithological Institute"
) {
  pkg <- create_gldp(version = version)
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      scientific_name = "Upupa epops",
      manufacturer = manufacturer,
      model = "GDL3pam-v2.3"
    )
  )
  sensor <- c(
    "magnetic_x",
    "magnetic_y",
    "magnetic_z",
    "acceleration_x",
    "acceleration_y",
    "acceleration_z"
  )
  pkg <- add_gldp_resource(
    pkg,
    "measurements",
    data.frame(
      tag_id = "AA",
      sensor = c(sensor, "pressure"),
      datetime = as.POSIXct("2020-01-01", tz = "UTC"),
      value = c(value, 1000)
    )
  )
  pkg$params <- params
  pkg
}

mag_values <- function(pkg) {
  m <- measurements(pkg)
  setNames(m$value, m$sensor)[c(
    "magnetic_x",
    "magnetic_y",
    "magnetic_z",
    "acceleration_x",
    "acceleration_y",
    "acceleration_z"
  )]
}

test_that("simulated edges with a wrong groundspeed are detected", {
  expect_equal(gldp_edges_gs_mismatch(make_edges()), c(FALSE, TRUE))
  # Other trajectories, multi-flight edges and known staps cannot be checked.
  expect_equal(gldp_edges_gs_mismatch(make_edges("most_likely")), c(NA, NA))
  expect_equal(gldp_edges_gs_mismatch(make_edges(n = 2L)), c(NA, NA))
  staps <- data.frame(tag_id = "AA", stap_id = 1:2, known_lat = c(46, NA), known_lon = c(7, NA))
  expect_equal(gldp_edges_gs_mismatch(make_edges(), staps), c(NA, NA))
  # A tag whose most likely edges also mismatch uses another convention.
  e <- rbind(make_edges(), make_edges("most_likely"))
  expect_equal(gldp_edges_gs_mismatch(e), rep(NA, 4))
})

test_that("gldp_fix_edges_gs() recomputes gs and removes ws of mismatched rows only", {
  pkg <- add_gldp_resource(create_gldp(), "edges", make_edges())
  expect_message(pkg <- gldp_fix_edges_gs(pkg), "1 of 2")
  e <- edges(pkg)
  # Eastward flight at 10 km/h.
  expect_equal(e$gs_u, c(10, 10))
  expect_equal(e$gs_v, c(0, 0), tolerance = 1e-12)
  expect_equal(e$ws_u, c(1, NA))
  expect_equal(e$ws_v, c(2, NA))
  # A second pass finds nothing to repair.
  expect_silent(gldp_fix_edges_gs(pkg))
})

test_that("validate_gldp() warns about mismatched simulated edges and buggy versions", {
  pkg <- add_gldp_resource(create_gldp(), "edges", make_edges())
  pkg$params <- list(list(id = "AA", GeoPressureR_version = package_version("3.6.3")))
  messages <- capture_messages(validate_gldp_geopressure_coherence(pkg))
  expect_match(messages, "1 of 2 checked simulated", all = FALSE)
  expect_match(messages, "Tags created with GeoPressureR 3.2.0 to 3.7.0", all = FALSE)
})

test_that("gldp_mag_axis_matrix() reproduces the GeoPressureR mappings", {
  new <- gldp_mag_axis_matrix(c("right", "forward", "down"))
  expect_equal(new, matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 1), 3, byrow = TRUE))
  # hw <= 2.3: stored x = -raw y, y = -raw x (180 degrees off).
  expect_equal(
    gldp_mag_axis_matrix(c("left", "backward", "down")),
    matrix(c(0, -1, 0, -1, 0, 0, 0, 0, 1), 3, byrow = TRUE)
  )
  # hw >= 2.4: stored x = -raw x, y = -raw y.
  expect_equal(gldp_mag_axis_matrix(c("backward", "left", "down")), diag(c(-1, -1, 1)))
})

test_that("gldp_fix_soi_axes() converts from the mag_axis recorded by GeoPressureR < 3.8.0", {
  # hw <= 2.3 stored (x, y, z) = (-raw y, -raw x, raw z): raw (1, 2, 3) is (-2, -1, 3),
  # and the new frame (forward = raw y, right = raw x, down = raw z) is (2, 1, 3).
  params <- list(list(id = "AA", mag_axis = c("left", "backward", "down"), other = 1))
  pkg <- make_mag_pkg(params, value = c(-2, -1, 3, -20, -10, 30))
  expect_message(pkg <- gldp_fix_soi_axes(pkg), "Converted")
  expect_equal(unname(mag_values(pkg)), c(2, 1, 3, 20, 10, 30))
  # The params entry records the new mapping instead of the old one.
  expect_equal(
    pkg$params,
    list(list(id = "AA", other = 1, tag_create = list(mag_axis = c("right", "forward", "down"))))
  )
  expect_equal(measurements(pkg)$value[measurements(pkg)$sensor == "pressure"], 1000)

  # hw >= 2.4 stored (-raw x, -raw y, raw z): raw (1, 2, 3) is (-1, -2, 3).
  params <- list(list(id = "AA", mag_axis = c("backward", "left", "down")))
  pkg <- make_mag_pkg(params, value = c(-1, -2, 3, -10, -20, 30))
  pkg <- suppressMessages(gldp_fix_soi_axes(pkg))
  expect_equal(unname(mag_values(pkg)), c(2, 1, 3, 20, 10, 30))

  # A mapping that left the raw frame: raw (1, 2, 3) becomes (2, 1, 3).
  params <- list(list(id = "AA", mag_axis = c("forward", "right", "down")))
  pkg <- suppressMessages(gldp_fix_soi_axes(make_mag_pkg(params)))
  expect_equal(unname(mag_values(pkg)), c(2, 1, 3, 5, 4, 6))
})

test_that("gldp_fix_soi_axes() leaves data in the GeoPressureR 3.8.0 frame unchanged", {
  # A mapping recorded by GeoPressureR >= 3.8.0 (here a reversed mount) is already applied.
  params <- list(list(
    id = "AA",
    mag_axis = c("left", "backward", "down"),
    tag_create = list(mag_axis = c("left", "backward", "down"))
  ))
  expect_silent(pkg <- gldp_fix_soi_axes(make_mag_pkg(params)))
  expect_equal(unname(mag_values(pkg)), c(1, 2, 3, 4, 5, 6))
  expect_equal(pkg$params, params)

  # GeoPressureR >= 3.8.0, as a version object or as text read from JSON.
  for (version in list(package_version("3.8.0"), "3.10.1")) {
    params <- list(list(id = "AA", GeoPressureR_version = version))
    expect_silent(pkg <- gldp_fix_soi_axes(make_mag_pkg(params)))
    expect_equal(unname(mag_values(pkg)), c(1, 2, 3, 4, 5, 6))
  }

  # Other manufacturers are not mapped by GeoPressureR.
  pkg <- make_mag_pkg(manufacturer = "Migrate Technology Limited")
  expect_silent(pkg <- gldp_fix_soi_axes(pkg))
  expect_equal(unname(mag_values(pkg)), c(1, 2, 3, 4, 5, 6))
})

test_that("gldp_fix_soi_axes() leaves data of unknown frame unchanged, with one warning", {
  # No params: the data may come from any software.
  pkg <- make_mag_pkg()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(tag_id = c("AA", "BB"), ring_number = "R1", manufacturer = NA_character_),
    replace = TRUE
  )
  m <- measurements(pkg)
  measurements(pkg) <- rbind(m, transform(m, tag_id = "BB"))
  expect_warning(out <- gldp_fix_soi_axes(pkg), "unknown; data left as stored")
  expect_identical(measurements(out), measurements(pkg))
  expect_identical(out$params, list())
  warning <- conditionMessage(tryCatch(gldp_fix_soi_axes(pkg), warning = identity))
  expect_match(warning, "2 SOI tags")
  expect_match(warning, "BB")

  # GeoPressureR < 3.4.1 stored raw counts with magnetic and acceleration swapped.
  params <- list(list(id = "AA", GeoPressureR_version = package_version("3.4.0")))
  expect_warning(out <- gldp_fix_soi_axes(make_mag_pkg(params)), "swapped")
  expect_equal(unname(mag_values(out)), c(1, 2, 3, 4, 5, 6))
  expect_equal(out$params, params)

  # GeoPressureR 3.4.1 to 3.7.x only exported x/y data with a recorded mag_axis.
  params <- list(list(id = "AA", GeoPressureR_version = "3.7.1"))
  expect_warning(out <- gldp_fix_soi_axes(make_mag_pkg(params)), "unknown")
  expect_equal(unname(mag_values(out)), c(1, 2, 3, 4, 5, 6))
})

test_that("validate_gldp() warns about SOI axes outside the forward-right-down frame", {
  messages <- capture_messages(validate_gldp_geopressure_coherence(make_mag_pkg()))
  expect_match(messages, "may not be in the forward-right-down frame", all = FALSE)
  params <- list(list(id = "AA", mag_axis = c("left", "backward", "down")))
  pkg <- suppressMessages(gldp_fix_soi_axes(make_mag_pkg(params)))
  messages <- capture_messages(validate_gldp_geopressure_coherence(pkg))
  expect_false(any(grepl("forward-right-down", messages)))
})

test_that("SOI axes are converted once, when a v1.2 package is upgraded", {
  params <- list(list(id = "AA", mag_axis = c("left", "backward", "down")))
  pkg <- make_mag_pkg(params, value = c(-2, -1, 3, -20, -10, 30), version = "v1.2")

  # In memory: a second upgrade changes nothing.
  upgraded <- suppressMessages(upgrade_gldp(pkg))
  expect_identical(gldp_version(upgraded), "v1.3")
  expect_equal(unname(mag_values(upgraded)), c(2, 1, 3, 20, 10, 30))
  expect_identical(upgrade_gldp(upgraded), upgraded)

  # From disk, with and without force_read; reading the saved package changes nothing.
  dir <- tempfile()
  on.exit(unlink(dir, recursive = TRUE))
  write_gldp(pkg, file.path(dir, "v1.2"))
  for (force_read in c(TRUE, FALSE)) {
    expect_message(
      read <- read_gldp(file.path(dir, "v1.2"), force_read = force_read),
      "Converted"
    )
    expect_equal(unname(mag_values(read)), c(2, 1, 3, 20, 10, 30))
    expect_equal(read$params[[1]]$tag_create$mag_axis, c("right", "forward", "down"))
    write_gldp(read, file.path(dir, "v1.3"))
    expect_no_message(reread <- read_gldp(file.path(dir, "v1.3")), message = "Converted")
    expect_equal(unname(mag_values(reread)), c(2, 1, 3, 20, 10, 30))
  }

  # A package declaring v1.3 is never converted, whatever its params say.
  write_gldp(make_mag_pkg(params), file.path(dir, "declared"))
  declared <- read_gldp(file.path(dir, "declared"))
  expect_equal(unname(mag_values(declared)), c(1, 2, 3, 4, 5, 6))
})

test_that("a lazy upgrade reads measurements only for SOI tags outside the new frame", {
  dir <- tempfile()
  on.exit(unlink(dir, recursive = TRUE))
  params <- list(list(id = "AA", tag_create = list(mag_axis = c("right", "forward", "down"))))
  write_gldp(make_mag_pkg(params, version = "v1.2"), dir)
  pkg <- suppressMessages(read_gldp(dir, force_read = FALSE))
  expect_null(gldp_resource(pkg, "measurements")$data)
  expect_equal(unname(mag_values(pkg)), c(1, 2, 3, 4, 5, 6))
})

test_that("gldp_params_migrate() moves old param keys to tag_create$settings", {
  params <- list(
    list(
      id = "A",
      soi_settings = list(HWVersion = "GDL3pam_V2.3", SWVersion = "1.0", Other = 1),
      drift = 2.5,
      tag_create = list(manufacturer = "soi")
    ),
    list(id = "B", migratec_model = "13.2061.8"),
    list(id = "C", tag_create = list(settings = list(model = "x")))
  )
  migrated <- gldp_params_migrate(params)
  expect_equal(
    migrated[[1]],
    list(
      id = "A",
      tag_create = list(
        manufacturer = "soi",
        settings = list(
          HWVersion = "GDL3pam_V2.3",
          SWVersion = "1.0",
          Other = 1,
          model = "GDL3pam_V2.3",
          firmware = "1.0",
          clock_drift = 2.5
        )
      )
    )
  )
  expect_equal(
    migrated[[2]],
    list(id = "B", tag_create = list(settings = list(model = "13.2061.8")))
  )
  expect_equal(migrated[[3]], params[[3]])
  expect_equal(gldp_params_migrate(migrated), migrated)
  # Tags are unchanged by the migration.
  expect_equal(params_to_tags(migrated), params_to_tags(params))
})
