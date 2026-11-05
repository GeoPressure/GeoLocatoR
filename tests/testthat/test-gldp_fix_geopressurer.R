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

make_mag_pkg <- function(model = "GDL3pam-v2.3", params = list()) {
  pkg <- create_gldp()
  pkg <- add_gldp_resource(
    pkg,
    "tags",
    data.frame(
      tag_id = "AA",
      ring_number = "R1",
      scientific_name = "Upupa epops",
      manufacturer = "Swiss Ornithological Institute",
      model = model
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
      value = c(1, 2, 3, 4, 5, 6, 1000)
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

test_that("gldp_fix_soi_axes() converts x/y from the hardware version", {
  # hw <= 2.3: new = -old on x and y.
  expect_message(pkg <- gldp_fix_soi_axes(make_mag_pkg("GDL3pam-v2.3")), "Converted")
  expect_equal(unname(mag_values(pkg)), c(-1, -2, 3, -4, -5, 6))
  expect_equal(pkg[["soiMagAxis"]], c("right", "forward", "down"))
  expect_equal(measurements(pkg)$value[measurements(pkg)$sensor == "pressure"], 1000)

  # hw >= 2.4: new x = -old y, new y = -old x.
  pkg <- suppressMessages(gldp_fix_soi_axes(make_mag_pkg("HW_GDL3pam_V2.4")))
  expect_equal(unname(mag_values(pkg)), c(-2, -1, 3, -5, -4, 6))
})

test_that("gldp_fix_soi_axes() prefers the mag_axis and version saved in params", {
  # params record the hw > 2.3 mapping although the model says v2.3.
  params <- list(list(id = "AA", mag_axis = c("backward", "left", "down")))
  pkg <- suppressMessages(gldp_fix_soi_axes(make_mag_pkg("GDL3pam-v2.3", params)))
  expect_equal(unname(mag_values(pkg)), c(-2, -1, 3, -5, -4, 6))

  # Data from GeoPressureR >= 3.8.0 are already in the new frame.
  params <- list(list(id = "AA", GeoPressureR_version = package_version("3.8.0")))
  pkg <- gldp_fix_soi_axes(make_mag_pkg("GDL3pam-v2.3", params))
  expect_equal(unname(mag_values(pkg)), c(1, 2, 3, 4, 5, 6))
})

test_that("gldp_fix_soi_axes() flags tags it cannot decide and skips other manufacturers", {
  expect_warning(pkg <- gldp_fix_soi_axes(make_mag_pkg(NA_character_)), "Could not decide")
  expect_equal(unname(mag_values(pkg)), c(1, 2, 3, 4, 5, 6))

  pkg <- make_mag_pkg("v2.3")
  t <- tags(pkg)
  t$manufacturer <- "Migrate Technology Limited"
  tags(pkg) <- t
  expect_silent(pkg <- gldp_fix_soi_axes(pkg))
  expect_equal(unname(mag_values(pkg)), c(1, 2, 3, 4, 5, 6))
})

test_that("validate_gldp() warns about SOI axes that are not converted", {
  messages <- capture_messages(validate_gldp_geopressure_coherence(make_mag_pkg()))
  expect_match(messages, "frame of GeoPressureR < 3.8.0", all = FALSE)
  pkg <- suppressMessages(gldp_fix_soi_axes(make_mag_pkg()))
  messages <- capture_messages(validate_gldp_geopressure_coherence(pkg))
  expect_false(any(grepl("frame of GeoPressureR", messages)))
})

test_that("gldp_fix_geopressurer() converts the SOI axes only once", {
  pkg <- suppressMessages(gldp_fix_geopressurer(make_mag_pkg()))
  expect_equal(unname(mag_values(pkg)), c(-1, -2, 3, -4, -5, 6))
  pkg <- gldp_fix_geopressurer(pkg)
  expect_equal(unname(mag_values(pkg)), c(-1, -2, 3, -4, -5, 6))
})

test_that("the SOI axis marker survives write_gldp() and read_gldp()", {
  pkg <- suppressMessages(gldp_fix_geopressurer(make_mag_pkg()))
  dir <- tempfile()
  on.exit(unlink(dir, recursive = TRUE))
  suppressMessages(write_gldp(pkg, dir))
  pkg2 <- suppressMessages(suppressWarnings(read_gldp(file.path(dir, "datapackage.json"))))
  expect_equal(unlist(pkg2[["soiMagAxis"]]), c("right", "forward", "down"))
  expect_equal(unname(mag_values(pkg2)), c(-1, -2, 3, -4, -5, 6))
})
