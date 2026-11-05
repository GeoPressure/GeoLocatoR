test_that("read_soi_directory() only takes a .glf file outside a tag folder", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "O1"))
  file.create(file.path(dir, "O1", c("AAAA_2020.pressure", "BBBB_2020.glf", "BBBB_2020.pressure")))
  gdl <- tibble(OrderName = "O1", GDL_ID = c("AAAA", "BBBB"))

  # A tag with only a .pressure file has no readable data, rather than a file passed as a directory
  expect_warning(gdl <- read_soi_directory(gdl, dir), "AAAA")
  expect_identical(gdl$directory, c(NA, file.path(dir, "O1", "BBBB_2020.glf")))
})

test_that("read_soi() reads only the tag's own sensor files next to a .glf file", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "O1"))
  # Minimal SOI files: six header lines, then date, time and value
  write_soi <- function(file, value) {
    writeLines(
      c(rep("header", 6), glue::glue("01.05.2020 00:{c('00', '30')}\t{value}")),
      file.path(dir, "O1", file)
    )
  }
  write_soi("AAAA_2020.glf", 10)
  write_soi("AAAA_2020.pressure", 1000)
  write_soi("BBBB_2020.glf", 20)
  write_soi("BBBB_2020.pressure", 900)
  gdl <- tibble(OrderName = "O1", GDL_ID = c("AAAA", "BBBB"))

  pkg <- suppressMessages(read_soi(gdl, dir))
  pressure <- filter(measurements(pkg), .data$sensor == "pressure")
  expect_identical(unique(pressure$value[pressure$tag_id == "AAAA"]), 1000)
  expect_identical(unique(pressure$value[pressure$tag_id == "BBBB"]), 900)
})

test_that("read_soi() keeps tags and observations when no tag data can be read", {
  dir <- withr::local_tempdir()
  gdl <- tibble(
    GDL_ID = c("AAAA", "BBBB"),
    RingNumber = c("R1", "R2"),
    UTC_Attached = as.POSIXct("2020-05-01", tz = "UTC"),
    directory = c(dir, NA)
  )

  # An empty directory makes GeoPressureR fail for the only tag with a directory
  expect_warning(pkg <- suppressMessages(read_soi(gdl, dir)), "Could not read the data")
  expect_error(measurements(pkg), class = "gldp_error_resource_not_found")
  expect_identical(tags(pkg)$tag_id, c("AAAA", "BBBB"))
})

test_that("read_soi() names the model by the tag type when the hardware version is unknown", {
  gdl <- tibble(
    GDL_ID = c("AAAA", "BBBB"),
    GDL_Type = "GDL3pam",
    HardwareVersion = c("2.4", NA),
    directory = NA_character_
  )

  pkg <- suppressMessages(read_soi(gdl))
  expect_identical(tags(pkg)$model, c("GDL3pam-2.4", "GDL3pam"))
})

test_that("read_soi() describes the attachment from the harness columns, or leaves it missing", {
  gdl <- tibble(
    GDL_ID = c("AAAA", "BBBB"),
    Harness_data = c("leg-loop", NA),
    HarnessAttachement_data = c("glued", NA),
    directory = NA_character_
  )

  pkg <- suppressMessages(read_soi(gdl))
  expect_identical(tags(pkg)$attachment_type, c("leg-loop|attachment:glued", NA))
})

test_that("read_soi() keeps undated observations only with generate_observations = TRUE", {
  gdl <- tibble(
    GDL_ID = c("AAAA", "BBBB"),
    RingNumber = c("R1", "R2"),
    UTC_Attached = as.POSIXct(c("2020-05-01", NA), tz = "UTC"),
    directory = NA_character_
  )

  o <- observations(suppressMessages(read_soi(gdl, generate_observations = TRUE)))
  expect_identical(nrow(o), 4L)
  o <- observations(suppressMessages(read_soi(gdl, generate_observations = FALSE)))
  expect_identical(o$tag_id, "AAAA")
  expect_identical(o$observation_type, "equipment")
})

test_that("read_soi_directory() matches the whole GDL_ID and warns on several candidates", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "O1", "A10_2020"), recursive = TRUE)
  dir.create(file.path(dir, "O1", "B1_2019"))
  dir.create(file.path(dir, "O1", "B1_2020"))
  file.create(file.path(dir, "O1", c("A1-2020.glf", "A10_2020.glf")))
  gdl <- tibble(OrderName = "O1", GDL_ID = c("A1", "B1"))

  expect_warning(gdl <- read_soi_directory(gdl, dir), "B1_2019")
  expect_identical(
    gdl$directory,
    file.path(dir, "O1", c("A1-2020.glf", "B1_2020"))
  )
})

test_that("read_soi() marks the tag as present on equipment and retrieval", {
  gdl <- tibble(
    GDL_ID = "AAAA",
    UTC_Attached = as.POSIXct("2020-05-01", tz = "UTC"),
    directory = NA_character_
  )
  o <- observations(suppressMessages(read_soi(gdl)))
  expect_identical(o$device_status, c("present", "present"))
})
