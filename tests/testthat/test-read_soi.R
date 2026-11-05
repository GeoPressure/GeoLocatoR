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
