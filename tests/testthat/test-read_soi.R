test_that("read_soi_directory() only takes a .glf file outside a tag folder", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "O1"))
  file.create(file.path(dir, "O1", c("AAAA_2020.pressure", "BBBB_2020.glf", "BBBB_2020.pressure")))
  gdl <- tibble(OrderName = "O1", GDL_ID = c("AAAA", "BBBB"))

  # A tag with only a .pressure file has no readable data, rather than a file passed as a directory
  expect_warning(gdl <- read_soi_directory(gdl, dir), "AAAA")
  expect_identical(gdl$directory, c(NA, file.path(dir, "O1", "BBBB_2020.glf")))
})
