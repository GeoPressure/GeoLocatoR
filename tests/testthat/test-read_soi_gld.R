test_that("read_soi_gld() keeps retrieval coordinates and handles column selection", {
  d <- tibble(
    OrderName = c("O1", "O1"),
    GDL_ID = c("A1", "A2"),
    GDL_Type = "GDL3pam",
    Species = "Upupa epops",
    UTC_Attached = as.POSIXct("2020-05-01", tz = "UTC"),
    SiteAttached = "Site",
    LongitudeAttached = 7.5,
    LatitudeAttached = 46.2,
    UTC_Removed = as.POSIXct("2021-05-01", tz = "UTC"),
    LongitudeRemoved = c(8.31, 99999),
    LatitudeRemoved = c(47.47, 99999),
    RingNumber = c("R1", "R2"),
    FirmwareVersion = "2.4",
    HardwareVersion = "2.4",
    TotalWeight = 1.5,
    Remarks = NA_character_
  )
  o <- tibble(
    OrderName = "O1",
    NumberOrdered = 2L,
    NumberDelivered = 2L,
    GDL_Type = "GDL3pam",
    Species = "Upupa epops",
    Client = "SOI",
    ResponsibleP3 = "P3",
    ClientCategory = "own",
    RemarksProduction = NA_character_,
    RemarksOrder = NA_character_
  )

  # Default selection keeps the retrieval latitude, and 99999 is read as unknown
  gdl <- read_soi_gld(data_file = d, order_file = o)
  expect_true(all(c("LongitudeRemoved", "LatitudeRemoved") %in% names(gdl)))
  expect_equal(gdl$LatitudeRemoved, c(47.47, NA))
  expect_equal(gdl$LongitudeRemoved, c(8.31, NA))

  # A character vector selects these columns
  gdl <- read_soi_gld(data_file = d, order_file = o, filter_col = c("GDL_ID", "Client"))
  expect_named(gdl, c("GDL_ID", "Client"))

  expect_error(read_soi_gld(data_file = d), "Need")
})
