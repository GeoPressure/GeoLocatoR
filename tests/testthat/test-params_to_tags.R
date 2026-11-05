library(testthat)
library(GeoLocatoR)

test_that("params_to_tags reads model and firmware from old and new SOI settings", {
  params <- list(
    list(id = "A", soi_settings = list(`HW Version` = "HW_A", `FW Version` = "FW_A")),
    list(id = "B", soi_settings = list(HWVersion = "HW_B", SWVersion = "FW_B"))
  )
  t <- params_to_tags(params)
  expect_identical(t$model, c("HW_A", "HW_B"))
  expect_identical(t$firmware, c("FW_A", "FW_B"))
})

test_that("params_to_tags names British Antarctic Survey tags", {
  t <- params_to_tags(list(list(id = "A", tag_create = list(manufacturer = "bas"))))
  expect_identical(t$manufacturer, "British Antarctic Survey")
  t <- params_to_tags(list(list(id = "A", tag_create = list(manufacturer = "prestag"))))
  expect_identical(t$manufacturer, "PresTag")
})

test_that("params_to_tags reads the GeoPressureR 3.8.0 tag_create$settings first", {
  params <- list(
    list(
      id = "A",
      tag_create = list(settings = list(model = "HW_new", firmware = "FW_new")),
      soi_settings = list(`HW Version` = "HW_old", `FW Version` = "FW_old")
    ),
    list(id = "B", migratec_model = "13.2061.8")
  )
  t <- params_to_tags(params)
  expect_identical(t$model, c("HW_new", "13.2061.8"))
  expect_identical(t$firmware, c("FW_new", NA))
})
