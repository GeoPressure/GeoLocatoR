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
