library(testthat)
library(GeoLocatoR)

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
