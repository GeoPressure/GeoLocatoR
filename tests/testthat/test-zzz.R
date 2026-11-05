library(testthat)
library(GeoLocatoR)

test_that("contributors_to_persons accepts a contributor without a name", {
  persons <- contributors_to_persons(list(
    list(title = "Swiss Ornithological Institute", roles = list("rightsHolder")),
    list(email = "anonymous@example.org")
  ))

  expect_length(persons, 2)
  expect_identical(persons[[1]]$given, "Swiss Ornithological Institute")
  expect_identical(persons[[2]]$email, "anonymous@example.org")
})

test_that("cast_table leaves untyped and any fields as they are", {
  schema <- list(
    fields = list(
      list(name = "value", type = "number"),
      list(name = "note"),
      list(name = "extra", type = "any")
    )
  )
  data <- data.frame(value = "1.5", note = "a", extra = 2L)

  expect_no_warning(out <- cast_table(data, schema))
  expect_identical(out, data.frame(value = 1.5, note = "a", extra = 2L))
})

test_that("cast_table warns when a cast turns values into NA", {
  schema <- list(
    fields = list(
      list(name = "value", type = "number"),
      list(name = "count", type = "integer"),
      list(name = "day", type = "date"),
      list(name = "time", type = "datetime")
    )
  )
  data <- data.frame(
    value = c("1.5", "a", NA, ""),
    count = c("1", "2", "3", "4"),
    day = c("2020-01-01", "x", NA, NA),
    time = c("2020-01-01T00:00:00Z", NA, NA, NA)
  )

  warnings <- capture_warnings(cast_table(data, schema))
  # Missing and empty values are not counted; fully cast fields stay quiet.
  expect_length(warnings, 2)
  expect_match(warnings[1], "1 non-missing value of value became NA when cast to \"number\"")
  expect_match(warnings[2], "day")
  expect_warning(cast_table(data["value"], schema), class = "gldp_warning_cast_loss")
  expect_identical(suppressWarnings(cast_table(data, schema))$count, 1:4)
})
