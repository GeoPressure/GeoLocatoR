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
