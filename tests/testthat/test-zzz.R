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
