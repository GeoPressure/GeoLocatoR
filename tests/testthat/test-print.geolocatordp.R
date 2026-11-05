library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("print.geolocatordp displays package information", {
  # Should print without error
  suppressMessages({
    expect_output(print(pkg_shared))
  })
})

test_that("print.geolocatordp renders HTML description as plain text", {
  pkg <- pkg_shared
  pkg[["description"]] <- "<p>This is <strong>HTML</strong> &amp; text.</p>"

  suppressMessages({
    expect_output(print(pkg))
  })
})

test_that("print.geolocatordp prints a plain-text description", {
  pkg <- pkg_shared
  # Plain text must not be handed to xml2, which would read it as a file path.
  pkg[["description"]] <- "The Master compiles curated, open-access packages. It contains tags."

  out <- cli::cli_fmt(print(pkg))
  expect_true(any(grepl("compiles curated", out, fixed = TRUE)))
})

test_that("print.geolocatordp prints braces in free text literally", {
  pkg <- pkg_shared
  pkg[["description"]] <- "Range {not glue} and <b>bold</b>"
  pkg[["contributors"]] <- list(list(title = "Doe, {Jane}", roles = list("researcher")))

  out <- cli::cli_fmt(print(pkg))
  expect_true(any(grepl("{not glue}", out, fixed = TRUE)))
  expect_true(any(grepl("{Jane}", out, fixed = TRUE)))
})

test_that("html_to_text handles plain text, HTML and malformed fragments", {
  expect_identical(html_to_text("plain  text\nhere"), "plain text here")
  expect_identical(html_to_text("<p>a <strong>b</strong></p>"), "a b")
  expect_identical(html_to_text(NA_character_), NA_character_)
  expect_match(html_to_text(strrep("x", 300)), "\\.\\.\\.$")
})

test_that("print.geolocatordp reports access and community only for a Zenodo package", {
  pkg <- create_gldp()
  out <- cli::cli_fmt(print(pkg))
  expect_false(any(grepl("access:|community:", out)))

  pkg[["access_status"]] <- "metadata-only"
  out <- cli::cli_fmt(print(pkg))
  expect_true(any(grepl("access: metadata only", out, fixed = TRUE)))
  expect_true(any(grepl("no Geolocator DP community", out, fixed = TRUE)))

  pkg[["access_status"]] <- "open"
  out <- cli::cli_fmt(print(pkg))
  expect_true(any(grepl("access: Open access", out, fixed = TRUE)))
})

test_that("print.geolocatordp labels the package identifier by its field name", {
  pkg <- create_gldp()
  pkg[["id"]] <- "my-local-package"
  out <- cli::cli_fmt(print(pkg))
  expect_true(any(grepl('id: "my-local-package"', out, fixed = TRUE)))
  expect_false(any(grepl("doi:", out, fixed = TRUE)))

  pkg[["id"]] <- "https://doi.org/10.5281/zenodo.1"
  out <- cli::cli_fmt(print(pkg))
  expect_true(any(grepl("id: <https://doi.org/10.5281/zenodo.1>", out, fixed = TRUE)))
})
