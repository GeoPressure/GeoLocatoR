library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("create_geopressuretemplate creates a project shell", {
  # Create project
  suppressMessages({
    project_dir <- create_geopressuretemplate(
      path = tempfile(),
      pkg = pkg_shared,
      open = FALSE
    )
  })

  t <- config_to_tibble(file = file.path(project_dir, "config.yml"))
  t <- config_to_tibble(file = file.path(project_dir, "config.yml"), filter_return = FALSE)

  expect_true(all(c("id", "tag_create.manufacturer") %in% names(t)))
})
