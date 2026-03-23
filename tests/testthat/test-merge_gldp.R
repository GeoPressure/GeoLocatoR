library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

clone_pkg_for_merge <- function(
  pkg,
  new_id,
  tag_suffix = "",
  set_tags_datapackage_id = TRUE
) {
  pkg2 <- pkg
  pkg2$id <- new_id

  pkg2$resources <- lapply(pkg2$resources, function(resource) {
    data <- resource$data
    if (!is.data.frame(data)) {
      return(resource)
    }

    if (nzchar(tag_suffix) && "tag_id" %in% names(data)) {
      data$tag_id <- paste0(as.character(data$tag_id), tag_suffix)
    }

    if (identical(resource$name, "tags") && set_tags_datapackage_id) {
      if (!"datapackage_id" %in% names(data)) {
        data$datapackage_id <- NA_character_
      }
      data$datapackage_id <- as.character(pkg2$id)[1]
    }

    resource$data <- data
    resource
  })

  pkg2
}

source_ids_from_pkg <- function(pkg) {
  if (length(pkg$resources) == 0) {
    return(character(0))
  }

  resource_names <- vapply(
    pkg$resources,
    function(resource) as.character(resource$name)[1],
    character(1)
  )
  idx <- which(resource_names == "tags")
  if (length(idx) == 0) {
    return(character(0))
  }

  tags_data <- pkg$resources[[idx[1]]]$data
  if (!is.data.frame(tags_data)) {
    return(character(0))
  }

  if ("datapackage_id" %in% names(tags_data)) {
    ids <- as.character(tags_data$datapackage_id)
    pkg_id <- as.character(pkg$id)[1]
    ids[is.na(ids) | !nzchar(trimws(ids))] <- pkg_id
    ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
    return(unique(ids))
  }

  pkg_id <- as.character(pkg$id)[1]
  if (is.na(pkg_id) || !nzchar(trimws(pkg_id))) {
    return(character(0))
  }
  pkg_id
}

expected_source_ids <- function(pkgs) {
  unique(unlist(lapply(pkgs, source_ids_from_pkg), use.names = FALSE))
}

test_that("merge_gldp merges two packages", {
  pkg <- pkg_shared
  suppressMessages({
    pkg2 <- read_zenodo("15259763")
  })

  pkg_merged <- expect_no_error(merge_gldp(pkg, pkg2))

  expect_s3_class(pkg_merged, "geolocatordp")
  suppressMessages({
    expect_no_error(validate_gldp(pkg_merged))
  })

  merged_tags <- tags(pkg_merged)
  expect_true(nrow(merged_tags) > 0)

  expected_cols <- c(
    "datapackage_id",
    "title",
    "version",
    "created",
    "status",
    "is_draft",
    "is_published",
    "access_status",
    "embargo",
    "conceptid",
    "codeRepository",
    "homepage",
    "communities",
    "contributors",
    "licenses",
    "keywords",
    "grants",
    "relatedIdentifiers",
    "temporal_start",
    "temporal_end",
    "taxonomic",
    "numberTags_tags",
    "numberTags_measurements",
    "numberTags_light",
    "numberTags_pressure",
    "numberTags_activity",
    "numberTags_temperature_external",
    "numberTags_temperature_internal",
    "numberTags_magnetic",
    "numberTags_wet_count",
    "numberTags_conductivity",
    "numberTags_paths",
    "numberTags_pressurepaths",
    "bibliographicCitation"
  )
  expect_s3_class(pkg_merged$datapackages, "tbl_df")
  expect_equal(names(pkg_merged$datapackages), expected_cols)
  expect_false(any(
    is.na(pkg_merged$datapackages$datapackage_id) | pkg_merged$datapackages$datapackage_id == ""
  ))
  expect_false("taxonomy" %in% names(pkg_merged$datapackages))
  expect_true("taxonomic" %in% names(pkg_merged$datapackages))

  expected_ids <- expected_source_ids(list(pkg, pkg2))
  expect_setequal(unique(merged_tags$datapackage_id), expected_ids)
  source_ids <- unique(c(as.character(pkg$id)[1], as.character(pkg2$id)[1]))
  expect_true(all(source_ids %in% pkg_merged$datapackages$datapackage_id))
})

test_that("merge_gldp errors on packages with duplicate tag_ids", {
  pkg <- pkg_shared
  pkg2 <- pkg
  pkg2$id <- "https://doi.org/10.5281/zenodo.9999999"

  expect_error(merge_gldp(pkg, pkg2), "duplicate .*tag_id")
})

test_that("merge_gldp errors on packages with duplicate datapackage_id", {
  pkg <- pkg_shared
  pkg2 <- pkg
  pkg2$resources <- lapply(pkg2$resources, function(resource) {
    data <- resource$data
    if (!is.data.frame(data)) {
      return(resource)
    }
    if ("tag_id" %in% names(data)) {
      data$tag_id <- paste0(as.character(data$tag_id), "_copy")
    }
    resource$data <- data
    resource
  })

  expect_error(merge_gldp(pkg, pkg2), "duplicate .*datapackage_id")
})

test_that("merge_gldp supports sequential merges", {
  pkg1 <- pkg_shared
  suppressMessages({
    pkg2 <- read_zenodo("15259763")
  })

  pkg3 <- clone_pkg_for_merge(
    pkg = pkg2,
    new_id = "https://zenodo.org/records/15259763-copy",
    tag_suffix = "_copy",
    set_tags_datapackage_id = TRUE
  )

  merged <- expect_no_error(Reduce(merge_gldp, list(pkg1, pkg2, pkg3)))

  expect_s3_class(merged, "geolocatordp")
  merged_tags <- tags(merged)
  expect_true("datapackage_id" %in% names(merged_tags))
  expect_false(any(is.na(merged_tags$datapackage_id) | merged_tags$datapackage_id == ""))
  expect_false(any(
    is.na(merged$datapackages$datapackage_id) | merged$datapackages$datapackage_id == ""
  ))

  expected_ids <- expected_source_ids(list(pkg1, pkg2, pkg3))
  expect_setequal(unique(merged_tags$datapackage_id), expected_ids)
  source_ids <- unique(c(
    as.character(pkg1$id)[1],
    as.character(pkg2$id)[1],
    as.character(pkg3$id)[1]
  ))
  expect_true(all(source_ids %in% merged$datapackages$datapackage_id))
})

test_that("merge_gldp supports variadic and list inputs", {
  pkg1 <- pkg_shared
  pkg2 <- clone_pkg_for_merge(
    pkg = pkg_shared,
    new_id = "https://zenodo.org/records/15259763-copy-a",
    tag_suffix = "_copy_a",
    set_tags_datapackage_id = TRUE
  )
  pkg3 <- clone_pkg_for_merge(
    pkg = pkg_shared,
    new_id = "https://zenodo.org/records/15259763-copy-b",
    tag_suffix = "_copy_b",
    set_tags_datapackage_id = TRUE
  )

  merged_variadic <- expect_no_error(merge_gldp(pkg1, pkg2, pkg3))
  merged_list <- expect_no_error(merge_gldp(list(pkg1, pkg2, pkg3)))

  expect_s3_class(merged_variadic, "geolocatordp")
  expect_s3_class(merged_list, "geolocatordp")

  expected_ids <- expected_source_ids(list(pkg1, pkg2, pkg3))
  expect_setequal(unique(tags(merged_variadic)$datapackage_id), expected_ids)
  expect_setequal(unique(tags(merged_list)$datapackage_id), expected_ids)
  source_ids <- unique(c(
    as.character(pkg1$id)[1],
    as.character(pkg2$id)[1],
    as.character(pkg3$id)[1]
  ))
  expect_true(all(source_ids %in% merged_variadic$datapackages$datapackage_id))
  expect_true(all(source_ids %in% merged_list$datapackages$datapackage_id))
})

test_that("merge_gldp merges params with distinct ids", {
  pkg1 <- pkg_shared
  pkg2 <- clone_pkg_for_merge(
    pkg = pkg_shared,
    new_id = "https://zenodo.org/records/15259763-params-a",
    tag_suffix = "_params_a",
    set_tags_datapackage_id = TRUE
  )

  id1 <- unique(as.character(tags(pkg1)$tag_id))[1]
  id2 <- unique(as.character(tags(pkg2)$tag_id))[1]
  pkg1$params <- list(list(id = id1, source = "pkg1"))
  pkg2$params <- list(list(id = id2, source = "pkg2"))

  merged <- expect_no_error(merge_gldp(pkg1, pkg2))
  merged_param_ids <- vapply(merged$params, function(param) as.character(param$id)[1], character(1))
  expect_equal(merged_param_ids, c(id1, id2))
})

test_that("merge_gldp keeps duplicate param ids", {
  pkg1 <- pkg_shared
  pkg2 <- clone_pkg_for_merge(
    pkg = pkg_shared,
    new_id = "https://zenodo.org/records/15259763-params-b",
    tag_suffix = "_params_b",
    set_tags_datapackage_id = TRUE
  )

  duplicate_id <- "DUP-PARAM-ID"
  pkg1$params <- list(list(id = duplicate_id, source = "pkg1"))
  pkg2$params <- list(list(id = duplicate_id, source = "pkg2"))

  merged <- expect_no_error(merge_gldp(pkg1, pkg2))
  merged_param_ids <- vapply(merged$params, function(param) as.character(param$id)[1], character(1))
  expect_equal(merged_param_ids, c(duplicate_id, duplicate_id))
})

test_that("merge_gldp errors on missing/empty param ids", {
  pkg1 <- pkg_shared
  pkg2 <- clone_pkg_for_merge(
    pkg = pkg_shared,
    new_id = "https://zenodo.org/records/15259763-params-c",
    tag_suffix = "_params_c",
    set_tags_datapackage_id = TRUE
  )

  pkg1$params <- list(
    list(id = "", label = "empty"),
    list(label = "missing")
  )
  pkg2$params <- list(list(id = NA_character_, label = "na"))

  expect_error(merge_gldp(pkg1, pkg2), "must contain a non-empty")
})

test_that("merge_gldp keeps orphan param ids", {
  pkg1 <- pkg_shared
  pkg2 <- clone_pkg_for_merge(
    pkg = pkg_shared,
    new_id = "https://zenodo.org/records/15259763-params-d",
    tag_suffix = "_params_d",
    set_tags_datapackage_id = TRUE
  )

  pkg1$params <- list(list(id = "ORPHAN-PARAM-ID", label = "orphan"))
  pkg2$params <- list()

  merged <- expect_no_error(merge_gldp(pkg1, pkg2))
  expect_equal(as.character(merged$params[[1]]$id)[1], "ORPHAN-PARAM-ID")
})
