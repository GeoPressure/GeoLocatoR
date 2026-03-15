# Default GeoLocator-DP schema version used when creating/upgrading packages.
.gldp_supported_versions <- c("main", "v0.1", "v0.2", "v0.3", "v0.4", "v0.5", "v0.6", "v1.0")
.gldp_default_version <- utils::tail(.gldp_supported_versions, 1)

#' Get GeoLocator DP version
#'
#' @description
#' Extract the GeoLocator-DP version from `x$schema`.
#'
#' @param x A GeoLocator Data Package object.
#' @return Character string version (e.g. `"v1.0"`), or `NA` when not parseable.
#' @family misc functions
#' @export
gldp_version <- function(x) {
  check_gldp(x)
  supported_versions <- .gldp_supported_versions

  schema <- purrr::pluck(x, "$schema", .default = NULL)

  if (is.null(schema) || !nzchar(schema)) {
    cli_abort("{.field $schema} is missing or empty, cannot determine the GLDP version.")
  }

  m <- regexec("GeoLocator-DP/([0-9A-Za-z.-]+)/", schema)
  out <- regmatches(schema, m)[[1]]

  if (length(out) < 2) {
    cli_abort("Cannot extract version from {.field $schema}: {.val {schema}}.")
  }

  version <- out[2]

  if (!version %in% supported_versions) {
    cli_abort(
      c(
        "Unsupported GeoLocator-DP version: {.val {version}}.",
        "i" = "Supported versions are: {.val {supported_versions}}."
      )
    )
  }

  version
}

#' @noRd
gldp_schema_url <- function(version = NULL) {
  supported_versions <- .gldp_supported_versions

  if (is.null(version) || is.na(version)) {
    version <- .gldp_default_version
  }

  if (!version %in% supported_versions) {
    cli_abort(c(
      "!" = "Unsupported GeoLocator-DP version {.val {version}}.",
      "i" = "Allowed versions are {.val {supported_versions}}."
    ))
  }

  schema_url <- glue::glue(
    "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/{version}/geolocator-dp-profile.json"
  )

  schema_url
}

#' @noRd
set_version <- function(x, version) {
  x$resources <- purrr::map(x$resources %||% list(), \(r) {
    name <- r$name %||% NULL
    if (is.character(name) && length(name) == 1 && !is.na(name) && nzchar(name)) {
      schema <- glue::glue(
        "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/{version}/{name}-table-schema.json"
      )
      r$`$schema` <- schema
    }
    r
  })
  x$`$schema` <- gldp_schema_url(version)
  x
}

#' @noRd
upgrade_gldp <- function(x, to_version = .gldp_default_version) {
  check_gldp(x)
  supported_versions <- .gldp_supported_versions

  from_version <- gldp_version(x)

  if (!to_version %in% supported_versions) {
    cli_abort(c(
      "!" = "Unsupported target GeoLocator-DP version {.val {to_version}}.",
      "i" = "Allowed versions are {.val {supported_versions}}."
    ))
  }

  if (identical(from_version, to_version)) {
    return(x)
  }

  from_rank <- match(from_version, supported_versions)
  to_rank <- match(to_version, supported_versions)
  if (is.na(from_rank) || is.na(to_rank)) {
    cli_abort(c(
      "x" = "Unsupported version comparison ({.val {from_version}} -> {.val {to_version}})."
    ))
  }

  if (from_rank > to_rank) {
    cli_abort(c(
      "x" = "Downgrade is not supported ({.val {from_version}} -> {.val {to_version}})."
    ))
  }

  while (!identical(from_version, to_version)) {
    next_version <- switch(
      from_version,
      "main" = "v0.1",
      "v0.1" = "v0.2",
      "v0.2" = "v0.3",
      "v0.3" = "v0.4",
      "v0.4" = "v0.5",
      "v0.5" = "v0.6",
      "v0.6" = "v1.0",
      NA_character_
    )

    cli_inform(c(
      "i" = "Upgrading GeoLocator-DP from {.val {from_version}} to {.val {next_version}}."
    ))

    x <- switch(
      from_version,
      "main" = set_version(x, "v0.1"),
      "v0.1" = step_v0_1_to_v0_2(x),
      "v0.2" = step_v0_2_to_v0_3(x),
      "v0.3" = step_v0_3_to_v0_4(x),
      "v0.4" = step_v0_4_to_v0_5(x),
      "v0.5" = step_v0_5_to_v0_6(x),
      "v0.6" = step_v0_6_to_v1_0(x)
    )

    from_version <- gldp_version(x)
  }

  x
}

#' @noRd
step_v0_1_to_v0_2 <- function(x) {
  # Release: https://github.com/Rafnuss/GeoLocator-DP/releases/tag/v0.2
  # citation -> bibliographicCitation
  # https://github.com/Rafnuss/GeoLocator-DP/commit/477f3899194edf2a31b93a1019d654714084eeb4
  if (!is.null(x$citation) && is.null(x$bibliographicCitation)) {
    x$bibliographicCitation <- x$citation
  }

  # reference_location -> referenceLocation
  # https://github.com/Rafnuss/GeoLocator-DP/commit/b9a1f0dc1d1788fcd5dcc3be4bf52ac3ac4c6de8
  if (!is.null(x$reference_location) && is.null(x$referenceLocation)) {
    x$referenceLocation <- x$reference_location
  }

  # Drop removed metadata fields.
  # https://github.com/Rafnuss/GeoLocator-DP/commit/db091f74245c1b521d3dbe2a3665452bf4f70c53
  x$citation <- NULL
  x$reference_location <- NULL
  x$homepage <- NULL
  x$image <- NULL
  x$references <- NULL
  x$name <- NULL

  # Add numberTags top-level field.
  # https://github.com/Rafnuss/GeoLocator-DP/commit/8e059382e1157fa9a97962f01a3bf05da600f7f7
  if (is.null(x$numberTags)) {
    x$numberTags <- list()
  }

  # Build spatial from referenceLocation when missing.
  # https://github.com/Rafnuss/GeoLocator-DP/commit/2e91036e6bfb718623b4b784e41abf43541c827d
  if (is.null(x$spatial) && !is.null(x$referenceLocation)) {
    lat <- as.numeric(x$referenceLocation$latitude %||% NA_real_)
    lon <- as.numeric(x$referenceLocation$longitude %||% NA_real_)
    if (!is.na(lat) && !is.na(lon)) {
      x$spatial <- list(type = "Point", coordinates = c(lon, lat))
    }
  }

  # firwmare -> firmware (typo fix).
  # https://github.com/Rafnuss/GeoLocator-DP/commit/90b65c555720a006aa814e91f04dc85209be7df1
  x <- mutate_resource(x, "tags", \(d) {
    if ("firwmare" %in% names(d) && !"firmware" %in% names(d)) {
      names(d)[names(d) == "firwmare"] <- "firmware"
    }
    d
  })
  x <- mutate_resource(x, "observations", \(d) {
    # life_stage -> age_class
    # https://github.com/Rafnuss/GeoLocator-DP/commit/4a6d8bcae5d211822799ad4f09e341687d53d321
    if ("life_stage" %in% names(d) && !"age_class" %in% names(d)) {
      names(d)[names(d) == "life_stage"] <- "age_class"
    }
    if ("device_status" %in% names(d)) {
      # Replace broken_damage with missing.
      # https://github.com/Rafnuss/GeoLocator-DP/commit/56f19ad6de7dd2c80e5839b05f49975ea79dfa3a
      i <- !is.na(d$device_status) & d$device_status == "broken_damage"
      d$device_status[i] <- "missing"
    }
    d
  })

  set_version(x, "v0.2")
}

#' @noRd
step_v0_2_to_v0_3 <- function(x) {
  # Release: https://github.com/Rafnuss/GeoLocator-DP/releases/tag/v0.3
  # embargo removed from required fields (schema-only change)
  # https://github.com/Rafnuss/GeoLocator-DP/commit/c489984a3c4928d28f52615d2032a17c93d0909e
  set_version(x, "v0.3")
}

#' @noRd
step_v0_3_to_v0_4 <- function(x) {
  # Release: https://github.com/Rafnuss/GeoLocator-DP/releases/tag/v0.4
  # Add tags.datapackage_id
  # https://github.com/Rafnuss/GeoLocator-DP/commit/a85af2d8ad9f16297fb5cbb07a448d1c3aab24b2
  x <- mutate_resource(x, "tags", \(d) {
    if (!"datapackage_id" %in% names(d)) {
      fill <- if (!is.null(x$id)) as.character(x$id) else NA_character_
      d$datapackage_id <- rep(fill, nrow(d))
    }
    d
  })

  set_version(x, "v0.4")
}

#' @noRd
step_v0_4_to_v0_5 <- function(x) {
  # Release: https://github.com/Rafnuss/GeoLocator-DP/releases/tag/v0.5
  # Add edges.type
  # https://github.com/Rafnuss/GeoLocator-DP/commit/60251cd0117856fc7302ce9d3d0abfab81ad0893
  x <- mutate_resource(x, "edges", \(d) {
    if ("type" %in% names(d) && !any(is.na(d$type) | trimws(as.character(d$type)) == "")) {
      return(d)
    }

    if (!"type" %in% names(d)) {
      d$type <- NA_character_
    }
    missing <- is.na(d$type) | trimws(as.character(d$type)) == ""

    # Reconstruct missing edge type using the same logic as GeoLocatorAggregator/scripts/fix_edges.R.
    if (all(c("tag_id", "j", "stap_s") %in% names(d))) {
      idx_by_tag <- split(seq_len(nrow(d)), as.character(d$tag_id))
      for (idx in idx_by_tag) {
        j_num <- suppressWarnings(as.numeric(d$j[idx]))
        j1 <- !is.na(j_num) & j_num == 1
        n_staps <- length(unique(stats::na.omit(d$stap_s[idx][j1])))
        j1_rank <- cumsum(j1)
        inferred <- ifelse(j1 & j1_rank <= n_staps, "most_likely", "simulation")

        fill <- idx[missing[idx]]
        if (length(fill) > 0) {
          d$type[fill] <- inferred[missing[idx]]
        }
      }
      missing <- is.na(d$type) | trimws(as.character(d$type)) == ""
    }

    if (any(missing)) {
      paths <- {
        idx <- which(vapply(x$resources %||% list(), \(r) identical(r$name, "paths"), logical(1)))
        if (length(idx) == 0) NULL else x$resources[[idx[1]]]$data %||% NULL
      }
      can_map <- is.data.frame(paths) &&
        all(c("tag_id", "stap_id", "type") %in% names(paths)) &&
        all(c("tag_id", "stap_s", "stap_t") %in% names(d))

      if (can_map) {
        key <- paste(paths$tag_id, paths$stap_id)
        type_map <- stats::setNames(as.character(paths$type), key)

        d$type[missing] <- type_map[paste(d$tag_id[missing], d$stap_s[missing])]
        missing <- is.na(d$type) | trimws(as.character(d$type)) == ""
        if (any(missing)) {
          d$type[missing] <- type_map[paste(d$tag_id[missing], d$stap_t[missing])]
        }
      }
    }

    missing <- is.na(d$type) | trimws(as.character(d$type)) == ""
    d$type[is.na(d$type)] <- "most_likely"
    d$type[missing] <- "most_likely"
    d$type <- as.character(d$type)
    d
  })

  set_version(x, "v0.5")
}

#' @noRd
step_v0_5_to_v0_6 <- function(x) {
  # Release: https://github.com/Rafnuss/GeoLocator-DP/releases/tag/v0.6
  # Remove pressurepaths.ind
  # https://github.com/Rafnuss/GeoLocator-DP/commit/c278695d094ea40e13fea6458a078a7711ea0d1a
  x <- mutate_resource(x, "pressurepaths", \(d) {
    if ("ind" %in% names(d)) {
      d$ind <- NULL
    }
    d
  })

  set_version(x, "v0.6")
}

#' @noRd
step_v0_6_to_v1_0 <- function(x) {
  # Release: https://github.com/Rafnuss/GeoLocator-DP/releases/tag/v1.0

  # Drop non-deployed tags (missing ring_number or scientific_name).
  # https://github.com/Rafnuss/GeoLocator-DP/commit/795ccc4cdd0bf575b39378ca3e63c61247ac2b8e
  x <- mutate_resource(x, "tags", \(d) {
    if (!"ring_number" %in% names(d)) {
      d$ring_number <- NA_character_
    }
    if (!"scientific_name" %in% names(d)) {
      d$scientific_name <- NA_character_
    }

    is_missing <- function(v) {
      is.na(v) | trimws(as.character(v)) == ""
    }

    drop <- is_missing(d$ring_number) | is_missing(d$scientific_name)
    if (any(drop)) {
      dropped_ids <- if ("tag_id" %in% names(d)) d$tag_id[drop] else NA_character_
      dropped_ids <- as.character(dropped_ids)
      dropped_ids[is.na(dropped_ids) | trimws(dropped_ids) == ""] <- "<missing_tag_id>"
      dropped_ids <- unique(dropped_ids)

      cli_inform(c(
        "i" = "Removed {sum(drop)} non-deployed tag row{?s} while upgrading to {.val v1.0}.",
        "i" = "Dropped {.field tag_id}: {.val {paste(dropped_ids, collapse = ', ')}}."
      ))
    }

    d[!drop, , drop = FALSE]
  })

  set_version(x, "v1.0")
}

#' @noRd
mutate_resource <- function(pkg, resource_name, fn) {
  idx <- which(vapply(pkg$resources, \(r) identical(r$name, resource_name), logical(1)))
  if (length(idx) == 0) {
    return(pkg)
  }

  resource <- pkg$resources[[idx[1]]]
  if (!is.data.frame(resource$data)) {
    return(pkg)
  }

  resource$data <- fn(resource$data)
  pkg$resources[[idx[1]]] <- resource
  pkg
}
