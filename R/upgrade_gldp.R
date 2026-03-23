#' @noRd
upgrade_gldp <- function(x, to_version = .gldp_default_version) {
  check_gldp(x)

  from_version <- gldp_version(x)

  if (!to_version %in% .gldp_supported_versions) {
    cli_abort(c(
      "!" = "Unsupported target GeoLocator-DP version {.val {to_version}}.",
      "i" = "Allowed versions are {.val {.gldp_supported_versions}}."
    ))
  }

  if (identical(from_version, to_version)) {
    return(x)
  }

  from_rank <- match(from_version, .gldp_supported_versions)
  to_rank <- match(to_version, .gldp_supported_versions)
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

  cli_alert_success("Upgrading GeoLocator-DP from {.val {from_version}} to {.val {to_version}}.")

  while (!identical(from_version, to_version)) {
    x <- switch(
      from_version,
      "main" = {
        x[["$schema"]] <- gldp_schema_url("v0.2")
        x
      },
      "v0.1" = upgrade_gldp_v0_1_to_v0_2(x),
      "v0.2" = upgrade_gldp_v0_2_to_v0_3(x),
      "v0.3" = upgrade_gldp_v0_3_to_v0_4(x),
      "v0.4" = upgrade_gldp_v0_4_to_v0_5(x),
      "v0.5" = upgrade_gldp_v0_5_to_v0_6(x),
      "v0.6" = upgrade_gldp_v0_6_to_v1_0(x)
    )

    from_version <- gldp_version(x)
  }

  # Final normalization enforces target-version schemas/types after stepwise mutations.
  x <- normalize_upgraded_resources(x)

  x
}

#' @noRd
normalize_upgraded_resources <- function(x) {
  # Step upgrades modify data and bump profile version incrementally. This final pass
  # rebuilds known resources against the target schema so write/read stays stable.
  # We only touch resources already present in the package and keep custom resources unchanged.
  target_version <- gldp_version(x)
  profile <- gldp_profile_schema(target_version)
  supported_resources <- profile$allOf[[2]]$properties$resources$items$oneOf |>
    purrr::map(~ .x$properties$name$enum %||% .x$properties$name$const %||% character(0)) |>
    purrr::flatten_chr()

  resources <- x$resources %||% list()
  for (resource in resources) {
    resource_name <- as.character(resource$name %||% NA_character_)[1]
    if (is.na(resource_name) || !nzchar(resource_name)) {
      next
    }
    if (!resource_name %in% supported_resources) {
      next
    }
    if (is.data.frame(resource$data)) {
      # Rebuild resource from target schema for stable write/read round-trips.
      x <- add_gldp_resource(
        pkg = x,
        resource_name = resource_name,
        data = resource$data,
        cast_type = TRUE,
        replace = TRUE
      )
    } else {
      # Keep schema in sync when data are not loaded in memory.
      idx <- which(vapply(
        x$resources %||% list(),
        \(r) identical(r$name, resource_name),
        logical(1)
      ))
      if (length(idx) > 0) {
        x$resources[[idx[1]]]$schema <- gldp_resource_schema(target_version, resource_name)
      }
    }
  }

  x
}

#' @noRd
upgrade_gldp_v0_1_to_v0_2 <- function(x) {
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

  x[["$schema"]] <- gldp_schema_url("v0.2")
  x
}

#' @noRd
upgrade_gldp_v0_2_to_v0_3 <- function(x) {
  # Release: https://github.com/Rafnuss/GeoLocator-DP/releases/tag/v0.3
  # embargo removed from required fields (schema-only change)
  # https://github.com/Rafnuss/GeoLocator-DP/commit/c489984a3c4928d28f52615d2032a17c93d0909e
  x[["$schema"]] <- gldp_schema_url("v0.3")
  x
}

#' @noRd
upgrade_gldp_v0_3_to_v0_4 <- function(x) {
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

  x[["$schema"]] <- gldp_schema_url("v0.4")
  x
}

#' @noRd
upgrade_gldp_v0_4_to_v0_5 <- function(x) {
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

    # Fill missing `type` using legacy ordering, then paths lookup, then a required fallback.
    if (all(c("tag_id", "j", "stap_s") %in% names(d))) {
      # Legacy exports encode the most-likely chain first in `j == 1` rows.
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
      # Recover from `paths$type` when available; edges may match on either endpoint.
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
    # v0.5 requires `type`; unresolved rows keep the historical default.
    d$type[is.na(d$type)] <- "most_likely"
    d$type[missing] <- "most_likely"
    d$type <- as.character(d$type)
    d
  })

  x[["$schema"]] <- gldp_schema_url("v0.5")
  x
}

#' @noRd
upgrade_gldp_v0_5_to_v0_6 <- function(x) {
  # Release: https://github.com/Rafnuss/GeoLocator-DP/releases/tag/v0.6
  # Remove pressurepaths.ind
  # https://github.com/Rafnuss/GeoLocator-DP/commit/c278695d094ea40e13fea6458a078a7711ea0d1a
  x <- mutate_resource(x, "pressurepaths", \(d) {
    if ("ind" %in% names(d)) {
      d$ind <- NULL
    }
    d
  })

  x[["$schema"]] <- gldp_schema_url("v0.6")
  x
}

#' @noRd
upgrade_gldp_v0_6_to_v1_0 <- function(x) {
  # Release: https://github.com/Rafnuss/GeoLocator-DP/releases/tag/v1.0

  # Remove deprecated fields from paths/staps/edges.
  # https://github.com/Rafnuss/GeoLocator-DP/commit/8ef7ad8cf9e48e82d8396f535536fb8e5b9c119f
  # https://github.com/Rafnuss/GeoLocator-DP/commit/418f7c926fdff4af0a1c5e48d57a50f8a61c95d2
  x <- mutate_resource(x, "paths", \(d) {
    d$ind <- NULL
    d$interp <- NULL
    d$known <- NULL
    d
  })
  x <- mutate_resource(x, "staps", \(d) {
    d$include <- NULL
    d
  })
  x <- mutate_resource(x, "edges", \(d) {
    d$s <- NULL
    d$t <- NULL
    d
  })

  # Rename legacy SOI "pitch" sensor to "mean_acceleration_z".
  # https://github.com/Rafnuss/GeoLocator-DP/issues/24
  x <- mutate_resource(x, "measurements", \(d) {
    if (!"sensor" %in% names(d)) {
      return(d)
    }

    sensor <- as.character(d$sensor)
    renamed <- !is.na(sensor) & sensor == "pitch"
    if (any(renamed)) {
      sensor[renamed] <- "mean_acceleration_z"
      d$sensor <- sensor
    }

    d
  })

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
        "i" = "Dropped {.field tag_id}: {.val {glue::glue_collapse(dropped_ids, sep = ', ')}}."
      ))
    }

    d[!drop, , drop = FALSE]
  })

  x[["$schema"]] <- gldp_schema_url("v1.0")
  x
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
