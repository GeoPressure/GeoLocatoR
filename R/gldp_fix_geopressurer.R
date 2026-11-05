#' Repair data written by GeoPressureR < 3.8.0
#'
#' @description
#' Runs `gldp_fix_edges_gs()`, `gldp_fix_soi_axes()` and
#' `gldp_params_migrate()`. Each changes data only on the evidence recorded in
#' the params of a tag and is idempotent: repaired edges are consistent,
#' converted tags record `param$tag_create$mag_axis` and migrated params have no
#' old key left.
#'
#' @param pkg A GeoLocator Data Package object.
#' @return The repaired `pkg`.
#' @noRd
gldp_fix_geopressurer <- function(pkg) {
  pkg <- gldp_fix_edges_gs(pkg)
  # Axes first: the old top-level mag_axis is the record of the old frame.
  pkg <- gldp_fix_soi_axes(pkg)
  pkg[["params"]] <- gldp_params_migrate(pkg$params)
  pkg
}


#' Move GeoPressureR < 3.8.0 param keys to the GeoPressureR 3.8.0 layout
#'
#' @description
#' GeoPressureR 3.8.0 stores the logger settings in `param$tag_create$settings`
#' with normalised `model`, `firmware` and `clock_drift`, instead of
#' `param$soi_settings`, `param$migratec_model` and `param$drift`. The old
#' `param$mag_axis` is moved by `gldp_fix_soi_axes()`, as it marks data in the
#' old frame.
#'
#' @param params A list of GeoPressureR parameter objects.
#' @return `params` in the new layout.
#' @noRd
gldp_params_migrate <- function(params) {
  purrr::map(params %||% list(), \(p) {
    if (is.null(p$soi_settings) && is.null(p$migratec_model) && is.null(p$drift)) {
      return(p)
    }
    settings <- p$tag_create$settings %||% p$soi_settings %||% list()
    soi <- p$soi_settings
    settings$model <- settings$model %||%
      soi[["HW Version"]] %||%
      soi$HWVersion %||%
      p$migratec_model
    settings$firmware <- settings$firmware %||% soi[["FW Version"]] %||% soi$SWVersion
    settings$clock_drift <- settings$clock_drift %||% p$drift
    p$tag_create$settings <- settings
    p$soi_settings <- NULL
    p$migratec_model <- NULL
    p$drift <- NULL
    p
  })
}

#' GeoPressureR version used for each tag
#'
#' @param pkg A GeoLocator Data Package object.
#' @return A named character vector of GeoPressureR versions, named by tag id.
#'   Tags without a recorded version are absent.
#' @noRd
gldp_geopressurer_version <- function(pkg) {
  params <- purrr::keep(pkg$params %||% list(), \(p) {
    !is.null(p$id) && !is.null(p$GeoPressureR_version)
  })
  purrr::set_names(
    purrr::map_chr(params, \(p) as.character(p$GeoPressureR_version)),
    purrr::map_chr(params, \(p) as.character(p$id))
  )
}

#' Check the groundspeed of simulated edges
#'
#' @description
#' GeoPressureR 3.2.0 to 3.7.0 assigned `gs` and `ws` to the wrong edges of
#' `edge_simulation` when several simulated paths share a transition (fixed in
#' GeoPressureR 3.7.1). Positions, `distance`, `bearing`, `start`, `end` and `n`
#' are correct, so the groundspeed must equal `distance / (end - start)`.
#' Graph and path positions differ by up to 0.7% on correct edges, hence the 2%
#' tolerance. Tags whose `most_likely` edges also mismatch follow another
#' convention (packages from 2025 show 0.3 to 1 times the speed) and are not
#' checked.
#'
#' @param edges An `edges` data frame.
#' @param staps A `staps` data frame or `NULL`.
#' @return A logical vector: `TRUE` for a mismatched groundspeed, `FALSE` for a
#'   consistent one and `NA` for rows that cannot be checked.
#' @noRd
gldp_edges_gs_mismatch <- function(edges, staps = NULL) {
  if (!all(c("type", "gs_u", "gs_v", "distance", "start", "end") %in% names(edges))) {
    return(rep(NA, nrow(edges)))
  }
  duration <- as.numeric(difftime(edges$end, edges$start, units = "hours"))
  expected <- edges$distance / duration
  mismatch <- abs(sqrt(edges$gs_u^2 + edges$gs_v^2) - expected) > 0.02 * expected

  # The graph groundspeed is computed between grid cell centres, while known
  # positions are stored exactly, so flights from/to a known stap differ by design.
  known <- if (all(c("known_lat", "tag_id", "stap_id") %in% names(staps))) {
    k <- staps[!is.na(staps$known_lat), , drop = FALSE]
    glue::glue("{k$tag_id}\r{k$stap_id}")
  } else {
    character()
  }
  from_known <- glue::glue("{edges$tag_id}\r{edges$stap_s}") %in%
    known |
    glue::glue("{edges$tag_id}\r{edges$stap_t}") %in% known

  # With several flights (n > 1), end - start includes the stationary periods in between.
  n_flight <- if ("n" %in% names(edges)) edges$n else 1
  checkable <- n_flight %in% 1 & !from_known & duration > 0
  other_convention <- edges$tag_id[edges$type %in% "most_likely" & checkable & mismatch %in% TRUE]
  checkable <- checkable & edges$type %in% "simulation" & !edges$tag_id %in% other_convention
  mismatch[!checkable %in% TRUE] <- NA
  mismatch
}

#' Repair the groundspeed of simulated edges
#'
#' @description
#' Recomputes `gs_u` and `gs_v` from `distance`, `bearing` (0 = north,
#' clockwise) and the flight duration on the rows flagged by
#' `gldp_edges_gs_mismatch()`, and sets their `ws_u` and `ws_v` to missing:
#' the wind can only be recomputed from the original wind files.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param quiet If `FALSE`, report the number of rows repaired.
#' @return `pkg` with the `edges` resource repaired.
#' @noRd
gldp_fix_edges_gs <- function(pkg, quiet = FALSE) {
  if (!"edges" %in% frictionless::resource_names(pkg)) {
    return(pkg)
  }
  staps <- if ("staps" %in% frictionless::resource_names(pkg)) gldp_read_resource(pkg, "staps")
  mismatch <- gldp_edges_gs_mismatch(gldp_read_resource(pkg, "edges"), staps)
  fix <- mismatch %in% TRUE
  if (!any(fix)) {
    return(pkg)
  }

  pkg <- mutate_resource(pkg, "edges", \(d) {
    duration <- as.numeric(difftime(d$end[fix], d$start[fix], units = "hours"))
    gs_abs <- d$distance[fix] / duration
    # Same convention as GeoPressureR: u eastward, v northward; no displacement has gs 0.
    arg <- ifelse(gs_abs == 0, 0, (90 - d$bearing[fix]) * pi / 180)
    d$gs_u[fix] <- gs_abs * cos(arg)
    d$gs_v[fix] <- gs_abs * sin(arg)
    d$ws_u[fix] <- NA
    d$ws_v[fix] <- NA
    d
  })

  if (!quiet) {
    n_sim <- sum(!is.na(mismatch))
    cli_inform(c(
      "!" = "Recomputed {.field gs_u}/{.field gs_v} and removed {.field ws_u}/{.field ws_v} on \\
      {sum(fix)} of {n_sim} checked simulated {.field edges} {cli::qty(n_sim)}row{?s}, \\
      assigned to the wrong flight by GeoPressureR 3.2.0 to 3.7.0.",
      "i" = "See {.url https://github.com/GeoPressure/GeoPressureR/commit/6f5dda6527eed51ec67e8393043c91579160da64}.",
      "i" = "Save the corrected package with {.fun write_gldp} and re-upload it."
    ))
  }
  pkg
}


#' Signed permutation applied by GeoPressureR `mag_axes()`
#'
#' @description
#' Reproduces how GeoPressureR 3.4.1 to 3.7.x maps the raw SOI axes for a given
#' `mag_axis`: row k of the matrix gives the raw axis (and sign) stored as axis
#' k. GeoPressureR 3.8.0 inverts the permutation first, which only matters for
#' 3-cycles: the mappings recorded by GeoPressureR 3.4.1 to 3.7.x and
#' `c("right", "forward", "down")` are their own inverse.
#'
#' @param mag_axis A character vector of length 3 such as
#'   `c("left", "backward", "down")`.
#' @return A 3 x 3 signed permutation matrix.
#' @noRd
gldp_mag_axis_matrix <- function(mag_axis) {
  ref <- matrix(c("forward", "backward", "right", "left", "down", "up"), 2, 3)
  axes_ind <- ceiling(match(mag_axis, ref) / 2)
  axes_sign <- ifelse(match(mag_axis[axes_ind], ref) %% 2, 1, -1)
  m <- matrix(0, 3, 3)
  m[cbind(1:3, axes_ind)] <- axes_sign
  m
}

#' Stored axis mapping of SOI magnetic and acceleration data
#'
#' @description
#' GeoPressureR >= 3.8.0 maps the raw SOI axes to forward = raw y, right = raw
#' x, down = raw z (`mag_axis = c("right", "forward", "down")`), the frame of
#' GeoLocator-DP v1.3, and records the mapping it applied, possibly a reversed
#' mount chosen by the user, in `param$tag_create$mag_axis`. GeoPressureR 3.4.1
#' to 3.7.x applied a hardware-dependent mapping recorded in `param$mag_axis`.
#'
#' Without either record, or a GeoPressureR version >= 3.8.0, the frame is
#' unknown and no mapping is guessed: GeoPressureR < 3.4.1 stored raw counts
#' with the magnetic and acceleration sensors swapped, GeoPressureR 3.4.1 to
#' 3.7.x only exported x/y data after recording `param$mag_axis`, and data
#' without params may come from any software.
#'
#' `measurements` is only read when params leave a SOI tag outside the
#' GeoPressureR 3.8.0 frame.
#'
#' @param pkg A GeoLocator Data Package object.
#' @return A named list by tag id of the SOI tags with magnetic or acceleration
#'   x/y data that are not in the GeoPressureR 3.8.0 frame. Each element is the
#'   `mag_axis` the data are stored with, or `NULL` when it is unknown.
#' @noRd
gldp_soi_mag_axis <- function(pkg) {
  if (!all(c("tags", "measurements") %in% frictionless::resource_names(pkg))) {
    return(list())
  }
  params <- purrr::keep(pkg$params %||% list(), \(p) !is.null(p$id))
  params <- purrr::set_names(params, purrr::map_chr(params, \(p) as.character(p$id)))
  t <- gldp_read_resource(pkg, "tags")
  # Only SOI tags are mapped by GeoPressureR. Published packages also spell it
  # "Swiss Ornithological Institue"; tags of unknown manufacturer are kept.
  soi <- is.na(t$manufacturer) |
    grepl("^swiss ornith|^soi$|^unknown$", t$manufacturer, ignore.case = TRUE)

  new_frame <- c("right", "forward", "down")
  mag_axis <- purrr::set_names(as.character(t$tag_id[soi])) |>
    purrr::map(\(id) {
      p <- params[[id]]
      version <- p$GeoPressureR_version
      if (
        !is.null(p$tag_create$mag_axis) ||
          (!is.null(version) && package_version(version) >= "3.8.0")
      ) {
        return(new_frame)
      }
      p$mag_axis
    }) |>
    purrr::discard(\(a) identical(as.character(a), new_frame))
  if (length(mag_axis) == 0) {
    return(list())
  }

  m <- gldp_read_resource(pkg, "measurements")
  xy <- as.character(m$tag_id[
    m$sensor %in% c("magnetic_x", "magnetic_y", "acceleration_x", "acceleration_y")
  ])
  mag_axis[names(mag_axis) %in% xy]
}

#' Convert SOI magnetic and acceleration axes to the GeoPressureR 3.8.0 frame
#'
#' @description
#' Rewrites `magnetic_x/y/z` and `acceleration_x/y/z` of the tags whose stored
#' mapping is recorded in params (see `gldp_soi_mag_axis()`) to forward = raw
#' y, right = raw x, down = raw z, and records
#' `param$tag_create$mag_axis = c("right", "forward", "down")` in their params,
#' so that they are converted once. Tags whose frame is unknown are left as
#' stored and named in one warning.
#'
#' @param pkg A GeoLocator Data Package object.
#' @return `pkg` with converted `measurements` and params.
#' @noRd
gldp_fix_soi_axes <- function(pkg) {
  mag_axis <- gldp_soi_mag_axis(pkg)
  new_frame <- c("right", "forward", "down")
  # new = P_new raw and stored = P_old raw, so new = P_new P_old' stored.
  conversion <- purrr::compact(mag_axis) |>
    purrr::map(\(a) gldp_mag_axis_matrix(new_frame) %*% t(gldp_mag_axis_matrix(a)))

  if (length(conversion) > 0) {
    pkg <- mutate_resource(pkg, "measurements", \(d) {
      sensor <- as.character(d$sensor)
      tag_id <- as.character(d$tag_id)
      for (id in names(conversion)) {
        cv <- conversion[[id]]
        # A signed permutation relabels each stored axis as a new axis.
        for (family in c("magnetic", "acceleration")) {
          for (k in 1:3) {
            j <- which(cv[k, ] != 0)
            rows <- tag_id %in% id & d$sensor %in% glue::glue("{family}_{c('x', 'y', 'z')[j]}")
            sensor[rows] <- glue::glue("{family}_{c('x', 'y', 'z')[k]}")
            d$value[rows] <- d$value[rows] * cv[k, j]
          }
        }
      }
      d$sensor <- sensor
      d
    })
    # The data cannot tell which frame they are in: recording the new mapping
    # makes a later conversion a no-op.
    pkg[["params"]] <- purrr::map(pkg$params, \(p) {
      if (as.character(p$id %||% NA) %in% names(conversion)) {
        p$tag_create$mag_axis <- new_frame
        p$mag_axis <- NULL
      }
      p
    })
    cli_inform(c(
      "!" = "Converted the magnetic and acceleration x/y of {length(conversion)} SOI \\
      tag{?s} from the axis mapping recorded in {.field mag_axis} by GeoPressureR < 3.8.0 to \\
      forward = raw y, right = raw x: {.val {names(conversion)}}.",
      "i" = "See {.url https://github.com/GeoPressure/GeoPressureR/pull/187}.",
      "i" = "Save the converted package with {.fun write_gldp}."
    ))
  }

  unknown <- names(purrr::keep(mag_axis, is.null))
  if (length(unknown) > 0) {
    swapped <- names(purrr::keep(
      gldp_geopressurer_version(pkg)[unknown],
      \(v) !is.na(v) && package_version(v) < "3.4.1"
    ))
    cli_warn(c(
      "!" = "The axis frame of the magnetic and acceleration x/y of {length(unknown)} SOI \\
      tag{?s} is unknown; data left as stored: {.val {unknown}}.",
      "i" = "GeoLocator-DP v1.3 expects forward, right and down axes, as written by \\
      GeoPressureR >= 3.8.0. Their params record no axis mapping.",
      "i" = if (length(swapped) > 0) {
        "GeoPressureR < 3.4.1, used for {.val {swapped}}, stored the raw magnetic and \\
        acceleration data swapped and uncalibrated \\
        ({.url https://github.com/GeoPressure/GeoPressureR/commit/b1099c96})."
      }
    ))
  }
  pkg
}
