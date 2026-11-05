#' Repair data written by GeoPressureR bugs
#'
#' @description
#' Runs [gldp_fix_edges_gs()], which only changes inconsistent rows, and
#' [gldp_fix_soi_axes()] unless `soiMagAxis` records that the SOI axes are
#' already in the GeoPressureR 3.8.0 frame.
#'
#' @param pkg A GeoLocator Data Package object.
#' @return The repaired `pkg`.
#' @noRd
gldp_fix_geopressurer <- function(pkg) {
  pkg <- gldp_fix_edges_gs(pkg)
  if (is.null(pkg[["soiMagAxis"]])) {
    pkg <- gldp_fix_soi_axes(pkg)
  }
  pkg
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
#' [gldp_edges_gs_mismatch()], and sets their `ws_u` and `ws_v` to missing:
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
      "i" = "Recomputed {.field gs_u}/{.field gs_v} and removed {.field ws_u}/{.field ws_v} on \\
      {sum(fix)} of {n_sim} checked simulated {.field edges} {cli::qty(n_sim)}row{?s}, \\
      mismatched by GeoPressureR 3.2.0 to 3.7.0."
    ))
  }
  pkg
}

#' Signed permutation applied by GeoPressureR `mag_axes()`
#'
#' @description
#' Reproduces how GeoPressureR maps the raw SOI axes to forward-right-down for
#' a given `mag_axis`: row k of the matrix gives the raw axis (and sign) that
#' becomes stored axis k.
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

#' Axis convention used to store the SOI magnetic data of each tag
#'
#' @description
#' GeoPressureR 3.8.0 maps the raw SOI axes as forward = raw y and right = raw x
#' for all hardware (`mag_axis = c("right", "forward", "down")`). Earlier
#' versions used `c("left", "backward", "down")` for hardware <= 2.3 and
#' `c("backward", "left", "down")` above. The convention is taken from the
#' `mag_axis` saved in `params`, else from the hardware version in the params
#' SOI settings or in `tags$model`.
#'
#' @param pkg A GeoLocator Data Package object.
#' @return A named list by tag id of tags with magnetic or acceleration x/y
#'   data. Each element is the stored `mag_axis`, or `NULL` when it cannot be
#'   decided.
#' @noRd
gldp_soi_mag_axis <- function(pkg) {
  if (!"measurements" %in% frictionless::resource_names(pkg)) {
    return(list())
  }
  m <- gldp_read_resource(pkg, "measurements")
  ids <- unique(as.character(m$tag_id[
    m$sensor %in%
      c(
        "magnetic_x",
        "magnetic_y",
        "acceleration_x",
        "acceleration_y"
      )
  ]))
  t <- if ("tags" %in% frictionless::resource_names(pkg)) gldp_read_resource(pkg, "tags")
  # Only SOI tags are mapped by GeoPressureR. Published packages also spell it
  # "Swiss Ornithological Institue"; tags of unknown manufacturer are kept.
  soi <- is.na(t$manufacturer) |
    grepl("^swiss ornith|^soi$|^unknown$", t$manufacturer, ignore.case = TRUE)
  other <- t$tag_id[!soi]
  ids <- setdiff(ids, other)
  params <- purrr::keep(pkg$params %||% list(), \(p) !is.null(p$id))
  params <- purrr::set_names(params, purrr::map_chr(params, \(p) as.character(p$id)))

  purrr::set_names(ids) |>
    purrr::map(\(id) {
      p <- params[[id]]
      if (!is.null(p$mag_axis)) {
        return(as.character(p$mag_axis))
      }
      if (!is.null(p$GeoPressureR_version) && package_version(p$GeoPressureR_version) >= "3.8.0") {
        return(c("right", "forward", "down"))
      }
      # The hardware version ends the model: "GDL3pam-v2.3", "GDL3pam_V2.4", "HW_GDL3pam_V2.3".
      model <- c(
        p$soi_settings[["HW Version"]],
        p$soi_settings$HWVersion,
        t$model[t$tag_id %in% id]
      )
      hw <- suppressWarnings(as.numeric(sub(
        ".*[^0-9.]([0-9]+\\.[0-9]+)\\D*$",
        "\\1",
        glue::glue(" {model}")
      )))
      hw <- hw[!is.na(hw)][1]
      if (is.na(hw)) {
        return(NULL)
      }
      if (hw <= 2.3) c("left", "backward", "down") else c("backward", "left", "down")
    })
}

#' Convert SOI magnetic and acceleration axes to the GeoPressureR 3.8.0 frame
#'
#' @description
#' Rewrites `magnetic_x/y/z` and `acceleration_x/y/z` of the tags whose stored
#' axis convention is known (see [gldp_soi_mag_axis()]) to forward = raw y,
#' right = raw x, down = raw z. Intensity and inclination are unchanged. Tags
#' whose convention cannot be decided are left unchanged and reported.
#'
#' @param pkg A GeoLocator Data Package object.
#' @param quiet If `FALSE`, report converted and undecided tags.
#' @return `pkg` with converted `measurements`.
#' @noRd
gldp_fix_soi_axes <- function(pkg, quiet = FALSE) {
  mag_axis <- gldp_soi_mag_axis(pkg)
  undecided <- names(purrr::keep(mag_axis, is.null))
  # new = P_new raw and stored = P_old raw, so new = P_new P_old' stored.
  conversion <- purrr::compact(mag_axis) |>
    purrr::map(\(a) {
      gldp_mag_axis_matrix(c("right", "forward", "down")) %*% t(gldp_mag_axis_matrix(a))
    }) |>
    purrr::discard(\(cv) all(cv == diag(3)))

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
  }
  # The stored data cannot tell which frame they are in: mark the package as
  # converted so that reading it again does not convert it twice.
  if (length(mag_axis) > 0) {
    pkg[["soiMagAxis"]] <- c("right", "forward", "down")
  }

  if (!quiet && length(conversion) > 0) {
    cli_inform(c(
      "i" = "Converted magnetic and acceleration axes of {length(conversion)} SOI \\
      tag{?s} to forward = raw y, right = raw x (GeoPressureR >= 3.8.0): \\
      {.val {names(conversion)}}."
    ))
  }
  if (!quiet && length(undecided) > 0) {
    cli_warn(c(
      "!" = "Could not decide the axis convention of {length(undecided)} tag{?s} with \\
      magnetic data: {.val {undecided}}.",
      "i" = "Their x/y axes may be in the frame of GeoPressureR < 3.8.0."
    ))
  }
  pkg
}
