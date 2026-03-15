.get_resource_data <- function(x, resource_name) {
  check_gldp(x)
  frictionless::read_resource(x, resource_name = resource_name)
}

.set_resource_data <- function(x, value, resource_name, post = identity) {
  if (!is.data.frame(value)) {
    cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}."
    )
  }

  x <- add_gldp_resource(
    pkg = x,
    resource_name = resource_name,
    data = value,
    cast_type = TRUE,
    replace = resource_name %in% frictionless::resources(x)
  )

  post(x)
}

#' Get or set tags
#'
#' @description
#' `tags()` gets the tags from a GeoLocator Data Package object.
#'
#' `tags<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with tags
#' @export
tags <- function(x) {
  .get_resource_data(x, resource_name = "tags")
}

#' @rdname tags
#' @param value A data frame to assign as tags. Must conform to the tags schema specification.
#' @export
"tags<-" <- function(x, value) {
  .set_resource_data(
    x = x,
    value = value,
    resource_name = "tags",
    post = \(pkg) {
      pkg |>
        update_gldp_taxonomic() |>
        update_gldp_number_tags()
    }
  )
}

#' Get or set observations
#'
#' @description
#' `observations()` gets the observations from a GeoLocator Data Package object.
#'
#' `observations<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with observations
#' @export
observations <- function(x) {
  .get_resource_data(x, resource_name = "observations")
}

#' @rdname observations
#' @param value A data frame to assign as observations. Must conform to the observations schema
#'   specification.
#' @export
"observations<-" <- function(x, value) {
  .set_resource_data(
    x = x,
    value = value,
    resource_name = "observations"
  )
}

#' Get or set measurements
#'
#' @description
#' `measurements()` gets the measurements from a GeoLocator Data Package object.
#'
#' `measurements<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with measurements
#' @export
measurements <- function(x) {
  .get_resource_data(x, resource_name = "measurements")
}

#' @rdname measurements
#' @param value A data frame to assign as measurements. Must conform to the measurements schema
#'   specification.
#' @export
"measurements<-" <- function(x, value) {
  .set_resource_data(
    x = x,
    value = value,
    resource_name = "measurements",
    post = \(pkg) {
      pkg |>
        update_gldp_temporal() |>
        update_gldp_number_tags()
    }
  )
}

#' Get or set staps
#'
#' @description
#' `staps()` gets the staps from a GeoLocator Data Package object.
#'
#' `staps<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with staps
#' @export
staps <- function(x) {
  .get_resource_data(x, resource_name = "staps")
}

#' @rdname staps
#' @param value A data frame to assign as staps. Must conform to the staps schema specification.
#' @export
"staps<-" <- function(x, value) {
  .set_resource_data(
    x = x,
    value = value,
    resource_name = "staps"
  )
}

#' Get or set twilights
#'
#' @description
#' `twilights()` gets the twilights from a GeoLocator Data Package object.
#'
#' `twilights<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with twilights
#' @export
twilights <- function(x) {
  .get_resource_data(x, resource_name = "twilights")
}

#' @rdname twilights
#' @param value A data frame to assign as twilights. Must conform to the twilights schema
#'   specification.
#' @export
"twilights<-" <- function(x, value) {
  .set_resource_data(
    x = x,
    value = value,
    resource_name = "twilights"
  )
}

#' Get or set paths
#'
#' @description
#' `paths()` gets the paths from a GeoLocator Data Package object.
#'
#' `paths<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with paths
#' @export
paths <- function(x) {
  .get_resource_data(x, resource_name = "paths")
}

#' @rdname paths
#' @param value A data frame to assign as paths. Must conform to the paths schema specification.
#' @export
"paths<-" <- function(x, value) {
  .set_resource_data(
    x = x,
    value = value,
    resource_name = "paths",
    post = update_gldp_number_tags
  )
}

#' Get or set edges
#'
#' @description
#' `edges()` gets the edges from a GeoLocator Data Package object.
#'
#' `edges<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with edges
#' @export
edges <- function(x) {
  .get_resource_data(x, resource_name = "edges")
}

#' @rdname edges
#' @param value A data frame to assign as edges. Must conform to the edges schema specification.
#' @export
"edges<-" <- function(x, value) {
  .set_resource_data(
    x = x,
    value = value,
    resource_name = "edges"
  )
}

#' Get or set pressurepaths
#'
#' @description
#' `pressurepaths()` gets the pressurepaths from a GeoLocator Data Package object.
#'
#' `pressurepaths<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with pressurepaths
#' @export
pressurepaths <- function(x) {
  .get_resource_data(x, resource_name = "pressurepaths")
}

#' @rdname pressurepaths
#' @param value A data frame to assign as pressurepaths. Must conform to the pressurepaths schema
#'   specification.
#' @export
"pressurepaths<-" <- function(x, value) {
  .set_resource_data(
    x = x,
    value = value,
    resource_name = "pressurepaths",
    post = update_gldp_number_tags
  )
}
