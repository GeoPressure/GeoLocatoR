#' Print a GeoLocator Data Package
#'
#' @description
#' Prints a human-readable summary of a GeoLocator Data Package, as an
#' extension of [print.datapackage()].
#'
#' @param x A GeoLocator Data Package object, as returned by `read_gldp()`.
#' @param ... Further arguments, they are ignored by this function.
#'
#' @return [print()] with a summary of the GeoLocator Data Package object.
#' @family print functions
#' @export
print.geolocatordp <- function(x, ...) {
  # check_geolocatordp() not necessary: print only triggered for geolocatordp object

  check_gldp(x)

  cli_h3("A GeoLocator Data Package ({version(x)})")

  cli_bullets(c("*" = "{.field title}: {.val {x$title}}"))

  contributors <- sapply(x$contributors, \(x) {
    str <- x$title
    # if (!is.null(x$givenName)) {
    #  str <- paste0(str, " ", x$givenName)
    # }
    # if (!is.null(x$familyName)) {
    #  str <- paste0(str, " ", x$familyName)
    # }
    if (!is.null(x$email) && length(x$email) > 0) {
      str <- paste0(str, " ({.email ", x$email, "})")
    }
    if (!is.null(x$roles) && length(x$roles) > 0) {
      str <- paste0(str, " (", paste(x$roles, collapse = ", "), ")")
    }
    if (!is.null(x$path) && length(x$path) > 0) {
      str <- paste0(str, " - {.url ", x$path, "}")
    }
    str
  })
  cli_bullets(c("*" = "{.field contributors}:"))
  for (ctr in contributors) {
    cli_bullets(c(" " = ctr))
  }

  embargo_date <- as.POSIXct(x$embargo, format = "%Y-%m-%d", tz = "UTC")
  cli_bullets(c("*" = "{.field embargo}: {.val {embargo_date}}"))

  licenses <- sapply(x$licenses, \(x) {
    if (!is.null(x$title)) {
      str <- x$title
      if (!is.null(x$name)) {
        str <- paste0(str, " (", x$name, ")")
      }
    } else {
      str <- x$name
    }
    if (!is.null(x$path)) {
      str <- paste0(str, " - {.url ", x$path, "}")
    }
    str
  })
  cli_bullets(c("*" = "{.field licenses}:"))
  for (l in licenses) {
    cli_bullets(c(" " = l))
  }
  if (!is.null(x$id)) {
    cli_bullets(c("*" = "{.field id}: {.url {x$id}}"))
  }

  if (!is.null(x$description)) {
    cli_bullets(c("*" = "{.field description}: {.val {x$description}}"))
  }

  if (!is.null(x$version)) {
    cli_bullets(c("*" = "{.field version}: {.val {x$version}}"))
  }

  if (!is.null(x$relatedIdentifiers)) {
    ris <- sapply(x$relatedIdentifiers, \(x) {
      if (
        x$relatedIdentifierType == "doi" &&
          !grepl("^https?://", x$relatedIdentifier)
      ) {
        x$relatedIdentifier <- paste0("https://doi.org/", x$relatedIdentifier)
      }
      paste0(x$relationType, " {.url ", x$relatedIdentifier, "}")
    })
    cli_bullets(c("*" = "{.field relatedIdentifiers}:"))
    for (ri in ris) {
      cli_bullets(c(" " = ri))
    }
  }

  if (!is.null(x$grants)) {
    cli_bullets(c("*" = "{.field grants}: {.val {x$grants}}"))
  }

  if (!is.null(x$keywords)) {
    cli_bullets(c("*" = "{.field keywords}: {.val {x$keywords}}"))
  }

  created_datetime <- as.POSIXct(
    x$created,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  cli_bullets(c("*" = "{.field created}: {.val {created_datetime}}"))

  if (!is.null(x$bibliographicCitation)) {
    cli_bullets(c(
      "*" = "{.field bibliographicCitation}: {.val {x$bibliographicCitation}}"
    ))
  }

  cli_bullets(c("*" = "{.field spatial}: {.val {x$spatial}}"))
  cli_bullets(
    c(
      "*" = "{.field temporal}: {.val {x$temporal$start}} to {.val {x$temporal$end}}"
    )
  )
  cli_bullets(c("*" = "{.field taxonomic}: {.val {x$taxonomic}}"))

  if (!is.null(x$referenceLocation)) {
    cli_bullets(c(
      "*" = "{.field referenceLocation}: {.val {x$referenceLocation}}"
    ))
  }

  cli_bullets(c("*" = "{.field numberTags}:"))
  for (nt in names(x$numberTags)) {
    if (x$numberTags[[nt]] > 0) {
      cli_bullets(c(" " = "{.strong {nt}}: {.val {x$numberTags[[nt]]}}"))
    }
  }

  # cli_bullets(c("*" = "{.field schema}: {.url {x$`$schema`}}"))

  cli_h3("{length(x$resources)} {.field resources}")
  if (length(x$resources) > 0) {
    purrr::walk(x$resources, \(x) {
      cli_bullets(c(
        "*" = "{.field {x$name}} (n={format(nrow(x$data), big.mark=',')})"
      ))
    })
  }

  # Provide help
  cat_line(
    format_inline(
      "Use {.fun unclass} to print the Geolocator Data Package as a list."
    ),
    col = "silver"
  )

  invisible(x)
}


#' Print formatted bullets for package fields
#'
#' Internal helper function to print formatted bullet points for specific fields
#' in a GeoLocator Data Package object.
#'
#' @param pkg A GeoLocator Data Package object
#' @param field_name Character string of the field name to print
#' @return Nothing (side effect: prints to console)
#' @noRd
bullets <- function(pkg, field_name) {
  val <- pkg[[field_name]]
  if (!is.null(val)) {
    if (is.data.frame(val)) {
      cli_bullets(c("*" = "{.field {field_name}}:"))
      cat_print(val)
    } else if (is.list(val) && length(val) > 1) {
      cli_bullets(c("*" = "{.field {field_name}}:"))
      for (n in names(val)) {
        cli_bullets(c(" " = "{.field {n}}: {.val {val[[n]]}}"))
      }
    } else {
      cli_bullets(c("*" = "{.field {field_name}}: {.val {val}}"))
    }
  }
}
