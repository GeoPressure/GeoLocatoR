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
  x <- update_gldp_order_resources(x)

  has_value <- function(value) !is.null(value) && length(value) > 0
  format_creator_name <- function(name) {
    if (
      !is.character(name) || length(name) == 0 || is.na(name) || !grepl(",", name, fixed = TRUE)
    ) {
      return(name)
    }
    parts <- trimws(strsplit(name, ",", fixed = TRUE)[[1]])
    if (length(parts) < 2) {
      return(name)
    }
    paste(c(parts[-1], parts[1]), collapse = " ")
  }
  x_display <- list(
    title = x$title %||% NULL,
    description = x$description %||% NULL,
    version = x$version %||% NULL,
    grants = x$grants %||% NULL,
    keywords = x$keywords %||% NULL,
    created = x$created %||% NULL,
    embargo = x$embargo %||% NULL,
    id = x$id %||% NULL,
    doi = x$id %||% NULL,
    contributors = x$contributors %||% NULL,
    relatedIdentifiers = x$relatedIdentifiers %||% NULL,
    licenses = x$licenses %||% NULL,
    bibliographicCitation = x$bibliographicCitation %||% NULL,
    temporal = x$temporal %||% NULL,
    taxonomic = x$taxonomic %||% NULL,
    numberTags = x$numberTags %||% NULL
  )

  cli_h3("A GeoLocator Data Package ({gldp_version(x)})")

  bullets(x_display, "title")
  if (has_value(x_display$doi)) {
    cli_bullets(c("*" = "{.field doi}: {.url {x_display$doi}}"))
  }

  if (has_value(x_display$contributors)) {
    contributors <- sapply(x_display$contributors, \(ctr) {
      str <- format_creator_name(ctr$title)
      if (has_value(ctr$email)) {
        str <- glue::glue("{str} ('{ctr$email}')")
      }
      if (has_value(ctr$roles)) {
        str <- glue::glue("{str} ({glue::glue_collapse(ctr$roles, sep = ', ')})")
      }
      if (has_value(ctr$path)) {
        str <- glue::glue("{str} - {{.url {ctr$path}}}")
      }
      str
    })
    contributors <- contributors[nzchar(contributors)]
    if (length(contributors) > 0) {
      cli_bullets(c("*" = "{.field contributors}:"))
      for (ctr in contributors) {
        cli_bullets(c(" " = ctr))
      }
    }
  }

  if (has_value(x_display$embargo)) {
    embargo_date <- as.POSIXct(x_display$embargo, format = "%Y-%m-%d", tz = "UTC")
    cli_bullets(c("*" = "{.field embargo}: {.val {embargo_date}}"))
  }

  if (has_value(x_display$licenses)) {
    licenses <- sapply(x_display$licenses, \(license) {
      if (has_value(license$title)) {
        str <- license$title
        if (has_value(license$name)) {
          str <- glue::glue("{str} ({license$name})")
        }
      } else {
        str <- license$name
      }
      if (has_value(license$path)) {
        str <- glue::glue("{str} - {{.url {license$path}}}")
      }
      str
    })
    licenses <- licenses[nzchar(licenses)]
    if (length(licenses) > 0) {
      cli_bullets(c("*" = "{.field licenses}: {glue::glue_collapse(licenses, sep = ', ')}"))
    }
  }

  if (has_value(x_display$description)) {
    desc <- as.character(x_display$description)[1]
    max_chars <- 250L
    if (!is.na(desc) && nchar(desc) > max_chars) {
      desc <- glue::glue("{substr(desc, 1, max_chars)}...")
    }
    cli_bullets(c("*" = "{.field description}: {.val {desc}}"))
  }
  bullets(x_display, "version")

  if (has_value(x_display$relatedIdentifiers)) {
    ris <- sapply(x_display$relatedIdentifiers, \(ri) {
      if (
        toupper(ri$relatedIdentifierType %||% "") == "DOI" &&
          !grepl("^https?://", ri$relatedIdentifier)
      ) {
        ri$relatedIdentifier <- glue::glue("https://doi.org/{ri$relatedIdentifier}")
      }
      glue::glue("{ri$relationType} {{.url {ri$relatedIdentifier}}}")
    })
    cli_bullets(c("*" = "{.field relatedIdentifiers}:"))
    for (ri in ris) {
      cli_bullets(c(" " = ri))
    }
  }

  bullets(x_display, "grants")
  bullets(x_display, "keywords")

  if (has_value(x_display$created)) {
    created_raw <- x_display$created
    if (is.character(created_raw)) {
      created_raw <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", created_raw)
      created_datetime <- as.POSIXct(
        created_raw,
        format = "%Y-%m-%dT%H:%M:%OS%z",
        tz = "UTC"
      )
      if (is.na(created_datetime)) {
        created_datetime <- as.POSIXct(
          created_raw,
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )
      }
    } else {
      created_datetime <- as.POSIXct(created_raw, tz = "UTC")
    }
    cli_bullets(c("*" = "{.field created}: {.val {created_datetime}}"))
  }

  bullets(x_display, "bibliographicCitation")

  if (
    has_value(x_display$temporal) &&
      (has_value(x_display$temporal$start) || has_value(x_display$temporal$end))
  ) {
    temporal_start <- x_display$temporal$start
    temporal_end <- x_display$temporal$end
    if (has_value(temporal_start) && has_value(temporal_end)) {
      cli_bullets(c(
        "*" = "{.field temporal}: {.val {temporal_start}} to {.val {temporal_end}}"
      ))
    } else if (has_value(temporal_start)) {
      cli_bullets(c("*" = "{.field temporal}: {.val {temporal_start}}"))
    } else if (has_value(temporal_end)) {
      cli_bullets(c("*" = "{.field temporal}: {.val {temporal_end}}"))
    }
  }

  bullets(x_display, "taxonomic")

  if (has_value(x_display$numberTags)) {
    number_tags <- names(x_display$numberTags)[x_display$numberTags > 0]
    if (length(number_tags) > 0) {
      cli_bullets(c("*" = "{.field numberTags}:"))
      for (nt in number_tags) {
        cli_bullets(c(" " = "{.strong {nt}}: {.val {x_display$numberTags[[nt]]}}"))
      }
    }
  }

  # cli_bullets(c("*" = "{.field schema}: {.url {x$`$schema`}}"))

  cli_h3("{length(x$resources)} {.field resources}")
  if (length(x$resources) > 0) {
    purrr::walk(x$resources, \(res) {
      n <- if (is.data.frame(res$data)) {
        nrow(res$data)
      } else if (is.list(res$data)) {
        length(res$data)
      } else {
        NA_integer_
      }

      if (is.na(n)) {
        cli_bullets(c("*" = "{.field {res$name}}"))
      } else {
        cli_bullets(c(
          "*" = "{.field {res$name}} (n={format(n, big.mark=',')})"
        ))
      }
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
