#' Print a GeoLocator Data Package
#'
#' @description
#' Prints a human-readable summary of a GeoLocator Data Package, as an
#' extension of [print.datapackage()].
#'
#' @param x A GeoLocator Data Package object, as returned by `read_gldp()`.
#' @param ... Further arguments, they are ignored by this function.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' pkg <- read_zenodo("17367319", quiet = TRUE)
#' print(pkg)
#'
#' @family print functions
#' @export
print.geolocatordp <- function(x, ...) {
  check_gldp(x)
  x <- update_gldp_order_resources(x)

  cli_h1("A GeoLocator Data Package `pkg` ({gldp_version(x)})")

  cli::cli_h3("Metadata")
  cat_line(
    format_inline(
      "{.strong Note}: All {.field green} texts are fields of `pkg` which can be accessed with `pkg${.field field}`)."
    ),
    col = "silver"
  )
  if (isTRUE(x$is_draft)) {
    cli_bullets(c("!" = "{.field status}: {.strong draft}"))
  }

  bullets(x, "title")
  if (has_value(x$id)) {
    cli_bullets(c("*" = "{.field doi}: {.url {x$id}}"))
  }

  if (has_value(x$contributors)) {
    contributors <- purrr::map_chr(x$contributors, \(ctr) {
      name <- ctr$title
      if (
        is.character(name) && length(name) > 0 && !is.na(name) && grepl(",", name, fixed = TRUE)
      ) {
        parts <- trimws(strsplit(name, ",", fixed = TRUE)[[1]])
        if (length(parts) >= 2) {
          name <- glue::glue_collapse(c(parts[-1], parts[1]), sep = " ")
        }
      }
      if (!has_value(name)) {
        return(NA_character_)
      }
      str <- as.character(name)[1]
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
    contributors <- contributors[!is.na(contributors) & nzchar(trimws(contributors))]
    if (length(contributors) > 0) {
      cli_bullets(c("*" = "{.field contributors}:"))
      purrr::walk(contributors, \(ctr) cli_bullets(c(" " = ctr)))
    }
  }

  access_status <- tolower(as.character(x$access_status %||% "")[1])
  embargo <- as.character(x$embargo %||% "")[1]
  has_embargo <- nzchar(embargo) && !identical(embargo, "1970-01-01")
  if (identical(access_status, "restricted")) {
    cli::cli_bullets(c("x" = "{.field access}: restricted"))
  } else if (has_embargo) {
    cli::cli_bullets(c("!" = "{.field access}: with embargo until {embargo}"))
  } else {
    cli::cli_bullets(c("v" = "{.field access}: Open access"))
  }

  if (has_value(x$licenses)) {
    licenses <- purrr::map_chr(x$licenses, \(license) {
      str <- if (has_value(license$title)) {
        if (has_value(license$name)) {
          glue::glue("{license$title} ({license$name})")
        } else {
          license$title
        }
      } else {
        license$name
      }
      if (!has_value(str)) {
        return(NA_character_)
      }
      if (has_value(license$path)) {
        str <- glue::glue("{str} - {{.url {license$path}}}")
      }
      str
    })
    licenses <- licenses[!is.na(licenses) & nzchar(trimws(licenses))]
    if (length(licenses) > 0) {
      cli_bullets(c("*" = "{.field licenses}: {glue::glue_collapse(licenses, sep = ', ')}"))
    }
  }

  if (has_value(x$description)) {
    desc <- as.character(x$description)[1] |>
      rvest::read_html() |>
      rvest::html_text2()
    desc <- trimws(gsub("\\s+", " ", desc))
    if (has_value(desc)) {
      if (nchar(desc) > 250L) {
        desc <- glue::glue("{substr(desc, 1, 250L)}...")
      }
      cli_bullets(c("*" = "{.field description}: {.val {desc}}"))
    }
  }

  bullets(x, "version")

  if (has_value(x$codeRepository)) {
    code_repository <- as.character(x$codeRepository)[1]
    if (grepl("^https?://", code_repository)) {
      cli_bullets(c("*" = "{.field codeRepository}: {.url {code_repository}}"))
    } else {
      cli_bullets(c("*" = "{.field codeRepository}: {.val {code_repository}}"))
    }
  }

  communities <- as.character(unlist(x$communities, recursive = TRUE, use.names = FALSE))
  communities <- communities[!is.na(communities) & nzchar(trimws(communities))]
  allowed_communities <- c(
    "b7c70316-310b-435e-9a8b-84188d60a3cc",
    "6e9c24fb-954a-4087-ae9b-71fcba41a624"
  )
  if (any(communities %in% allowed_communities)) {
    cli::cli_bullets(c("v" = "{.field community}: Geolocator DP community"))
  } else {
    cli::cli_bullets(c("!" = "{.field community}: no Geolocator DP community"))
  }

  if (has_value(x$relatedIdentifiers)) {
    ris <- purrr::map_chr(x$relatedIdentifiers, \(ri) {
      identifier <- ri$relatedIdentifier
      if (
        toupper(ri$relatedIdentifierType %||% "") == "DOI" &&
          has_value(identifier) &&
          !grepl("^https?://", identifier)
      ) {
        identifier <- glue::glue("https://doi.org/{identifier}")
      }
      if (!has_value(identifier)) {
        return(NA_character_)
      }
      if (!has_value(ri$relationType)) {
        return(glue::glue("{{.url {identifier}}}"))
      }
      glue::glue("{ri$relationType} {{.url {identifier}}}")
    })
    ris <- ris[!is.na(ris) & nzchar(trimws(ris))]
    if (length(ris) > 0) {
      cli_bullets(c("*" = "{.field relatedIdentifiers}:"))
      purrr::walk(ris, \(ri) cli_bullets(c(" " = ri)))
    }
  }

  bullets(x, "grants")
  bullets(x, "keywords")

  if (has_value(x$created)) {
    created_raw <- x$created
    created_datetime <- if (is.character(created_raw)) {
      created_raw <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", created_raw)
      created_datetime <- as.POSIXct(created_raw, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
      if (is.na(created_datetime)) {
        created_datetime <- as.POSIXct(created_raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      }
      created_datetime
    } else {
      as.POSIXct(created_raw, tz = "UTC")
    }
    cli_bullets(c("*" = "{.field created}: {.val {created_datetime}}"))
  }

  bullets(x, "bibliographicCitation")

  if (has_value(x$temporal) && (has_value(x$temporal$start) || has_value(x$temporal$end))) {
    if (has_value(x$temporal$start) && has_value(x$temporal$end)) {
      cli_bullets(c(
        "*" = "{.field temporal}: {.val {x$temporal$start}} to {.val {x$temporal$end}}"
      ))
    } else if (has_value(x$temporal$start)) {
      cli_bullets(c("*" = "{.field temporal}: {.val {x$temporal$start}}"))
    } else {
      cli_bullets(c("*" = "{.field temporal}: {.val {x$temporal$end}}"))
    }
  }

  bullets(x, "taxonomic")

  if (has_value(x$numberTags)) {
    number_tags <- names(x$numberTags)[x$numberTags > 0]
    if (length(number_tags) > 0) {
      number_tags_inline <- purrr::map_chr(
        number_tags,
        \(nt) glue::glue("{nt}: {x$numberTags[[nt]]}")
      )
      cli_bullets(c(
        "*" = "{.field numberTags}: {glue::glue_collapse(number_tags_inline, sep = ', ')}"
      ))
    }
  }

  cli_h3("Resources: ({length(x$resources)} {.field resources}) ")
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
        cli_bullets(c("*" = "{.fun {res$name}}"))
      } else {
        cli_bullets(c("*" = "{.fun {res$name}} (n={format(n, big.mark=',')})"))
      }
    })
  }

  invisible(x)
}

# Returns TRUE when a value contains at least one meaningful entry.
has_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(FALSE)
  }
  if (is.character(value)) {
    return(any(!is.na(value) & nzchar(trimws(value))))
  }
  if (is.atomic(value)) {
    return(!all(is.na(value)))
  }
  TRUE
}

# Print formatted bullets for package fields
bullets <- function(pkg, field_name) {
  val <- pkg[[field_name]]
  if (!has_value(val)) {
    return(invisible(NULL))
  }
  if (is.data.frame(val)) {
    cli_bullets(c("*" = "{.field {field_name}}:"))
    cat_print(val)
  } else if (is.list(val) && length(val) > 1) {
    cli_bullets(c("*" = "{.field {field_name}}:"))
    purrr::walk(names(val), \(n) {
      if (has_value(val[[n]])) {
        cli_bullets(c(" " = "{.field {n}}: {.val {val[[n]]}}"))
      }
    })
  } else {
    cli_bullets(c("*" = "{.field {field_name}}: {.val {val}}"))
  }
  invisible(NULL)
}
