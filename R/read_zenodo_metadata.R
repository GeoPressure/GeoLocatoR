#' @noRd
read_zenodo_metadata <- function(pkg, zenodo_record) {
  # Use shorter variable name
  z <- zenodo_record
  m <- z$metadata %||% list()
  rights <- m$rights %||% list()
  creators <- m$creators %||% list()
  related_identifiers <- m$related_identifiers %||% list()
  funding <- m$funding %||% list()
  subjects <- m$subjects %||% list()

  # Standard Datapackage variable
  pkg$id <- z$links$self_doi_html %||% NULL
  pkg$name <- z$id
  pkg$title <- m$title
  pkg$description <- m$description
  pkg$version <- m$version
  pkg$created <- z$created
  pkg$homepage <- z$links$self_html %||% NULL
  pkg$licenses <- purrr::map(rights, \(x) {
    list(
      name = x$id,
      title = x$title$en %||% NULL,
      path = x$props$url %||% NULL
    )
  })
  pkg$contributors <- purrr::map(creators, \(creator) {
    orcid <- creator$person_or_org$identifiers |>
      purrr::keep(~ .x$scheme == "orcid") |>
      purrr::pluck(1, "identifier", .default = NULL)

    purrr::compact(list(
      title = creator$person_or_org$name,
      givenName = creator$person_or_org$given_name,
      familyName = creator$person_or_org$family_name,
      path = if (is.null(orcid)) NULL else glue::glue("https://orcid.org/{orcid}"),
      # email = NULL,
      roles = creator$role$id %||% NULL,
      organization = glue::glue_collapse(
        purrr::map_chr(creator$affiliations, "name"),
        sep = ", "
      )
    ))
  })
  pkg$relatedIdentifiers <- purrr::map(related_identifiers, \(x) {
    list(
      relationType = x$relation_type$id %||% NULL,
      relatedIdentifier = x$identifier,
      resourceTypeGeneral = x$resource_type$id %||% NULL,
      relatedIdentifierType = x$scheme
    )
  })

  # Additional old GeoLocatoR
  pkg$embargo <- if (isTRUE(z$access$embargo$active)) {
    z$access$embargo$until
  } else {
    "1970-01-01"
  }
  pkg$grants <- purrr::map_chr(funding, \(x) x$funder$name %||% "")
  pkg$keywords <- purrr::map_chr(subjects, \(x) x$subject %||% "")

  ## Additional field
  pkg$conceptdoi <- z$getConceptDOI()
  pkg$codeRepository <- z$custom$`code:codeRepository` %||% NULL
  pkg$status <- z$status
  pkg$record_type <- m$resource_type$id %||% NULL
  pkg$publisher <- m$publisher

  pkg
}
