#' @noRd
read_zenodo_attach_metadata <- function(pkg, zenodo_record) {
  z <- zenodo_record
  m <- z$metadata %||% list()
  pkg$id <- glue::glue("https://zenodo.org/doi/10.5281/zenodo.{z$id}")
  pkg$name <- z$id
  pkg$title <- m$title
  pkg$description <- m$description
  pkg$version <- m$version
  pkg$created <- z$created
  pkg$homepage <- z$links$self_html %||% NULL
  pkg$licenses <- purrr::map(m$rights %||% list(), \(x) {
    list(
      name = x$id,
      title = x$title$en %||% NULL,
      path = x$props$url %||% NULL
    )
  })
  pkg$contributors <- purrr::map(m$creators %||% list(), \(creator) {
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
  pkg$relatedIdentifiers <- purrr::map(m$related_identifiers %||% list(), \(x) {
    list(
      relationType = x$relation_type$id %||% NULL,
      relatedIdentifier = x$identifier,
      resourceTypeGeneral = x$resource_type$id %||% NULL,
      relatedIdentifierType = x$scheme
    )
  })
  access_status <- tolower(
    as.character(z$access$status %||% z$access$record %||% "")[1]
  )
  pkg$access_status <- if (nzchar(access_status)) access_status else NULL
  pkg$embargo <- if (identical(pkg$access_status, "embargoed") || isTRUE(z$access$embargo$active)) {
    z$access$embargo$until %||% NULL
  } else {
    NULL
  }
  pkg$grants <- purrr::map_chr(m$funding %||% list(), \(x) x$funder$name %||% "")
  pkg$keywords <- purrr::map_chr(m$subjects %||% list(), \(x) x$subject %||% "")
  pkg$conceptdoi <- z$conceptdoi %||% purrr::pluck(z, "parent", "doi", .default = NULL)
  pkg$codeRepository <- z$custom_fields$`code:codeRepository` %||% NULL
  pkg$status <- z$status
  pkg$record_type <- m$resource_type$id %||% NULL
  pkg$publisher <- m$publisher
  pkg$communities <- z$parent$communities$ids %||%
    purrr::pluck(z, "parent", "review", "receiver", "community", .default = NULL)
  pkg$is_published <- z$is_published
  pkg$is_draft <- z$is_draft
  pkg
}
