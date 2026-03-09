#' Create or update the metadata of a GeoLocator Data Package based on a Zenodo Record
#'
#' @description
#' This function update the metadata of a [GeoLocator Data Package
#' ](https://datapackage.org/standard/data-package/) based on an `zen4R` ZenodoRecord provided. If
#' no package is provided (`pkg = NULL`), a new data package is created.
#'
#' All metadata are retrieved with the Zenodo API through the R package
#' [zen4R](https://github.com/eblondel/zen4R).
#'
#' Note that no data is actually retrieved/copied.
#'
#' @param zenodo_record A `zen4R` ZenodoRecord object from which to extract metadata.
#' @param pkg (optionall) A Geolocator Data Package object on which the metadata should be added. If
#' not provided, create a new one.
#'
#' @return An updated or new Geolocator Data Package object with metadata from the zenodo record.
#'
#' @export
zenodo_to_gldp <- function(zenodo_record, pkg = NULL) {
  z <- zenodo_record
  assertthat::assert_that(all(
    c("ZenodoRecord", "zen4RLogger", "R6") %in% class(z)
  ))

  concept_doi <- z$getConceptDOI()
  if (
    is.null(concept_doi) || length(concept_doi) != 1 || is.na(concept_doi) || !nzchar(concept_doi)
  ) {
    cli_abort("Zenodo concept DOI is missing from the record. Make sure to reserve one on Zenodo")
  }

  # Process contributors
  contributors <- purrr::map(z$metadata$creators, \(creator) {
    orcid <- creator$person_or_org$identifiers |>
      purrr::keep(~ .x$scheme == "orcid") |>
      purrr::pluck(1, "identifier", .default = NULL)

    purrr::compact(list(
      title = creator$person_or_org$name,
      givenName = creator$person_or_org$given_name,
      familyName = creator$person_or_org$family_name,
      path = if (is.null(orcid)) NULL else glue::glue("https://orcid.org/{orcid}"),
      # email = NULL,
      roles = map_gldp_to_zenodo(
        creator$role$id,
        .gldp_contributor_roles
      ),
      organization = glue::glue_collapse(
        purrr::map_chr(creator$affiliations, "name"),
        sep = ", "
      )
    ))
  })

  # Determine embargo date
  embargo <- if (z$access$embargo$active) {
    z$access$embargo$until
  } else {
    "1970-01-01"
  }

  # Process licenses
  licenses <- purrr::map(z$metadata$rights, \(x) {
    list(
      name = x$id,
      title = x$title$en,
      path = x$props$url
    )
  })

  # Process related identifiers
  relatedIdentifiers <- purrr::map(z$metadata$related_identifiers, \(x) {
    list(
      relationType = map_gldp_to_zenodo(
        x$relation_type$id,
        .gldp_relation_types,
        default = x$relation_type$id
      ),
      relatedIdentifier = x$identifier,
      resourceTypeGeneral = map_gldp_to_zenodo(
        x$resource_type$id,
        .gldp_resource_types
      ),
      relatedIdentifierType = map_gldp_to_zenodo(
        x$scheme,
        .gldp_identifier_types
      )
    )
  })

  # Extract grants
  grants <- if (length(z$metadata$funding) == 0) {
    NULL
  } else {
    purrr::map_chr(z$metadata$funding, ~ .x$funder$name)
  }

  # Extract keywords
  keywords <- if (length(z$metadata$subjects) == 0) {
    NULL
  } else {
    purrr::map_chr(z$metadata$subjects, "subject")
  }

  # Extract created tiime
  created_time <- as.POSIXct(
    z$metadata$publication_date,
    tz = "UTC",
    tryFormats = c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%d")
  )
  created_time <- format(created_time, "%Y-%m-%dT%H:%M:%SZ")

  if (!is.null(pkg)) {
    check_gldp(pkg)
  } else {
    # Create the GLDP package
    pkg <- create_gldp()
  }

  # Add all metadata
  pkg$title <- z$metadata$title
  pkg$contributors <- contributors
  pkg$embargo <- embargo
  pkg$licenses <- licenses
  pkg$id <- glue::glue("https://doi.org/{concept_doi}")
  pkg$description <- z$metadata$description
  pkg$version <- z$metadata$version
  pkg$relatedIdentifiers <- relatedIdentifiers
  pkg$grants <- grants
  pkg$keywords <- keywords
  pkg$created <- created_time
  # pkg$bibliographicCitation = format(bib)
  # pkg$schema = NULL

  pkg <- pkg %>%
    update_gldp_bibliographic_citation() %>%
    update_gldp_metadata()

  # Return
  return(pkg)
}
