#' Transform a GeoLocator Data Package to EML
#'
#' @description
#' Transforms a [GeoLocator Data Package (GLDP)](https://geopressure.org/GeoLocator-DP/)
#' to [Ecological Metadata Language (EML)](https://eml.ecoinformatics.org/).
#' The resulting EML file can be uploaded to an [IPT](https://www.gbif.org/ipt) for
#' publication to GBIF and/or OBIS. A corresponding Darwin Core Archive can be created
#' with [gldp_to_dwc()].
#'
#' @param pkg A GeoLocator Data Package object.
#' @param directory Path to local directory to write files to.
#'
#' @return `eml.xml` file written to disk.
#'   Invisibly, an [EML::eml] object.
#'
#' @section Transformation details:
#' Metadata are derived from the GeoLocator Data Package and transformed to EML.
#' The following properties are set:
#'
#' - `title`: Package title.
#' - `abstract`: Package description.
#' - `pubDate`: Publication year extracted from publication/creation date.
#' - `creator`: Contributors with roles `"ProjectLeader"`, `"Researcher"`, or
#'   `"DataCurator"` when available.
#' - `contact`: Contributors with role `"ContactPerson"` as provided in
#'   contributors.
#' - `associatedParty`: Other contributors,
#'   including those with role `"RightsHolder"`.
#' - `intellectualRights`: License information.
#' - `keywords`: Package keywords.
#' - `packageId`: Package identifier. If no ID is
#'   provided, a UUID is generated. As a result, no new DOI will be created when
#'   publishing to GBIF if the package ID contains a DOI.
#'
#' Coverage information:
#' - `temporalCoverage`: Date range from package temporal coverage (if available),
#'   derived from measurement timestamps.
#' - `geographicCoverage`: Bounding box from package spatial coverage (if available),
#'   calculated from all locations in observations, paths, and pressurepaths.
#' - `taxonomicCoverage`: Species list from package taxonomic coverage (if available),
#'   derived from unique scientific names in tags.
#'
#' Methods:
#' - `methodStep`: Generic description indicating data were processed using the
#'   GeoLocator Data Package standard.
#'
#' The following EML properties are not set:
#' - `type`
#' - `subtype`
#' - `update frequency`
#' - `publishing organization`
#' - `project data`
#' - `citations`
#' - `collection data`
#'
#' @export
gldp_to_eml <- function(pkg, directory) {
  check_gldp(pkg)

  if (!requireNamespace("EML", quietly = TRUE)) {
    cli_abort("The {.pkg EML} package is required for this function.")
  }
  if (!requireNamespace("uuid", quietly = TRUE)) {
    cli_abort("The {.pkg uuid} package is required for this function.")
  }

  # Convert HTML-like strings to plain text with rvest.
  to_plain_text <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    x <- as.character(x)
    x <- stats::na.omit(x)
    if (length(x) == 0) {
      return(NULL)
    }
    is_html_like <- grepl("<[^>]+>|&[A-Za-z#0-9]+;", x, perl = TRUE)
    x[is_html_like] <- vapply(
      x[is_html_like],
      \(value) rvest::read_html(value) |> rvest::html_text2(),
      character(1),
      USE.NAMES = FALSE
    )
    x
  }

  # Title
  title <- to_plain_text(pkg$title %||% "GeoLocator Data Package")[1]

  # Creators and Contacts
  creators <- list()
  contacts <- list()
  associated_parties <- list()

  for (contributor in (pkg$contributors %||% list())) {
    # Normalise possibly missing fields
    given_name <- contributor$givenName
    family_name <- contributor$familyName
    organization_name <- contributor$organization
    contributor_title <- contributor$title %||% contributor$name %||% NULL
    if (is.null(organization_name)) {
      organization_name <- contributor_title
    }
    email <- contributor$email

    # If absolutely no identifying info is available, skip this contributor
    if (is.null(given_name) && is.null(family_name) && is.null(organization_name)) {
      next
    }

    # Create EML person/party (role will be added later, if needed)
    party <- EML::set_responsibleParty(
      givenName = given_name,
      surName = family_name,
      organizationName = organization_name,
      electronicMailAddress = email,
      userId = if (!is.null(contributor$path) && grepl("orcid.org", contributor$path)) {
        list(directory = "https://orcid.org/", sub("https://orcid.org/", "", contributor$path))
      } else {
        NULL
      }
    )

    roles <- contributor$roles
    if (is.null(roles)) {
      roles <- character(0)
    }
    roles <- as.character(roles)
    roles_lower <- tolower(roles)

    if ("contactperson" %in% roles_lower) {
      contacts <- c(contacts, list(party))
    }

    if (
      length(roles) == 0 ||
        any(c("projectleader", "researcher", "datacurator") %in% roles_lower)
    ) {
      creators <- c(creators, list(party))
    } else if (!("contactperson" %in% roles_lower) && length(roles) > 0) {
      # If not a creator or contact, add as associated party with first role (when available)
      party$role <- roles[1]
      associated_parties <- c(associated_parties, list(party))
    }

    if ("rightsholder" %in% roles_lower) {
      party$role <- "rightsHolder"
      associated_parties <- c(associated_parties, list(party))
    }
  }

  # EML requires a contact. If none is explicitly provided, fallback to first creator.
  if (length(contacts) == 0 && length(creators) > 0) {
    contacts <- creators[1]
  }

  # Deduplicate creators if needed (simple check)
  # EML::set_responsibleParty returns a list, so we have a list of lists.

  # Abstract
  abstract_text <- to_plain_text(pkg$description)
  abstract <- if (
    !is.null(abstract_text) && length(abstract_text) > 0 && nzchar(abstract_text[1])
  ) {
    list(para = abstract_text[1])
  } else {
    NULL
  }

  # Keywords
  keyword_values <- to_plain_text(pkg$keywords)
  keyword_set <- if (!is.null(keyword_values) && length(keyword_values) > 0) {
    list(keyword = keyword_values)
  } else {
    NULL
  }

  # Intellectual Rights
  license_names <- character(0)
  if (is.list(pkg$licenses) && length(pkg$licenses) > 0) {
    license_names <- purrr::map_chr(pkg$licenses, \(lic) {
      lic$name %||% lic$title %||% lic$id %||% ""
    })
    license_names <- license_names[nzchar(license_names)]
  }
  intellectual_rights <- if (length(license_names) > 0) {
    list(
      para = to_plain_text(glue::glue(
        "This work is licensed under a {glue::glue_collapse(license_names, sep = ', ')} license."
      ))[1]
    )
  } else {
    NULL
  }

  # Coverage
  coverage <- list()
  temporal <- pkg$temporal
  spatial <- pkg$spatial
  taxonomic <- pkg$taxonomic

  # Temporal
  if (!is.null(temporal)) {
    coverage$temporalCoverage <- list(
      rangeOfDates = list(
        beginDate = list(calendarDate = temporal$start),
        endDate = list(calendarDate = temporal$end)
      )
    )
  }

  # Spatial
  if (!is.null(spatial)) {
    # Extract bounding box from polygon
    coords <- spatial$coordinates
    # coords is array(dim=c(1,5,2))
    lons <- coords[1, , 1]
    lats <- coords[1, , 2]

    coverage$geographicCoverage <- list(
      geographicDescription = "Bounding box of all locations",
      boundingCoordinates = list(
        westBoundingCoordinate = min(lons),
        eastBoundingCoordinate = max(lons),
        northBoundingCoordinate = max(lats),
        southBoundingCoordinate = min(lats)
      )
    )
  }

  # Taxonomic
  if (!is.null(taxonomic)) {
    coverage$taxonomicCoverage <- list(
      taxonomicClassification = lapply(taxonomic, function(sp) {
        list(
          taxonRankName = "Species",
          taxonRankValue = sp
        )
      })
    )
  }

  # Dataset
  pub_year <- NA_character_
  if (is_non_empty_string(as.character(pkg$created %||% NA_character_)[1])) {
    pub_year <- format(as.Date(substr(as.character(pkg$created[1]), 1, 10)), "%Y")
  }
  if (is.na(pub_year)) {
    pub_year <- NULL
  }

  dataset <- list(
    title = title,
    creator = creators,
    contact = contacts,
    associatedParty = associated_parties,
    pubDate = pub_year,
    abstract = abstract,
    keywordSet = keyword_set,
    intellectualRights = intellectual_rights,
    coverage = coverage
  )

  # EML object
  eml <- list(
    packageId = if (is.null(pkg$id)) uuid::UUIDgenerate() else pkg$id,
    system = "uuid",
    dataset = dataset
  )

  # Write file
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  eml_path <- file.path(directory, "eml.xml")

  EML::write_eml(eml, eml_path)

  cli_alert_success("EML metadata file written to {.file {eml_path}}")

  invisible(eml)
}
