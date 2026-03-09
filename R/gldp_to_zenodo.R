#' Create or update a Zenodo Record based on the metadata from a GeoLocatoR Data Package
#'
#' @description
#' This function update a `zen4R` ZenodoRecord based on the GeoLocator Data Package provided.
#' If no `zenodo_record` is provided, the function will create a new one.
#'
#' Note that (1) the record is not deposited online (`zenodo_record` is a variable in your computer)
#' and (2) no data is actually retrieved/copied.
#'
#' @param pkg A Geolocator Data Package object
#' @param zenodo_record A `zen4R` ZenodoRecord object from which to extract metadata
#' @param token A Zenodo API token. Best practice is to store it in your keyring (see description
#' above), in which case the default value will retrieve it using `keyring::key_get()`.
#'
#' @return A ZenodoRecord object updated with the metadata from `pkg`.
#'
#' @export
gldp_to_zenodo <- function(
  pkg,
  zenodo_record = zen4R::ZenodoRecord$new(),
  token = keyring::key_get(service = "ZENODO_PAT")
) {
  check_gldp(pkg)

  z <- zenodo_record
  assertthat::assert_that(all(
    c("ZenodoRecord", "zen4RLogger", "R6") %in% class(z)
  ))

  # Initialize the ZenodoManager with the provided token. This is necessary to check and convert
  # some metadata.
  zenodo <- zen4R::ZenodoManager$new(token = token) # no-lint

  # Set de resource type to dataset
  z$setResourceType("dataset")

  z$setPublisher("Zenodo")

  # Set title
  z$setTitle(pkg$title)

  # Set Contributor/Creators
  z$metadata$creators <- NULL
  warn_role <- FALSE
  scalar_or_null <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NULL)
    }
    x <- x[1]
    if (is.na(x) || !nzchar(x)) {
      return(NULL)
    }
    x
  }

  purrr::map(pkg$contributors, \(c) {
    title <- scalar_or_null(c$title)
    given_name <- scalar_or_null(c$givenName)
    family_name <- scalar_or_null(c$familyName)
    org_name <- scalar_or_null(c$organization)

    org_matches_title <- !is.null(title) &&
      !is.null(org_name) &&
      tolower(trimws(title)) == tolower(trimws(org_name))

    if (is.null(given_name) || is.null(family_name)) {
      if (!org_matches_title && !is.null(title)) {
        if (grepl(",", title, fixed = TRUE)) {
          parts <- strsplit(title, ",", fixed = TRUE)[[1]]
          family_guess <- trimws(parts[1])
          given_guess <- trimws(paste(parts[-1], collapse = ","))
        } else {
          parts <- strsplit(title, "\\s+")[[1]]
          if (length(parts) >= 2) {
            family_guess <- trimws(parts[length(parts)])
            given_guess <- trimws(paste(parts[-length(parts)], collapse = " "))
          } else {
            family_guess <- NULL
            given_guess <- NULL
          }
        }

        if (is.null(family_name) && !is.null(family_guess) && nzchar(family_guess)) {
          family_name <- family_guess
        }
        if (is.null(given_name) && !is.null(given_guess) && nzchar(given_guess)) {
          given_name <- given_guess
        }
      }
    }

    orgs <- if ("organization" %in% names(c) && !is.null(c$organization[1])) {
      purrr::keep(
        strsplit(c$organization[1], ", ")[[1]],
        ~ !is.null(zenodo$getAffiliationByName(.x))
      )
    } else {
      character() # Return an empty character vector if the column is missing or NULL
    }

    allowed_roles <- c(
      "contactperson",
      "datacollector",
      "datacurator",
      "datamanager",
      "distributor",
      "editor",
      "funder",
      "hostinginstitution",
      "producer",
      "projectleader",
      "projectmanager",
      "projectmember",
      "registrationagency",
      "registrationauthority",
      "relatedperson",
      "researcher",
      "researchgroup",
      "rightsholder",
      "supervisor",
      "sponsor",
      "workpackageleader",
      "other"
    )

    if (length(c$roles) > 0 && !warn_role) {
      cli_warn(c(
        "!" = "Zenodo's creator can only have a single {.field role}.",
        ">" = "Only the first role will be kept"
      ))
      warn_role <<- TRUE
    }
    roles <- tolower(c$roles[[1]])
    roles <- ifelse(roles %in% allowed_roles, roles, "other")

    if (!is.null(family_name) && !is.null(given_name)) {
      name <- paste(family_name, given_name, sep = ", ")
    } else if (!is.null(title)) {
      name <- title
    } else {
      name <- paste(family_name, given_name, sep = ", ")
    }

    z$addCreator(
      firstname = given_name,
      lastname = family_name,
      name = name,
      orcid = gsub("https://orcid.org/", "", c$path),
      role = roles,
      affiliations = orgs
    )
  })

  # Set embargo, Visibility, AccessPolicy
  if (as.Date(pkg$embargo) > Sys.Date()) {
    z$setAccessPolicyEmbargo(
      active = TRUE,
      until = as.Date(pkg$embargo),
      reason = ""
    )
    z$setAccessPolicyFiles(access = "restricted")
  } else {
    z$setAccessPolicyEmbargo(active = FALSE, until = NULL, reason = "")
    z$setAccessPolicyFiles(access = "public")
  }

  # Set license
  if (length(pkg$licenses) > 1) {
    cli_warn(c(
      "!" = "Multiple licenses detected ({length(pkg$licenses)}).",
      ">" = "Only the first license will be used."
    ))
  }
  z$setLicense(licenseId = tolower(pkg$licenses[[1]]$name))

  # Set DOI
  # No need as either generate a new one if not present, or retrieve record
  # z$setDOI(pkg$id)

  # Set description
  z$setDescription(pkg$description)

  # Set version
  z$setVersion(version = pkg$version)

  # Set relatedidentifier
  z$metadata$related_identifiers <- NULL

  purrr::map(pkg$relatedIdentifiers, \(r) {
    z$addRelatedIdentifier(
      identifier = r$relatedIdentifier,
      scheme = tolower(r$relatedIdentifierType),
      relation_type = tolower(r$relationType),
      resource_type = r$resourceTypeGeneral
    )
  })

  # Add grant/funding
  purrr::map(pkg$grants, \(g) {
    f <- zenodo$getFundersByName(g)
    if (!is.null(f)) {
      z$addFunding(f$id[1])
    }
  })

  # Set keyword
  z$setSubjects(pkg$keywords)

  # Set created/publication date
  z$setPublicationDate(format(as.POSIXct(pkg$created), "%Y-%m-%d"))

  # Locations is not saved on Zenodo
  # z$addLocation(place="equipement", lat=2, lon=32)

  # Not working
  # z$addCommunity("geolocator-dp")

  #
  return(z)
}
