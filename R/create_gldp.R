#' Create a GeoLocator Data Package
#'
#' @description
#' This function create a geolocator data package R object. The arguments of the functions
#' correspond to the [standard geolocator Data Package
#' properties](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/).
#'
#'
#' This function initiate a Geolocator Data Package R object. This function is similar to
#' [`frictionless::create_package()`] but required specific metadata provided as arguments to the
#' function rather than the list `descriptor`. These arguments correspond to field of
#' [`datapackage.json`](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/).
#'
#' A geolocator data package is also a Data Package object created by
#' [`frictionless::create_package()`
#' ](https://docs.ropensci.org/frictionless/reference/create_package.html) and most (all?) functions
#' from the frictionless package should work.
#'
#' Note that this function does not add any data resources, it returns an empty shell with only
#' project level metadata.
#'
#' @param title A string providing a title or one sentence description for this package. It should
#' be plain text (no markup), capitalised like a title, NOT end in a period and less than 65
#' characters. See [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#title)
#' and the [Data Package specification](https://datapackage.org/standard/data-package/#title).
#' @param contributors A list of contributors, where each contributor is a list with properties
#' including at least `title` but also optionally `givenName`, `familyName`, `path`, `email`,
#' `roles`, and `organization`. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#contributors), the
#' [Data Package specification](https://datapackage.org/standard/data-package/#contributors) and the
#' [R Packages](https://r-pkgs.org/description.html#sec-description-authors-at-r).
#' @param embargo End date of the embargo as ISO 8601 date string (YYYY-MM-DD). The repository
#' will restrict access to the data until the end of the embargo period; at which time, the
#' content will become publicly available automatically. Default to `"1970-01-01"` (no embargo).
#' See the [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#embargo) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#embargo).
#' @param licenses A list of licenses under which the data is provided. Usually, a single license
#' is sufficient and preferred. If you're not sure, check out the [Creative Commons License Chooser
#' ](https://chooser-beta.creativecommons.org/) and the [Open Data Commons
#' ](https://opendatacommons.org/). `name` or `path` must be provided. Default is a CC-BY-4.0
#' license. See the [Geolocator DP
#' ](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#licenses) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#licenses).
#' @param id A globally unique identifier for the package, typically the DOI link of the
#' corresponding Zenodo repository which [can be reserved prior to publication
#' ](https://help.zenodo.org/docs/deposit/describe-records/reserve-doi/#reserve-doi). Defaults to
#' `NULL`. See the [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#id) and
#' the [Data Package specification](https://datapackage.org/standard/data-package/#id).
#' @param description A markdown-formatted string describing the package. You can
#' (and should!) use multiple sentences, but limited to a single paragraph. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#description) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#description).
#' @param version (optional) A version string identifying the version of the package, following
#' Semantic Versioning. Defaults to "1.0.0". See
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#version) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#version) and [Data
#' Package Version recipe](https://datapackage.org/recipes/data-package-version/).
#' @param relatedIdentifiers (optional) A list of related identifiers for the package. Each related
#' identifier is a list with properties `relationType`, `relatedIdentifier`, and
#' `relatedIdentifierType`. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#relatedIdentifiers) and
#' the [Camtrap DP specification
#' ](https://camtrap-dp.tdwg.org/metadata/#relatedIdentifiers).
#' @param grants (optional) A list of grants that funded the creation of the package. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#grants) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#grants).
#' @param keywords (optional) A list of keywords to assist users in searching for the package.
#' See the [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#keywords) and
#' the [Data Package specification](https://datapackage.org/standard/data-package/#keywords).
#' @param created Datetime on which this was created. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#created) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#created).
#' @param bibliographicCitation (optional) A string providing a citation for the package. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#citation) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#citation).
#' @param schema (optional) A URL to the `$schema` that describes the data package. Defaults
#' to the GeoLocator Data Package profile. See the [Data Package specification
#' ](https://datapackage.org/standard/data-package/#dollar-schema).
#'
#' @return A Geolocator Data Package object containing only metadata.
#'
#' @examples
#' # Create a Data Package with all possible metadata
#' pkg <- create_gldp(
#'   title = "Geolocator Data Package example",
#'   contributors = list(
#'     list(
#'       title = "Raphaël Nussbaumer",
#'       roles = c("ContactPerson", "DataCurator", "ProjectLeader")
#'     ),
#'     list(
#'       title = "Yann Rime",
#'       givenName = "Yann",
#'       familyName = "Rime",
#'       path = "https://orcid.org/0000-0003-2745-0557",
#'       email = "yann.rime@vogelwarte.ch",
#'       roles = c("Researcher"),
#'       organization = "Swiss Ornithological Institute"
#'     )
#'   ),
#'   embargo = "2025-01-01",
#'   licenses = list(list(name = "CC-BY-4.0")),
#'   id = "https://doi.org/10.5281/zenodo.13829929",
#'   description = NULL,
#'   version = "1.0.1",
#'   relatedIdentifiers = list(
#'     list(
#'       relationType = "IsPartOf",
#'       relatedIdentifier = "10.5281/zenodo.11207081",
#'       relatedIdentifierType = "DOI"
#'     ),
#'     list(
#'       relationType = "IsSupplementTo",
#'       relatedIdentifier = "10.1007/s00114-018-1566-9",
#'       relatedIdentifierType = "DOI"
#'     )
#'   ),
#'   grants = c("Swiss National Foundation grant no. 354251"),
#'   keywords = c("Woodland Kingfisher", "intra-african", "multi-sensor geolocator"),
#'   created = "2024-05-17",
#'   bibliographicCitation = "Nussbaumer, R., & Rime, Y. (2024). Woodland Kingfisher: Migration
#'   route and timing of South African Woodland Kingfisher (v1.1). Zenodo.
#'   https://doi.org/10.5281/zenodo.11207141"
#' )
#'
#' @export
create_gldp <- function(
  title = "",
  contributors = list(list(title = "")),
  embargo = "1970-01-01",
  licenses = list(list(
    name = "CC-BY-4.0",
    title = "Creative Commons Attribution 4.0",
    path = "https://creativecommons.org/licenses/by/4.0/"
  )),
  id = NULL,
  description = NULL,
  version = NULL,
  relatedIdentifiers = NULL,
  grants = NULL,
  keywords = NULL,
  created = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
  bibliographicCitation = NULL,
  schema = NULL
) {
  # Assertions to check input validity

  # Check title
  if (!is.character(title) || length(title) != 1 || is.na(title)) {
    cli_abort(c(
      "x" = "{.arg title} must be a string.",
      ">" = "Example: {.val {'Geolocator Data Package example'}}",
      "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#title}"
    ))
  }

  # Check contributors
  if (!is.list(contributors)) {
    cli_abort(c(
      "x" = "{.arg contributors} must be a list.",
      ">" = "Example: {.code list(list(title = 'John Doe', roles = c('ContactPerson')))}",
      "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#contributors}"
    ))
  }

  # Check each contributor has required fields
  for (i in seq_along(contributors)) {
    if (!is.list(contributors[[i]])) {
      cli_abort(c(
        "x" = "Contributor {i} must be a list.",
        ">" = "Example: {.code list(title = 'John Doe', roles = c('ContactPerson'))}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#contributors}"
      ))
    }
    if (is.null(contributors[[i]]$title) || !is.character(contributors[[i]]$title)) {
      cli_abort(c(
        "x" = "Contributor {i} is missing a {.field title}.",
        "!" = "The {.field title} field is required for each contributor.",
        ">" = "Example: {.code list(title = 'John Doe')}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#contributors}"
      ))
    }
  }

  # Check embargo date
  embargo_date <- as.Date(embargo)
  if (is.na(embargo_date)) {
    cli_abort(c(
      "x" = "{.arg embargo} contains an invalid date: {.val {embargo}}",
      ">" = "Use ISO format: {.val YYYY-MM-DD} (e.g., {.val '2025-01-01'})",
      "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#embargo}"
    ))
  }

  # Check licenses
  if (!is.list(licenses)) {
    cli_abort(c(
      "x" = "{.arg licenses} must be a list.",
      ">" = "Example: {.code list(list(name = 'CC-BY-4.0'))}",
      "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#licenses}"
    ))
  }

  for (i in seq_along(licenses)) {
    if (!is.list(licenses[[i]])) {
      cli_abort(c(
        "x" = "License {i} must be a list.",
        ">" = "Example: {.code list(name = 'CC-BY-4.0')}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#licenses}"
      ))
    }
    if (is.null(licenses[[i]]$name) && is.null(licenses[[i]]$path)) {
      cli_abort(c(
        "x" = "License {i} must have either {.field name} or {.field path}.",
        ">" = "Example: {.code list(name = 'CC-BY-4.0')}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#licenses}"
      ))
    }
  }

  # Check id
  if (!is.null(id)) {
    if (!is.character(id) || length(id) != 1 || is.na(id) || id == "") {
      cli_abort(c(
        "x" = "{.arg id} must be a non-empty string or {.val NULL}.",
        ">" = "Provide a globally unique identifier (typically a DOI): \\
        {.val 'https://doi.org/10.5281/zenodo.1234567'}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#id}"
      ))
    }
  }

  # Check description
  if (is.null(description) || description == "") {
    description <- NULL
  }
  if (!is.null(description)) {
    if (
      !is.character(description) ||
        length(description) != 1 ||
        is.na(description)
    ) {
      cli_abort(c(
        "x" = "{.arg description} must be a string or {.val NULL}.",
        "!" = "Provide a markdown-formatted description of your data package.",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#description}"
      ))
    }
  }

  # Check version
  if (!is.null(version)) {
    if (
      !is.character(version) ||
        length(version) != 1 ||
        is.na(version) ||
        version == ""
    ) {
      cli_abort(c(
        "x" = "{.arg version} must be a non-empty string or {.val NULL}.",
        ">" = "Use semantic versioning: {.val '1.0.0'}, {.val '1.2.3'}, {.val '2.0.0-beta.1'}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#version}"
      ))
    }
  }

  # Check relatedIdentifiers
  if (!is.null(relatedIdentifiers)) {
    if (!is.list(relatedIdentifiers)) {
      cli_abort(c(
        "x" = "{.arg relatedIdentifiers} must be a list.",
        ">" = "Example: \\
        {.code list(list(relationType = 'IsPartOf', relatedIdentifier = '10.1234/example'))}",
        "i" = "See \\
        {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#relatedIdentifiers}"
      ))
    }

    for (i in seq_along(relatedIdentifiers)) {
      if (!is.list(relatedIdentifiers[[i]])) {
        cli_abort(c(
          "x" = "Related identifier {i} must be a list.",
          ">" = "Example: \\
          {.code list(relationType = 'IsPartOf', relatedIdentifier = '10.1234/example')}",
          "i" = "See \\
          {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#relatedIdentifiers}"
        ))
      }
      if (is.null(relatedIdentifiers[[i]]$relationType)) {
        cli_abort(c(
          "x" = "Related identifier {i} is missing {.field relationType}.",
          ">" = "Common values: {.val IsPartOf}, {.val IsSupplementTo}, {.val IsCitedBy}",
          "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/} \\
          {.url #relatedIdentifiers}"
        ))
      }
      if (is.null(relatedIdentifiers[[i]]$relatedIdentifier)) {
        cli_abort(c(
          "x" = "Related identifier {i} is missing {.field relatedIdentifier}.",
          ">" = "Provide the DOI, URL, or other identifier",
          "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/} \\
          {.url #relatedIdentifiers}"
        ))
      }
      if (is.null(relatedIdentifiers[[i]]$relatedIdentifierType)) {
        cli_abort(c(
          "x" = "Related identifier {i} is missing {.field relatedIdentifierType}.",
          ">" = "Common values: {.val DOI}, {.val URL}, {.val ISBN}",
          "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/} \\
          {.url #relatedIdentifiers}"
        ))
      }
    }
  }

  # Check grants
  if (!is.null(grants)) {
    if (!is.character(grants) || anyNA(grants) || any(grants == "")) {
      cli_abort(c(
        "x" = "{.arg grants} must be a character vector with non-empty strings.",
        ">" = "Provide grant information as character strings. Example:
        {.code c('Swiss National Foundation grant no. 354251', 'NSF Grant 123456')}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#grants}"
      ))
    }
  }

  # Check keywords
  if (!is.null(keywords)) {
    if (!is.character(keywords) || anyNA(keywords) || any(keywords == "")) {
      cli_abort(c(
        "x" = "{.arg keywords} must be a character vector with non-empty strings.",
        ">" = "Provide keywords to help users find your data package. Example:
        {.code c('geolocator', 'migration', 'bird tracking')}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#keywords}"
      ))
    }
  }

  # Check created datetime
  tryCatch(
    {
      created_time <- as.POSIXct(
        created,
        tryFormats = c(
          "%Y-%m-%dT%H:%M:%SZ",
          "%Y-%m-%d %H:%M:%OS",
          "%Y/%m/%d %H:%M:%OS",
          "%Y-%m-%d %H:%M",
          "%Y/%m/%d %H:%M",
          "%Y-%m-%d",
          "%Y/%m/%d"
        ),
        tz = "UTC"
      )
      if (is.na(created_time)) {
        cli_abort(c(
          "x" = "{.arg created} contains an invalid datetime: {.val {created}}",
          ">" = "Use ISO datetime format or a recognizable date format. Examples:
        {.val {'2024-01-01T12:00:00Z'}}, {.val {'2024-01-01'}}",
          "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#created}"
        ))
      }
    },
    error = function(e) {
      cli_abort(c(
        "x" = "{.arg created} contains an invalid datetime: {.val {created}}",
        ">" = "Use ISO datetime format or a recognizable date format. Examples:
      {.val {'2024-01-01T12:00:00Z'}}, {.val {'2024-01-01'}}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#created}"
      ))
    }
  )

  # Check bibliographicCitation
  if (!is.null(bibliographicCitation)) {
    if (
      !is.character(bibliographicCitation) ||
        length(bibliographicCitation) != 1 ||
        is.na(bibliographicCitation) ||
        bibliographicCitation == ""
    ) {
      cli_abort(c(
        "x" = "{.arg bibliographicCitation} must be a non-empty string.",
        ">" = "Provide a properly formatted citation for your data package. Example:
        {.val {'Smith, J. (2024).
        Bird Migration Data. Zenodo. https://doi.org/10.5281/zenodo.1234567'}}",
        "i" = "See {.url https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#citation}"
      ))
    }
  }

  # Check schema
  if (is.null(schema) || is.na(schema)) {
    # version is defined in version.R
    schema <- glue::glue(
      "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/{.geolocator_dp_version}/geolocator-dp-profile.json"
    )
  }

  if (
    !is.character(schema) ||
      length(schema) != 1 ||
      is.na(schema) ||
      schema == ""
  ) {
    cli_abort(c(
      "x" = "{.arg schema} must be a non-empty string.",
      ">" = "Provide a URL to a valid JSON schema.",
      "i" = "Leave as {.val NULL} to use the default GeoLocator Data Package schema."
    ))
  }

  if (!grepl("^https?://[[:alnum:].-]+/?", schema)) {
    cli_abort(c(
      "x" = "{.arg schema} must be a valid URL: {.val {schema}}",
      ">" = "URLs should start with {.val http://} or {.val https://}",
      "i" = "Example: {.val {'https://example.com/schema.json'}}"
    ))
  }

  # Create the descriptor list
  descriptor <- list(
    title = title,
    contributors = contributors,
    embargo = format(embargo_date, "%Y-%m-%d"),
    licenses = licenses,
    created = format(created_time, "%Y-%m-%dT%H:%M:%SZ"),
    "$schema" = schema
  )

  # Conditionally add optional elements
  if (!is.null(id)) {
    descriptor$id <- id
  }
  if (!is.null(description)) {
    descriptor$description <- description
  }
  if (!is.null(version)) {
    descriptor$version <- version
  }
  if (!is.null(keywords)) {
    descriptor$keywords <- keywords
  }
  if (!is.null(bibliographicCitation)) {
    descriptor$bibliographicCitation <- bibliographicCitation
  }
  if (!is.null(grants)) {
    descriptor$grants <- grants
  }
  if (!is.null(relatedIdentifiers)) {
    descriptor$relatedIdentifiers <- relatedIdentifiers
  }

  # Create frictionless package
  pkg <- frictionless::create_package(descriptor = descriptor)

  class(pkg) <- c("geolocatordp", class(pkg))
  # This is the version of the GeoLocator Data Package standard (as defined by the release
  # version on GitHub). Not to be confused with the version of the data package being created.
  attr(pkg, "version") <- version(pkg)

  pkg <- pkg %>% update_gldp()

  return(pkg)
}
