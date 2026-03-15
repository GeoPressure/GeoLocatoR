#' Convert contributors to person objects
#'
#' Internal helper function to convert a list of contributors to person objects
#' for use in DESCRIPTION files.
#'
#' @param contributors A list of contributor objects from a GeoLocator Data Package
#' @return A list of person objects
#' @noRd
contributors_to_persons <- function(contributors) {
  role_mapping <- c(
    "contactperson" = "ctr", # Contractor (assumed due to lack of clear match)
    "contributor" = "ctb", # Contributor
    "datacollector" = "dtc", # Data contributor
    "datacurator" = "dtc", # Data contributor (no closer match)
    "datamanager" = "dtc", # Data contributor (no closer match)
    "distributor" = "ctr", # Contractor (assumed)
    "editor" = "rev", # Reviewer
    "hostinginstitution" = "cph", # Copyright holder (legal entity)
    "producer" = "aut", # Author (substantial contribution)
    "projectleader" = "cre", # Creator (project leader matches)
    "projectmanager" = "cre", # Creator (manager of the project)
    "projectmember" = "ctb", # Contributor (smaller contributions)
    "registrationagency" = "ctr", # Contractor (assumed)
    "registrationauthority" = "ctr", # Contractor (assumed)
    "relatedperson" = "trl", # Translator (assumed)
    "researcher" = "aut", # Author
    "researchergroup" = "aut", # Author (group treated as authors)
    "rightsholder" = "cph", # Copyright holder
    "sponsor" = "fnd", # Funder
    "supervisor" = "ths", # Thesis advisor
    "workpackageleader" = "cre" # Creator (assumed leader role)
  )

  persons <- contributors %>%
    purrr::map(
      ~ {
        utils::person(
          given = ifelse(
            is.null(.x$givenName) & !is.null(.x$title),
            .x$title,
            .x$givenName
          ),
          family = .x$familyName,
          email = .x$email,
          role = purrr::map_vec(
            .x$roles,
            ~ coalesce(role_mapping[tolower(.x)], "ctb")
          ),
          comment = c(.x$path, .x$organization)
        )
      }
    )

  persons <- do.call(c, Filter(Negate(is.null), persons))

  persons
}

#' @noRd
normalize_enum <- function(value, allowed, default = NULL, output = "allowed") {
  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  if (length(value) > 1) {
    value <- value[1]
  }

  if (is.na(value)) {
    return(default)
  }

  match_idx <- match(tolower(value), tolower(allowed))
  if (is.na(match_idx)) {
    result <- if (is.null(default)) value else default
  } else {
    result <- allowed[match_idx]
  }

  if (identical(output, "lower")) {
    return(tolower(result))
  }

  result
}

.gldp_contributor_roles <- c(
  "ContactPerson",
  "ProjectLeader",
  "DataCollector",
  "DataCurator",
  "Researcher",
  "RightsHolder",
  "Supervisor",
  "Other"
)

.gldp_relation_types <- c(
  "IsCitedBy",
  "Cites",
  "IsSupplementTo",
  "IsSupplementedBy",
  "IsContinuedBy",
  "Continues",
  "IsNewVersionOf",
  "IsPreviousVersionOf",
  "IsPartOf",
  "HasPart",
  "IsPublishedIn",
  "IsReferencedBy",
  "References",
  "IsDocumentedBy",
  "Documents",
  "IsCompiledBy",
  "Compiles",
  "IsVariantFormOf",
  "IsOriginalFormOf",
  "IsIdenticalTo",
  "HasMetadata",
  "IsMetadataFor",
  "Reviews",
  "IsReviewedBy",
  "IsDerivedFrom",
  "IsSourceOf",
  "Describes",
  "IsDescribedBy",
  "HasVersion",
  "IsVersionOf",
  "Requires",
  "IsRequiredBy",
  "Obsoletes",
  "IsObsoletedBy"
)

.gldp_identifier_types <- c(
  "DOI",
  "URL",
  "ARK",
  "arXiv",
  "bibcode",
  "CSTR",
  "EAN13",
  "EISSN",
  "Handle",
  "IGSN",
  "ISBN",
  "ISSN",
  "ISTC",
  "LISSN",
  "LSID",
  "PMID",
  "PURL",
  "RRID",
  "UPC",
  "URN",
  "w3id",
  "Other"
)

.gldp_resource_types <- c(
  "Audiovisual",
  "Book",
  "BookChapter",
  "Collection",
  "ComputationalNotebook",
  "ConferencePaper",
  "ConferenceProceeding",
  "DataPaper",
  "Dataset",
  "Dissertation",
  "Event",
  "Image",
  "InteractiveResource",
  "Journal",
  "JournalArticle",
  "Model",
  "OutputManagementPlan",
  "PeerReview",
  "PhysicalObject",
  "Preprint",
  "Report",
  "Service",
  "Software",
  "Sound",
  "Standard",
  "Text",
  "Workflow",
  "Other"
)

#' @noRd
map_gldp_to_zenodo <- function(value, allowed, default = "Other") {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(default)
  }

  match_idx <- match(tolower(value), tolower(allowed))
  if (is.na(match_idx)) {
    return(default)
  }

  allowed[[match_idx]]
}


#' Cast data frame columns according to schema types
#'
#' Internal helper function to cast data frame columns to the appropriate types
#' based on a table schema specification.
#'
#' @param data A data frame to cast
#' @param schema A table schema object with field definitions
#' @return The data frame with properly cast column types
#' @noRd
cast_table <- function(data, schema) {
  schema_fields <- sapply(schema$fields, \(x) x$name)
  schema_types <- sapply(schema$fields, \(x) x$type)

  for (i in seq_along(schema_fields)) {
    field <- schema_fields[i]
    type <- schema_types[i]

    if (field %in% names(data)) {
      if (type == "string") {
        data[[field]] <- as.character(data[[field]])
      } else if (type == "number") {
        data[[field]] <- as.numeric(data[[field]])
      } else if (type == "integer") {
        data[[field]] <- as.integer(data[[field]])
      } else if (type == "boolean") {
        data[[field]] <- as.logical(data[[field]])
      } else if (type == "date") {
        data[[field]] <- as.Date(data[[field]])
      } else if (type == "datetime") {
        data[[field]] <- as.POSIXct(data[[field]], tz = "UTC")
      } else if (type == "time") {
        # For time fields, convert to character if not already
        if (!is.character(data[[field]])) {
          data[[field]] <- as.character(data[[field]])
        }
      } else if (type == "year") {
        # For year fields, convert to integer
        data[[field]] <- as.integer(data[[field]])
      } else if (type == "yearmonth") {
        # For yearmonth fields, keep as character
        data[[field]] <- as.character(data[[field]])
      } else if (type == "duration") {
        # For duration fields, keep as character (ISO 8601 format)
        data[[field]] <- as.character(data[[field]])
      } else if (type == "geopoint") {
        # For geopoint fields, keep as character
        data[[field]] <- as.character(data[[field]])
      } else if (type == "geojson") {
        # For geojson fields, keep as character
        data[[field]] <- as.character(data[[field]])
      } else {
        cli_warn(c(
          "!" = "No casting for {.field {field}} of type {.val {type}}."
        ))
      }
    }
  }
  data
}
