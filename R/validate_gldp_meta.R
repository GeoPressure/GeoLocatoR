#' Validate GeoLocator Data Package metadata recommendations
#'
#' Internal helper function to report metadata recommendations for a GeoLocator
#' Data Package. All checks are advisory and do not affect the result returned
#' by [validate_gldp()].
#'
#' The current recommendations cover title formatting, contributor roles,
#' Zenodo `record_type`, related identifiers, community selection,
#' `codeRepository`, expected manufacturer names in `tags$manufacturer`, and
#' taxonomic names in `pkg$taxonomic`. Taxonomic validation relies on the eBird
#' taxonomy API; lookup failures are reported as warnings.
#'
#' @param pkg A GeoLocator Data Package object
#' @return Invisibly returns `TRUE` when all recommendations pass, `FALSE` otherwise.
#' @noRd
validate_gldp_meta <- function(pkg) {
  cli_h3("Check Metadata")

  # Summary of metadata recommendations checked below:
  # - title prefix convention
  # - contributor role completeness
  # - record_type should be "dataset"
  # - related identifiers presence
  # - community selection
  # - GitHub codeRepository URL
  # - allowed tag manufacturer names in tags metadata
  # - taxonomic names present in eBird taxonomy
  has_warnings <- FALSE

  # 1) Title should follow the shared "GeoLocator Data Package: " prefix.
  title <- pkg$title %||% ""

  if (!nzchar(title)) {
    cli::cli_alert_warning(
      "{.field pkg$title} is missing or empty."
    )
    has_warnings <- TRUE
  } else if (!startsWith(title, "GeoLocator Data Package: ")) {
    cli::cli_alert_warning(
      "Missing expected prefix {.val GeoLocator Data Package: } in {.field pkg$title}."
    )
    has_warnings <- TRUE
  }

  # 2) Contributors should include roles when possible.
  contributors <- pkg$contributors %||% list()
  contributors_with_role <- contributors |>
    purrr::map_lgl(\(x) {
      roles <- x$roles %||% character(0)
      any(!is.na(roles) & nzchar(as.character(roles)))
    }) |>
    sum()

  if (contributors_with_role == 0) {
    cli::cli_alert_warning(
      "Missing {.field role} in {.field pkg$contributors}."
    )
    has_warnings <- TRUE
  }

  # 3) Zenodo record type should be "dataset".
  record_type <- pkg$record_type %||% NA_character_
  is_dataset_record <- is.character(record_type) &&
    length(record_type) == 1 &&
    identical(trimws(tolower(record_type)), "dataset")
  if (!is_dataset_record) {
    cli::cli_alert_warning(
      "Expected {.field pkg$record_type} to be {.val dataset}; got {.val {record_type}}."
    )
    has_warnings <- TRUE
  }

  # 4) tags$manufacturer should use one of the expected institution names.
  allowed_manufacturers <- c(
    "Swiss Ornithological Institute",
    "Migrate Technology Limited",
    "Lund University",
    "British Antarctic Survey"
  )
  if ("tags" %in% frictionless::resources(pkg)) {
    t <- tags(pkg)
    manufacturers <- unique(t$manufacturer)
    invalid_manufacturers <- manufacturers[
      !is.na(manufacturers) &
        nzchar(trimws(manufacturers)) &
        !(manufacturers %in% allowed_manufacturers)
    ]
    if (length(invalid_manufacturers) > 0) {
      cli::cli_alert_warning(
        "Unusual {.field tags$manufacturer}: {.val {invalid_manufacturers}}. Please, try to use {.val {allowed_manufacturers}} if possible."
      )
      has_warnings <- TRUE
    }
  }

  # 5) Data-related publications/resources should be linked in related identifiers.
  related <- pkg$relatedIdentifiers %||% list()
  if (length(related) == 0) {
    cli::cli_alert_warning(
      "No {.field relatedIdentifiers} present. Add any resources related to the data (e.g. papers, project pages, derived datasets, etc.)."
    )
    has_warnings <- TRUE
  }

  # 6) At least one community should be selected for the record.
  communities <- pkg$communities %||% list()
  allowed_communities <- c(
    "b7c70316-310b-435e-9a8b-84188d60a3cc",
    "6e9c24fb-954a-4087-ae9b-71fcba41a624" # sandbox community
  )
  has_expected_community <- purrr::some(
    communities,
    \(x) x %in% allowed_communities
  )
  if (!has_expected_community) {
    cli::cli_alert_warning(
      "Missing selected community {.url https://zenodo.org/communities/geolocator-dp/}."
    )
    has_warnings <- TRUE
  }

  # 7) Software/repository URL should be a GitHub repository URL.
  code_repo <- pkg$codeRepository %||% NA_character_

  is_github_repo <- is.character(code_repo) &&
    length(code_repo) == 1 &&
    grepl(
      "^https://github\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(\\.git)?/?$",
      trimws(code_repo)
    )

  if (!is_github_repo) {
    cli::cli_alert_warning(
      "Missing or invalid {.field pkg$codeRepository}; expected a GitHub repository URL."
    )
    has_warnings <- TRUE
  }
  has_warnings <- has_warnings | validate_gldp_taxonomic(pkg)

  if (!has_warnings) {
    cli_alert_success("Metadata recommendations passed.")
  }

  invisible(!has_warnings)
}

#' Validate taxonomic names against the eBird taxonomy
#'
#' Internal helper for metadata recommendations. It compares the unique
#' scientific names listed in `pkg$taxonomic` against the eBird taxonomy API.
#' Missing matches and API lookup failures are reported as warnings.
#'
#' @param pkg A GeoLocator Data Package object.
#'
#' @return Returns `TRUE` when a warning condition is detected, `FALSE` when no
#'   issue is found or no taxonomic names are provided.
#' @noRd
validate_gldp_taxonomic <- function(pkg) {
  species <- pkg$taxonomic %||% character(0)
  species <- unique(trimws(as.character(species)))
  species <- species[!is.na(species) & nzchar(species)]
  if (length(species) == 0) {
    return(FALSE)
  }

  req <- httr2::request("https://api.ebird.org/v2/ref/taxonomy/ebird?fmt=json") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(20)

  ebird <- tryCatch(
    req |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE),
    error = function(e) {
      cli::cli_alert_warning(
        "Could not validate {.field pkg$taxonomic} against eBird taxonomy: {e$message}."
      )
      NULL
    }
  )
  if (is.null(ebird)) {
    return(TRUE)
  }

  ebird_names <- if (is.data.frame(ebird) && "sciName" %in% names(ebird)) {
    ebird$sciName
  } else {
    purrr::map_chr(ebird, \(x) x$sciName %||% NA_character_)
  }
  ebird_names <- unique(trimws(as.character(ebird_names)))
  ebird_names <- ebird_names[!is.na(ebird_names) & nzchar(ebird_names)]

  invalid <- setdiff(species, ebird_names)
  if (length(invalid) > 0) {
    cli::cli_alert_warning(
      "Scientific names in {.field pkg$taxonomic} not matching any entry in eBird taxonomy: {.val {invalid}}."
    )
    return(TRUE)
  }

  FALSE
}
