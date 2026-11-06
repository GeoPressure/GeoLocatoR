#' Read a GeoLocator Data Package from Zenodo
#'
#' @description
#' Retrieve a GeoLocator Data Package from a Zenodo record identifier:
#'
#' 1. Resolve and fetch Zenodo record metadata from the REST API.
#' 2. Download `datapackage.json`, then the files its resources refer to and
#'    `params.json` when the record has one, to a temporary directory. Other
#'    files of the record are not downloaded. Each file is checked against the
#'    MD5 checksum Zenodo records for it.
#' 3. Read `datapackage.json` with [read_gldp()], loading every table into
#'    memory, then delete the temporary directory.
#' 4. Attach Zenodo metadata and recompute derived package properties with
#'    [update_gldp()].
#'
#' @details
#'
#' - If the record has no files, because it is metadata-only or its files are
#'   restricted or embargoed, the function warns and returns a package with
#'   only metadata, created with [create_gldp()].
#' - If the record does not contain `datapackage.json` or a file it refers to,
#'   the function aborts.
#' - If one or more file downloads fail or do not match their checksum, the
#'   function aborts with download details.
#'
#' Downloaded files are written under `tempdir()` in a run-specific folder,
#' which is deleted once the package is read. A resource without a table
#' schema, which is not loaded into memory, keeps a `path` into that deleted
#' folder.
#'
#' To read a draft:
#' 1. Create or identify the Zenodo deposition and note its record id.
#' 2. Obtain a Zenodo access token with permission to view that draft.
#' 3. Provide the token directly with `token =`, or make it available as
#'    `ZENODO_TOKEN` (main Zenodo) or `ZENODO_TOKEN_SANDBOX` (Zenodo Sandbox).
#'    The same names are also checked in the system keyring.
#' 4. Call `read_zenodo(id, draft = TRUE)`, and set `sandbox = TRUE` when the
#'    draft is hosted on Zenodo Sandbox.
#'
#' If `token = NULL`, token lookup is attempted in this order:
#' - for main Zenodo: `ZENODO_TOKEN`, then keyring service `ZENODO_TOKEN`;
#' - for Zenodo Sandbox: `ZENODO_TOKEN_SANDBOX`, `ZENODO_TOKEN`, then keyring
#'   services `ZENODO_TOKEN_SANDBOX` and `ZENODO_TOKEN`.
#'
#' @param id Zenodo identifier. Can be a record id, DOI, DOI URL, or Zenodo
#'   record URL. Supported examples include `"18467383"`,
#'   `"10.5281/zenodo.18467383"`, `"https://doi.org/10.5281/zenodo.18467383"`,
#'   and `"https://zenodo.org/records/18467383"`. A Zenodo Sandbox record URL
#'   or DOI (prefix `10.5072`) is read from Zenodo Sandbox; for a bare Sandbox
#'   record id, set `sandbox = TRUE`.
#' @param token Optional Zenodo access token for authenticated requests. Supply
#'   it explicitly, or leave `NULL` to resolve it from environment variables or
#'   the system keyring. Draft access usually requires a token.
#' @param draft Logical. If `TRUE`, read the draft version of the record instead
#'   of the public record. This queries the draft endpoint
#'   (`/api/records/{id}/draft`), which requires a token and permission
#'   to access the deposition. Use this for unpublished records or when draft
#'   files or metadata differ from the published record.
#' @param sandbox Logical. If `TRUE`, query Zenodo Sandbox instead of the main
#'   Zenodo service. This also switches token lookup to sandbox-first sources.
#' @param quiet Logical. If `TRUE`, suppress progress messages from
#'   `read_zenodo()`.
#' @param timeout Maximum time in seconds allowed to download each file.
#'
#' @return A `geolocatordp` object from the Zenodo record.
#'
#' @examples
#' \dontrun{
#' pkg <- read_zenodo("18467383", quiet = TRUE)
#'
#' # Resolve from DOI URL
#' pkg <- read_zenodo("https://doi.org/10.5281/zenodo.18467383", quiet = TRUE)
#'
#' # Access a draft in Zenodo Sandbox using a token from the environment/keyring
#' pkg <- read_zenodo("470406", sandbox = TRUE, draft = TRUE, quiet = TRUE)
#'
#' # Or provide the token explicitly
#' pkg <- read_zenodo(
#'   "470406",
#'   sandbox = TRUE,
#'   draft = TRUE,
#'   token = "<TOKEN>",
#'   quiet = TRUE
#' )
#' }
#'
#' @seealso [read_gldp()] to read a local or remote `datapackage.json`, and
#'   [create_gldp()] for the empty package shell used when record files are not
#'   accessible.
#' @export
read_zenodo <- function(
  id,
  token = NULL,
  draft = FALSE,
  sandbox = FALSE,
  quiet = FALSE,
  timeout = 600
) {
  # A Sandbox record URL or DOI names a Sandbox record whatever `sandbox` says
  sandbox <- sandbox || grepl("sandbox\\.zenodo\\.org|10\\.5072/", id)
  token <- resolve_zenodo_token(token = token, sandbox = sandbox)

  # 1) Fetch Zenodo record
  if (!quiet) {
    cli_progress_step("Retrieve Zenodo record {.field {parse_zenodo_id(id)}}")
  }
  zenodo_record <- read_zenodo_fetch_record(
    id,
    token = token,
    draft = draft,
    sandbox = sandbox
  )

  # 2) Download the files the package uses
  if (!quiet) {
    cli_progress_step("Download files from Zenodo")
  }
  download_dir <- read_zenodo_download_files(
    zenodo_record = zenodo_record,
    token = token,
    sandbox = sandbox,
    timeout = timeout
  )

  # 3) Read the tables into memory, so the downloaded files can be deleted
  if (!is.null(download_dir)) {
    on.exit(unlink(download_dir, recursive = TRUE), add = TRUE)
    if (!quiet) {
      cli_progress_step("Read and upgrade GeoLocator-DP")
    }
    pkg <- read_gldp(file.path(download_dir, "datapackage.json"), force_read = TRUE)
  } else {
    pkg <- create_gldp()
  }

  # 4) Attach Zenodo metadata and update derived properties
  if (!quiet) {
    cli_progress_step("Add metadata")
  }
  pkg <- read_zenodo_attach_metadata(pkg = pkg, zenodo_record = zenodo_record)
  pkg <- update_gldp(pkg)

  if (!quiet) {
    cli_progress_done()
  }
  pkg
}

#' @noRd
read_zenodo_fetch_record <- function(
  id,
  token = NULL,
  draft = FALSE,
  sandbox = FALSE
) {
  record_id <- parse_zenodo_id(id)
  base_url <- if (isTRUE(sandbox)) "https://sandbox.zenodo.org" else "https://zenodo.org"
  url <- glue::glue("{base_url}/api/records/{record_id}{ifelse(draft,'/draft','')}")

  req <- httr2::request(url) |>
    httr2::req_user_agent(glue::glue("GeoLocatoR/{utils::packageVersion('GeoLocatoR')}")) |>
    httr2::req_headers(Accept = "application/vnd.inveniordm.v1+json") |>
    httr2::req_timeout(20) |>
    httr2::req_retry(max_tries = 2)

  if (is_non_empty_string(token)) {
    req <- httr2::req_auth_bearer_token(req, token)
  }

  req |>
    httr2::req_error(
      body = \(resp) {
        # Zenodo explains errors in JSON, but a gateway error page is HTML
        if (!grepl("json", httr2::resp_content_type(resp))) {
          return(NULL)
        }
        body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
        body$message %||% httr2::resp_status_desc(resp)
      }
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
}

#' @noRd
resolve_zenodo_token <- function(token = NULL, sandbox = FALSE) {
  token <- as.character(token %||% "")[1]
  if (is_non_empty_string(token)) {
    return(token)
  }

  env_vars <- if (isTRUE(sandbox)) {
    c("ZENODO_TOKEN_SANDBOX", "ZENODO_TOKEN")
  } else {
    "ZENODO_TOKEN"
  }
  for (var in env_vars) {
    env_token <- Sys.getenv(var, unset = "")
    if (nzchar(env_token)) {
      return(env_token)
    }
  }

  services <- if (isTRUE(sandbox)) {
    c("ZENODO_TOKEN_SANDBOX", "ZENODO_TOKEN")
  } else {
    "ZENODO_TOKEN"
  }
  for (service in services) {
    keyring_token <- tryCatch(
      keyring::key_get(service = service),
      error = function(...) NULL
    )
    if (is_non_empty_string(keyring_token)) {
      return(keyring_token)
    }
  }

  NULL
}

#' @noRd
parse_zenodo_id <- function(id) {
  # DOIs and host names are case-insensitive; DataCite writes DOIs in upper case
  id <- tolower(trimws(as.character(id)[1]))
  doi <- "^10\\.(?:5281|5072)/zenodo\\.(\\d+)$"
  doi_url <- "^https?://(?:dx\\.)?doi\\.org/10\\.(?:5281|5072)/zenodo\\.(\\d+)/?$"
  # Legacy record URLs use /record/ rather than /records/
  record_url <- "^https?://(?:www\\.)?zenodo\\.org/records?/(\\d+)/?(?:\\?.*)?(?:#.*)?$"
  sandbox_record_url <- "^https?://sandbox\\.zenodo\\.org/records?/(\\d+)/?(?:\\?.*)?(?:#.*)?$"
  record_id <- "^(\\d+)$"

  if (grepl(doi, id)) {
    return(sub(doi, "\\1", id))
  }
  if (grepl(doi_url, id)) {
    return(sub(doi_url, "\\1", id))
  }
  if (grepl(record_url, id)) {
    return(sub(record_url, "\\1", id))
  }
  if (grepl(sandbox_record_url, id)) {
    return(sub(sandbox_record_url, "\\1", id))
  }
  if (grepl(record_id, id)) {
    return(sub(record_id, "\\1", id))
  }

  cli_abort(c(
    "x" = "Could not parse a Zenodo record id from {.val {id}}."
  ))
}

#' @noRd
read_zenodo_download_files <- function(
  zenodo_record,
  token = NULL,
  sandbox = FALSE,
  timeout = 600
) {
  rec_url <- zenodo_record$links$self_html %||% ""
  files <- zenodo_record$files$entries

  if (length(files) == 0) {
    # Zenodo disables files on a metadata-only record; otherwise they are hidden from us.
    reason <- if (isFALSE(zenodo_record$files$enabled)) {
      "is a metadata-only record: it has no files."
    } else {
      "has no files you can access: they are restricted or embargoed."
    }
    auth_hint <- if (isTRUE(sandbox)) {
      "If you have access, pass {.arg token} or set {.envvar ZENODO_TOKEN_SANDBOX} (or {.envvar ZENODO_TOKEN})."
    } else {
      "If you have access, pass {.arg token} or set {.envvar ZENODO_TOKEN}."
    }
    cli_warn(c(
      "!" = "Zenodo record {.url {rec_url}} {reason}",
      "i" = if (!isFALSE(zenodo_record$files$enabled)) auth_hint,
      ">" = "A geolocator-dp object with only metadata is returned."
    ))
    return(NULL)
  }

  keys <- purrr::map_chr(files, "key")
  if (!"datapackage.json" %in% keys) {
    cli_abort(c(
      "!" = "Zenodo record {.url {rec_url}} does not contain {.file datapackage.json}.",
      "i" = "Check that this Zenodo record includes a valid geolocator {.file datapackage.json}."
    ))
  }

  # A unique folder per call, so that two downloads never share files
  download_dir <- tempfile("geolocator-zenodo-")
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  # Delete the folder if the download fails; read_zenodo() deletes it after reading.
  downloaded <- FALSE
  on.exit(if (!downloaded) unlink(download_dir, recursive = TRUE), add = TRUE)

  # Download the record files named in `wanted` and check them against their MD5
  download <- function(wanted) {
    entries <- files[keys %in% wanted]
    if (length(entries) == 0) {
      return()
    }
    local_paths <- file.path(download_dir, purrr::map_chr(entries, "key"))
    reqs <- purrr::map(entries, \(f) {
      url <- f$links$content
      if (is.null(url)) {
        url <- glue::glue("{zenodo_record$links$self}/files/{f$key}/content")
      }
      req <- httr2::request(url) |>
        httr2::req_user_agent(glue::glue("GeoLocatoR/{utils::packageVersion('GeoLocatoR')}")) |>
        httr2::req_timeout(timeout) |>
        httr2::req_retry(max_tries = 2) |>
        httr2::req_error(body = \(resp) {
          status <- httr2::resp_status(resp)
          glue::glue(
            "Failed to download file <{url}> from Zenodo record <{rec_url}> (HTTP {status})."
          )
        })
      if (is_non_empty_string(token)) {
        req <- httr2::req_auth_bearer_token(req, token)
      }
      req
    })

    resps <- httr2::req_perform_parallel(
      reqs = reqs,
      paths = local_paths,
      on_error = "continue",
      progress = FALSE,
      max_active = min(4L, length(reqs))
    )

    failed <- purrr::pmap_chr(list(entries, resps, local_paths), \(file, resp, path) {
      if (inherits(resp, "error")) {
        return(glue::glue("{file$key}: {conditionMessage(resp)}"))
      }
      # Zenodo gives `checksum` as "md5:<hex>"
      expected <- sub("^md5:", "", file$checksum)
      if (!identical(unname(tools::md5sum(path)), expected)) {
        return(glue::glue("{file$key}: its MD5 checksum does not match the record."))
      }
      NA_character_
    }) |>
      purrr::discard(is.na)

    if (length(failed) > 0) {
      cli::cli_abort(c(
        "x" = "Failed to download {length(failed)} file{?s} from Zenodo record {.url {rec_url}}.",
        "i" = glue::glue_collapse(failed, sep = "; ")
      ))
    }
  }

  # Only the files the package uses are downloaded, so the descriptor comes first.
  download("datapackage.json")
  descriptor <- jsonlite::read_json(file.path(download_dir, "datapackage.json"))
  paths <- purrr::map(descriptor$resources, "path") |>
    unlist() |>
    purrr::discard(\(p) grepl("^https?://", p))
  missing <- setdiff(paths, keys)
  if (length(missing) > 0) {
    cli_abort(c(
      "x" = "Zenodo record {.url {rec_url}} does not contain {.file {missing}}.",
      "i" = "{.file datapackage.json} refers to {cli::qty(missing)}{?this file/these files}."
    ))
  }
  download(c(paths, intersect("params.json", keys)))

  downloaded <- TRUE
  download_dir
}

#' @noRd
read_zenodo_attach_metadata <- function(pkg, zenodo_record) {
  z <- zenodo_record
  m <- z$metadata %||% list()
  doi <- z$doi %||% purrr::pluck(z, "pids", "doi", "identifier", .default = NULL)
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", as.character(doi)[1], ignore.case = TRUE)
  pkg[["id"]] <- if (!is.na(doi) && nzchar(doi)) {
    as.character(glue::glue("https://doi.org/{doi}"))
  } else {
    NULL
  }
  concept_id <- as.character(purrr::pluck(z, "parent", "id", .default = NA_character_))[1]
  concept_doi <- if (!is.na(concept_id) && nzchar(concept_id) && !is.na(doi) && nzchar(doi)) {
    sub("[0-9]+$", concept_id, doi)
  } else {
    NA_character_
  }
  pkg[["conceptid"]] <- if (!is.na(concept_doi) && nzchar(concept_doi)) {
    as.character(glue::glue("https://doi.org/{concept_doi}"))
  } else {
    NULL
  }
  pkg[["name"]] <- z$id
  pkg[["title"]] <- m$title
  pkg[["description"]] <- m$description
  pkg[["version"]] <- m$version
  pkg[["created"]] <- z$created
  pkg[["homepage"]] <- z$links$self_html %||% NULL
  # Absent values are dropped rather than written as JSON null or empty arrays
  licenses <- purrr::map(m$rights %||% list(), \(x) {
    purrr::compact(list(
      name = x$id,
      title = x$title$en %||% NULL,
      path = x$props$url %||% NULL
    ))
  })
  pkg[["licenses"]] <- if (length(licenses) > 0) licenses
  pkg[["contributors"]] <- purrr::map(m$creators %||% list(), \(creator) {
    orcid <- creator$person_or_org$identifiers |>
      purrr::keep(~ .x$scheme == "orcid") |>
      purrr::pluck(1, "identifier", .default = NULL)

    purrr::compact(list(
      title = creator$person_or_org$name,
      givenName = creator$person_or_org$given_name,
      familyName = creator$person_or_org$family_name,
      path = if (is.null(orcid)) NULL else glue::glue("https://orcid.org/{orcid}"),
      # email = NULL,
      # A list, so that a single role is written as a JSON array
      roles = as.list(creator$role$id),
      organization = if (length(creator$affiliations) > 0) {
        as.character(glue::glue_collapse(
          purrr::map_chr(creator$affiliations, "name"),
          sep = ", "
        ))
      }
    ))
  })
  pkg[["relatedIdentifiers"]] <- purrr::map(m$related_identifiers %||% list(), \(x) {
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
  pkg[["access_status"]] <- if (nzchar(access_status)) access_status else NULL
  pkg[["embargo"]] <- if (
    identical(pkg$access_status, "embargoed") || isTRUE(z$access$embargo$active)
  ) {
    z$access$embargo$until %||% NULL
  } else {
    NULL
  }
  # Lists, so that a single value is written as a JSON array
  grants <- purrr::compact(purrr::map(m$funding %||% list(), \(x) x$funder$name))
  pkg[["grants"]] <- if (length(grants) > 0) grants
  keywords <- purrr::compact(purrr::map(m$subjects %||% list(), \(x) x$subject))
  pkg[["keywords"]] <- if (length(keywords) > 0) keywords
  pkg[["codeRepository"]] <- z$custom_fields$`code:codeRepository` %||% NULL
  pkg[["status"]] <- z$status
  pkg[["record_type"]] <- m$resource_type$id %||% NULL
  pkg[["publisher"]] <- m$publisher
  pkg[["communities"]] <- z$parent$communities$ids %||%
    purrr::pluck(z, "parent", "review", "receiver", "community", .default = NULL)
  pkg[["is_published"]] <- z$is_published
  pkg[["is_draft"]] <- z$is_draft
  pkg
}
