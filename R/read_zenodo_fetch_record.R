#' @noRd
read_zenodo_fetch_record <- function(
  id,
  token = NULL,
  draft = FALSE,
  sandbox = FALSE
) {
  record_id <- parse_zenodo_id(id)
  token <- resolve_zenodo_token(token, sandbox = sandbox)
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
  id <- trimws(as.character(id)[1])
  doi <- "^10\\.(?:5281|5072)/zenodo\\.(\\d+)$"
  doi_url <- "^https?://doi\\.org/10\\.(?:5281|5072)/zenodo\\.(\\d+)$"
  record_url <- "^https?://zenodo\\.org/records/(\\d+)/?(?:\\?.*)?(?:#.*)?$"
  sandbox_record_url <- "^https?://sandbox\\.zenodo\\.org/records/(\\d+)/?(?:\\?.*)?(?:#.*)?$"
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
