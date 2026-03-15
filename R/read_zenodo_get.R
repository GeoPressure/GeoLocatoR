#' @noRd
read_zenodo_get <- function(
  id,
  endpoint = c("record", "deposit"),
  token = NULL,
  sandbox = FALSE,
  logger = NULL
) {
  endpoint <- match.arg(endpoint)
  token <- get_zenodo_token(token)

  if (identical(endpoint, "deposit") && !is_non_empty_string(token)) {
    cli_warn(c(
      "!" = "Missing Zenodo token for {.val deposit} endpoint.",
      "i" = "Pass {.arg token}, set {.envvar ZENODO_TOKEN}, or store one with {.code keyring::key_set(service = 'ZENODO_TOKEN')}."
    ))
    return(NULL)
  }

  url <- if (isTRUE(sandbox)) {
    "https://sandbox.zenodo.org/api"
  } else {
    "https://zenodo.org/api"
  }

  zen <- zen4R::ZenodoManager$new(
    url = url,
    token = token,
    logger = logger
  )

  record_id <- parse_zenodo_id(id)

  switch(
    endpoint,
    record = zen$getRecordById(record_id),
    deposit = zen$getDepositionById(record_id)
  )
}

#' @noRd
get_zenodo_token <- function(token = NULL) {
  # 1) explicit function argument
  token <- as.character(token %||% NA_character_)[1]
  if (is_non_empty_string(token)) {
    return(token)
  }

  # 2) environment variables (session or persistent .Renviron)
  env_token <- Sys.getenv("ZENODO_TOKEN", unset = "")
  if (nzchar(env_token)) {
    return(env_token)
  }

  # 3) keyring secret store (if available)
  if (interactive() && requireNamespace("keyring", quietly = TRUE)) {
    keyring_token <- tryCatch(
      keyring::key_get(service = "ZENODO_TOKEN"),
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

  doi <- "^10\\.5281/zenodo\\.(\\d+)$"
  doi_url <- "^https?://doi\\.org/10\\.5281/zenodo\\.(\\d+)$"
  record_url <- "^https?://zenodo\\.org/records/(\\d+)"
  sandbox_record_url <- "^https?://sandbox\\.zenodo\\.org/records/(\\d+)"
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
