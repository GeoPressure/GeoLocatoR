#' @noRd
read_zenodo_download_files <- function(zenodo_record, token = NULL, sandbox = FALSE) {
  token <- resolve_zenodo_token(token, sandbox = sandbox)
  rec_url <- zenodo_record$links$self_html %||% ""
  files <- zenodo_record$files$entries

  if (length(files) == 0) {
    auth_hint <- if (isTRUE(sandbox)) {
      "If restricted, pass {.arg token} or set {.envvar ZENODO_TOKEN_SANDBOX} (or {.envvar ZENODO_TOKEN})."
    } else {
      "If restricted, pass {.arg token} or set {.envvar ZENODO_TOKEN}."
    }
    cli_warn(c(
      "!" = "Zenodo record {.url {rec_url}} has no downloadable files.",
      "i" = auth_hint,
      ">" = "A geolocator-dp object with only metadata is returned."
    ))
    return(NULL)
  }

  has_datapackage <- any(vapply(
    files,
    \(f) identical(f$filename %||% f$key %||% "", "datapackage.json"),
    logical(1)
  ))
  if (!has_datapackage) {
    cli_abort(c(
      "!" = "Zenodo record {.url {rec_url}} does not contain {.file datapackage.json}.",
      "i" = "Check that this Zenodo record includes a valid geolocator {.file datapackage.json}."
    ))
  }

  download_dir <- file.path(
    tempdir(),
    glue::glue("geolocator-zenodo-{as.integer(Sys.time())}")
  )
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  local_paths <- file.path(download_dir, purrr::map_chr(files, "key"))

  reqs <- purrr::map(files, \(f) {
    url <- f$links$content
    if (is.null(url)) {
      url <- glue::glue(("{zenodo_record$links$self}/files/{f$key}/content"))
    }
    req <- httr2::request(url) |>
      httr2::req_user_agent(glue::glue("GeoLocatoR/{utils::packageVersion('GeoLocatoR')}")) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_error(body = \(resp) {
        status <- httr2::resp_status(resp)
        glue::glue(
          "Failed to download file {.url {url}} from Zenodo record <{rec_url}> (HTTP {status})."
        )
      })
    if (is_non_empty_string(token)) {
      req <- httr2::req_auth_bearer_token(req, token)
    }
    req
  })

  max_parallel <- min(4L, length(reqs))
  resps <- httr2::req_perform_parallel(
    reqs = reqs,
    paths = local_paths,
    on_error = "return",
    progress = FALSE,
    max_active = max_parallel
  )

  failed <- purrr::map2_chr(files, resps, \(file, resp) {
    if (!inherits(resp, "error")) {
      return(NA_character_)
    }
    glue::glue(
      "{file$filename %||% file$key %||% '<unknown>'}: {conditionMessage(resp)}"
    )
  }) |>
    purrr::discard(is.na)

  if (length(failed) > 0) {
    cli::cli_abort(c(
      "x" = "Failed to download {length(failed)} file{?s} from Zenodo record {.url {rec_url}}.",
      "i" = glue::glue_collapse(failed, sep = "; ")
    ))
  }

  download_dir
}
