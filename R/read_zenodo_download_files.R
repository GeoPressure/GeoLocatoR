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
        url <- glue::glue(("{zenodo_record$links$self}/files/{f$key}/content"))
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
