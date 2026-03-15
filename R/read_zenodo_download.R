#' @noRd
read_zenodo_download <- function(files, token = NULL, rec_url) {
  download_dir <- file.path(
    tempdir(),
    glue::glue("geolocator-zenodo-{as.integer(Sys.time())}")
  )
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  local_paths <- file.path(download_dir, vapply(files, `[[`, character(1), "filename"))

  reqs <- purrr::map(files, \(f) {
    req <- httr2::request(f$download) |>
      httr2::req_user_agent(glue::glue("GeoLocatoR/{utils::packageVersion('GeoLocatoR')}")) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 2)
    if (is_non_empty_string(token)) {
      req <- httr2::req_auth_bearer_token(req, token)
    }
    req
  })

  max_parallel <- min(4L, length(reqs))
  cli_progress_step(
    "Download {length(reqs)} file{?s} from Zenodo record {.url {rec_url}} ({max_parallel} concurrent)"
  )

  resps <- httr2::req_perform_parallel(
    reqs = reqs,
    paths = local_paths,
    on_error = "return",
    progress = TRUE,
    max_active = max_parallel
  )

  failed <- which(vapply(resps, inherits, logical(1), "error"))
  if (length(failed) > 0) {
    failed_files <- vapply(
      files[failed],
      \(f) {
        f$filename %||% f$key %||% "<unknown>"
      },
      character(1)
    )
    failed_msg <- vapply(resps[failed], conditionMessage, character(1))

    cli_abort(c(
      "x" = "Failed to download {length(failed)} file{?s} from Zenodo record {.url {rec_url}}.",
      "i" = "File{?s}: {.val {paste(failed_files, collapse = ', ')}}.",
      ">" = paste(failed_msg, collapse = "\n")
    ))
  }

  download_dir
}
