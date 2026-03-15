#' Read a GeoLocator Data Package from Zenodo
#'
#' Fetches a Zenodo record, downloads all attached files to a temporary
#' directory, reads `datapackage.json`, upgrades the package if needed, and
#' enriches `pkg$meta` with Zenodo metadata.
#'
#' @param id Zenodo identifier. Can be a record id, DOI, DOI URL, or Zenodo
#'   record URL.
#' @param endpoint Zenodo endpoint used to fetch record metadata. Default is
#'   `"record"`.
#' @param token Optional Zenodo token. If `NULL`, token resolution follows the
#'   same logic as Zenodo request helpers: explicit argument, then `ZENODO_TOKEN`,
#'   then keyring service `ZENODO_TOKEN` when available.
#' @param sandbox Logical. If `TRUE`, query
#'   `"https://sandbox.zenodo.org"`; otherwise query `"https://zenodo.org"`.
#'
#' @return A `geolocatordp` object with Zenodo metadata attached in `pkg$meta`.
#'
#' @examples
#' \dontrun{
#' pkg <- read_zenodo("18467383")
#' }
#' @export
read_zenodo <- function(
  id,
  endpoint = "record",
  token = NULL,
  sandbox = FALSE
) {
  on.exit(cli_progress_done(), add = TRUE)
  token <- get_zenodo_token(token)

  # 1) Fetch Zenodo record
  cli_progress_step("Retrieve Zenodo record")
  zenodo_record <- read_zenodo_get(
    id,
    endpoint = endpoint,
    token = token,
    sandbox = sandbox
  )

  # 2) Download every record file to a dedicated temporary directory
  rec_url <- zenodo_record$links$self_html
  if (length(zenodo_record$files) == 0) {
    if (!is_non_empty_string(token)) {
      cli_abort(c(
        "x" = "Zenodo record {.url {rec_url}} has no downloadable files visible to anonymous access.",
        "i" = "If the record is restricted, pass {.arg token}, set {.envvar ZENODO_TOKEN}, or store a key with {.code keyring::key_set(service = 'ZENODO_TOKEN')}."
      ))
    } else {
      cli_abort(c(
        "x" = "Zenodo record {.url {rec_url}} has no downloadable files for the current token.",
        "i" = "Check that your token has access to this record."
      ))
    }
  }

  dp_file <- purrr::detect(zenodo_record$files, \(f) {
    identical(f$filename, "datapackage.json")
  })
  if (is.null(dp_file)) {
    cli_abort(c(
      "x" = "Zenodo record {.url {rec_url}} does not contain {.file datapackage.json}."
    ))
  }

  download_dir <- read_zenodo_download(
    files = zenodo_record$files,
    token = token,
    rec_url = rec_url
  )

  # 3) Read downloaded datapackage with the local reader
  cli_progress_step("Read and upgrade downloaded {.file datapackage.json}")
  pkg <- read_gldp(file.path(download_dir, dp_file$filename))

  # 4) Add zenodo metadata
  pkg <- read_zenodo_metadata(pkg = pkg, zenodo_record = zenodo_record)

  # Update computed value
  pkg <- update_gldp(pkg)

  pkg
}
