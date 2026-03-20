#' Read a GeoLocator Data Package from Zenodo
#'
#' Retrieve a GeoLocator Data Package from a Zenodo record identifier.
#'
#' @details
#' Workflow:
#'
#' 1. Resolve and fetch Zenodo record metadata from the REST API.
#' 2. Validate record files and download them to a temporary directory.
#' 3. Read `datapackage.json` with [read_gldp()] (or create an empty shell when
#'    files are not accessible).
#' 4. Attach Zenodo metadata and recompute derived package fields with
#'    [update_gldp()].
#'
#' Behavior notes:
#'
#' - If files are not visible/downloadable (for example restricted record access),
#'   the function returns a metadata-only package created with [create_gldp()]
#'   and emits a warning.
#' - If the record does not contain `datapackage.json`, the function aborts.
#' - If one or more file downloads fail, the function aborts with download
#'   details.
#'
#' Downloaded files are written under `tempdir()` in a run-specific folder. This
#' directory is temporary and may be removed by the operating system after the
#' R session.
#'
#' @param id Zenodo identifier. Can be a record id, DOI, DOI URL, or Zenodo
#'   record URL. Supported examples include `"18467383"`,
#'   `"10.5281/zenodo.18467383"`, `"https://doi.org/10.5281/zenodo.18467383"`,
#'   `"10.5072/zenodo.470406"`, `"https://doi.org/10.5072/zenodo.470406"`,
#'   `"https://zenodo.org/records/18467383"`, and sandbox record URLs when
#'   `sandbox = TRUE`.
#' @param token Optional Zenodo token. If `NULL`, token resolution follows the
#'   same logic as Zenodo request helpers: explicit argument, then environment
#'   variable `ZENODO_TOKEN_SANDBOX` when `sandbox = TRUE` (then
#'   `ZENODO_TOKEN` as fallback), then keyring secret services
#'   `"ZENODO_TOKEN_SANDBOX"` / `"ZENODO_TOKEN"` when available (for example
#'   `keyring::key_set(service = "ZENODO_TOKEN_SANDBOX")`).
#' @param draft Logical. If `TRUE`, query the draft record endpoint
#'   (`/api/records/{id}/draft`). Draft access usually requires authentication
#'   and sufficient permissions.
#' @param sandbox Logical. If `TRUE`, query
#'   `"https://sandbox.zenodo.org"`; otherwise query `"https://zenodo.org"`.
#'
#' @return A `geolocatordp` object.
#'   When downloads succeed, resources are loaded from the Zenodo
#'   `datapackage.json`. When downloads are unavailable, an empty package shell
#'   is returned with Zenodo metadata fields attached.
#'
#' @examples
#' \dontrun{
#' pkg <- read_zenodo("18467383")
#'
#' # Resolve from DOI URL
#' pkg <- read_zenodo("https://doi.org/10.5281/zenodo.18467383")
#'
#' # Access a draft in Zenodo Sandbox (token usually required)
#' pkg <- read_zenodo("470406", sandbox = TRUE, draft = TRUE, token = "<TOKEN>")
#' }
#' @export
read_zenodo <- function(
  id,
  token = NULL,
  draft = FALSE,
  sandbox = FALSE
) {
  token <- resolve_zenodo_token(token = token, sandbox = sandbox)

  # 1) Fetch Zenodo record
  cli_progress_step("Retrieve Zenodo record")
  zenodo_record <- read_zenodo_fetch_record(
    id,
    token = token,
    draft = draft,
    sandbox = sandbox
  )

  # 2) Validate and download record files
  cli_progress_step("Download files from Zenodo")
  download_dir <- read_zenodo_download_files(
    zenodo_record = zenodo_record,
    token = token,
    sandbox = sandbox
  )

  # 3) Read downloaded datapackage
  if (!is.null(download_dir)) {
    cli_progress_step("Read and upgrade geolocator dp")
    pkg <- read_gldp(file.path(download_dir, "datapackage.json"))
  } else {
    pkg <- create_gldp()
  }

  # 4) Attach Zenodo metadata and update derived fields
  cli_progress_step("Add metadata")
  pkg <- read_zenodo_attach_metadata(pkg = pkg, zenodo_record = zenodo_record)
  pkg <- update_gldp(pkg)

  cli_progress_done()
  pkg
}
