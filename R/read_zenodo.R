#' Read a GeoLocator Data Package from Zenodo
#'
#' @description
#' Retrieve a GeoLocator Data Package from a Zenodo record identifier:
#'
#' 1. Resolve and fetch Zenodo record metadata from the REST API.
#' 2. Validate record files and download them to a temporary directory.
#' 3. Read `datapackage.json` with [read_gldp()].
#' 4. Attach Zenodo metadata and recompute derived package properties with
#'    [update_gldp()].
#'
#' @details
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
#'   and `"https://zenodo.org/records/18467383"`. For Zenodo Sandbox records,
#'   set `sandbox = TRUE`.
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
  quiet = FALSE
) {
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

  # 2) Validate and download record files
  if (!quiet) {
    cli_progress_step("Download files from Zenodo")
  }
  download_dir <- read_zenodo_download_files(
    zenodo_record = zenodo_record,
    token = token,
    sandbox = sandbox
  )

  # 3) Read downloaded datapackage
  if (!is.null(download_dir)) {
    if (!quiet) {
      cli_progress_step("Read and upgrade GeoLocator-DP")
    }
    pkg <- read_gldp(file.path(download_dir, "datapackage.json"))
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
