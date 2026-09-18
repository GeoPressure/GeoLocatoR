# AGENTS.md

## Scope
- Applies to the whole repository.

## Default behavior
- Do not add validation for internal functions.
- Only add checks at clear external boundaries (I/O, user input) when necessary.
- Do not add defensive guards/fallback logic unless explicitly asked or required for external I/O.
- Ask or inform on potential fallback logic, but don't add them by default
- Warn me in case of breaking back compatibility. I generally only care about released/version back-compabtility not during develeopement.
- Do not run tests unless explicitly asked.
- Do not run `devtools::document()` or regenerate roxygen outputs unless explicitly asked.
- Do not use `:::` or `::` in tests.

Comments
- Add minimal comments per logical code section (roughly every 1–10 lines).
- Comments should describe the purpose of the section, not obvious R behavior.
- Explicitly comment deliberate design choices when they are non-obvious and explain the reason.
- Do not add redundant or tutorial-style comments.

## Code constraints
- Keep code compact and direct; avoid verbose patterns.
- Do not define a function used only once; inline it.
- Introduce helpers only when they clearly remove real duplication.
- Prefer `glue::glue()` / `glue::glue_collapse()` over `paste()` / `paste0()`.
- Prefer `purrr` for mapping and list transformations.
- Do not introduce new dependencies unless explicitly approved.

## File organisation
- Name each file in `R/` after the concern it covers, not after a single export:
  `read_gldp.R`, `validate_gldp_schema.R`, `gldp_read_resource.R`.
- Internal `@noRd` helpers live in a file named for their concern, exactly as
  exported functions do. `validate_gldp_schema.R` is entirely internal.
- Keep `zzz.R` for package-level hooks (`.onLoad()`, `.onAttach()`,
  `globalVariables()`) and small generic utilities. It is not a home for
  whatever has no file yet: once a group of related helpers has a subject of
  its own, give it a file.
- Keep a function next to the helpers it calls; do not insert an unrelated
  concern between them.
- Preserve the testthat pairing: `tests/testthat/test-X.R` tests `R/X.R`.
- Moving code between files is a move, not an edit. Make the source file
  byte-identical to before the code was added, and verify with
  `git diff <sha> -- <file>` rather than by eye.

## geolocatordp conventions
- Main object class: `geolocatordp` (extends `datapackage`).
- Prefer `pkg[["field"]] <- value` over `pkg$field <- value` when mutating package objects.
- Run `update_gldp(pkg)` after metadata changes that affect derived fields.

## Docs and generated files
- Roxygen only for exported/user-facing functions; internal helpers should be `@noRd`.
- For exported functions include at minimum: title, `@description`, all `@param`, and `@return`.
- Keep docs behavior-oriented and concise.
- Wrap network/auth-dependent examples in `\\dontrun{}`.
- Edit `README.Rmd`, not `README.md`.
- Do not manually edit generated files (`NAMESPACE`, `man/*.Rd`).

## Development, PR, and release workflow
- Develop on `dev`: make focused commits, run the mandatory checks, and push to
  `origin/dev`.
- Open pull requests from `dev` to `main`. Merge `main` into `dev` and resolve
  conflicts before merging the pull request.
- Use a draft pull request while release metadata or release checks are
  incomplete. Mark it ready only when the final version, NEWS entry, and
  required GitHub checks are complete.

### Choosing the version number
- `X.Y.Z` is judged by what a user must do to take the release, not by how
  large the diff is.
- Patch (`Z`): fixes and internal changes only. Safe to take blindly — the same
  call on the same input behaves as before.
- Minor (`Y`): new features, or any behaviour a user can notice. A stricter
  `validate_gldp()`, a different GeoLocator-DP version written by
  `create_gldp()`, or a changed return value are minor, never patch.
- Major (`X`): an exported function is removed or changes signature, or
  packages that used to be readable no longer are.
- Say in the release block when behaviour a user relied on has changed, even
  though the bump is only minor.
- After a release, bump `DESCRIPTION` on `dev` to the next `X.Y.Z.9000`.

### Canonical release block
- For every release, write one Markdown release block first. It is the source
  of truth and must be copied verbatim to the new top section of `NEWS.md`, the
  pull-request body, and the GitHub Release body for the version tag.
- Do not shorten, paraphrase, reorder, or add items independently in any of
  those three places.
- Use `vX.Y.Z` as the pull-request title and `# GeoLocatoR vX.Y.Z` as the top
  NEWS heading. Keep headings, subsection headings, bullets, Markdown links,
  and the full-changelog link identical in all copies.
- Link every release-note bullet to the specific commit or commits that support
  it. Use multiple inline links when one bullet summarises several changes.

```md
# GeoLocatoR vX.Y.Z

## Main

- [Describe the principal user-facing change](https://github.com/GeoPressure/GeoLocatoR/commit/<sha>).

## Minor

- [Describe a smaller change or fix](https://github.com/GeoPressure/GeoLocatoR/commit/<sha>).

**Full Changelog**: <https://github.com/GeoPressure/GeoLocatoR/compare/vX.Y.(Z-1)...vX.Y.Z>
```

### Release checklist
- Set `DESCRIPTION` and `CITATION.cff` to the final `X.Y.Z` version; do not
  merge a release with `.9000`.
- Add the canonical release block to `NEWS.md` before opening the pull request,
  then paste that exact block into the pull-request body.
- Resolve all `R CMD check` warnings and release-relevant notes, and confirm
  the pull request's GitHub Actions matrix is green.
- After merging to `main`, create tag `vX.Y.Z` and paste the unchanged
  canonical release block into the GitHub Release description.
- Refresh `inst/schemas/` with `sync_gldp_schemas()` against a released
  GeoLocator-DP tag, never by copying from a branch or a working tree. A sync
  that changes nothing is the proof the bundle matches the release.
- Edit `CITATION.cff` by hand for the version line. `cffr::cff_write()`
  regenerates the whole file and drops its dependency references.

## Checks and format
- Format code with `air format . --check`
- Check lint with: `jarl check .`

## Uncertainty
- If required information is missing or ambiguous, ask for clarification instead of guessing.

## When modifying existing code
- Make the smallest possible patch.
- Do not refactor unrelated code.
- Do not reorder code or change formatting unless required.

## Side effects
- Do not introduce hidden side effects (options, global state, working directory).
