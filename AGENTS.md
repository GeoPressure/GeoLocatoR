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