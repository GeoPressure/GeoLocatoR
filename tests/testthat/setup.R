# This file runs once before all tests
# It creates shared pkg objects that can be used across all test files

# Read the shared package once, keeping the error when offline
pkg_shared <- tryCatch(
  suppressMessages(read_zenodo("17367320")),
  error = identity
)

# Shared package, skipped offline
shared_pkg <- function() {
  skip_if_offline("zenodo.org")
  if (inherits(pkg_shared, "error")) {
    stop(pkg_shared)
  }
  pkg_shared
}
