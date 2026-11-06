# This file runs once before all tests
# It creates shared pkg objects that can be used across all test files

# Trimmed local copy of Zenodo record 17367320: the first two days of
# measurements and pressurepaths of each tag, all other rows kept.
pkg_shared <- suppressMessages(read_gldp(test_path("fixtures", "zenodo-17367320")))

# Shared package
shared_pkg <- function() {
  pkg_shared
}
