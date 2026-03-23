# This file runs once before all tests
# It creates shared pkg objects that can be used across all test files

# Download and save the first package once
suppressMessages({
  pkg_shared <- read_zenodo("17367320")
})
