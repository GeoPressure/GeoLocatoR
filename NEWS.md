# GeoLocatoR 1.1.1

- Added a configurable timeout for Zenodo downloads and excluded discarded
  pressure-path altitudes from exported paths.
- Preserved measurement labels, magnetic acceleration axes, and saved
  GeoPressureR parameters when converting a data package to a tag.
- Improved GeoPressureTemplate exports by writing tabular sensor data,
  preserving supported saved parameters, omitting derived defaults, and using
  compact YAML vectors.

# GeoLocatoR 1.1.0

- Added support for frictionless 1.3.0 and migrated to its `resource_names()`
  API. GeoLocator-DP profile-version warnings are suppressed while the package
  remains on Data Package v1. Compatibility with frictionless 2.0.0 will be
  addressed separately.
- Simplified internal resource updates by using frictionless's add-or-replace
  behaviour.

# GeoLocatoR 1.0.2

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/ae8532f...cbc8819) · [PR #33](https://github.com/GeoPressure/GeoLocatoR/pull/33)

- Improved `read_gldp()` diagnostics for unreadable resources and added support
  for passing a package directory.
- Improved Darwin Core export by using most-likely paths for exported locations,
  rounding elevations, and completing occurrence and event fields.
- `read_soi()` now fails early when duplicate `GDL_ID` values are encountered.
- Resolved package-check notes and tidyselect deprecation warnings.

# GeoLocatoR 1.0.1

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/fcf7bb4...ae8532f) · [PR #31](https://github.com/GeoPressure/GeoLocatoR/pull/31)

- Added `status_geopressuretemplate()` to inspect GeoPressureTemplate projects.
- `read_geopressuretemplate()` now retains `ring_number` and `tag_comments`
  from `config.yml`.
- Improved GeoPressureTemplate tag creation, including manufacturer defaults and
  movement-graph handling.
- Added experimental lifecycle labels and require GeoPressureR 3.5.4 or later.
- Made cloning GeoPressureTemplate repositories more reliable by quoting clone
  arguments and reporting clone errors.

# GeoLocatoR 1.0.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/v0.5...v1.0) · [PR #29](https://github.com/GeoPressure/GeoLocatoR/pull/29)

- Released the first stable 1.0 version of GeoLocatoR and moved project URLs to
  the GeoPressure organisation and domain.
- Updated GeoLocator Data Package schema URLs to the GeoPressure location.
- Added `select_gldp()` and helpers to remove measurements from packages.
- Refined GLDP coherence checks and added clearer validation warnings.
- Improved SOI import helpers, package merging, schema handling, and resource
  parsing diagnostics.
- Bundled local GeoLocator Data Package schemas for reproducible validation.
- Reworked `merge_gldp()` to accept a list of packages and normalise parameters
  before merging.
- Added `write_gldp()` and `update_gldp_order_resources()`.
- Added `normalize_gldp_params()` and improved resource-schema upgrades.
- Removed deprecated path, stap, and edge fields; renamed `pitch` to
  `mean_acceleration_z`.

## Breaking changes

- Renamed `add_gldp_geopressuretemplate()` to `read_geopressuretemplate()` and
  `create_gldp_geopressuretemplate()` to `create_geopressuretemplate()`.
- Renamed `add_gldp_soi()` to `read_soi()` and `read_gdl()` to `read_soi_gld()`.
- Renamed `zenodo_to_gldp()` to `read_zenodo()` and stopped accepting Zenodo
  records directly in `read_gldp()`.
- Renamed `config2tibble()` to `config_to_tibble()`, `version()` to
  `gldp_version()`, and several raw-tag and contributor conversion helpers.
- Replaced the former Zenodo export workflow and removed `gldp_to_zenodo()`.

# GeoLocatoR 0.5.1

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/v0.5...af4141e) · [PR #24](https://github.com/GeoPressure/GeoLocatoR/pull/24)

- Corrected GeoLocator Data Package validation and parameter-to-observation
  conversion for the current schema.

# GeoLocatoR 0.5.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/v0.4...v0.5) · [PR #21](https://github.com/GeoPressure/GeoLocatoR/pull/21)

- Improved package metadata cleanup, particularly contributor values, paths,
  licences, identifiers, and citations.
- Strengthened validation with local references, `oneOf` support, and concept
  DOI checks.
- Improved tag merging and normalised enum values while importing Zenodo data.
- Improved GeoPressureTemplate datetime handling and removed missing values from
  measurements where appropriate.

# GeoLocatoR 0.4.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/40ba0d3...v0.4) · [PR #20](https://github.com/GeoPressure/GeoLocatoR/pull/20)

- Updated package metadata and citations for GeoLocator Data Package v0.4.
- Added configuration parsing improvements and clearer errors for missing
  package components.

# GeoLocatoR 0.2.10

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/4fc8957...40ba0d3) · [PR #19](https://github.com/GeoPressure/GeoLocatoR/pull/19)

- Refined licence selection while creating GeoPressureTemplate projects.
- Corrected type extraction during import.

# GeoLocatoR 0.2.9

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/90d7844...4fc8957) · [Export commit](https://github.com/GeoPressure/GeoLocatoR/commit/4fc8957)

- Added `gldp_to_dwc()` and `gldp_to_eml()` to export packages as Darwin Core
  and Ecological Metadata Language.
- Improved GeoPressureTemplate project path handling and pkgdown export
  documentation.

# GeoLocatoR 0.2.8

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/1cf0871...90d7844) · [PR #17](https://github.com/GeoPressure/GeoLocatoR/pull/17)

- Added `config_to_tibble()` for converting GeoPressureTemplate `config.yml`
  files to tibbles.
- Added `gldp_to_tag()` to convert package data to GeoPressureR tag objects.
- Removed the `gert` dependency and improved Git clone handling.
- Extended `read_gldp()` to support Zenodo inputs and made raw-tag processing
  conditional on new tag data.
- Replaced the lintr workflow with air and jarl checks.

# GeoLocatoR 0.2.7

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/cb53780...1cf0871) · [PR #13](https://github.com/GeoPressure/GeoLocatoR/pull/13)

- Improved package validation, taxonomy selection, GeoPressureTemplate imports,
  and resource-overwrite prompts.
- Added map and most-likely-path plotting methods.
- Improved handling of missing sex values, tag reads, and pressure-path data.
- Adopted the GPL licence.

# GeoLocatoR 0.2.6

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/5a65726...cb53780) · [URL fix](https://github.com/GeoPressure/GeoLocatoR/commit/cb53780)

- Corrected package URLs and taxonomic metadata handling.

# GeoLocatoR 0.2.5

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/3b5e158...5a65726) · [PR #12](https://github.com/GeoPressure/GeoLocatoR/pull/12)

- Renamed `params2*` conversion helpers to `params_to_*`.
- Improved `create_gldp()` validation, resource type and format validation, and
  package messages.
- Updated documentation, citations, tests, and code formatting.

# GeoLocatoR 0.2.4

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/dffe123...3b5e158) · [PR #11](https://github.com/GeoPressure/GeoLocatoR/pull/11)

- Improved GeoPressureTemplate import, package creation, SOI import defaults,
  and pressure-path compatibility.

# GeoLocatoR 0.2.3

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/fc12fd7...dffe123) · [PR #9](https://github.com/GeoPressure/GeoLocatoR/pull/9)

- Added `merge_gldp()` for combining GeoLocator Data Packages.
- Improved metadata update helpers, licence display, and
  `create_geopressuretemplate()` labels.

# GeoLocatoR 0.2.2

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/d3a620c...fc12fd7) · [PR #7](https://github.com/GeoPressure/GeoLocatoR/pull/7)

- Added clearer errors for missing GeoPressureTemplate files and missing tag or
  parameter data.
- Improved handling of paths containing spaces and duplicate measurements.

# GeoLocatoR 0.2.1

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/92f4a6a...d3a620c) · [PR #6](https://github.com/GeoPressure/GeoLocatoR/pull/6)

- Improved the GeoPressureTemplate writing workflow and package test coverage.

# GeoLocatoR 0.2.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/74734e2...92f4a6a) · [PR #5](https://github.com/GeoPressure/GeoLocatoR/pull/5)

- Added Zenodo import and metadata conversion helpers.
- Added SOI import support for GDL files and database data.
- Added package-resource validation, computed-property updates, and
  bibliographic-citation updates.
- Improved package printing, resource loading, and handling of empty resources,
  twilights, and staps.

# GeoLocatoR 0.1.1

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/c33d28e...74734e2) · [Pkgdown setup](https://github.com/GeoPressure/GeoLocatoR/commit/f965d9c)

- Added pkgdown site configuration and removed the package vignette.
- Improved raw-data GeoPressureTemplate imports and handling of missing package
  descriptions.

# GeoLocatoR 0.1.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/5e426dd...c33d28e) · [Version commit](https://github.com/GeoPressure/GeoLocatoR/commit/aa2d84a)

- Added plotting for `geolocatordp` objects and pressure-path resources.
- Added GeoPressureTemplate project creation, including `config.yml` creation
  and optional opening of the new RStudio project.
- Added package coherence checks, schema-based metadata ordering, and resource
  type casting.

# GeoLocatoR 0.0.0.9000

[Initial development history](https://github.com/GeoPressure/GeoLocatoR/commits/5e426dd)

- Initial development release of GeoLocatoR.
