# GeoLocatoR v1.1.1

## Main

- [Preserved measurement labels](https://github.com/GeoPressure/GeoLocatoR/commit/811c771), [magnetic acceleration axes](https://github.com/GeoPressure/GeoLocatoR/commit/a26ba4b), and [saved GeoPressureR parameters](https://github.com/GeoPressure/GeoLocatoR/commit/fb93f91) when converting a data package to a tag.
- [Improved GeoPressureTemplate exports](https://github.com/GeoPressure/GeoLocatoR/commit/4b03e6e) by writing tabular sensor data, preserving supported saved parameters, omitting derived defaults, and using compact YAML vectors.

## Minor

- [Added a configurable timeout for Zenodo downloads](https://github.com/GeoPressure/GeoLocatoR/commit/6abb643) and [excluded discarded pressure-path altitudes from exported paths](https://github.com/GeoPressure/GeoLocatoR/commit/d8c8fd2).

**Full Changelog**: <https://github.com/GeoPressure/GeoLocatoR/compare/v1.1.0...v1.1.1>

# GeoLocatoR 1.1.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/cbc8819...7f1e98d) · [PR #34](https://github.com/GeoPressure/GeoLocatoR/pull/34)

- [Migrated from `frictionless::resources()` to `resource_names()`](https://github.com/GeoPressure/GeoLocatoR/commit/f56f55b) and [suppressed the expected GeoLocator-DP profile-version warning](https://github.com/GeoPressure/GeoLocatoR/commit/8ee81ef) while the package remains on Data Package v1.
- [Simplified internal resource updates with Frictionless’s add-or-replace behaviour](https://github.com/GeoPressure/GeoLocatoR/commit/1054804).

# GeoLocatoR 1.0.2

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/ae8532f...cbc8819) · [PR #33](https://github.com/GeoPressure/GeoLocatoR/pull/33)

- [Improved `read_gldp()` diagnostics](https://github.com/GeoPressure/GeoLocatoR/commit/2594dc0) and [added support for package-directory paths](https://github.com/GeoPressure/GeoLocatoR/commit/fd9184e).
- [Improved Darwin Core export](https://github.com/GeoPressure/GeoLocatoR/commit/33a04d7), [rounded elevations](https://github.com/GeoPressure/GeoLocatoR/commit/c1e5b13), and [used most-likely paths](https://github.com/GeoPressure/GeoLocatoR/commit/3f9acb3).
- [Made `read_soi()` fail early for duplicate `GDL_ID` values](https://github.com/GeoPressure/GeoLocatoR/commit/f0dd964).
- [Resolved package-check notes and tidyselect deprecation warnings](https://github.com/GeoPressure/GeoLocatoR/commit/1eb90a7).

# GeoLocatoR 1.0.1

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/fcf7bb4...ae8532f) · [PR #31](https://github.com/GeoPressure/GeoLocatoR/pull/31)

- [Added `status_geopressuretemplate()`](https://github.com/GeoPressure/GeoLocatoR/commit/bebd93f) to inspect GeoPressureTemplate projects.
- [Retained `ring_number` and `tag_comments` from `config.yml`](https://github.com/GeoPressure/GeoLocatoR/commit/0d61a35) while reading GeoPressureTemplate projects.
- [Improved GeoPressureTemplate tag creation](https://github.com/GeoPressure/GeoLocatoR/commit/21ced56) and [movement-graph handling](https://github.com/GeoPressure/GeoLocatoR/commit/506dbb2).
- [Added experimental lifecycle labels](https://github.com/GeoPressure/GeoLocatoR/commit/d292586) and [required GeoPressureR 3.5.4 or later](https://github.com/GeoPressure/GeoLocatoR/commit/6630201).
- [Made GeoPressureTemplate cloning more reliable](https://github.com/GeoPressure/GeoLocatoR/commit/8bc51ae) by quoting clone arguments and reporting errors.

# GeoLocatoR 1.0.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/v0.5...v1.0) · [PR #29](https://github.com/GeoPressure/GeoLocatoR/pull/29)

- [Released the first stable 1.0 version](https://github.com/GeoPressure/GeoLocatoR/commit/fcf7bb4) and [moved project URLs to GeoPressure](https://github.com/GeoPressure/GeoLocatoR/commit/cd6dee2).
- [Updated GeoLocator Data Package schema URLs](https://github.com/GeoPressure/GeoLocatoR/commit/acbc992) to the GeoPressure location.
- [Added `select_gldp()`](https://github.com/GeoPressure/GeoLocatoR/commit/2c0303e) and [helpers to remove measurements](https://github.com/GeoPressure/GeoLocatoR/commit/1d5d729).
- [Refined GLDP coherence checks](https://github.com/GeoPressure/GeoLocatoR/commit/7bd2103) and added clearer validation warnings.
- [Improved SOI imports](https://github.com/GeoPressure/GeoLocatoR/commit/d848c15), [package merging](https://github.com/GeoPressure/GeoLocatoR/commit/ca8a620), and [resource-read diagnostics](https://github.com/GeoPressure/GeoLocatoR/commit/f0d9d83).
- [Bundled local GeoLocator Data Package schemas](https://github.com/GeoPressure/GeoLocatoR/commit/01110b9) for reproducible validation.
- [Reworked `merge_gldp()` to accept lists of packages](https://github.com/GeoPressure/GeoLocatoR/commit/f97c43d) and [normalised parameters before merging](https://github.com/GeoPressure/GeoLocatoR/commit/33a18c1).
- [Added `write_gldp()`](https://github.com/GeoPressure/GeoLocatoR/commit/2b56483) and [resource ordering](https://github.com/GeoPressure/GeoLocatoR/commit/fdca06c).
- [Removed deprecated path, stap, and edge fields](https://github.com/GeoPressure/GeoLocatoR/commit/fa5a2ec) and [renamed `pitch` to `mean_acceleration_z`](https://github.com/GeoPressure/GeoLocatoR/commit/6d25929).
- **Breaking:** [renamed GeoPressureTemplate helpers](https://github.com/GeoPressure/GeoLocatoR/commit/f16f498), [renamed SOI readers](https://github.com/GeoPressure/GeoLocatoR/commit/d848c15), [renamed `zenodo_to_gldp()` to `read_zenodo()`](https://github.com/GeoPressure/GeoLocatoR/commit/d36c2bf), [renamed configuration and version helpers](https://github.com/GeoPressure/GeoLocatoR/commit/e8b2fa5), and [removed `gldp_to_zenodo()`](https://github.com/GeoPressure/GeoLocatoR/commit/2fb6058).

# GeoLocatoR 0.5.1

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/v0.5...af4141e) · [PR #24](https://github.com/GeoPressure/GeoLocatoR/pull/24)

- [Corrected GeoLocator Data Package validation and parameter-to-observation conversion](https://github.com/GeoPressure/GeoLocatoR/commit/728a2e0) for the current schema.

# GeoLocatoR 0.5.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/v0.4...v0.5) · [PR #21](https://github.com/GeoPressure/GeoLocatoR/pull/21)

- [Improved package metadata cleanup](https://github.com/GeoPressure/GeoLocatoR/commit/faa3a86), including contributor values, paths, licences, identifiers, and citations.
- [Strengthened validation with local references and `oneOf` support](https://github.com/GeoPressure/GeoLocatoR/commit/4739f96) and [added concept DOI checks](https://github.com/GeoPressure/GeoLocatoR/commit/dbe2bd9).
- [Improved tag merging and normalised enum values](https://github.com/GeoPressure/GeoLocatoR/commit/7fa7d4a) while importing Zenodo data.
- [Improved GeoPressureTemplate datetime handling](https://github.com/GeoPressure/GeoLocatoR/commit/c9d4dfe) and [removed missing measurement values](https://github.com/GeoPressure/GeoLocatoR/commit/73d00f5).

# GeoLocatoR 0.4.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/40ba0d3...v0.4) · [PR #20](https://github.com/GeoPressure/GeoLocatoR/pull/20)

- [Updated package metadata and citations](https://github.com/GeoPressure/GeoLocatoR/commit/347a3d4) for GeoLocator Data Package v0.4.
- [Improved configuration parsing](https://github.com/GeoPressure/GeoLocatoR/commit/5a39def) and [added clearer package errors](https://github.com/GeoPressure/GeoLocatoR/commit/c6af25d).

# GeoLocatoR 0.2.10

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/4fc8957...40ba0d3) · [PR #19](https://github.com/GeoPressure/GeoLocatoR/pull/19)

- [Refined licence selection](https://github.com/GeoPressure/GeoLocatoR/commit/1a8545b) while creating GeoPressureTemplate projects.
- [Corrected type extraction during import](https://github.com/GeoPressure/GeoLocatoR/commit/c0aa5e0).

# GeoLocatoR 0.2.9

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/90d7844...4fc8957) · [Export commit](https://github.com/GeoPressure/GeoLocatoR/commit/4fc8957)

- [Added Darwin Core and EML exports](https://github.com/GeoPressure/GeoLocatoR/commit/4fc8957).
- [Improved GeoPressureTemplate path handling](https://github.com/GeoPressure/GeoLocatoR/commit/1342fce) and [pkgdown export documentation](https://github.com/GeoPressure/GeoLocatoR/commit/a38ca02).

# GeoLocatoR 0.2.8

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/1cf0871...90d7844) · [PR #17](https://github.com/GeoPressure/GeoLocatoR/pull/17)

- [Added `config_to_tibble()`](https://github.com/GeoPressure/GeoLocatoR/commit/149aa8b) for converting GeoPressureTemplate `config.yml` files to tibbles.
- [Added `gldp_to_tag()`](https://github.com/GeoPressure/GeoLocatoR/commit/b146a85) to convert packages to GeoPressureR tag objects.
- [Removed the `gert` dependency and improved Git clone handling](https://github.com/GeoPressure/GeoLocatoR/commit/90d7844).
- [Extended `read_gldp()` to support Zenodo inputs](https://github.com/GeoPressure/GeoLocatoR/commit/a8af9d7) and [processed raw tag data only for new tags](https://github.com/GeoPressure/GeoLocatoR/commit/1c611e8).
- [Replaced lintr with air and jarl workflows](https://github.com/GeoPressure/GeoLocatoR/commit/a32a81a).

# GeoLocatoR 0.2.7

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/cb53780...1cf0871) · [PR #13](https://github.com/GeoPressure/GeoLocatoR/pull/13)

- [Improved package validation](https://github.com/GeoPressure/GeoLocatoR/commit/4318798), taxonomy selection, GeoPressureTemplate imports, and resource-overwrite prompts.
- [Added map plotting](https://github.com/GeoPressure/GeoLocatoR/commit/933f07b) and [most-likely-path plotting](https://github.com/GeoPressure/GeoLocatoR/commit/3dadd26).
- [Improved handling of missing sex values](https://github.com/GeoPressure/GeoLocatoR/commit/dbf63a5), [tag reads](https://github.com/GeoPressure/GeoLocatoR/commit/ed1fa55), and pressure-path data.
- [Adopted the GPL licence](https://github.com/GeoPressure/GeoLocatoR/commit/6c2394b).

# GeoLocatoR 0.2.6

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/5a65726...cb53780) · [URL fix](https://github.com/GeoPressure/GeoLocatoR/commit/cb53780)

- [Corrected package URLs](https://github.com/GeoPressure/GeoLocatoR/commit/cb53780) and taxonomic metadata handling.

# GeoLocatoR 0.2.5

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/3b5e158...5a65726) · [PR #12](https://github.com/GeoPressure/GeoLocatoR/pull/12)

- [Renamed `params2*` conversion helpers to `params_to_*`](https://github.com/GeoPressure/GeoLocatoR/commit/a67ba2b).
- [Improved `create_gldp()` validation and resource type and format validation](https://github.com/GeoPressure/GeoLocatoR/commit/c891210).
- [Updated documentation, citations, tests, and code formatting](https://github.com/GeoPressure/GeoLocatoR/commit/593b8c6).

# GeoLocatoR 0.2.4

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/dffe123...3b5e158) · [PR #11](https://github.com/GeoPressure/GeoLocatoR/pull/11)

- [Improved GeoPressureTemplate imports](https://github.com/GeoPressure/GeoLocatoR/commit/785b5c5), [package creation](https://github.com/GeoPressure/GeoLocatoR/commit/c8a2b40), SOI import defaults, and pressure-path compatibility.

# GeoLocatoR 0.2.3

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/fc12fd7...dffe123) · [PR #9](https://github.com/GeoPressure/GeoLocatoR/pull/9)

- [Added `merge_gldp()`](https://github.com/GeoPressure/GeoLocatoR/commit/4ebb690) for combining GeoLocator Data Packages.
- [Improved metadata updates](https://github.com/GeoPressure/GeoLocatoR/commit/4e0c6a1), licence display, and [GeoPressureTemplate labels](https://github.com/GeoPressure/GeoLocatoR/commit/e726acd).

# GeoLocatoR 0.2.2

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/d3a620c...fc12fd7) · [PR #7](https://github.com/GeoPressure/GeoLocatoR/pull/7)

- [Added clearer errors for missing GeoPressureTemplate files and missing tag or parameter data](https://github.com/GeoPressure/GeoLocatoR/commit/9126597).
- [Improved handling of paths with spaces](https://github.com/GeoPressure/GeoLocatoR/commit/0067071) and [duplicate measurements](https://github.com/GeoPressure/GeoLocatoR/commit/47d8d19).

# GeoLocatoR 0.2.1

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/92f4a6a...d3a620c) · [PR #6](https://github.com/GeoPressure/GeoLocatoR/pull/6)

- [Improved GeoPressureTemplate writing](https://github.com/GeoPressure/GeoLocatoR/commit/5045077) and [package test coverage](https://github.com/GeoPressure/GeoLocatoR/commit/92f4a6a).

# GeoLocatoR 0.2.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/74734e2...92f4a6a) · [PR #5](https://github.com/GeoPressure/GeoLocatoR/pull/5)

- [Added Zenodo import and metadata conversion helpers](https://github.com/GeoPressure/GeoLocatoR/commit/c2d0273).
- [Added SOI import support](https://github.com/GeoPressure/GeoLocatoR/commit/70b6d6f) for GDL files and database data.
- [Added package-resource validation](https://github.com/GeoPressure/GeoLocatoR/commit/17bbad4), [computed-property updates](https://github.com/GeoPressure/GeoLocatoR/commit/86abf42), and [bibliographic-citation updates](https://github.com/GeoPressure/GeoLocatoR/commit/13acf5f).
- [Improved package printing](https://github.com/GeoPressure/GeoLocatoR/commit/c8183cc), [resource loading](https://github.com/GeoPressure/GeoLocatoR/commit/9c78aa5), and [handling of empty twilights and staps](https://github.com/GeoPressure/GeoLocatoR/commit/217b6d3).

# GeoLocatoR 0.1.1

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/c33d28e...74734e2) · [Pkgdown setup](https://github.com/GeoPressure/GeoLocatoR/commit/f965d9c)

- [Added pkgdown site configuration](https://github.com/GeoPressure/GeoLocatoR/commit/f965d9c) and [removed the package vignette](https://github.com/GeoPressure/GeoLocatoR/commit/74734e2).
- [Improved raw-data GeoPressureTemplate imports](https://github.com/GeoPressure/GeoLocatoR/commit/37cac06) and handling of missing package descriptions.

# GeoLocatoR 0.1.0

[Full changelog](https://github.com/GeoPressure/GeoLocatoR/compare/5e426dd...c33d28e) · [Version commit](https://github.com/GeoPressure/GeoLocatoR/commit/aa2d84a)

- [Added plotting for `geolocatordp` objects](https://github.com/GeoPressure/GeoLocatoR/commit/45f2f14) and [pressure-path resources](https://github.com/GeoPressure/GeoLocatoR/commit/3cbc92e).
- [Added GeoPressureTemplate project creation](https://github.com/GeoPressure/GeoLocatoR/commit/cf56f4c), including [`config.yml` creation](https://github.com/GeoPressure/GeoLocatoR/commit/cf56f4c) and [optional RStudio opening](https://github.com/GeoPressure/GeoLocatoR/commit/8baed0d).
- [Added package coherence checks](https://github.com/GeoPressure/GeoLocatoR/commit/3dfdcf8), [schema-based metadata ordering](https://github.com/GeoPressure/GeoLocatoR/commit/9da704f), and [resource type casting](https://github.com/GeoPressure/GeoLocatoR/commit/874586f).

# GeoLocatoR 0.0.0.9000

[Initial development history](https://github.com/GeoPressure/GeoLocatoR/commits/5e426dd)

- [Initial development release of GeoLocatoR](https://github.com/GeoPressure/GeoLocatoR/commit/c022375).
