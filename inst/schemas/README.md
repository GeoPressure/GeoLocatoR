# Local GeoLocator-DP Schemas

GeoLocatoR reads GeoLocator-DP profile/table schemas from this directory at runtime.
Schema loading is offline-only.

## Source of truth

Supported versions are defined once in:

- `R/versioning.R` as `.gldp_supported_versions`

## Refresh workflow

When GeoLocator-DP schemas change, refresh local files from repository root:

```r
source("data-raw/sync_schemas.R")
sync_gldp_schemas()
```

This script syncs every version listed in `.gldp_supported_versions` into:

- `inst/schemas/<version>/geolocator-dp-profile.json`
- `inst/schemas/<version>/<resource>-table-schema.json`
