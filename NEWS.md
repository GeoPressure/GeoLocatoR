# GeoLocatoR (development version)

## Main

- [Require frictionless 2.0.0 or later and stop writing the Data Package v1 `profile` on resources](https://github.com/GeoPressure/GeoLocatoR/commit/3622dc0) ([#38](https://github.com/GeoPressure/GeoLocatoR/issues/38)). Packages that carry `profile` are still read and validated, and GeoLocatoR 1.3.2 reads packages without it. Also [require GeoPressureR 3.6.0 or later and drop the unused movepub suggestion](https://github.com/GeoPressure/GeoLocatoR/commit/2837690).
- `validate_gldp()` [enforces the `device_status` rules of the observations schema](https://github.com/GeoPressure/GeoLocatoR/commit/2fa3181): `equipment` and `retrieval` must be `present`, and a missing status now fails; `none` and `missing` are only for `capture`, and `unknown` only for `sighting`. It is also stricter on custom tables: [patterns must match the whole value](https://github.com/GeoPressure/GeoLocatoR/commit/e4bff76) and [geopoints are read as "lon, lat"](https://github.com/GeoPressure/GeoLocatoR/commit/2bb881e). In return, [implausible years, `staps$stap_id` of 300 or more and `paths$j` of 500 or more only warn](https://github.com/GeoPressure/GeoLocatoR/commit/8e97713), and datetimes back to 1990 pass.
- `gldp_to_dwc()` [writes `sex` and `lifeStage` with Darwin Core and GBIF vocabularies](https://github.com/GeoPressure/GeoLocatoR/commit/020cf63) (`male`/`female`, `juvenile`/`adult`) and keeps the EURING age class in `dynamicProperties`. It also [accepts explicit taxonomy identifiers and can return the tables without writing](https://github.com/GeoPressure/GeoLocatoR/commit/1dae3b4), [leaves taxonomy identifiers blank by default](https://github.com/GeoPressure/GeoLocatoR/commit/c381e05), [uses the grid-cell size as the uncertainty of known locations](https://github.com/GeoPressure/GeoLocatoR/commit/2aaab6b), [adapts the geolocation uncertainty to the number of simulations](https://github.com/GeoPressure/GeoLocatoR/commit/322d56f) and [always writes `datasetID`](https://github.com/GeoPressure/GeoLocatoR/commit/c8f18fc).
- `read_zenodo()` [downloads only `datapackage.json`, `params.json` and the files its resources refer to, checks them against the record's MD5 checksums and deletes them after reading](https://github.com/GeoPressure/GeoLocatoR/commit/29a4f81), and explains when a record is metadata-only or restricted.

## Minor

- [Soft-deprecate `update_gldp_order_resources()`, `update_gldp_temporal()`, `update_gldp_taxonomic()`, `update_gldp_number_tags()` and `update_gldp_bibliographic_citation()`](https://github.com/GeoPressure/GeoLocatoR/commit/a8c2f55); use `update_gldp()`. [Deprecate the unused `delim` argument of `add_gldp_resource()`](https://github.com/GeoPressure/GeoLocatoR/commit/c6476f5) and [rename the `tags` argument of `tags_to_measurements()` to `tag_list`](https://github.com/GeoPressure/GeoLocatoR/commit/04f08f7); the old names still work with a warning. [Remove the internal `version()` shim](https://github.com/GeoPressure/GeoLocatoR/commit/db6e7e9).
- Upgrades: packages whose `$schema` points to GeoLocator-DP `main` [are upgraded from v0.1](https://github.com/GeoPressure/GeoLocatoR/commit/4d9a4b0), `create_gldp()` no longer accepts `version = "main"`, and `dev` schemas get a clear error. [Upgrading to v1.0 removes the rows of non-deployed tags from every table](https://github.com/GeoPressure/GeoLocatoR/commit/5a2fc81), and [upgrading to v1.2 sets an `observations$mass` of 0 to missing](https://github.com/GeoPressure/GeoLocatoR/commit/d76bedd).
- [Warn when casting a table turns values into missing ones](https://github.com/GeoPressure/GeoLocatoR/commit/12f98fd), naming the field and the number of values lost. Accessors such as `measurements()` [return the same column types whatever `force_read` was](https://github.com/GeoPressure/GeoLocatoR/commit/db6c779): enum fields such as `sensor` are character, no longer factors.
- [Accept PresTag as a tag manufacturer](https://github.com/GeoPressure/GeoLocatoR/commit/1def299) and [name PresTag tags so in `params_to_tags()`](https://github.com/GeoPressure/GeoLocatoR/commit/966f476); the title-prefix recommendation now says the GeoLocator-DP Zenodo community expects it.
- `read_soi()` [finds a tag's data by its whole `GDL_ID`](https://github.com/GeoPressure/GeoLocatoR/commit/bad1deb) (`A1` no longer matches `A10_…`), warns when several folders match, no longer searches dataset-specific folders (use `gdl$directory`), and marks equipment and retrieval observations as `present`.
- GeoPressureTemplate: [a read–create–read cycle no longer moves `crop_start` by one day](https://github.com/GeoPressure/GeoLocatoR/commit/dec427a); `create_geopressuretemplate()` [leaves the active usethis project unchanged and checks the package before cloning](https://github.com/GeoPressure/GeoLocatoR/commit/0d947b1), and [exports unlabelled tags to `data/tag-label/{id}.csv` for labelling](https://github.com/GeoPressure/GeoLocatoR/commit/16b5451) instead of saving automatic labels as final. `tags_to_measurements()` [keeps the temperature of tags created with GeoPressureR before 3.7.1](https://github.com/GeoPressure/GeoLocatoR/commit/0e8758b). `read_geopressuretemplate()` [errors outside interactive sessions when `pkg` already has resources](https://github.com/GeoPressure/GeoLocatoR/commit/6cb21d0). `config_to_tibble()` [shows known positions](https://github.com/GeoPressure/GeoLocatoR/commit/c93eae3) and `status_geopressuretemplate()` [renames `interim_path_pressurepath_most_likely` to `interim_pressurepath_most_likely`](https://github.com/GeoPressure/GeoLocatoR/commit/8303cfc).
- `gldp_to_eml()` [returns the EML without writing when `directory = NULL`](https://github.com/GeoPressure/GeoLocatoR/commit/f76c92a) and [identifies a DOI `packageId` as such](https://github.com/GeoPressure/GeoLocatoR/commit/6319a58). `merge_gldp()` [drops custom resources with a message instead of aborting](https://github.com/GeoPressure/GeoLocatoR/commit/ecefd8b) and [records source DOIs as bare DOIs](https://github.com/GeoPressure/GeoLocatoR/commit/e7d84ff).
- `print()` [writes everything through cli](https://github.com/GeoPressure/GeoLocatoR/commit/48f8b9f), so `capture.output(print(pkg))` is now empty, and `plot(type = "map")` [sizes markers on one scale and errors when there is no most-likely path](https://github.com/GeoPressure/GeoLocatoR/commit/d23a18b).
- [Add Peter Desmet as an author](https://github.com/GeoPressure/GeoLocatoR/commit/168bf3e).

# GeoLocatoR v1.3.2

## Main

- Read every package the GeoLocator-DP profile allows. `read_gldp()` [resolves GeoLocator-DP table schemas given by URL to the bundled copies, and reads resources that declare `type: "table"` without the Data Package v1 `profile`](https://github.com/GeoPressure/GeoLocatoR/commit/c83aba7), so the official GeoLocator-DP example package can now be read with frictionless 1.3.0. It also [leaves custom and `params` resources unread instead of aborting](https://github.com/GeoPressure/GeoLocatoR/commit/73a5515), [upgrades v1.0 and v1.1 packages that carry a `params` resource](https://github.com/GeoPressure/GeoLocatoR/commit/20c909c), [writes v1.1 descriptors for file-backed tables read with `force_read = FALSE`](https://github.com/GeoPressure/GeoLocatoR/commit/45ca2cf), [matches columns by name for packages read from a URL](https://github.com/GeoPressure/GeoLocatoR/commit/1f09ce1), [accepts `refs/tags/vX.Y` schema URLs](https://github.com/GeoPressure/GeoLocatoR/commit/41e9594) and [reads `params.json` next to a remote `datapackage.json`](https://github.com/GeoPressure/GeoLocatoR/commit/8d99c3f).
- Fix GeoPressureTemplate imports and exports. `read_geopressuretemplate()` [applies the labels and known positions of raw tags](https://github.com/GeoPressure/GeoLocatoR/commit/4d1a0d7), which every raw-tag import silently lost, [imports simulated pressurepaths](https://github.com/GeoPressure/GeoLocatoR/commit/b080705), takes the [species](https://github.com/GeoPressure/GeoLocatoR/commit/30b4fae) and [ring number](https://github.com/GeoPressure/GeoLocatoR/commit/b59b0b9) of interim tags from `config.yml`, and [fills the retrieval position from a single known longitude and latitude](https://github.com/GeoPressure/GeoLocatoR/commit/9d035fa). `create_geopressuretemplate()` [lists only each tag's own sensor files](https://github.com/GeoPressure/GeoLocatoR/commit/7929313) (GeoPressureR aborted on the others), [includes temperature files](https://github.com/GeoPressure/GeoLocatoR/commit/4a2720f), [writes `config.yml` for packages read from interim files](https://github.com/GeoPressure/GeoLocatoR/commit/1913ccd) and [no longer applies the clock drift a second time](https://github.com/GeoPressure/GeoLocatoR/commit/44cad17). [`config_to_tibble()` reads each tag's resolved configuration](https://github.com/GeoPressure/GeoLocatoR/commit/f280776) instead of GeoPressureR defaults.
- Fix `gldp_to_tag()`: [temperature sensors were silently left out](https://github.com/GeoPressure/GeoLocatoR/commit/a3465f7), [a measurements table without `label` made it fail](https://github.com/GeoPressure/GeoLocatoR/commit/3f31d12), [pressure labels were dropped when flights are labelled on acceleration](https://github.com/GeoPressure/GeoLocatoR/commit/73f9ede), which then broke `tag_label_stap()` and `geopressure_map()`, and [GeoPressureR discarded every unlabelled twilight](https://github.com/GeoPressure/GeoLocatoR/commit/644db84). The tag can now have an empty pressure `label` column, and twilight labels are `""` instead of `NA`.
- Make `validate_gldp()` accept every package the profile allows. It [no longer aborts on observations without the optional `tag_id` or `device_status` columns, or on a package without `observations`](https://github.com/GeoPressure/GeoLocatoR/commit/8245d62), [reports custom and `params` resources as not checked instead of failing](https://github.com/GeoPressure/GeoLocatoR/commit/dc0e2a0), and [checks table fields with Table Schema rather than JSON Schema rules](https://github.com/GeoPressure/GeoLocatoR/commit/3429b6e), [checks self-referencing foreign keys against their own resource](https://github.com/GeoPressure/GeoLocatoR/commit/dd030ae) and [no longer reports follow-up errors after a type mismatch](https://github.com/GeoPressure/GeoLocatoR/commit/5d02124). Packages that failed only for these reasons now pass.
- [Keep each tag's data separate in `read_soi()`](https://github.com/GeoPressure/GeoLocatoR/commit/808ccc0). When several tags share a folder, each tag now reads only its own sensor files; previously it could silently take another tag's pressure or acceleration. Packages created from such folders should be recreated.

## Minor

- `read_geopressuretemplate()` [removes known GeoPressureR working columns from interim tables](https://github.com/GeoPressure/GeoLocatoR/commit/07b95c0) before adding them, [applies column parsers only to the columns present](https://github.com/GeoPressure/GeoLocatoR/commit/2218b73), [reads `datapackage_id` as text](https://github.com/GeoPressure/GeoLocatoR/commit/93832cd), [reads unquoted numeric ring numbers](https://github.com/GeoPressure/GeoLocatoR/commit/04ba882), [parses `tags.xlsx` and `observations.xlsx` by column name](https://github.com/GeoPressure/GeoLocatoR/commit/4bdd807) and [says why a raw tag could not be read](https://github.com/GeoPressure/GeoLocatoR/commit/a7caccb).
- `create_geopressuretemplate()` [skips observations without coordinates](https://github.com/GeoPressure/GeoLocatoR/commit/10dd3a9) and [writes only `stap_id`, `known_lat` and `known_lon`](https://github.com/GeoPressure/GeoLocatoR/commit/58fc95e) in known positions, [handles plain-text descriptions](https://github.com/GeoPressure/GeoLocatoR/commit/7fa3932), [keeps CC-BY-SA, NC and ND licences](https://github.com/GeoPressure/GeoLocatoR/commit/e67b4bb) and [builds the README DOI badge from the package DOI](https://github.com/GeoPressure/GeoLocatoR/commit/43d5f5f). [`config_to_tibble()` always returns `id`](https://github.com/GeoPressure/GeoLocatoR/commit/382c74e), which was dropped for single-tag projects.
- `params_to_tags()` [reads model and firmware from older SOI settings files](https://github.com/GeoPressure/GeoLocatoR/commit/cfbad5b) and [names BAS tags "British Antarctic Survey"](https://github.com/GeoPressure/GeoLocatoR/commit/e3233fd). `tags_to_measurements()` [returns an empty table for tags without sensor data](https://github.com/GeoPressure/GeoLocatoR/commit/d2fb9a0).
- Setters [keep factor labels and the time of ISO 8601 datetimes such as `2020-01-01T12:30:00Z`](https://github.com/GeoPressure/GeoLocatoR/commit/a9e5bb8), and [`measurements<-` updates `taxonomic`](https://github.com/GeoPressure/GeoLocatoR/commit/5a01bb8).
- Clearer errors and fewer spurious warnings: a Data Package [without `$schema` is reported as not a GeoLocator Data Package](https://github.com/GeoPressure/GeoLocatoR/commit/8f9bc56), [an unreadable table names its real cause once](https://github.com/GeoPressure/GeoLocatoR/commit/56dfb5e), [an empty base `data.frame` in `add_gldp_resource()` is reported as empty](https://github.com/GeoPressure/GeoLocatoR/commit/ec9271d), and [untyped fields no longer warn "No casting"](https://github.com/GeoPressure/GeoLocatoR/commit/18342a7). [A contributor without a name no longer breaks the citation](https://github.com/GeoPressure/GeoLocatoR/commit/b547251), and [`write_gldp()` writes `params.json` only when there are params](https://github.com/GeoPressure/GeoLocatoR/commit/3430f4c).
- `update_gldp()` [accepts a partial `created` date such as "2024-06"](https://github.com/GeoPressure/GeoLocatoR/commit/3d64723), [uses the DOI in `id` for the citation when `conceptid` is not a DOI](https://github.com/GeoPressure/GeoLocatoR/commit/4514d25), and [keeps the temporal coverage of a package read with `drop_measurements = TRUE`](https://github.com/GeoPressure/GeoLocatoR/commit/336cac8).
- `validate_gldp()` messages [name the ring numbers, resources and rows at fault, pluralise correctly and report a missing value once](https://github.com/GeoPressure/GeoLocatoR/commit/b59cef0), [list unknown `tag_id`/`ring_number` combinations](https://github.com/GeoPressure/GeoLocatoR/commit/ca67c5c) and [show a missing core resource as an error](https://github.com/GeoPressure/GeoLocatoR/commit/7cf15d9). Its [documentation says what each step checks](https://github.com/GeoPressure/GeoLocatoR/commit/13bfd7e).
- `merge_gldp()` [explains that a package `id` is required](https://github.com/GeoPressure/GeoLocatoR/commit/d4b8539), [no longer merges unrelated contributors whose names are non-ASCII, non-Latin or missing](https://github.com/GeoPressure/GeoLocatoR/commit/d76bae5) and [keeps a role such as `researcher` next to `researchergroup`](https://github.com/GeoPressure/GeoLocatoR/commit/ac0bc66). `select_gldp()` [keeps untagged observations of the kept birds](https://github.com/GeoPressure/GeoLocatoR/commit/1f8da6c) and [drops the `params` of removed tags](https://github.com/GeoPressure/GeoLocatoR/commit/1dee33e).
- `gldp_to_eml()` [writes the right bounding box for a Point coverage](https://github.com/GeoPressure/GeoLocatoR/commit/33d4de7), [always writes a creator and a contact](https://github.com/GeoPressure/GeoLocatoR/commit/0e73cd3), [lists a rights holder once](https://github.com/GeoPressure/GeoLocatoR/commit/ce62fdb) and [no longer gives a person's name as their organisation](https://github.com/GeoPressure/GeoLocatoR/commit/dde0b6d).
- `read_soi()` [no longer aborts when no tag data can be read](https://github.com/GeoPressure/GeoLocatoR/commit/63c2fab), [ignores non-`.glf` files outside tag folders](https://github.com/GeoPressure/GeoLocatoR/commit/ea24e2e), [writes the model without "-NA"](https://github.com/GeoPressure/GeoLocatoR/commit/4fe05a3) and [leaves an unknown attachment missing](https://github.com/GeoPressure/GeoLocatoR/commit/5f6612f). `read_soi_gld()` [aborts when `mdb-export` fails](https://github.com/GeoPressure/GeoLocatoR/commit/20bfcfa) and [keeps missing order values missing instead of writing "NA"](https://github.com/GeoPressure/GeoLocatoR/commit/6afd3bb).
- `read_zenodo()` [requires httr2 1.1.1 or later](https://github.com/GeoPressure/GeoLocatoR/commit/8f4fcfb), reports [every failed download](https://github.com/GeoPressure/GeoLocatoR/commit/7ebbb04) and [non-JSON HTTP errors](https://github.com/GeoPressure/GeoLocatoR/commit/385da75), [reads Zenodo Sandbox URLs and DOIs from Sandbox](https://github.com/GeoPressure/GeoLocatoR/commit/9398663), [accepts more identifier forms](https://github.com/GeoPressure/GeoLocatoR/commit/d94d9e6), [writes contributor `roles` as an array](https://github.com/GeoPressure/GeoLocatoR/commit/19cf96d), [omits empty licence, keyword and grant values](https://github.com/GeoPressure/GeoLocatoR/commit/b2bd556) and [downloads each record to its own folder](https://github.com/GeoPressure/GeoLocatoR/commit/993413a).
- `print()` [shows access and community only for packages from Zenodo](https://github.com/GeoPressure/GeoLocatoR/commit/15293f0), [labels `pkg$id` as id rather than doi](https://github.com/GeoPressure/GeoLocatoR/commit/3425c3e) and [loses a stray parenthesis](https://github.com/GeoPressure/GeoLocatoR/commit/e8e5901). `plot()`: [the coverage plot works without observations](https://github.com/GeoPressure/GeoLocatoR/commit/7322df9), [draws `mean_acceleration_z` with activity](https://github.com/GeoPressure/GeoLocatoR/commit/5da27f6) [in consistent colours](https://github.com/GeoPressure/GeoLocatoR/commit/00e5d94), and [the ring plot draws periods in black](https://github.com/GeoPressure/GeoLocatoR/commit/1f9ceaf).
- [Fix the repository URL in DESCRIPTION and CITATION.cff](https://github.com/GeoPressure/GeoLocatoR/commit/f38e651), wrap network examples in `\dontrun{}` ([1](https://github.com/GeoPressure/GeoLocatoR/commit/9160a63), [2](https://github.com/GeoPressure/GeoLocatoR/commit/2ebd54a), [3](https://github.com/GeoPressure/GeoLocatoR/commit/bb6beb9)), and [pair each test file with its source file](https://github.com/GeoPressure/GeoLocatoR/commit/41455b7).

**Full Changelog**: <https://github.com/GeoPressure/GeoLocatoR/compare/v1.3.1...v1.3.2>

# GeoLocatoR v1.3.1

## Main

- [Keep the retrieval latitude in `read_soi_gld()`](https://github.com/GeoPressure/GeoLocatoR/commit/315d186). The default column selection (`filter_col = TRUE`) listed `LongitudeRemoved` and `LongitudeAttached` twice and omitted `LatitudeRemoved`, so every retrieval observation created by `read_soi()` had no latitude. Packages created from SOI data with earlier versions should be recreated, or their retrieval latitudes filled. Coordinates recorded as `99999` (unknown in the SOI database) are now read as missing instead of being written as observations.
- [Read dates from the Access file in `read_soi_gld()` on macOS and Linux](https://github.com/GeoPressure/GeoLocatoR/commit/d442338). `mdb-export` wrote them as `MM/DD/YY HH:MM:SS`, which was parsed as `NA`, so every date (attachment, retrieval, RTC, first and last data, delivery) was missing and `read_soi()` created equipment and retrieval observations without a datetime. They are now exported in ISO format. Packages created from an Access file on these systems with earlier versions should be recreated.

## Minor

- [Select the columns given in a character `filter_col`](https://github.com/GeoPressure/GeoLocatoR/commit/315d186) in `read_soi_gld()`, which previously always failed, and accept data frames for `data_file` and `order_file` as documented.
- [Read `AccSamplingRate`, `PrioritySensor` and `PriorityMemory` with their actual types](https://github.com/GeoPressure/GeoLocatoR/commit/315d186) (number, text and percentage) instead of logical, which turned their values into `NA`.
- [Warn when `read_soi()` cannot read the data of a tag](https://github.com/GeoPressure/GeoLocatoR/commit/315d186), naming the tag and the error, instead of silently leaving its measurements out.

**Full Changelog**: <https://github.com/GeoPressure/GeoLocatoR/compare/v1.3.0...v1.3.1>

# GeoLocatoR v1.3.0

## Main

- Support [GeoLocator-DP v1.2](https://github.com/GeoPressure/GeoLocator-DP/releases/tag/v1.2): [bundle its schemas](https://github.com/GeoPressure/GeoLocatoR/commit/d9e3d4c) and [upgrade packages to it](https://github.com/GeoPressure/GeoLocatoR/commit/315c913), making it the version `create_gldp()` writes. v1.2 requires `j` on the trajectory tables, adds their primary keys and requires `observations$mass` > 0. Packages written against any earlier version are upgraded on read.
- [Handle file-backed tables consistently during selection, upgrades and validation](https://github.com/GeoPressure/GeoLocatoR/commit/c667b69). `select_gldp()` now filters packages read with `force_read = FALSE`, and upgrades preserve declared types, units and descriptions for extra fields such as `pressure_tag`. Packages older than GeoLocator-DP v1.0 load their tables before upgrading because those migrations transform data. Validation loads file-backed tables and rejects even a single unmatched measurement tag or ring number; packages that previously passed incomplete checks may now fail.
- [Restore the source-package catalogue as a tibble when reading a package](https://github.com/GeoPressure/GeoLocatoR/commit/cf66fac). `read_gldp()` and `read_zenodo()` return `pkg$datapackages` with one row per source package and JSON nulls restored as missing cells. Code that indexed the catalogue as a list of rows should use table indexing.
- Fill the trajectory index `j` on `pressurepaths` flight rows ([1](https://github.com/GeoPressure/GeoLocatoR/commit/cc2c217), [2](https://github.com/GeoPressure/GeoLocatoR/commit/315c913)). GeoPressureR <= 3.6.3 left it empty on rows with a fractional `stap_id` (fixed in [GeoPressureR#180](https://github.com/GeoPressure/GeoPressureR/pull/180)), which affects every published package with `pressurepaths`. `upgrade_gldp()` (run by `read_gldp()`) fills it when upgrading a package to GeoLocator-DP v1.2 and the `j` of a tag and type is unambiguous, loading `pressurepaths` even with `force_read = FALSE`, `read_geopressuretemplate()` fills it for interim files written by older GeoPressureR and now imports `pressurepath_tag` (it listed `pressurepath_geopressureviz` twice), and [`validate_gldp()` warns about trajectory rows without `j`](https://github.com/GeoPressure/GeoLocatoR/commit/f65b815), which GeoLocator-DP v1.2 [requires](https://github.com/GeoPressure/GeoLocator-DP/commit/68d4622).
- [`merge_gldp()` merges contributor entries](https://github.com/GeoPressure/GeoLocatoR/commit/4d0b38a) for the same person (same ORCID, or an ORCID-less entry whose name matches a single ORCID), keeping all roles and organizations. Source contributor lists in `datapackages$contributors` are now separated by `; `.
- [`validate_gldp()` checks the schema `primaryKey` and the Table Schema v2 `exclusiveMinimum` and `exclusiveMaximum` constraints](https://github.com/GeoPressure/GeoLocatoR/commit/f65b815): rows must be unique on the primary key. These support the [trajectory primary keys](https://github.com/GeoPressure/GeoLocator-DP/commit/68d4622) and [mass > 0](https://github.com/GeoPressure/GeoLocator-DP/commit/60f8bdc) of GeoLocator-DP v1.2. Plausibility bounds are guidelines and now only warn: `pressurepaths$altitude` outside -200 to 10,000 m (previously an error above 7,000 m) and `observations$mass` above 10,000 g.

## Minor

- [Make `print()` robust](https://github.com/GeoPressure/GeoLocatoR/commit/b55323f) to plain-text descriptions, which were passed to the HTML parser and read as a file path, and to braces in free-text fields. Long contributor lists are shortened and the `datapackages` catalogue of merged packages is summarised.
- [Give short errors when a resource is missing](https://github.com/GeoPressure/GeoLocatoR/commit/ad990c4), from accessors such as `measurements()` and from `plot()`, which names the resources a plot type needs.
- [Name custom resources in `validate_gldp()` messages](https://github.com/GeoPressure/GeoLocatoR/commit/f65b815), and [document why `read_gldp()` loads packages older than v1.0](https://github.com/GeoPressure/GeoLocatoR/commit/4b4fd1c).
- [Retain manual tags without sensor outputs during GeoPressureTemplate imports](https://github.com/GeoPressure/GeoLocatoR/commit/ac58421), including when generated tags are missing from the manual metadata table. Manual metadata keeps precedence; generated-only tags are added with the existing warning.
- [Fix unsupported-version error messages with cli 3.4.0 and later](https://github.com/GeoPressure/GeoLocatoR/commit/419b2a2) and [correct diagnostic wording](https://github.com/GeoPressure/GeoLocatoR/commit/02a9148), so errors identify the underlying data or version issue.
- [Add regression tests for missing foreign-key columns, invalid observation histories and template metadata precedence](https://github.com/GeoPressure/GeoLocatoR/commit/13a19ab).

**Full Changelog**: <https://github.com/GeoPressure/GeoLocatoR/compare/v1.2.0...v1.3.0>

# GeoLocatoR v1.2.0

Supports [GeoLocator-DP v1.1](https://github.com/GeoPressure/GeoLocator-DP/releases/tag/v1.1), which aligns the standard with Data Package v2, and makes it the version `create_gldp()` writes. Packages written against any earlier version are upgraded on read, so archived packages need no rewriting.

## Main

- [Bundle the GeoLocator-DP v1.1 schemas](https://github.com/GeoPressure/GeoLocatoR/commit/867b2a1) and [upgrade packages to v1.1](https://github.com/GeoPressure/GeoLocatoR/commit/e63c112).
- [Match a resource schema to its file by column name](https://github.com/GeoPressure/GeoLocatoR/commit/02db970). [`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html) maps schema fields to columns by position, `fieldsMatch` being unimplemented ([frictionless-r#216](https://github.com/frictionlessdata/frictionless-r/issues/216)), so a CSV that legitimately left a column out read every later column under the wrong name, with no error and no warning. Reads now reconcile the schema with the CSV header first.
- [Report a table missing a required column](https://github.com/GeoPressure/GeoLocatoR/commit/5d4a0ac). `fieldsMatch` says nothing about which columns are required, so a `tags` table with no `tag_id` at all previously validated clean. **`validate_gldp()` is therefore stricter than in v1.1.1** and may report packages it used to pass.
- [Set the data resource profile on resources](https://github.com/GeoPressure/GeoLocatoR/commit/0f6ff84) and [mark them as tabular with `type`](https://github.com/GeoPressure/GeoLocatoR/commit/b67e218). frictionless only writes `$schema` and `type` from version 2.0, so GeoLocatoR sets them itself; without them `frictionless::version()` reads a resource as Data Package v1 inside a package declaring v2.

## Minor

- [Accept Data Package v2 tabular resources in `validate_gldp()`](https://github.com/GeoPressure/GeoLocatoR/commit/3255d6e), and [support v2 field types while no longer emitting an empty `$schema`](https://github.com/GeoPressure/GeoLocatoR/commit/2f07495).
- [Read allowed values from `categories` as well as `enum`](https://github.com/GeoPressure/GeoLocatoR/commit/6b7d4e2), GeoLocator-DP v1.1 declaring both.
- [Read resource names from either profile shape](https://github.com/GeoPressure/GeoLocatoR/commit/9a1b471), v1.1 selecting resource schemas with `if`/`then` where v1.0 used `oneOf`.
- [Simplify `fieldsMatch` handling and warn on dropped columns](https://github.com/GeoPressure/GeoLocatoR/commit/adad0b1), and [add `gldp_resource()` accessors](https://github.com/GeoPressure/GeoLocatoR/commit/73489b5).

## Compatibility

Addresses [#38](https://github.com/GeoPressure/GeoLocatoR/issues/38): verified against both frictionless 1.3.0 (CRAN) and 1.3.0.9000, the development version that becomes 2.0.0. The full suite passes on both, and packages written by either validate against the published GeoLocator-DP v1.1 profile. Users need change nothing.

Resources still carry `profile: "tabular-data-resource"` alongside `$schema` and `type`, because frictionless 1.3.0 refuses to read a resource without it. Once frictionless 2.0.0 reaches CRAN, that can be dropped and `DESCRIPTION` raised to `frictionless (>= 2.0.0)`.

**Full Changelog**: <https://github.com/GeoPressure/GeoLocatoR/compare/v1.1.1...v1.2.0>

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
