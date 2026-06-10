# R2camtrapdp

<!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/R2camtrapdp)](https://CRAN.R-project.org/package=R2camtrapdp) -->
<!-- [![R-CMD-check](https://github.com/kfukasawa37/R2camtrapdp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kfukasawa37/R2camtrapdp/actions/workflows/R-CMD-check.yaml) -->
<!-- badges: end -->

*(日本語版 README: [README.ja.md](README.ja.md))*

Convert arbitrary spreadsheet camera-trap (and acoustic) data into a
[Camera Trap Data Package (Camtrap DP)](https://camtrap-dp.tdwg.org/), the
[Frictionless](https://frictionlessdata.io/) data package standard for camera-trap data.

`R2camtrapdp` is **schema-driven**: the structure, types, constraints and
relations of the output tables are read from the official Frictionless *table
schemas* of the Camtrap DP version you choose. As a result it works with any
Camtrap DP version (`1.0`, `1.0.1`, `1.0.2`) and with other schema flavors such
as the [bioacoustics extension](https://github.com/camera-traps/bioacoustics) —
including custom/extra columns — without hard-coding any rules.

## What it does

1. **Derive** shell tables and constraints from the chosen version's schema.
2. **Build & validate** — map your columns, coerce types, and validate against
   the schema constraints (`required`, `unique`, `enum`, `minimum`/`maximum`,
   `pattern`, date/datetime **format**, ...) plus cross-table relations
   (primary/foreign keys).
3. **Assemble** the data package (`deployments.csv`, `media.csv`,
   `observations.csv` and `datapackage.json`).
4. **Validate** the written package with the Python
   [Frictionless](https://framework.frictionlessdata.io/) validator and report
   the errors back in R (file, column, row, rule, **offending value**).

## Installation

```r
# from GitHub (build_vignettes = TRUE to install the vignettes too)
# install.packages("devtools")
devtools::install_github("kfukasawa37/R2camtrapdp", build_vignettes = TRUE)

# (once on CRAN)
# install.packages("R2camtrapdp")
```

The Python Frictionless validator (step 4) is optional and only needed for
`validate_frictionless()` / `ctdp_validate_frictionless()`:

```sh
pip install frictionless
```

## Quick start

```r
library(R2camtrapdp)

data("Idep")   # example deployment table
data("Iobs")   # example observation table

# 1-2. Build the three core tables (camera-trap helpers)
deployments <- create_deployments(
  deploymentID = Idep$deploymentID, latitude = Idep$latitude, longitude = Idep$longitude,
  deploymentStart_date = Idep$startDate, deploymentStart_time = Idep$startTime,
  deploymentEnd_date = Idep$endDate, deploymentEnd_time = Idep$endTime,
  cameraID = Idep$cameraID, setupBy = Idep$setupBy)

# 3. Assemble the data package (schema-driven validation runs in set_*())
dp <- R6_CamtrapDP$new(version = "1.0.1", title = "My dataset", description = "...")
dp$set_deployments(deployments)
# dp$set_media(media); dp$set_observations(observations)
dp$add_contributors(data.frame(title = "Jane Doe", role = "contact"))
dp$add_license(name = "CC0-1.0", scope = "data")
dp$set_project(title = "Project X", samplingDesign = "opportunistic",
               captureMethod = "activityDetection", individualAnimals = FALSE,
               observationLevel = "media")
dp$set_st()
dp$check_relations()                                  # primary/foreign keys

path <- file.path(tempdir(), "my-camtrapdp")
dp$out_camtrapdp(write = TRUE, directory = path)      # CSVs + datapackage.json

# 4. Validate with Python Frictionless
issues <- dp$validate_frictionless(directory = path, python = "python")
ctdp_is_valid(issues)
```

A complete, runnable end-to-end script (schema inspection → mapping → build →
relations/metadata checks → write → Frictionless validation) is in
[`examples/example_usage.R`](examples/example_usage.R):

```r
source("r2camtrapdp.R")      # or: library(R2camtrapdp)
# then run examples/example_usage.R
```

See the vignettes for the full workflow, mapping arbitrary spreadsheets, the
acoustic (audio) workflow, and validation details.

## Key functions and classes

### Table builders

| Function | Purpose |
|---|---|
| `create_deployments()`, `create_media()`, `create_observations()` | Build the three core tables (camera-trap convenience helpers). |
| `ctdp_build_table(schema, data, mapping, datetime_merges)` | Generic, schema-driven builder for any version/flavor, incl. custom columns. |
| `ctdp_apply_mapping(df, mapping)` | Rename source columns to Camtrap DP field names. |
| `ctdp_merge_datetime(df, date_col, time_col, target)` | Combine separate date/time columns into a datetime. |

### Schema & metadata-profile introspection (R6)

* **`TableSchema`** — a Frictionless table schema:
  `field_names()`, `required_field_names()`, `requirements()`, `empty_table()`,
  `coerce()`, `validate()`, `check_schema()`, `external_references()`,
  `semantic_only_fields()`.
* **`MetadataProfile`** — the package profile (required metadata structure).

### Data package builder (R6): `R6_CamtrapDP`

| Method | Purpose |
|---|---|
| `set_deployments()` / `set_media()` / `set_observations()` | Add a core table; coerces to schema types and validates. |
| `add_table()`, `set_custom()` | Add a custom / extra resource. |
| `add_contributors()`, `add_sources()`, `add_license()`, `set_project()`, `set_st()`, `set_taxon()`, `add_relatedIdentifiers()`, `add_references()`, `set_properties()`, `update_created()` | Metadata (`update_created()` sets the `created` timestamp). |
| `get_schema()`, `get_profile()`, `import_metadata()` | Load/cache a table schema or the package profile; import metadata from a list. |
| `metadata_requirements()`, `check_metadata()` | Profile-derived required-metadata checklist / check. |
| `check_relations()` | Primary/foreign-key integrity across tables. |
| `check_descriptor()`, `check_camtrap_profile()`, `external_references()` | Conformance pre-checks & URL-reference discovery. |
| `validate(relations, metadata, conformance, frictionless)` | Aggregate validation. |
| `out_camtrapdp(write, directory)` | Return / write the data package. |
| `validate_frictionless(directory)` | Run Python Frictionless on the written package. |

### Validation helpers

| Function | Purpose |
|---|---|
| `ctdp_validate_frictionless(directory)` | Validate an **existing** package on disk **without overwriting** it. |
| `ctdp_check_schema(x)` | Check a table schema is well-formed (types/constraints/keys). |
| `ctdp_schema_references(x)`, `ctdp_semantic_only_fields(x)` | List a schema's URL references / fields defined only by reference. |
| `ctdp_parse_frictionless(report)`, `ctdp_summarize_validation(issues)`, `ctdp_is_valid(issues)`, `ctdp_issues()` | Build / summarise / inspect the uniform issue table. |

Every check returns a tidy **issue table** with columns `source`, `field`,
`row`, `constraint`, `value` (the offending value), `severity`, `message`,
`engine`.

## Versions and flavors

* Camtrap DP **1.0 / 1.0.1 / 1.0.2** are supported. (The official `1.0` *profile*
  has an upstream bug that newer Frictionless rejects; `validate_frictionless()`
  works around it, but `1.0.1`+ is recommended.)
* **Bioacoustics (audio)** flavor is supported by pointing the package at its
  schemas/profile via `set_properties(schema_urls=, profile=)`. Audio data is
  media-based; see the acoustic vignette.

> **Tip:** pass datetimes as `POSIXct` — the package then writes each table's
> required datetime format (UTC offset, and fractional seconds where the schema
> needs them). Character datetimes are written as-is.

## Example data

* `Vdep`, `Vobs` — single camera-trap example: `Vdep` is the deployment of one
  camera trap (at NIES, Japan) and `Vobs` is the **video** data for it.
* `Idep`, `Iobs` — multiple camera-trap example (dummy): `Idep` is 10 deployments
  and `Iobs` is the **image** data for them.
* `Adep`, `Aobs` — acoustic example: deployment notebook / observation notebook
  with file names (`media` is derived from the file names).
* `datapackageVdata`, `datapackageIdata`, `datapackageAdata` — pre-built Camtrap
  DP objects, from `Vdep`/`Vobs`, `Idep`/`Iobs` and `Adep`/`Aobs` respectively
  (`datapackageAdata` is the bioacoustics flavor).

## Vignettes

Open a vignette with `vignette(...)`:

```r
library(R2camtrapdp)
vignette("Vignette_R2camtrapdp")               # multiple camera traps
vignette("Vignette_R2camtrapdp_SingleCamera")  # single camera trap
vignette("Vignette_R2camtrapdp_SchemaDriven")  # schema-driven workflow (also _ja)
vignette("Vignette_R2camtrapdp_Audio")         # acoustic (audio) data (also _ja)
```

## Dependencies

* **R**: R6, jsonlite, tibble, magrittr, lubridate, dplyr, tidyr, purrr, readr,
  httr, taxadb (for `set_taxon()`).
* **Suggests**: camtrapdp (the output object has class `camtrapdp`, for
  interoperability with the camtrapdp reader package), knitr, rmarkdown,
  testthat, jsonvalidate.
* **System (optional)**: Python with `frictionless` for `validate_frictionless()`.

## License

MIT — see [`LICENSE`](LICENSE) and [`LICENSE.md`](LICENSE.md).
