# R2camtrapdp 2.1.0

## Breaking changes

* `create_observations()` no longer silently drops rows that share an `observationID`. With `omitduplicate = TRUE`, only rows identical in every column are removed; if an `observationID` is still shared by different rows (e.g. several species in one image), an error is raised asking for a separate ID per species, sex or life stage (#35).

* `create_observations()` now requires `mediaID` for rows with `observationLevel = "media"`. The previous "either `mediaID` or `eventID`" check, which could be bypassed by omitting an argument, has been removed; event-level rows require neither, as in Camtrap DP (#34, #35).

## New features

* Validation now warns (rule `lineBreak`, severity `"warning"`) when a text value contains a line break. Such values are valid CSV but often break line-based tools and spreadsheets (#35).

## Bug fixes

* `create_deployments()` no longer drops `cameraID` when `cameraModel` is omitted (#34).

* `create_observations()` now accepts `"setup"` and `"calibration"` in `cameraSetupType` (#34).

* `create_deployments()`, `create_media()` and `create_observations()` now accept `NA` in the optional fields `timestampIssues`, `baitUse`, `captureMethod`, `favorite`, `cameraSetupType` and `classificationMethod` (#34).

* `add_contributors()` no longer adds empty contributors for a 0-row table (#34).

* `import_metadata()` no longer fails when the input has no importable properties (#34).

* Thanks to @northfox for reporting and fixing these issues.

## Documentation and tests

* New vignette "Verifying the installation" (English and Japanese) showing how to check conversion to all supported Camtrap DP versions.

* Added a testthat suite covering the table constructors, value-level validation and conversion to Camtrap DP 1.0, 1.0.1, 1.0.2 and the bioacoustics variant, validated in R and with Frictionless (network and Python tests are skipped on CRAN).

# R2camtrapdp 2.0.0

* Initial CRAN submission.