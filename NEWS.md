# R2camtrapdp (development version)

* `create_deployments()` no longer drops `cameraID` when `cameraModel` is omitted (#34).
* `create_observations()` now accepts `"setup"` and `"calibration"` in `cameraSetupType` (#34).
* `add_contributors()` no longer adds empty contributors for a 0-row table (#34).
* `import_metadata()` no longer fails when the input has no importable properties (#34).

# R2camtrapdp 2.0.0

* Initial CRAN submission.
