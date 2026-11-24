## Submission summary

This is an update of R2camtrapdp from version 2.0.0 (on CRAN) to 2.1.0.
R2camtrapdp converts arbitrary camera-trap (and acoustic) spreadsheets into
Camera Trap Data Packages (Camtrap DP), validating them against the official
Frictionless table schemas.

Main changes (see NEWS.md for details):

* Bug fixes in `create_deployments()`, `create_media()`, `create_observations()`,
  `add_contributors()` and `import_metadata()`, reported and fixed by a
  contributor (added to Authors@R with role "ctb").
* `create_observations()` now checks that `observationID` is unique and that
  `mediaID` is given for media-level observations, instead of silently dropping
  rows. These are intentional behaviour changes, documented in NEWS.md.
* Validation now warns about line breaks in text values.
* A testthat suite and a new vignette ("Verifying the installation", in English
  and Japanese) were added.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Local: Windows 11, R 4.4.1
* win-builder: R Under development (unstable) (R-devel), x86_64-w64-mingw32
* GitHub Actions (r-lib/actions "check-standard"), all passing:
  * macOS-latest, R release
  * Windows-latest, R release
  * Ubuntu-latest, R devel
  * Ubuntu-latest, R release
  * Ubuntu-latest, R oldrel-1

## Notes for the reviewer

* The package can optionally use a Python helper (the 'frictionless' validator,
  declared in SystemRequirements) in `validate_frictionless()` /
  `ctdp_validate_frictionless()`. This is entirely optional: it is never invoked
  during R CMD check. Tests that need network access or Python are skipped on
  CRAN with `skip_on_cran()`, and the corresponding vignette chunks and examples
  are not evaluated (`eval = FALSE` / `\dontrun{}`). The tests that run on CRAN
  are offline and fast.

* The 'jsonvalidate' package is used only when available (guarded with
  `requireNamespace()`); it is listed under Suggests.

## Downstream dependencies

There are currently no downstream dependencies on CRAN.
