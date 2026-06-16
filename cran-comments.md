## Resubmission

This is a resubmission. In the previous CRAN pre-test, vignette re-building
failed on Linux (Debian) with
`CCTZ: Unrecognized output timezone: "Japan"`. The cause was the use of the
legacy time-zone alias `"Japan"`, which the current CCTZ/timechange backend on
that platform does not recognise. We replaced every occurrence with the
canonical IANA name `"Asia/Tokyo"` (identical UTC+09:00 behaviour) throughout the
R code, vignettes and examples. All vignettes now re-build on Linux, macOS and
Windows.

## Submission summary

R2camtrapdp (version 2.0.0) converts arbitrary camera-trap (and acoustic)
spreadsheets into Camera Trap Data Packages (Camtrap DP), validating them against
the official Frictionless table schemas.

## R CMD check results

0 errors | 0 warnings | 1 note

The remaining NOTE is from "checking CRAN incoming feasibility":

* New submission.

* Possibly misspelled words in DESCRIPTION: 'Camtrap', 'schemas', 'validator'.
  These are spelled correctly:
  - "Camtrap" is part of "Camtrap DP", the proper name of the data standard
    (https://camtrap-dp.tdwg.org/).
  - "schemas" and "validator" are standard technical terms used in the
    Frictionless Data framework.

## Test environments

* Local: Windows 11, R <4.4.1>
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
  during R CMD check. All such code paths are guarded, and the examples, tests
  and vignette chunks that would require network access or Python are not
  executed on CRAN (they use `eval = FALSE` / `\dontrun{}` / `skip_on_cran()`).

* The 'jsonvalidate' package is used only when available (guarded with
  `requireNamespace()`); it is listed under Suggests.

## Downstream dependencies

There are currently no downstream dependencies (this is a new package).
