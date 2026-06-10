## Submission summary

This is a new submission of R2camtrapdp (version 2.0.0), an R package that
converts arbitrary camera-trap (and acoustic) spreadsheets into Camera Trap Data
Packages (Camtrap DP), validating them against the official Frictionless table
schemas.

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

* Local: Windows 11, R <your local R version, e.g. 4.5.3>
* win-builder: R Under development (unstable) (R-devel), x86_64-w64-mingw32
* <add any others actually run, e.g.:>
  * GitHub Actions: ubuntu-latest (R-release, R-devel), macOS-latest, windows-latest
  * R-hub

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