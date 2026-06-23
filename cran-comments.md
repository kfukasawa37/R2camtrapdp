## Resubmission

This is a resubmission addressing the CRAN reviewer's comments:

1. Removed the redundant "An R Package" from the start of the Title.
2. Put software, package and API names in single quotes in the Title and
   Description ('Camtrap DP', 'Frictionless', 'Python', 'frictionless').
3. Added a reference describing the methods to the Description:
   Bubnicki et al. (2023) <doi:10.1002/rse2.374>.
4. Added small executable examples to the Rd files of the exported functions
   (run during R CMD check where possible; calls that require internet or Python
   are wrapped in \dontrun{}).
5. Replaced console output via cat()/print() in R/camtrap_package.R with
   message(), so it can be suppressed.

(An earlier pre-test failure on Linux — the legacy time-zone alias "Japan" being
rejected by the current CCTZ/timechange backend — was also fixed by switching to
the canonical IANA name "Asia/Tokyo"; all vignettes now build on Linux, macOS
and Windows.)

## Submission summary

R2camtrapdp (version 2.0.0) converts arbitrary camera-trap (and acoustic)
spreadsheets into Camera Trap Data Packages (Camtrap DP), validating them against
the official Frictionless table schemas.

## R CMD check results

0 errors | 0 warnings | 1 note

The remaining NOTE is from "checking CRAN incoming feasibility":

* New submission.

* Any words still flagged as possibly misspelled (e.g. "schemas", "validator")
  are standard technical terms and are spelled correctly. Software/standard names
  such as 'Camtrap DP' and 'Frictionless' are now single-quoted in the Title and
  Description.

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
