#' @title Python Frictionless validation (shared internals + validate-only API)
#' @description Helpers to run the Python `frictionless` validator on a written
#'   data package and parse the report. [ctdp_validate_frictionless()] validates
#'   an **existing** data package directory **without writing or overwriting**
#'   anything (use this when you only want to validate a package that was created
#'   elsewhere). The [R6_CamtrapDP] method `validate_frictionless()` reuses the
#'   same internals but writes the package from the object first (its `write`
#'   argument).
#' @name ctdp-frictionless
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr GET status_code content
NULL

#' Resolve the path to the Python helper script
#' @keywords internal
.ctdp_resolve_frictionless_script <- function(script = NULL) {
  if (is.null(script)) script <- getOption("camtrapdp.frictionless_script", NULL)
  if (is.null(script)) {
    sys_script <- ""
    pkg <- tryCatch(utils::packageName(environment()), error = function(e) NULL)
    for (p in unique(c(pkg, "R2camtrapdp"))) {
      if (is.null(p) || !nzchar(p)) next
      cand <- system.file("python", "frictionless_validate.py", package = p)
      if (nzchar(cand)) { sys_script <- cand; break }
    }
    script <- if (nzchar(sys_script)) sys_script else "python/frictionless_validate.py"
  }
  if (!file.exists(script)) {
    stop(sprintf(paste0(
      "Frictionless helper script not found (looked at: %s).\n",
      "Fix one of:\n",
      "  - place 'frictionless_validate.py' at inst/python/ and reinstall the package\n",
      "    (system.file(\"python\", \"frictionless_validate.py\", package=<pkg>) must resolve), or\n",
      "  - pass script = '/abs/path/to/frictionless_validate.py', or\n",
      "  - options(camtrapdp.frictionless_script = '/abs/path/to/frictionless_validate.py')."),
      script), call. = FALSE)
  }
  script
}

#' Extract the first complete top-level JSON object from helper output
#' @keywords internal
.ctdp_extract_json <- function(txt) {
  starts <- gregexpr("\\{", txt)[[1]]
  if (starts[1] == -1) return(NULL)
  for (s in starts) {
    depth <- 0L; in_str <- FALSE; esc <- FALSE
    chars <- strsplit(substring(txt, s), "")[[1]]
    for (i in seq_along(chars)) {
      ch <- chars[i]
      if (esc) { esc <- FALSE; next }
      if (ch == "\\") { esc <- TRUE; next }
      if (ch == '"') { in_str <- !in_str; next }
      if (in_str) next
      if (ch == "{") depth <- depth + 1L
      else if (ch == "}") {
        depth <- depth - 1L
        if (depth == 0L) {
          cand <- substring(txt, s, s + i - 1L)
          ok <- tryCatch({ jsonlite::fromJSON(cand, simplifyVector = FALSE); TRUE },
                         error = function(e) FALSE)
          if (ok) return(cand)
          break
        }
      }
    }
  }
  NULL
}

#' Work around the malformed `#$defs/version` $ref in the Camtrap DP 1.0 profile
#'
#' Given a package directory and the profile URL declared in its descriptor,
#' returns the descriptor file name to validate: `"datapackage.json"` if no fix
#' is needed, or a separate `"datapackage.validate.json"` (pointing at a locally
#' corrected profile) when the profile contains the malformed ref. The original
#' `datapackage.json` and CSVs are never modified.
#' @keywords internal
.ctdp_patch_descriptor <- function(directory, profile) {
  default <- "datapackage.json"
  if (is.null(profile) || !grepl("^https?://", profile)) return(default)
  txt <- tryCatch({
    resp <- httr::GET(profile)
    if (httr::status_code(resp) == 200)
      httr::content(resp, as = "text", encoding = "UTF-8") else NULL
  }, error = function(e) NULL)
  if (is.null(txt) || !grepl("#$defs/", txt, fixed = TRUE)) return(default)
  fixed <- gsub("#$defs/", "#/$defs/", txt, fixed = TRUE)
  writeLines(fixed, file.path(directory, "camtrap-dp-profile.patched.json"), useBytes = TRUE)
  desc <- jsonlite::fromJSON(file.path(directory, default), simplifyVector = FALSE)
  desc$profile <- "camtrap-dp-profile.patched.json"
  validate_name <- "datapackage.validate.json"
  writeLines(jsonlite::toJSON(desc, pretty = NULL, null = "null", na = "null", auto_unbox = TRUE),
             file.path(directory, validate_name), useBytes = TRUE)
  message(sprintf(paste0("Note: the Camtrap DP profile (%s) has a malformed internal $ref; ",
                         "validating against a locally corrected copy."), profile))
  validate_name
}

#' Run the helper and return the parsed Frictionless report (a list)
#' @keywords internal
.ctdp_run_frictionless <- function(directory, descriptor, python, script) {
  dp_path <- file.path(directory, descriptor)
  out <- suppressWarnings(system2(python, args = c(shQuote(script), shQuote(dp_path)),
                                  stdout = TRUE, stderr = TRUE))
  txt <- paste(out, collapse = "\n")
  json_txt <- .ctdp_extract_json(txt)
  if (is.null(json_txt)) {
    stop(sprintf("Could not parse Frictionless output. Is 'frictionless' installed (pip install frictionless)?\nOutput:\n%s", txt),
         call. = FALSE)
  }
  report <- jsonlite::fromJSON(json_txt, simplifyVector = FALSE)
  if ("error" %in% names(report)) {
    stop(sprintf("Frictionless helper error: %s", report[["error"]]), call. = FALSE)
  }
  report
}

#' Validate an existing Camtrap DP directory with Python Frictionless
#'
#' Validates a data package that already exists on disk (e.g. created by another
#' tool or in a previous run) **without writing or overwriting** the
#' `datapackage.json` or the CSV files. This is the validate-only counterpart of
#' the `R6_CamtrapDP$validate_frictionless()` method, which (by default) rewrites
#' the package from the R object before validating.
#'
#' @param directory Directory containing `datapackage.json` and its CSV files.
#' @param python Path to the Python interpreter (with `frictionless` installed).
#' @param script Path to `frictionless_validate.py`; resolved automatically when
#'   `NULL` (option, then installed `inst/python/`, then loose-source path).
#' @param patch_profile If `TRUE`, work around the malformed `$ref` in the
#'   Camtrap DP 1.0 profile by validating against a locally corrected copy. This
#'   writes two **new** helper files (`camtrap-dp-profile.patched.json`,
#'   `datapackage.validate.json`) into `directory` only when the profile is
#'   actually malformed; it never modifies `datapackage.json` or the CSVs. Set
#'   `FALSE` to keep the directory strictly read-only.
#' @param summarize Whether to print a summary.
#' @return An issue table (see [ctdp_issues()]); engine `"frictionless"`.
#' @examples
#' \dontrun{
#' # Validate an existing Camtrap DP on disk (needs Python with 'frictionless'):
#' issues <- ctdp_validate_frictionless("path/to/camtrapdp", python = "python")
#' ctdp_is_valid(issues)
#' }
#' @export
ctdp_validate_frictionless <- function(directory, python = "python", script = NULL,
                                       patch_profile = TRUE, summarize = TRUE) {
  if (!dir.exists(directory)) stop("Directory not found: ", directory, call. = FALSE)
  dp_json <- file.path(directory, "datapackage.json")
  if (!file.exists(dp_json)) {
    stop("No 'datapackage.json' found in: ", directory, call. = FALSE)
  }
  script <- .ctdp_resolve_frictionless_script(script)
  desc <- jsonlite::fromJSON(dp_json, simplifyVector = FALSE)
  paths <- character(0)
  for (r in (desc$resources %||% list())) {
    if (!is.null(r$name) && !is.null(r$path)) paths[[r$name]] <- r$path
  }
  descriptor <- if (patch_profile) .ctdp_patch_descriptor(directory, desc$profile) else "datapackage.json"
  report <- .ctdp_run_frictionless(directory, descriptor, python, script)
  issues <- ctdp_parse_frictionless(report, resource_paths = if (length(paths)) paths else NULL,
                                    descriptor = desc)
  if (summarize) ctdp_summarize_validation(issues)
  issues
}
