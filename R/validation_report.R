#' @title Validation issue representation and reporting
#' @description A uniform structure for validation issues, whether they are
#'   produced by the R-side schema/relation checks or parsed from a Python
#'   Frictionless validation report. An "issue table" is a [tibble::tibble]
#'   with the columns described in [ctdp_issues()].
#' @name ctdp-validation-report
#' @import tibble
NULL

#' Create an issue table
#'
#' Constructs a tibble of validation issues. All arguments are recycled to a
#' common length. Use [ctdp_no_issues()] for an empty table with the right
#' columns.
#'
#' @param source Character. Where the issue lives, e.g. `"deployments.csv"` or
#'   `"datapackage.json"`.
#' @param location_type Character. One of `"table"`, `"schema"`, `"relation"`,
#'   `"package"`.
#' @param field Character. Column / field name, or `NA`.
#' @param row Integer. 1-based data row number, or `NA`.
#' @param constraint Character. The violated rule, e.g. `"required"`, `"unique"`,
#'   `"enum"`, `"minimum"`, `"maximum"`, `"minLength"`, `"maxLength"`,
#'   `"pattern"`, `"type"`, `"format"`, `"primaryKey"`, `"foreignKey"`.
#' @param severity Character. `"error"` or `"warning"`.
#' @param message Character. Human-readable description.
#' @param engine Character. `"R"` or `"frictionless"`.
#' @param value Character. The offending value(s), when known (e.g. the failing
#'   cell value, or the value resolved from the descriptor for a metadata error).
#' @return A tibble with one row per issue.
#' @examples
#' ctdp_issues(source = "deployments", field = "latitude",
#'             constraint = "required", severity = "error",
#'             message = "latitude is missing")
#' @export
ctdp_issues <- function(source, location_type = NA_character_, field = NA_character_,
                        row = NA_integer_, constraint = NA_character_,
                        severity = "error", message = NA_character_, engine = "R",
                        value = NA_character_) {
  tibble::tibble(
    source        = as.character(source),
    location_type = as.character(location_type),
    field         = as.character(field),
    row           = as.integer(row),
    constraint    = as.character(constraint),
    value         = as.character(value),
    severity      = as.character(severity),
    message       = as.character(message),
    engine        = as.character(engine)
  )
}

#' An empty issue table
#' @return A 0-row issue tibble.
#' @examples
#' ctdp_no_issues()
#' @export
ctdp_no_issues <- function() {
  ctdp_issues(source = character(0), location_type = character(0),
              field = character(0), row = integer(0), constraint = character(0),
              severity = character(0), message = character(0), engine = character(0),
              value = character(0))
}

#' Row-bind several issue tables
#' @param ... Issue tables (tibbles), or `NULL`s which are dropped.
#' @return A single combined issue tibble.
#' @examples
#' a <- ctdp_issues(source = "media", constraint = "required",
#'                  severity = "error", message = "missing mediaID")
#' ctdp_bind_issues(a, ctdp_no_issues())
#' @export
ctdp_bind_issues <- function(...) {
  parts <- Filter(Negate(is.null), list(...))
  parts <- Filter(function(x) nrow(x) > 0, parts)
  if (length(parts) == 0) return(ctdp_no_issues())
  do.call(rbind, parts)
}

#' Summarise an issue table to the console
#'
#' Prints a grouped summary (errors / warnings per source) followed by a
#' detailed listing showing the offending file, field, row and message.
#'
#' @param issues An issue table from [ctdp_issues()].
#' @param max_detail Maximum number of detail rows to print per source.
#' @return The issue table, invisibly.
#' @examples
#' issues <- ctdp_issues(source = "deployments", field = "latitude",
#'                       constraint = "minimum", severity = "error",
#'                       message = "latitude below -90", value = "-100")
#' ctdp_summarize_validation(issues)
#' @export
ctdp_summarize_validation <- function(issues, max_detail = 50L) {
  if (is.null(issues) || nrow(issues) == 0) {
    cat("\u2714 No validation issues found.\n")
    return(invisible(issues))
  }
  n_err <- sum(issues$severity == "error", na.rm = TRUE)
  n_warn <- sum(issues$severity == "warning", na.rm = TRUE)
  cat(sprintf("\u2716 Validation found %d error(s) and %d warning(s).\n\n", n_err, n_warn))

  # Summary by source + severity
  cat("Summary by location:\n")
  srcs <- unique(issues$source)
  for (s in srcs) {
    sub <- issues[issues$source == s, , drop = FALSE]
    cat(sprintf("  %-22s errors=%d warnings=%d\n", s,
                sum(sub$severity == "error", na.rm = TRUE),
                sum(sub$severity == "warning", na.rm = TRUE)))
  }
  cat("\nDetails:\n")
  for (s in srcs) {
    sub <- issues[issues$source == s, , drop = FALSE]
    cat(sprintf("  [%s]\n", s))
    n_show <- min(nrow(sub), max_detail)
    for (i in seq_len(n_show)) {
      r <- sub[i, ]
      loc <- character(0)
      if (!is.na(r$field) && nzchar(r$field)) loc <- c(loc, paste0("field=", r$field))
      if (!is.na(r$row)) loc <- c(loc, paste0("row=", r$row))
      if (!is.na(r$constraint) && nzchar(r$constraint)) loc <- c(loc, paste0("rule=", r$constraint))
      if (!is.null(r$value) && !is.na(r$value) && nzchar(r$value)) loc <- c(loc, paste0("value=", r$value))
      loc_str <- if (length(loc)) paste0(" (", paste(loc, collapse = ", "), ")") else ""
      cat(sprintf("    - [%s|%s]%s %s\n",
                  toupper(substr(r$severity, 1, 1)), r$engine, loc_str, r$message))
    }
    if (nrow(sub) > n_show) {
      cat(sprintf("    ... and %d more in %s\n", nrow(sub) - n_show, s))
    }
  }
  invisible(issues)
}

#' Parse a Frictionless validation report into an issue table
#'
#' Accepts either a parsed list (from [jsonlite::fromJSON]) or a JSON string,
#' supporting both Frictionless v4 and v5 report layouts.
#'
#' @param report A Frictionless report as a list or JSON string.
#' @param resource_paths Optional named character vector mapping resource names
#'   to file paths (e.g. `c(deployments = "deployments.csv")`) used to give a
#'   friendlier `source`. Falls back to the report's own `place`/`name`.
#' @param descriptor Optional parsed `datapackage.json` (list). When supplied,
#'   the offending value of a **metadata** (package) error is resolved from the
#'   descriptor using the property path in the error note (e.g.
#'   `contributors[].email`) and placed in the issue's `value` column. For
#'   table/cell errors the failing `cell` value is used directly.
#' @return An issue table (includes a `value` column with the offending value(s)
#'   when known).
#' @importFrom jsonlite fromJSON
#' @examples
#' # A valid Frictionless report parses to an empty issue table:
#' ctdp_parse_frictionless(list(valid = TRUE, tasks = list()))
#' \dontrun{
#' # Typically the report comes from validate_frictionless() / the Python validator:
#' issues <- ctdp_validate_frictionless("path/to/package")
#' }
#' @export
ctdp_parse_frictionless <- function(report, resource_paths = NULL, descriptor = NULL) {
  if (is.character(report)) {
    report <- jsonlite::fromJSON(paste(report, collapse = "\n"), simplifyVector = FALSE)
  }
  if (is.null(report)) return(ctdp_no_issues())

  # Helper to resolve a friendly source label
  resolve_src <- function(name, place) {
    if (!is.null(resource_paths) && !is.null(name) && name %in% names(resource_paths)) {
      return(unname(resource_paths[[name]]))
    }
    place %||% name %||% "datapackage.json"
  }

  collect <- function(errs, src) {
    if (is.null(errs) || length(errs) == 0) return(NULL)
    parts <- lapply(errs, function(e) {
      note <- e$note %||% e$message %||% e$description %||% e$title %||% "Frictionless error"
      is_cell <- !is.null(e$rowNumber) || !is.null(e$fieldName)
      # Offending value: cell value for table errors; for metadata errors resolve
      # the property path in the note against the descriptor (if provided).
      value <- NA_character_
      field <- e$fieldName %||% NA_character_
      if (is_cell) {
        if (!is.null(e$cell)) value <- as.character(e$cell)
      } else {
        path <- .ctdp_note_path(note)
        if (!is.na(path)) {
          if (is.na(field)) field <- path
          if (!is.null(descriptor)) value <- .ctdp_resolve_descriptor_path(descriptor, path)
        }
      }
      ctdp_issues(
        source        = src,
        location_type = if (is_cell) "table" else "package",
        field         = field,
        row           = if (!is.null(e$rowNumber)) as.integer(e$rowNumber) else NA_integer_,
        constraint    = e$type %||% e$code %||% NA_character_,
        value         = value,
        severity      = "error",
        message       = note,
        engine        = "frictionless"
      )
    })
    do.call(rbind, parts)
  }

  out <- list()

  # Package-level errors (both v4 and v5 may carry a top-level `errors`)
  out[[length(out) + 1]] <- collect(report$errors, "datapackage.json")

  # Per-resource tasks
  tasks <- report$tasks %||% report$valid  # `valid` is never a list; guards NULL
  if (is.list(report$tasks)) {
    for (task in report$tasks) {
      name  <- task$name %||% (task$resource$name %||% NULL)
      place <- task$place %||% (task$resource$path %||% NULL)
      src <- resolve_src(name, place)
      out[[length(out) + 1]] <- collect(task$errors, src)
    }
  }
  do.call(ctdp_bind_issues, out)
}

#' Did a validation pass (no errors)?
#' @param issues An issue table.
#' @return `TRUE` if there are no `"error"` severity rows.
#' @examples
#' ctdp_is_valid(ctdp_no_issues())   # TRUE
#' bad <- ctdp_issues(source = "deployments", constraint = "required",
#'                    severity = "error", message = "missing")
#' ctdp_is_valid(bad)                # FALSE
#' @export
ctdp_is_valid <- function(issues) {
  if (is.null(issues) || nrow(issues) == 0) return(TRUE)
  !any(issues$severity == "error", na.rm = TRUE)
}

#' Extract a property path from a Frictionless metadata-error note
#'
#' Handles the common JSON-Schema error note shapes, e.g.
#' `property "contributors[].email" is not valid "email"`,
#' `... at property 'temporal/start'`, and `'taxonomic' is a required property`.
#' @param note The error note/message string.
#' @return A property path string, or `NA_character_`.
#' @keywords internal
.ctdp_note_path <- function(note) {
  if (is.null(note) || is.na(note)) return(NA_character_)
  pats <- c('property "([^"]+)"', "at property '([^']+)'",
            "property '([^']+)'", "^'([^']+)' is a required")
  for (pat in pats) {
    m <- regmatches(note, regexec(pat, note))[[1]]
    if (length(m) >= 2 && nzchar(m[2])) return(m[2])
  }
  NA_character_
}

#' Resolve a property path against a parsed descriptor
#'
#' Walks a parsed `datapackage.json` list following a path that may use `.` or
#' `/` separators and `name[]` (all array elements) or `name[n]` (a 0-based
#' index). Returns the value(s) found, collapsed with ` | `.
#' @param descriptor A parsed descriptor (list).
#' @param path A property path, e.g. `contributors[].email` or `temporal/start`.
#' @return A length-one character of the value(s), or `NA_character_`.
#' @importFrom jsonlite toJSON
#' @keywords internal
.ctdp_resolve_descriptor_path <- function(descriptor, path) {
  if (is.null(descriptor) || is.null(path) || is.na(path) || !nzchar(path)) return(NA_character_)
  segs <- unlist(strsplit(path, "[./]"))
  segs <- segs[nzchar(segs)]
  if (length(segs) == 0) return(NA_character_)
  nodes <- list(descriptor)
  for (seg in segs) {
    m <- regmatches(seg, regexec("^([^\\[]*)(\\[([0-9]*)\\])?$", seg))[[1]]
    if (length(m) == 0) return(NA_character_)
    key <- m[2]; bracket <- nzchar(m[1]) && grepl("\\[", seg); idx <- m[4]
    nxt <- list()
    for (nd in nodes) {
      child <- if (nzchar(key)) (if (is.list(nd)) nd[[key]] else NULL) else nd
      if (is.null(child)) next
      if (bracket) {
        if (nzchar(idx)) {
          ii <- as.integer(idx) + 1L
          if (is.list(child) && ii >= 1 && ii <= length(child)) nxt <- c(nxt, list(child[[ii]]))
        } else if (is.list(child)) {
          for (el in child) nxt <- c(nxt, list(el))
        }
      } else {
        nxt <- c(nxt, list(child))
      }
    }
    nodes <- nxt
  }
  vals <- vapply(nodes, function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.list(x) || length(x) > 1)
      return(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")))
    as.character(x)
  }, character(1))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) return(NA_character_)
  paste(unique(vals), collapse = " | ")
}
