#' @title Frictionless conformance pre-checks (R-side)
#' @description The R-side validation in this package normally *trusts* that the
#'   supplied table schema is a well-formed Frictionless Table Schema. These
#'   helpers add an R-side pre-check of that assumption -- catching, before the
#'   Python Frictionless step, the kinds of structural problems Frictionless
#'   itself rejects (unsupported field `type`, a `constraint` that is not valid
#'   for a field's type, primary/foreign keys that reference undefined fields,
#'   etc.). The authoritative check remains [R6_CamtrapDP]'s
#'   `validate_frictionless()`.
#' @name ctdp-conformance
#' @import tibble
NULL

#' Field types supported by the Frictionless Table Schema specification
#' @keywords internal
.ctdp_frictionless_types <- function() {
  c("string", "number", "integer", "boolean", "object", "array", "date", "time",
    "datetime", "year", "yearmonth", "duration", "geopoint", "geojson", "any")
}

#' Constraint keys defined by the Frictionless Table Schema specification
#' @keywords internal
.ctdp_known_constraints <- function() {
  c("required", "unique", "enum", "minLength", "maxLength", "minimum", "maximum", "pattern")
}

#' Constraint keys that are valid for a given field type
#' @param type A Frictionless field type.
#' @keywords internal
.ctdp_allowed_constraints <- function(type) {
  base <- c("required", "unique", "enum")
  type <- type %||% "any"
  if (type == "string") return(c(base, "minLength", "maxLength", "pattern"))
  if (type %in% c("number", "integer", "date", "time", "datetime",
                  "year", "yearmonth", "duration")) return(c(base, "minimum", "maximum"))
  if (type %in% c("array", "object")) return(c(base, "minLength", "maxLength"))
  if (type %in% c("boolean", "geopoint", "geojson")) return(base)
  # any / unknown type: be permissive (the unknown type itself is flagged elsewhere)
  .ctdp_known_constraints()
}

#' Check that a Table Schema is well-formed per the Frictionless spec
#'
#' Verifies the schema structure that Frictionless requires of any Table Schema,
#' independently of the data: non-empty `fields`, each field with a `name` and a
#' supported `type`, constraints that are valid for the field's type, unique
#' field names, and primary/foreign keys that reference defined fields.
#'
#' @param x A [TableSchema] or a parsed table-schema list.
#' @return An issue table (see [ctdp_issues()]). Constraint codes:
#'   `schema-structure`, `schema-type`, `schema-constraint`, `schema-key`.
#' @examples
#' sch <- list(name = "deployments",
#'   fields = list(
#'     list(name = "deploymentID", type = "string", constraints = list(required = TRUE)),
#'     list(name = "latitude", type = "number", constraints = list(minimum = -90, maximum = 90))),
#'   primaryKey = "deploymentID")
#' ctdp_check_schema(sch)
#' @export
ctdp_check_schema <- function(x) {
  if (inherits(x, "TableSchema")) { raw <- x$raw; resource <- x$resource %||% x$name }
  else if (is.list(x)) { raw <- x; resource <- x$name }
  else stop("'x' must be a TableSchema or a parsed schema list.", call. = FALSE)

  src <- paste0(resource %||% "schema", " (schema)")
  issues <- list()
  add <- function(field, constraint, message, severity = "error") {
    issues[[length(issues) + 1]] <<- ctdp_issues(
      source = src, location_type = "schema", field = field, constraint = constraint,
      severity = severity, message = message)
  }

  fields <- raw$fields
  if (is.null(fields) || length(fields) == 0) {
    add(NA_character_, "schema-structure", "Schema has no 'fields' array.")
    return(do.call(ctdp_bind_issues, issues))
  }

  known_types <- .ctdp_frictionless_types()
  known_cons <- .ctdp_known_constraints()
  fnames <- character(0)
  for (f in fields) {
    nm <- f$name
    if (is.null(nm) || !nzchar(nm)) {
      add(NA_character_, "schema-structure", "A field is missing its 'name'.")
      next
    }
    fnames <- c(fnames, nm)
    ty <- f$type %||% "string"
    if (!is.null(f$type) && !(f$type %in% known_types)) {
      add(nm, "schema-type",
          sprintf("Field '%s': type '%s' is not a supported Frictionless type.", nm, f$type))
    }
    cons <- f$constraints %||% list()
    allowed <- .ctdp_allowed_constraints(ty)
    for (ck in names(cons)) {
      if (ck %in% known_cons && !(ck %in% allowed)) {
        add(nm, "schema-constraint",
            sprintf("Field '%s': constraint '%s' is not supported by type '%s'.", nm, ck, ty))
      }
    }
  }

  dup <- unique(fnames[duplicated(fnames)])
  for (d in dup) add(d, "schema-structure", sprintf("Field name '%s' is duplicated.", d))

  pk <- .ctdp_as_vector(raw$primaryKey)
  for (k in pk) if (!(k %in% fnames)) {
    add(k, "schema-key", sprintf("primaryKey '%s' is not a defined field.", k))
  }
  for (fk in (raw$foreignKeys %||% list())) {
    for (k in .ctdp_as_vector(fk$fields)) if (!(k %in% fnames)) {
      add(k, "schema-key", sprintf("foreignKey field '%s' is not a defined field.", k))
    }
  }
  do.call(ctdp_bind_issues, issues)
}
