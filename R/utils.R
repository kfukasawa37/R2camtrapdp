#' @title Internal utilities for schema-driven Camtrap DP conversion
#' @description Helper functions shared across the package: a null-coalescing
#'   operator, a Frictionless-type to R-type mapper, and small convenience
#'   helpers used by [TableSchema] and [R6_CamtrapDP].
#' @keywords internal
#' @name ctdp-utils
NULL

#' Null-coalescing operator
#'
#' Returns `a` unless it is `NULL` (or zero-length), otherwise `b`.
#' @param a,b Values.
#' @keywords internal
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}

#' Default Camtrap DP datetime format
#'
#' The Frictionless `datetime` format used by the Camtrap DP table schemas.
#' @keywords internal
.ctdp_datetime_format <- function() "%Y-%m-%dT%H:%M:%S%z"

#' Default Camtrap DP date / time formats
#' @keywords internal
.ctdp_date_format <- function() "%Y-%m-%d"
#' @rdname dot-ctdp_date_format
#' @keywords internal
.ctdp_time_format <- function() "%H:%M:%S"

#' Convert a strptime-style date/time format to a validation regex
#'
#' R's own `strptime` does not understand all the tokens used in Frictionless /
#' Camtrap DP formats (notably `%f` for fractional seconds, used by the
#' bioacoustics schemas: `%Y-%m-%dT%H:%M:%S.%f%z`). Rather than rely on
#' `strptime`, build an anchored regex from the format so any version's format
#' can be checked.
#' @param fmt A strptime-style format string.
#' @return An anchored PCRE regex string.
#' @keywords internal
.ctdp_temporal_regex <- function(fmt) {
  chars <- strsplit(fmt, "")[[1]]
  out <- character(0); i <- 1L; n <- length(chars)
  meta <- c(".", "\\", "+", "*", "?", "(", ")", "[", "]", "{", "}", "^", "$", "|")
  while (i <= n) {
    ch <- chars[i]
    if (ch == "%" && i < n) {
      tok <- chars[i + 1L]
      rep <- switch(tok,
        Y = "[0-9]{4}", y = "[0-9]{2}", m = "[0-9]{2}", d = "[0-9]{2}",
        H = "[0-9]{2}", M = "[0-9]{2}", S = "[0-9]{2}", j = "[0-9]{3}",
        f = "[0-9]+", z = "([+-][0-9]{2}:?[0-9]{2}|Z)", Z = "[A-Za-z0-9/_+-]+",
        paste0("%", tok))
      out <- c(out, rep); i <- i + 2L
    } else {
      out <- c(out, if (ch %in% meta) paste0("\\", ch) else ch)
      i <- i + 1L
    }
  }
  paste0("^", paste(out, collapse = ""), "$")
}

#' Ensure a datetime/time string carries a fractional-seconds part
#'
#' Inserts `.000` after `HH:MM:SS` when no fractional part is present, so values
#' conform to a schema format that requires fractional seconds (`%f`), e.g. the
#' bioacoustics `media.timestamp` / `observations.eventStart` format
#' `%Y-%m-%dT%H:%M:%S.%f%z`. Values that already have a fractional part, missing
#' values, and non-matching strings are returned unchanged.
#' @param x A character vector.
#' @keywords internal
.ctdp_ensure_fractional <- function(x) {
  sub("([0-9]{2}:[0-9]{2}:[0-9]{2})(?![.0-9])", "\\1.000", x, perl = TRUE)
}

#' Map a Frictionless field type to an R storage class
#'
#' Date / datetime / time values are stored as character strings formatted in
#' the schema's expected format so that the values written to CSV match exactly
#' what Frictionless validates.
#' @param type A Frictionless field `type` string.
#' @return A length-one character vector naming an R class.
#' @keywords internal
.ctdp_r_class <- function(type) {
  type <- type %||% "string"
  switch(type,
    string   = "character",
    number   = "numeric",
    integer  = "integer",
    boolean  = "logical",
    date     = "character",
    datetime = "character",
    time     = "character",
    year     = "integer",
    yearmonth = "character",
    # any, object, array, geopoint, geojson, duration -> character
    "character"
  )
}

#' Create an empty typed vector for a Frictionless field type
#' @param type A Frictionless field `type` string.
#' @param n Length of the vector to create.
#' @keywords internal
.ctdp_empty_vec <- function(type, n = 0L) {
  switch(.ctdp_r_class(type),
    character = character(n),
    numeric   = numeric(n),
    integer   = integer(n),
    logical   = logical(n),
    character(n)
  )
}

#' Create a typed NA-filled vector for a Frictionless field type
#' @param type A Frictionless field `type` string.
#' @param n Length of the vector to create.
#' @keywords internal
.ctdp_na_vec = function(type, n = 0L) {
  switch(.ctdp_r_class(type),
    character = rep(NA_character_, n),
    numeric   = rep(NA_real_, n),
    integer   = rep(NA_integer_, n),
    logical   = rep(NA, n),
    rep(NA_character_, n)
  )
}

#' Default Camtrap DP schema / profile URL templates
#'
#' Each template contains the literal `<version>` placeholder, substituted with
#' the requested Camtrap DP version at load time.
#' @keywords internal
.ctdp_default_urls <- function() {
  base <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/<version>/"
  list(
    deployments  = paste0(base, "deployments-table-schema.json"),
    media        = paste0(base, "media-table-schema.json"),
    observations = paste0(base, "observations-table-schema.json"),
    profile      = paste0(base, "camtrap-dp-profile.json")
  )
}

#' Deterministic short hash of a string (djb2)
#'
#' Used to disambiguate schema cache files: two different schema URLs that share
#' the same (version, resource) -- e.g. the camera-trap and the bioacoustics
#' "deployments" schemas for version 1.0.2 -- must not collide in the cache.
#' Avoids `digest`/random/time so it is reproducible.
#' @param s A string.
#' @return A 10-digit, zero-padded decimal token (filename-safe).
#' @keywords internal
.ctdp_str_hash <- function(s) {
  h <- 5381
  for (b in utf8ToInt(s)) h <- (h * 33 + b) %% 4294967296
  sprintf("%010.0f", h)
}

#' Substitute the `<version>` placeholder in a URL template
#' @param template A URL containing `<version>`.
#' @param version The Camtrap DP version string.
#' @keywords internal
.ctdp_sub_version <- function(template, version) {
  sub("<version>", version, template, fixed = TRUE)
}

#' Coerce a value that may be a list (from `jsonlite::fromJSON(simplifyVector =
#' FALSE)`) into a plain atomic vector.
#' @param x A value, possibly a list of scalars.
#' @keywords internal
.ctdp_as_vector <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x)) return(unlist(x, use.names = FALSE))
  x
}

#' Infer a Frictionless field type from an R vector
#' @param x A vector.
#' @return A Frictionless `type` string.
#' @keywords internal
.ctdp_infer_type <- function(x) {
  if (is.logical(x)) return("boolean")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("number")
  if (inherits(x, "Date")) return("date")
  if (inherits(x, "POSIXt")) return("datetime")
  "string"
}

#' Build a Frictionless field definition for a custom column
#' @param name Column name.
#' @param x The column vector (used to infer the type).
#' @keywords internal
.ctdp_infer_field <- function(name, x) {
  list(name = name, type = .ctdp_infer_type(x),
       description = "Custom field added from the input data.")
}

#' Recursively prune "empty" elements from a list / value
#'
#' Returns `NULL` for values that are `NULL`, zero-length, all-`NA`, or lists
#' that reduce to nothing after pruning. Used to drop unset metadata from
#' `datapackage.json` so that empty placeholders (`[]`, `null`) do not trigger
#' spurious profile-validation errors.
#' @param x A value.
#' @keywords internal
.ctdp_prune_empty <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x)) {
    x <- lapply(x, .ctdp_prune_empty)
    x <- x[!vapply(x, is.null, logical(1))]
    if (length(x) == 0) return(NULL)
    return(x)
  }
  if (length(x) == 0) return(NULL)
  if (all(is.na(x))) return(NULL)
  x
}

#' Test whether a character value represents a Frictionless "missing value"
#' @param x A character vector.
#' @param missing_values Character vector of tokens treated as missing.
#' @keywords internal
.ctdp_is_missing <- function(x, missing_values = "") {
  is.na(x) | (as.character(x) %in% missing_values)
}
