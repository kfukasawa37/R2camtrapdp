#' @title R6 class representing a Frictionless Table Schema
#'
#' @description
#' `TableSchema` loads a Frictionless Table Schema (such as the Camtrap DP
#' `deployments` / `media` / `observations` table schemas) from a URL, a local
#' file, or an in-memory list, and exposes everything needed to build and
#' validate a data table against it: field names and types, the primary key,
#' foreign keys, missing-value tokens, and per-field constraints
#' (`required`, `unique`, `enum`, `minimum`, `maximum`, `minLength`,
#' `maxLength`, `pattern`, `type`, `format`).
#'
#' Because the structure is read from the schema itself, an arbitrary Camtrap DP
#' version and any custom / extra columns are handled automatically: any field
#' present in the supplied schema participates in validation.
#'
#' @import R6
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code content
#' @importFrom tibble as_tibble tibble
#' @export
TableSchema <- R6::R6Class("TableSchema",
  public = list(
    #' @field resource Resource name, e.g. `"deployments"`.
    resource = NULL,
    #' @field version Camtrap DP version used to resolve the URL.
    version = NULL,
    #' @field url Resolved schema URL (if loaded from a URL).
    url = NULL,
    #' @field name Schema `name`.
    name = NULL,
    #' @field title Schema `title`.
    title = NULL,
    #' @field description Schema `description`.
    description = NULL,
    #' @field fields Named list of field definitions keyed by field name.
    fields = list(),
    #' @field field_order Character vector of field names, in schema order.
    field_order = character(0),
    #' @field primaryKey Character vector naming the primary key field(s).
    primaryKey = NULL,
    #' @field foreignKeys List of foreign-key definitions.
    foreignKeys = list(),
    #' @field missingValues Character vector of missing-value tokens.
    missingValues = "",
    #' @field raw The raw parsed schema (list).
    raw = NULL,

    #' @description Create a `TableSchema`.
    #' @param resource Resource name (used to pick a default URL template and to
    #'   label issues), e.g. `"deployments"`.
    #' @param version Camtrap DP version string, e.g. `"1.0.1"`.
    #' @param url_template URL containing the `<version>` placeholder. If `NULL`
    #'   and `resource` is one of the standard tables, a default template is used.
    #' @param local_path Path to a local schema JSON file. Takes precedence over
    #'   the URL.
    #' @param json A pre-parsed schema list. Takes precedence over `local_path`.
    #' @param cache_dir Directory used to cache downloaded schemas.
    #' @param use_cache If `TRUE`, reuse a cached copy when present and cache new
    #'   downloads.
    initialize = function(resource = NULL, version = "1.0.1", url_template = NULL,
                          local_path = NULL, json = NULL,
                          cache_dir = file.path(tempdir(), "camtrapdp-schemas"),
                          use_cache = TRUE) {
      self$resource <- resource
      self$version <- version
      raw <- private$load(resource, version, url_template, local_path, json,
                          cache_dir, use_cache)
      private$parse(raw)
      invisible(self)
    },

    #' @description Field names in schema order.
    field_names = function() self$field_order,

    #' @description Get a single field definition by name.
    #' @param name Field name.
    field = function(name) self$fields[[name]],

    #' @description Names of fields whose `constraints$required` is `TRUE`.
    required_field_names = function() {
      req <- vapply(self$field_order, function(n) {
        isTRUE(self$fields[[n]]$constraints$required)
      }, logical(1))
      self$field_order[req]
    },

    #' @description Type of a field (`"string"`, `"number"`, ...).
    #' @param name Field name.
    field_type = function(name) self$fields[[name]]$type %||% "string",

    #' @description A tidy summary of the schema's requirements: one row per
    #'   field with its `type`, `format`, and constraints (`required`, `unique`,
    #'   `enum`, `minimum`, `maximum`, `pattern`). Works for any version / flavor.
    #' @return A tibble.
    requirements = function() {
      rows <- lapply(self$field_order, function(n) {
        f <- self$fields[[n]]; cn <- f$constraints %||% list()
        enum <- .ctdp_as_vector(cn$enum)
        tibble::tibble(
          field    = n,
          type     = f$type %||% "string",
          format   = f$format %||% NA_character_,
          required = isTRUE(cn$required),
          unique   = isTRUE(cn$unique),
          enum     = if (length(enum)) paste(enum, collapse = " | ") else NA_character_,
          minimum  = if (!is.null(cn$minimum)) as.character(cn$minimum) else NA_character_,
          maximum  = if (!is.null(cn$maximum)) as.character(cn$maximum) else NA_character_,
          pattern  = cn$pattern %||% NA_character_)
      })
      do.call(rbind, rows)
    },

    #' @description Create an empty (0-row) tibble shell with one correctly typed
    #'   column per schema field, in schema order.
    #' @return A tibble with 0 rows.
    empty_table = function() {
      cols <- lapply(self$field_order, function(n) .ctdp_empty_vec(self$field_type(n)))
      names(cols) <- self$field_order
      tibble::as_tibble(cols)
    },

    #' @description Coerce a data frame to the schema. The result contains
    #'   **every** schema field, in schema order: present columns are cast to
    #'   the schema type (date/datetime/time formatted as canonical strings) and
    #'   fields absent from the input are added as typed `NA` columns (Camtrap DP
    #'   CSVs are expected to carry all schema columns). Columns not present in
    #'   the schema are kept after the schema columns (custom columns) and a
    #'   warning is emitted listing them.
    #' @param df A data.frame / tibble.
    #' @param tz Time zone used when formatting date/datetime values supplied as
    #'   `POSIXt` / `Date`.
    #' @param complete If `TRUE` (default), include all schema fields, filling
    #'   absent ones with typed `NA`. If `FALSE`, keep only the supplied columns.
    #' @return A tibble.
    coerce = function(df, tz = "Japan", complete = TRUE) {
      df <- tibble::as_tibble(df)
      nrow_df <- nrow(df)
      extra <- setdiff(names(df), self$field_order)
      if (length(extra)) {
        warning(sprintf("Column(s) not in the '%s' schema kept as-is: %s",
                        self$name %||% self$resource %||% "?",
                        paste(extra, collapse = ", ")), call. = FALSE)
      }
      out <- list()
      for (n in self$field_order) {
        if (n %in% names(df)) {
          out[[n]] <- private$coerce_column(df[[n]], self$field_type(n),
                                            self$fields[[n]]$format, tz)
        } else if (complete) {
          out[[n]] <- .ctdp_na_vec(self$field_type(n), nrow_df)
        }
      }
      for (n in extra) out[[n]] <- df[[n]]
      tibble::as_tibble(out)
    },

    #' @description List the external (URL) references this schema declares
    #'   (semantic `skos:*` mappings, reference URLs in field descriptions, the
    #'   schema URL itself). See [ctdp_schema_references()].
    external_references = function() ctdp_schema_references(self),

    #' @description List fields whose meaning is defined only by reference (a
    #'   semantic mapping or a description URL) and which therefore cannot be
    #'   fully validated against a controlled vocabulary. See
    #'   [ctdp_semantic_only_fields()].
    semantic_only_fields = function() ctdp_semantic_only_fields(self),

    #' @description Check that this schema is a well-formed Frictionless Table
    #'   Schema (supported types, constraints valid for each type, keys reference
    #'   defined fields). See [ctdp_check_schema()].
    check_schema = function() ctdp_check_schema(self),

    #' @description Validate a data frame against the schema constraints.
    #' @param df A data.frame / tibble (ideally already passed through
    #'   `$coerce()`).
    #' @param source Label used as the issue `source` (e.g. `"deployments.csv"`).
    #' @param raw Optional pre-coercion data (the original input passed to
    #'   `$coerce()`). When supplied, values that were present in `raw` but became
    #'   `NA` during coercion -- i.e. type-invalid entries such as a non-numeric
    #'   string in a `number` field -- are reported as `type` errors instead of
    #'   silently vanishing. [ctdp_build_table()] passes this automatically.
    #' @return An issue table (see [ctdp_issues()]).
    validate = function(df, source = paste0(self$resource %||% self$name, ".csv"),
                        raw = NULL) {
      df <- tibble::as_tibble(df)
      issues <- list()
      mv <- self$missingValues

      # Required schema fields entirely absent from the data
      missing_cols <- setdiff(self$required_field_names(), names(df))
      for (n in missing_cols) {
        issues[[length(issues) + 1]] <- ctdp_issues(
          source = source, location_type = "schema", field = n,
          constraint = "required",
          message = sprintf("Required field '%s' is missing from the table.", n))
      }

      for (n in intersect(self$field_order, names(df))) {
        issues[[length(issues) + 1]] <- private$validate_field(df, n, source, mv)
      }

      # Coercion loss: original value present but turned into NA by coercion
      # (a type-invalid entry). Without this, such values would be silently
      # dropped to NA and escape both the type and the enum checks.
      if (!is.null(raw)) {
        raw <- tibble::as_tibble(raw)
        for (n in intersect(self$field_order, intersect(names(df), names(raw)))) {
          orig <- as.character(raw[[n]])
          lost <- !.ctdp_is_missing(orig, mv) & is.na(df[[n]])
          rows <- which(lost)
          if (length(rows)) {
            ty <- self$field_type(n)
            issues[[length(issues) + 1]] <- ctdp_issues(
              source = source, location_type = "table", field = n, row = rows,
              constraint = "type", severity = "error",
              message = vapply(rows, function(r) sprintf(
                "'%s' value '%s' is not a valid %s (became NA on coercion).",
                n, orig[r], ty), character(1)))
          }
        }
      }

      # Primary key: implicitly required + unique
      pk <- .ctdp_as_vector(self$primaryKey)
      if (!is.null(pk)) {
        issues[[length(issues) + 1]] <- private$validate_primary_key(df, pk, source, mv)
      }

      do.call(ctdp_bind_issues, issues)
    }
  ),

  private = list(
    load = function(resource, version, url_template, local_path, json,
                    cache_dir, use_cache) {
      if (!is.null(json)) return(json)
      if (!is.null(local_path)) {
        if (!file.exists(local_path)) {
          stop(sprintf("Schema file not found: %s", local_path), call. = FALSE)
        }
        return(jsonlite::fromJSON(local_path, simplifyVector = FALSE))
      }
      if (is.null(url_template)) {
        defaults <- .ctdp_default_urls()
        if (is.null(resource) || is.null(defaults[[resource]])) {
          stop("Provide 'url_template', 'local_path' or 'json'; no default URL ",
               "is known for resource '", resource %||% "NULL", "'.", call. = FALSE)
        }
        url_template <- defaults[[resource]]
      }
      url <- .ctdp_sub_version(url_template, version)
      self$url <- url
      # Include a hash of the URL so different schema "flavors" sharing the same
      # (version, resource) -- e.g. camera-trap vs bioacoustics deployments 1.0.2
      # -- do not collide in the cache.
      cache_file <- file.path(
        cache_dir, sprintf("camtrapdp_%s_%s_%s_schema.json",
                           version, resource %||% "schema", .ctdp_str_hash(url)))
      if (use_cache && file.exists(cache_file)) {
        return(jsonlite::fromJSON(cache_file, simplifyVector = FALSE))
      }
      resp <- httr::GET(url)
      if (httr::status_code(resp) != 200) {
        stop(sprintf("Failed to download schema (HTTP %d): %s",
                     httr::status_code(resp), url), call. = FALSE)
      }
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      if (use_cache) {
        if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
        writeLines(txt, cache_file, useBytes = TRUE)
      }
      jsonlite::fromJSON(txt, simplifyVector = FALSE)
    },

    parse = function(raw) {
      self$raw <- raw
      self$name <- raw$name %||% self$resource
      self$title <- raw$title
      self$description <- raw$description
      self$missingValues <- .ctdp_as_vector(raw$missingValues) %||% ""
      self$primaryKey <- .ctdp_as_vector(raw$primaryKey)
      self$foreignKeys <- raw$foreignKeys %||% list()
      flds <- raw$fields %||% list()
      self$field_order <- vapply(flds, function(f) f$name, character(1))
      named <- flds
      names(named) <- self$field_order
      self$fields <- named
      invisible(self)
    },

    coerce_column = function(x, type, format, tz) {
      switch(.ctdp_r_class(type),
        character = {
          if (type %in% c("date", "datetime", "time")) {
            private$format_temporal(x, type, format, tz)
          } else {
            as.character(x)
          }
        },
        numeric = suppressWarnings(as.numeric(x)),
        integer = suppressWarnings(as.integer(round(as.numeric(x)))),
        logical = private$coerce_logical(x),
        as.character(x)
      )
    },

    coerce_logical = function(x) {
      if (is.logical(x)) return(x)
      cx <- tolower(trimws(as.character(x)))
      out <- rep(NA, length(cx))
      out[cx %in% c("true", "1", "t", "yes")] <- TRUE
      out[cx %in% c("false", "0", "f", "no")] <- FALSE
      out[.ctdp_is_missing(cx, "")] <- NA
      as.logical(out)
    },

    format_temporal = function(x, type, format, tz) {
      fmt <- format
      if (is.null(fmt) || identical(fmt, "default") || identical(fmt, "any")) {
        fmt <- switch(type,
          date = .ctdp_date_format(),
          datetime = .ctdp_datetime_format(),
          time = .ctdp_time_format())
      }
      # When the schema format requires fractional seconds (%f, e.g. the
      # bioacoustics media/observations timestamps), add `.000` to values that
      # lack a fractional part. `strftime()` cannot emit %f, so format POSIXt
      # without it first and then insert the fraction.
      wants_frac <- grepl("%f", fmt, fixed = TRUE)
      if (is.character(x)) {
        # Already character: assume caller-formatted; only normalise fractions.
        return(if (wants_frac) .ctdp_ensure_fractional(x) else x)
      }
      if (inherits(x, "Date") || inherits(x, "POSIXt")) {
        s <- strftime(x, gsub(".%f", "", fmt, fixed = TRUE), tz = tz)
        return(if (wants_frac) .ctdp_ensure_fractional(s) else s)
      }
      as.character(x)
    },

    validate_field = function(df, n, source, mv) {
      fld <- self$fields[[n]]
      cons <- fld$constraints %||% list()
      type <- fld$type %||% "string"
      val <- df[[n]]
      cval <- as.character(val)
      missing <- .ctdp_is_missing(cval, mv)
      present_idx <- which(!missing)
      issues <- list()

      add <- function(rows, constraint, msg_fun) {
        if (length(rows) == 0) return(invisible())
        issues[[length(issues) + 1]] <<- ctdp_issues(
          source = source, location_type = "table", field = n, row = rows,
          constraint = constraint, severity = "error",
          message = vapply(rows, msg_fun, character(1)))
      }

      # required
      if (isTRUE(cons$required)) {
        rows <- which(missing)
        add(rows, "required", function(r) sprintf("'%s' is required but missing.", n))
      }

      # type / format checks on present values
      if (length(present_idx)) {
        sub <- cval[present_idx]
        bad <- private$type_format_invalid(sub, type, fld$format)
        rows <- present_idx[bad]
        add(rows, if (type %in% c("date", "datetime", "time")) "format" else "type",
            function(r) sprintf("'%s' value '%s' is not a valid %s%s.",
                                n, cval[r], type,
                                if (!is.null(fld$format) && nzchar(fld$format) &&
                                    !fld$format %in% c("default", "any"))
                                  paste0(" (format ", fld$format, ")") else ""))
      }

      # enum
      enum <- .ctdp_as_vector(cons$enum)
      if (!is.null(enum) && length(present_idx)) {
        bad <- !(cval[present_idx] %in% as.character(enum))
        rows <- present_idx[bad]
        add(rows, "enum", function(r) sprintf(
          "'%s' value '%s' is not in the allowed set {%s}.",
          n, cval[r], paste(enum, collapse = ", ")))
      }

      # numeric bounds
      if (type %in% c("number", "integer") && length(present_idx)) {
        num <- suppressWarnings(as.numeric(cval))
        if (!is.null(cons$minimum)) {
          rows <- present_idx[which(num[present_idx] < cons$minimum)]
          add(rows, "minimum", function(r) sprintf(
            "'%s' value %s is below the minimum %s.", n, cval[r], cons$minimum))
        }
        if (!is.null(cons$maximum)) {
          rows <- present_idx[which(num[present_idx] > cons$maximum)]
          add(rows, "maximum", function(r) sprintf(
            "'%s' value %s exceeds the maximum %s.", n, cval[r], cons$maximum))
        }
      }

      # string length
      if (length(present_idx)) {
        len <- nchar(cval)
        if (!is.null(cons$minLength)) {
          rows <- present_idx[which(len[present_idx] < cons$minLength)]
          add(rows, "minLength", function(r) sprintf(
            "'%s' value '%s' is shorter than minLength %s.", n, cval[r], cons$minLength))
        }
        if (!is.null(cons$maxLength)) {
          rows <- present_idx[which(len[present_idx] > cons$maxLength)]
          add(rows, "maxLength", function(r) sprintf(
            "'%s' value '%s' is longer than maxLength %s.", n, cval[r], cons$maxLength))
        }
      }

      # pattern
      if (!is.null(cons$pattern) && length(present_idx)) {
        ok <- grepl(paste0("^(?:", cons$pattern, ")$"), cval[present_idx], perl = TRUE)
        rows <- present_idx[!ok]
        add(rows, "pattern", function(r) sprintf(
          "'%s' value '%s' does not match pattern /%s/.", n, cval[r], cons$pattern))
      }

      # unique
      if (isTRUE(cons$unique) && length(present_idx)) {
        dup <- duplicated(cval[present_idx]) | duplicated(cval[present_idx], fromLast = TRUE)
        rows <- present_idx[dup]
        add(rows, "unique", function(r) sprintf(
          "'%s' value '%s' is duplicated but must be unique.", n, cval[r]))
      }

      do.call(ctdp_bind_issues, issues)
    },

    validate_primary_key = function(df, pk, source, mv) {
      issues <- list()
      missing_pk <- setdiff(pk, names(df))
      for (n in missing_pk) {
        issues[[length(issues) + 1]] <- ctdp_issues(
          source = source, location_type = "schema", field = n, constraint = "primaryKey",
          message = sprintf("Primary-key field '%s' is missing from the table.", n))
      }
      have <- intersect(pk, names(df))
      if (length(have)) {
        key <- do.call(paste, c(lapply(have, function(n) as.character(df[[n]])), sep = "\r"))
        # non-missing
        any_missing <- Reduce(`|`, lapply(have, function(n) .ctdp_is_missing(as.character(df[[n]]), mv)))
        rows <- which(any_missing)
        if (length(rows)) {
          issues[[length(issues) + 1]] <- ctdp_issues(
            source = source, location_type = "table", field = paste(have, collapse = "+"),
            row = rows, constraint = "primaryKey",
            message = sprintf("Primary key (%s) must not be missing.", paste(have, collapse = ", ")))
        }
        dup <- (duplicated(key) | duplicated(key, fromLast = TRUE)) & !any_missing
        rows <- which(dup)
        if (length(rows)) {
          issues[[length(issues) + 1]] <- ctdp_issues(
            source = source, location_type = "table", field = paste(have, collapse = "+"),
            row = rows, constraint = "primaryKey",
            message = sprintf("Primary key (%s) must be unique; duplicate found.",
                              paste(have, collapse = ", ")))
        }
      }
      do.call(ctdp_bind_issues, issues)
    },

    type_format_invalid = function(x, type, format) {
      switch(type,
        integer = {
          num <- suppressWarnings(as.numeric(x))
          is.na(num) | (num %% 1 != 0)
        },
        number = is.na(suppressWarnings(as.numeric(x))),
        boolean = !(tolower(x) %in% c("true", "false", "0", "1")),
        date = private$temporal_invalid(x, format, .ctdp_date_format()),
        datetime = private$temporal_invalid(x, format, .ctdp_datetime_format()),
        time = private$temporal_invalid(x, format, .ctdp_time_format()),
        rep(FALSE, length(x))  # string / any: always valid
      )
    },

    temporal_invalid = function(x, format, default_fmt) {
      fmt <- format
      if (is.null(fmt) || identical(fmt, "default") || identical(fmt, "any")) fmt <- default_fmt
      # Build a regex from the (any-version) format -- handles tokens R's
      # strptime cannot, such as %f (fractional seconds).
      rx <- .ctdp_temporal_regex(fmt)
      !grepl(rx, x, perl = TRUE)
    }
  )
)
