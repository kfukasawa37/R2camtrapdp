#' @title Build a schema-conformant table from arbitrary input
#' @description `ctdp_build_table()` is the generic, schema-driven path from an
#'   arbitrary input spreadsheet to a Camtrap DP table. It (1) applies an
#'   optional column mapping, (2) optionally merges separate date/time columns
#'   into datetime columns, (3) coerces columns to the schema types, and
#'   (4) validates the result against the schema constraints. It works for any
#'   Camtrap DP version and for custom columns, because everything is read from
#'   the supplied [TableSchema].
#' @name ctdp-build-table
NULL

#' Build and validate a table against a Table Schema
#'
#' @param schema A [TableSchema] object.
#' @param data A data.frame / tibble of input data.
#' @param mapping Optional column mapping; see [ctdp_apply_mapping()].
#' @param datetime_merges Optional list of date/time merge specs. Each element
#'   is a list with `date_col`, `time_col`, `target` (and optionally `format`),
#'   applied with [ctdp_merge_datetime()] **after** mapping.
#' @param tz Time zone used for temporal coercion / merging.
#' @param source Issue `source` label; defaults to `"<resource>.csv"`.
#' @param coerce If `TRUE`, coerce columns to schema types before validating.
#' @param stop_on_error If `TRUE`, raise an error (with a summary) when the
#'   table has validation errors instead of returning them.
#' @return A list with elements `data` (the coerced tibble) and `issues` (an
#'   issue table from [ctdp_issues()]).
#' @examples
#' sch <- list(name = "deployments",
#'   fields = list(
#'     list(name = "deploymentID", type = "string", constraints = list(required = TRUE)),
#'     list(name = "latitude", type = "number")),
#'   primaryKey = "deploymentID")
#' schema <- TableSchema$new("deployments", json = sch)
#' built <- ctdp_build_table(schema, data.frame(deploymentID = "A01", latitude = 35.1))
#' built$data
#' @export
ctdp_build_table <- function(schema, data, mapping = NULL, datetime_merges = NULL,
                             tz = "Asia/Tokyo", source = NULL, coerce = TRUE,
                             stop_on_error = FALSE) {
  if (!inherits(schema, "TableSchema")) {
    stop("'schema' must be a TableSchema object.", call. = FALSE)
  }
  source <- source %||% paste0(schema$resource %||% schema$name, ".csv")

  df <- tibble::as_tibble(data)
  if (!is.null(mapping)) df <- ctdp_apply_mapping(df, mapping)

  if (!is.null(datetime_merges)) {
    for (spec in datetime_merges) {
      df <- ctdp_merge_datetime(
        df, date_col = spec$date_col, time_col = spec$time_col,
        target = spec$target, tz = tz,
        format = spec$format %||% .ctdp_datetime_format())
    }
  }

  df_raw <- df
  if (coerce) df <- schema$coerce(df, tz = tz)
  issues <- schema$validate(df, source = source, raw = if (coerce) df_raw else NULL)

  if (stop_on_error && !ctdp_is_valid(issues)) {
    ctdp_summarize_validation(issues)
    stop(sprintf("Table '%s' failed schema validation (%d error(s)).",
                 source, sum(issues$severity == "error", na.rm = TRUE)), call. = FALSE)
  }
  list(data = df, issues = issues)
}
