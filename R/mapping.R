#' @title Column mapping helpers
#' @description Map an arbitrary input spreadsheet onto Camtrap DP field names,
#'   and combine separate date / time columns into a single datetime column.
#' @name ctdp-mapping
#' @import tibble
NULL

#' Apply a column mapping to a data frame
#'
#' Renames the columns of `df` from the source (spreadsheet) names to Camtrap DP
#' field names. Columns of `df` that are not mentioned in `mapping` are kept
#' unchanged, so columns that already use Camtrap DP field names pass through.
#'
#' @param df A data.frame / tibble of input data.
#' @param mapping The mapping from source column names to target field names.
#'   Accepted forms:
#'   * a named character vector where **names are source columns** and **values
#'     are target fields**, e.g. `c(lat = "latitude", lon = "longitude")`;
#'   * a data.frame / tibble with columns `source` and `target`.
#' @param drop_unmapped If `TRUE`, keep only the mapped target columns; if
#'   `FALSE` (default) also keep unmapped columns as-is.
#' @return A tibble with renamed columns.
#' @export
ctdp_apply_mapping <- function(df, mapping, drop_unmapped = FALSE) {
  df <- tibble::as_tibble(df)
  if (is.null(mapping) || length(mapping) == 0) return(df)

  map_tbl <- .ctdp_normalize_mapping(mapping)
  miss <- setdiff(map_tbl$source, names(df))
  if (length(miss)) {
    stop("Mapping refers to source column(s) not found in the data: ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  dup_t <- map_tbl$target[duplicated(map_tbl$target)]
  if (length(dup_t)) {
    stop("Mapping targets must be unique; duplicated: ",
         paste(unique(dup_t), collapse = ", "), call. = FALSE)
  }

  out <- list()
  for (i in seq_len(nrow(map_tbl))) {
    out[[map_tbl$target[i]]] <- df[[map_tbl$source[i]]]
  }
  if (!drop_unmapped) {
    keep <- setdiff(names(df), c(map_tbl$source, map_tbl$target))
    for (k in keep) out[[k]] <- df[[k]]
  }
  tibble::as_tibble(out)
}

#' Normalise a mapping into a tibble with `source` and `target` columns
#' @keywords internal
.ctdp_normalize_mapping <- function(mapping) {
  if (is.data.frame(mapping)) {
    if (!all(c("source", "target") %in% names(mapping))) {
      stop("A data.frame mapping must have 'source' and 'target' columns.", call. = FALSE)
    }
    return(tibble::tibble(source = as.character(mapping$source),
                          target = as.character(mapping$target)))
  }
  if (is.character(mapping) && !is.null(names(mapping))) {
    return(tibble::tibble(source = names(mapping), target = unname(mapping)))
  }
  stop("'mapping' must be a named character vector (source = target) or a ",
       "data.frame with 'source'/'target' columns.", call. = FALSE)
}

#' Combine a date column and a time column into a datetime column
#'
#' Generalises the original `*_date` / `*_time` merging behaviour. Produces a
#' character column formatted in the Camtrap DP datetime format
#' (`%Y-%m-%dT%H:%M:%S%z`) so it round-trips cleanly to CSV.
#'
#' @param df A data.frame / tibble.
#' @param date_col Name of the date column.
#' @param time_col Name of the time column.
#' @param target Name of the datetime column to create.
#' @param tz Time zone used to interpret the local date/time.
#' @param format Output datetime format.
#' @param remove If `TRUE`, drop the source date/time columns.
#' @return A tibble with the `target` datetime column added.
#' @import magrittr
#' @importFrom lubridate as_datetime
#' @export
ctdp_merge_datetime <- function(df, date_col, time_col, target,
                                tz = "Asia/Tokyo", format = .ctdp_datetime_format(),
                                remove = TRUE) {
  df <- tibble::as_tibble(df)
  for (col in c(date_col, time_col)) {
    if (!col %in% names(df)) stop("Column not found: ", col, call. = FALSE)
  }
  dt <- as.POSIXlt(paste(df[[date_col]], df[[time_col]]), tz = tz)
  df[[target]] <- strftime(dt, format, tz = tz)
  if (remove) df[, setdiff(names(df), c(date_col, time_col)), drop = FALSE] else df
}
