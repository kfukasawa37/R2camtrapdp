#' @title Discover external references inside a schema
#' @description Camtrap DP schemas specify some information not through
#'   machine-enforceable Frictionless constraints, but through **URLs**:
#'   semantic mappings (`skos:exactMatch` / `skos:broadMatch` /
#'   `skos:narrowMatch` to Darwin Core, Audubon Core, Dublin Core, ... terms),
#'   reference URLs embedded in field `description`s (e.g. the IANA media-type
#'   registry for `fileMediatype`, or method DOIs for `individualSpeed`), and
#'   the resource `schema` / package `profile` URLs themselves.
#'
#'   These functions surface every such URL so that, when you adopt a new
#'   version or a new schema flavor, you do not overlook a specification that is
#'   expressed only by reference.
#' @name ctdp-references
#' @import tibble
NULL

#' All URLs found in a piece of text
#' @keywords internal
.ctdp_text_urls <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  m <- regmatches(x, gregexpr("https?://[^[:space:]\"'<>)]+", x, perl = TRUE))
  unique(unlist(m, use.names = FALSE))
}

#' List the external (URL) references declared by a Table Schema
#'
#' @param x A [TableSchema], or a parsed table-schema list
#'   (`jsonlite::fromJSON(..., simplifyVector = FALSE)`).
#' @return A tibble with columns `resource`, `field` (`NA` for schema-level),
#'   `key` (the JSON key carrying the URL), `category` and `url`. Categories:
#'   `"semantic-mapping"` (skos:*), `"description-reference"`, `"example"`,
#'   `"schema-ref"` (the table schema's own URL).
#' @examples
#' sch <- list(name = "deployments",
#'   fields = list(
#'     list(name = "captureMethod", type = "string",
#'          "skos:exactMatch" = "http://rs.tdwg.org/dwc/terms/samplingProtocol")))
#' ctdp_schema_references(sch)
#' @export
ctdp_schema_references <- function(x) {
  if (inherits(x, "TableSchema")) {
    raw <- x$raw; resource <- x$resource %||% x$name; self_url <- x$url
  } else if (is.list(x)) {
    raw <- x; resource <- x$name; self_url <- NULL
  } else {
    stop("'x' must be a TableSchema or a parsed schema list.", call. = FALSE)
  }
  rows <- list()
  add <- function(field, key, category, urls) {
    if (length(urls) == 0) return(invisible())
    rows[[length(rows) + 1]] <<- tibble::tibble(
      resource = resource %||% NA_character_, field = field, key = key,
      category = category, url = urls)
  }
  if (!is.null(self_url)) add(NA_character_, "schema", "schema-ref", self_url)
  add(NA_character_, "description", "description-reference", .ctdp_text_urls(raw$description))

  for (f in (raw$fields %||% list())) {
    nm <- f$name
    skos <- names(f)[startsWith(names(f), "skos:")]
    for (k in skos) add(nm, k, "semantic-mapping", .ctdp_text_urls(f[[k]]))
    add(nm, "description", "description-reference", .ctdp_text_urls(f$description))
    add(nm, "example", "example", .ctdp_text_urls(f$example))
  }
  if (length(rows) == 0) {
    return(tibble::tibble(resource = character(0), field = character(0),
                          key = character(0), category = character(0), url = character(0)))
  }
  do.call(rbind, rows)
}

#' Fields whose meaning is defined only by reference (not machine-validated)
#'
#' Reports fields that carry a semantic mapping (`skos:*`) or a reference URL in
#' their `description` but have **no** enforceable `enum` or `pattern`
#' constraint. For these fields Frictionless (and this package) can check the
#' type/format but not the controlled vocabulary, so the values should be
#' checked against the referenced authority manually.
#'
#' @param x A [TableSchema] or a parsed table-schema list.
#' @return A tibble: `resource`, `field`, `type`, `reason`, `urls`.
#' @examples
#' sch <- list(name = "deployments",
#'   fields = list(
#'     list(name = "captureMethod", type = "string",
#'          "skos:exactMatch" = "http://rs.tdwg.org/dwc/terms/samplingProtocol")))
#' ctdp_semantic_only_fields(sch)
#' @export
ctdp_semantic_only_fields <- function(x) {
  if (inherits(x, "TableSchema")) { raw <- x$raw; resource <- x$resource %||% x$name }
  else if (is.list(x)) { raw <- x; resource <- x$name }
  else stop("'x' must be a TableSchema or a parsed schema list.", call. = FALSE)

  rows <- list()
  for (f in (raw$fields %||% list())) {
    cons <- f$constraints %||% list()
    if (!is.null(cons$enum) || !is.null(cons$pattern)) next
    skos <- names(f)[startsWith(names(f), "skos:")]
    durls <- .ctdp_text_urls(f$description)
    urls <- c(unlist(lapply(skos, function(k) .ctdp_text_urls(f[[k]])), use.names = FALSE), durls)
    if (length(urls) == 0) next
    reason <- paste(c(if (length(skos)) "semantic-mapping",
                      if (length(durls)) "description-reference"), collapse = "+")
    rows[[length(rows) + 1]] <- tibble::tibble(
      resource = resource %||% NA_character_, field = f$name,
      type = f$type %||% "string", reason = reason,
      urls = paste(unique(urls), collapse = " | "))
  }
  if (length(rows) == 0) {
    return(tibble::tibble(resource = character(0), field = character(0),
                          type = character(0), reason = character(0), urls = character(0)))
  }
  do.call(rbind, rows)
}
