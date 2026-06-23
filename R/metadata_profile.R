#' @title R6 class representing a Camtrap DP package profile (metadata schema)
#'
#' @description
#' Frictionless validates the package *data* against the table schemas, but it
#' validates the *metadata* (the `datapackage.json` descriptor itself) against
#' the package **profile** -- a JSON Schema (e.g. `camtrap-dp-profile.json`).
#' `MetadataProfile` reads that profile and extracts the machine-readable
#' metadata requirements: the **required top-level properties**
#' (`contributors`, `project`, `spatial`, `temporal`, `taxonomic`, ...), their
#' types / `minItems`, and the nested required keys of object properties
#' (e.g. `project` requires `title`, `samplingDesign`, ...).
#'
#' This lets the package check / scaffold the required metadata structure on the
#' R side, instead of only discovering metadata problems when Python Frictionless
#' runs.
#'
#' @import R6
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code content
#' @examples
#' \dontrun{
#' # Fetches the package profile for a Camtrap DP version (needs internet):
#' prof <- MetadataProfile$new(version = "1.0.1")
#' prof
#' }
#' @export
MetadataProfile <- R6::R6Class("MetadataProfile",
  public = list(
    #' @field version Camtrap DP version.
    version = NULL,
    #' @field url Resolved profile URL (if loaded from a URL).
    url = NULL,
    #' @field raw The raw parsed profile (list).
    raw = NULL,
    #' @field required Character vector of required top-level property names.
    required = character(0),
    #' @field properties Named list of top-level property definitions.
    properties = list(),

    #' @description Create a `MetadataProfile`.
    #' @param version Camtrap DP version string.
    #' @param url A fully-resolved profile URL (takes precedence over template).
    #' @param url_template A URL containing `<version>`.
    #' @param local_path Path to a local profile JSON file.
    #' @param json A pre-parsed profile list.
    #' @param cache_dir Directory used to cache downloaded profiles.
    #' @param use_cache Reuse / store a cached copy.
    initialize = function(version = "1.0.1", url = NULL, url_template = NULL,
                          local_path = NULL, json = NULL,
                          cache_dir = file.path(tempdir(), "camtrapdp-schemas"),
                          use_cache = TRUE) {
      self$version <- version
      raw <- private$load(version, url, url_template, local_path, json, cache_dir, use_cache)
      private$parse(raw)
      invisible(self)
    },

    #' @description Definition of a top-level property.
    #' @param name Property name.
    property = function(name) self$properties[[name]],
    #' @description `type` of a property.
    #' @param name Property name.
    property_type = function(name) self$properties[[name]]$type,
    #' @description `minItems` of a property (or `NULL`).
    #' @param name Property name.
    property_min_items = function(name) self$properties[[name]]$minItems,
    #' @description Nested required keys of an object property (e.g. `project`).
    #' @param name Property name.
    property_required = function(name) .ctdp_as_vector(self$properties[[name]]$required),
    #' @description Required keys of each item of an array property
    #'   (e.g. `taxonomic` items require `scientificName`).
    #' @param name Property name.
    item_required = function(name) {
      it <- self$properties[[name]]$items
      if (is.null(it)) return(NULL)
      .ctdp_as_vector(it$required)
    }
  ),

  private = list(
    load = function(version, url, url_template, local_path, json, cache_dir, use_cache) {
      if (!is.null(json)) return(json)
      if (!is.null(local_path)) {
        if (!file.exists(local_path)) stop("Profile file not found: ", local_path, call. = FALSE)
        return(jsonlite::fromJSON(local_path, simplifyVector = FALSE))
      }
      if (is.null(url)) {
        if (is.null(url_template)) {
          url_template <- .ctdp_default_urls()$profile
        }
        url <- .ctdp_sub_version(url_template, version)
      }
      self$url <- url
      cache_file <- file.path(
        cache_dir, sprintf("camtrapdp_%s_profile_%s.json", version, .ctdp_str_hash(url)))
      if (use_cache && file.exists(cache_file)) {
        return(jsonlite::fromJSON(cache_file, simplifyVector = FALSE))
      }
      resp <- httr::GET(url)
      if (httr::status_code(resp) != 200) {
        stop(sprintf("Failed to download profile (HTTP %d): %s",
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
      # The camtrap-dp profile is a JSON Schema using `allOf`; the camtrap-specific
      # `required` / `properties` live in the allOf entry that declares them (the
      # other entry is an external `$ref` to the base data-package schema).
      req <- .ctdp_as_vector(raw$required)
      props <- raw$properties %||% list()
      for (a in (raw$allOf %||% list())) {
        if (!is.null(a$required)) req <- union(req, .ctdp_as_vector(a$required))
        if (!is.null(a$properties)) props <- modifyList(props, a$properties)
      }
      self$required <- req %||% character(0)
      self$properties <- props
    }
  )
)

#' Setter-function hints for Camtrap DP metadata properties
#'
#' Maps each metadata property to the `R6_CamtrapDP` method(s) that create it.
#' @keywords internal
.ctdp_metadata_setters <- function() {
  c(resources          = "set_deployments() / set_media() / set_observations()",
    profile            = "set_properties()",
    created            = "update_created() / new()",
    name               = "set_properties()",
    id                 = "set_properties()",
    title              = "set_properties()",
    description        = "set_properties()",
    version            = "set_properties() / new()",
    contributors       = "add_contributors()",
    project            = "set_project()",
    spatial            = "set_st()",
    temporal           = "set_st()",
    taxonomic          = "set_taxon()",
    licenses           = "add_license()",
    sources            = "add_sources()",
    keywords           = "set_properties()",
    image              = "set_properties()",
    homepage           = "set_properties()",
    bibliographicCitation = "set_properties()",
    coordinatePrecision   = "set_properties()",
    relatedIdentifiers = "add_relatedIdentifiers()",
    references         = "add_references()")
}
