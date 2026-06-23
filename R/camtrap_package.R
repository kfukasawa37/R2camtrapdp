#' @title R6 class representing Camtrap DP (schema-driven)
#'
#' @description
#' R6 class holding the Camtrap DP metadata, deployments, media and
#' observations. This is the schema-driven successor of the original
#' `R6_CamtrapDP`: the field names, types, constraints and relations used when
#' adding tables are read from the Frictionless Table Schemas for the configured
#' Camtrap DP `version`, so an arbitrary version and custom columns are handled
#' automatically.
#'
#' The public field names and the method names of the original class are
#' preserved for backward compatibility (`set_deployments()`, `set_media()`,
#' `set_observations()`, `set_custom()`, `add_contributors()`, `add_sources()`,
#' `add_license()`, `set_project()`, `set_st()`, `set_taxon()`,
#' `add_relatedIdentifiers()`, `add_references()`, `out_camtrapdp()`,
#' `import_metadata()`). New, schema-driven capabilities are added as new
#' methods (`get_schema()`, `add_table()`, `check_relations()`, `validate()`,
#' `validate_frictionless()`).
#'
#' @import R6
#' @import magrittr
#' @examples
#' # Create the builder (offline):
#' dp <- R6_CamtrapDP$new(version = "1.0.1", title = "Example", description = "...")
#' \dontrun{
#' # Registering tables fetches the schema for the chosen version (needs internet):
#' deployments <- create_deployments(
#'   deploymentID = "A01", latitude = 35.1, longitude = 139.5,
#'   deploymentStart_date = "2023-04-01", deploymentStart_time = "09:00:00",
#'   deploymentEnd_date = "2023-05-01", deploymentEnd_time = "09:00:00")
#' dp$set_deployments(deployments)
#' dp$check_relations()
#' dp$out_camtrapdp(write = TRUE, directory = tempfile())
#' }
#' @export
R6_CamtrapDP <- R6::R6Class("CamtrapDP",
  public = list(
    #' @field resources is the package data resources
    resources = list(),
    #' @field profile of the resource
    profile = NULL,
    #' @field name Identifier of the resource
    name = NULL,
    #' @field id A property reserved for globally unique identifiers
    id = NULL,
    #' @field created The datetime on which this Data Package was created
    created = NA,
    #' @field title Title of this Data Package
    title = NULL,
    #' @field contributors The people or organizations who contributed
    contributors = list(),
    #' @field description Description of this Data Package
    description = NULL,
    #' @field version The version of this Data Package
    version = NULL,
    #' @field keywords Keywords of this Data Package
    keywords = NULL,
    #' @field image A URL or Path of an image for this Data Package
    image = NULL,
    #' @field homepage A URL for the home on the web related to this Data Package
    homepage = NULL,
    #' @field sources A row sources for this Data Package
    sources = NULL,
    #' @field licenses The licenses under which the Data Package is provided
    licenses = NULL,
    #' @field bibliographicCitation A bibliographical reference for the resource
    bibliographicCitation = NULL,
    #' @field project Camera trap project or study
    project = list(),
    #' @field coordinatePrecision Least precise coordinate precision
    coordinatePrecision = NULL,
    #' @field spatial Spatial coverage, expressed as GeoJSON
    spatial = list(),
    #' @field temporal Temporal coverage of this Data Package
    temporal = list(start = NA, end = NA),
    #' @field taxonomic Taxonomic coverage of this Data Package
    taxonomic = list(),
    #' @field relatedIdentifiers Identifiers of related resources
    relatedIdentifiers = NULL,
    #' @field references List of references related to this Data Package
    references = list(),
    #' @field directory Directory of this Data Package
    directory = NULL,
    #' @field data Observation, Media and Deployments tables
    data = list(),
    #' @field schema_urls Named list of `<version>` URL templates per resource.
    schema_urls = NULL,
    #' @field schemas Cache of loaded [TableSchema] objects, keyed by resource.
    schemas = list(),
    #' @field profile_schema Cached [MetadataProfile] for the package profile.
    profile_schema = NULL,
    #' @field cache_dir Directory used to cache downloaded schemas.
    cache_dir = NULL,
    #' @field use_cache Whether to cache / reuse downloaded schemas.
    use_cache = TRUE,
    #' @field validation Per-table validation issues accumulated during build.
    validation = list(),

    #' @description Creates a new instance.
    #' @param tz Time zone.
    #' @param ... Passed to `set_properties()`.
    initialize = function(tz = "Asia/Tokyo", ...) {
      self$schema_urls <- .ctdp_default_urls()
      self$cache_dir <- file.path(tempdir(), "camtrapdp-schemas")
      self$update_created(tz = tz)
      self$set_properties(...)
    },

    #' @description Updates the created timestamp.
    #' @param tz Time zone.
    update_created = function(tz = "Asia/Tokyo") {
      self$created <- Sys.time() %>%
        as.POSIXct() %>%
        strftime("%Y-%m-%dT%H:%M:%S%z", tz = tz)
    },

    #' @description Sets package properties.
    #' @param directory Directory of datapackage.
    #' @param name Identifier of the resource.
    #' @param id Globally unique identifier.
    #' @param title Title of this Data Package.
    #' @param description Description of this Data Package.
    #' @param profile Profile of the resource (`<version>` template).
    #' @param version The Camtrap DP version.
    #' @param keywords Keywords.
    #' @param image Image URL/path.
    #' @param homepage Homepage URL.
    #' @param bibliographicCitation Bibliographic reference.
    #' @param coordinatePrecision Coordinate precision.
    #' @param schema_urls Optional named list of `<version>` schema URL templates.
    #' @param cache_dir Optional directory to cache schemas.
    #' @param use_cache Whether to cache / reuse downloaded schemas.
    set_properties = function(directory = getwd(), name = NULL, id = NULL, title = NULL,
                              description = NULL,
                              profile = "https://raw.githubusercontent.com/tdwg/camtrap-dp/<version>/camtrap-dp-profile.json",
                              version = "1.0.1", keywords = NULL, image = NULL,
                              homepage = NULL, bibliographicCitation = NULL,
                              coordinatePrecision = NULL, schema_urls = NULL,
                              cache_dir = NULL, use_cache = NULL) {
      # Non-destructive: only update the properties the caller actually supplied,
      # so e.g. set_properties(name = "x") does not reset a previously chosen
      # version / profile / title. First-time defaults (version, profile,
      # directory) are applied only when the field is still unset.
      if (!missing(name)) self$name <- name
      if (!missing(id)) self$id <- id
      if (!missing(title)) self$title <- title
      if (!missing(description)) self$description <- description
      if (!missing(keywords)) self$keywords <- keywords
      if (!missing(image)) self$image <- image
      if (!missing(homepage)) self$homepage <- homepage
      if (!missing(bibliographicCitation)) self$bibliographicCitation <- bibliographicCitation
      if (!missing(coordinatePrecision)) self$coordinatePrecision <- coordinatePrecision

      if (!missing(version)) {
        self$version <- version
        if (identical(as.character(version), "1.0")) {
          warning("Camtrap DP version '1.0' has a known upstream profile bug ",
                  "(a malformed internal $ref, '#$defs/version') that newer Frictionless ",
                  "rejects; validate_frictionless() works around it automatically, but ",
                  "version '1.0.1' or later is recommended.", call. = FALSE)
        }
      } else if (is.null(self$version)) {
        self$version <- version  # first-time default
      }

      # Profile tracks the version: set it when given, on first init, or when the
      # version was (re)specified without an explicit profile.
      if (!missing(profile)) self$profile <- .ctdp_sub_version(profile, self$version)
      else if (is.null(self$profile) || !missing(version)) {
        self$profile <- .ctdp_sub_version(profile, self$version)
      }

      if (!missing(directory)) self$directory <- directory
      else if (is.null(self$directory)) self$directory <- directory  # first-time default (getwd())

      if (!is.null(schema_urls)) self$schema_urls <- modifyList(self$schema_urls, schema_urls)
      if (!is.null(cache_dir)) self$cache_dir <- cache_dir
      if (!is.null(use_cache)) self$use_cache <- use_cache
      invisible(self)
    },

    #' @description Load (and cache) the [TableSchema] for a resource at the
    #'   configured version.
    #' @param resource Resource name, e.g. `"deployments"`.
    #' @param schema_url Optional `<version>` URL template (overrides the default).
    #' @param local_path Optional local schema file.
    #' @param json Optional pre-parsed schema list.
    #' @param refresh If `TRUE`, reload even if cached on the object.
    #' @return A [TableSchema].
    get_schema = function(resource, schema_url = NULL, local_path = NULL,
                          json = NULL, refresh = FALSE) {
      if (!refresh && is.null(schema_url) && is.null(local_path) && is.null(json) &&
          !is.null(self$schemas[[resource]])) {
        return(self$schemas[[resource]])
      }
      template <- schema_url %||% self$schema_urls[[resource]]
      sch <- TableSchema$new(resource = resource, version = self$version,
                             url_template = template, local_path = local_path,
                             json = json, cache_dir = self$cache_dir,
                             use_cache = self$use_cache)
      self$schemas[[resource]] <- sch
      sch
    },

    #' @description Sets deployments. Backward-compatible signature; when the
    #'   schema is reachable the data is coerced and validated against it.
    #' @param data Deployments dataset.
    #' @param path Path to the data file.
    #' @param profile Profile of the resource.
    #' @param format Format of the data.
    #' @param mediatype Media type.
    #' @param encoding Encoding.
    #' @param schema `<version>` URL template for the table schema. If `NULL`
    #'   (default), the resource's configured template is used (the camera-trap
    #'   default, or whatever was set via `set_properties(schema_urls=)`, e.g. a
    #'   bioacoustics flavor).
    #' @param mapping Optional column mapping (see [ctdp_apply_mapping()]).
    #' @param datetime_merges Optional date/time merge specs.
    #' @param validate Whether to validate against the schema.
    #' @param local_schema Optional local schema file path.
    #' @param tz Time zone for temporal coercion.
    set_deployments = function(data, path = "deployments.csv",
                               profile = "tabular-data-resource", format = "csv",
                               mediatype = "text/csv", encoding = "utf-8",
                               schema = NULL,
                               mapping = NULL, datetime_merges = NULL,
                               validate = TRUE, local_schema = NULL, tz = "Asia/Tokyo") {
      private$set_standard("deployments", 1L, data, path, profile, format, mediatype,
                           encoding, schema, mapping, datetime_merges, validate,
                           local_schema, tz)
    },

    #' @description Sets media. See `set_deployments()` for shared arguments.
    #' @param data Media dataset.
    #' @param path Path to the data file.
    #' @param profile Profile of the resource.
    #' @param format Format of the data.
    #' @param mediatype Media type.
    #' @param encoding Encoding.
    #' @param schema `<version>` URL template for the table schema.
    #' @param mapping Optional column mapping.
    #' @param datetime_merges Optional date/time merge specs.
    #' @param validate Whether to validate against the schema.
    #' @param local_schema Optional local schema file path.
    #' @param tz Time zone for temporal coercion.
    set_media = function(data, path = "media.csv",
                         profile = "tabular-data-resource", format = "csv",
                         mediatype = "text/csv", encoding = "utf-8",
                         schema = NULL,
                         mapping = NULL, datetime_merges = NULL,
                         validate = TRUE, local_schema = NULL, tz = "Asia/Tokyo") {
      private$set_standard("media", 2L, data, path, profile, format, mediatype,
                           encoding, schema, mapping, datetime_merges, validate,
                           local_schema, tz)
    },

    #' @description Sets observations. See `set_deployments()` for shared arguments.
    #' @param data Observations dataset.
    #' @param path Path to the data file.
    #' @param profile Profile of the resource.
    #' @param format Format of the data.
    #' @param mediatype Media type.
    #' @param encoding Encoding.
    #' @param schema `<version>` URL template for the table schema.
    #' @param mapping Optional column mapping.
    #' @param datetime_merges Optional date/time merge specs.
    #' @param validate Whether to validate against the schema.
    #' @param local_schema Optional local schema file path.
    #' @param tz Time zone for temporal coercion.
    set_observations = function(data, path = "observations.csv",
                                profile = "tabular-data-resource", format = "csv",
                                mediatype = "text/csv", encoding = "utf-8",
                                schema = NULL,
                                mapping = NULL, datetime_merges = NULL,
                                validate = TRUE, local_schema = NULL, tz = "Asia/Tokyo") {
      private$set_standard("observations", 3L, data, path, profile, format, mediatype,
                           encoding, schema, mapping, datetime_merges, validate,
                           local_schema, tz)
    },

    #' @description Add an arbitrary (custom or standard) resource table,
    #'   schema-driven. Unlike `set_custom()` this loads a Table Schema, applies
    #'   mapping, coerces, validates, stores the table in `$data[[name]]` and
    #'   appends a proper tabular-data resource.
    #' @param name Resource name.
    #' @param data Dataset.
    #' @param mapping Optional column mapping.
    #' @param datetime_merges Optional date/time merge specs.
    #' @param schema_url Optional `<version>` URL template for the schema.
    #' @param local_schema Optional local schema file path.
    #' @param schema_json Optional pre-parsed schema list.
    #' @param path Output CSV path (defaults to `<name>.csv`).
    #' @param description Optional resource description.
    #' @param profile Resource profile.
    #' @param format Resource format.
    #' @param mediatype Resource media type.
    #' @param encoding Resource encoding.
    #' @param validate Whether to validate against the schema.
    #' @param tz Time zone for temporal coercion.
    add_table = function(name, data, mapping = NULL, datetime_merges = NULL,
                         schema_url = NULL, local_schema = NULL, schema_json = NULL,
                         path = NULL, description = NULL,
                         profile = "tabular-data-resource", format = "csv",
                         mediatype = "text/csv", encoding = "utf-8",
                         validate = TRUE, tz = "Asia/Tokyo") {
      path <- path %||% paste0(name, ".csv")
      sch <- tryCatch(
        self$get_schema(name, schema_url = schema_url, local_path = local_schema,
                        json = schema_json),
        error = function(e) {
          warning(sprintf("Could not load schema for '%s' (%s). Storing table without validation.",
                          name, conditionMessage(e)), call. = FALSE)
          NULL
        })
      if (!is.null(sch)) {
        built <- ctdp_build_table(sch, data, mapping = mapping,
                                  datetime_merges = datetime_merges, tz = tz,
                                  source = path)
        self$data[[name]] <- built$data
        self$validation[[name]] <- built$issues
        if (validate) ctdp_summarize_validation(built$issues)
        url_fallback <- if (!is.null(schema_url))
          .ctdp_sub_version(schema_url, self$version) else NA
        schema_resolved <- private$resolve_schema(sch, built$data, url_fallback)
      } else {
        self$data[[name]] <- tibble::as_tibble(data)
        schema_resolved <- if (!is.null(schema_url))
          .ctdp_sub_version(schema_url, self$version) else NA
      }
      resource <- list(name = name, path = path, profile = profile, format = format,
                       mediatype = mediatype, encoding = encoding)
      if (!is.null(description)) resource$description <- description
      if (is.list(schema_resolved) || !(length(schema_resolved) == 1 && is.na(schema_resolved))) {
        resource$schema <- schema_resolved
      }
      private$upsert_resource(resource)
      invisible(self)
    },

    #' @description Sets a custom data resource (original behaviour preserved).
    #' @param name Name of dataset.
    #' @param description Description of dataset.
    #' @param data Custom dataset.
    set_custom = function(name, description, data) {
      resource <- list(name = name, description = description, data = data)
      nlist <- length(self$resources)
      if (!all(is.element(c("deployments", "media", "observations"),
                          sapply(self$resources, "[[", "name")))) {
        stop("Add deployments, media and obervations prior to adding custom datasets")
      }
      self$resources[[nlist + 1]] <- resource
    },

    #' @description Adds contributors.
    #' @param contrib_table data frame of contributors (title, email, path, role,
    #'   organization).
    add_contributors = function(contrib_table) {
      if (!all(is.element(colnames(contrib_table),
                          c("title", "email", "path", "role", "organization")))) {
        stop("Columns of 'contrib_table' should be either 'title','email','path','role' or 'organization'.")
      }
      ncontrib <- nrow(contrib_table)
      for (i in 1:ncontrib) {
        title <- contrib_table$title[i]
        email <- if (is.element("email", colnames(contrib_table))) contrib_table$email[i] else NA
        path <- if (is.element("path", colnames(contrib_table))) contrib_table$path[i] else NA
        role <- if (is.element("role", colnames(contrib_table))) as.character(contrib_table$role[i]) else NA
        organization <- if (is.element("organization", colnames(contrib_table))) contrib_table$organization[i] else NA

        contrib <- list(title = title)
        if (!is.na(email)) contrib <- c(contrib, email = email)
        if (!is.na(path)) contrib <- c(contrib, path = path)
        if (!is.na(role)) {
          if (!grepl("^(contact|principalInvestigator|rightsHolder|publisher|contributor)$", role)) {
            stop("'role' is invalid.")
          }
          contrib <- c(contrib, role = role)
        }
        if (!is.na(organization)) contrib <- c(contrib, organization = organization)
        if (length(self$contributors) == 0) {
          self$contributors <- list(contrib)
        } else {
          self$contributors <- c(self$contributors, list(contrib))
        }
      }
    },

    #' @description Add a source.
    #' @param title Title of source.
    #' @param path Path or URL to the source.
    #' @param email An email address.
    #' @param version The version of the source.
    add_sources = function(title, path = NULL, email = NULL, version = NULL) {
      if (is.null(self$sources)) self$sources <- list()
      source <- list(title = title)
      if (!is.null(path)) source <- c(source, path = path)
      if (!is.null(email)) source <- c(source, email = email)
      if (!is.null(version)) source <- c(source, version = version)
      self$sources <- c(self$sources, list(source))
    },

    #' @description Add a license.
    #' @param name Name of license.
    #' @param scope Scope ("data" or "media").
    #' @param path URL/path to the license details.
    #' @param title Title of license.
    add_license = function(name, scope, path = NULL, title = NULL) {
      if (!grepl("^(data|media)$", scope)) stop("'scope' is invalid.")
      if (is.null(self$licenses)) self$licenses <- list()
      license <- list(name = name)
      if (!is.null(path)) license <- c(license, path = path)
      if (!is.null(title)) license <- c(license, title = title)
      license <- c(license, scope = scope)
      self$licenses <- c(self$licenses, list(license))
    },

    #' @description Sets the project.
    #' @param title Title of project.
    #' @param samplingDesign Sampling design.
    #' @param captureMethod Capture method.
    #' @param individualAnimals Logical: individuals recognised?
    #' @param observationLevel Observation level.
    #' @param id Project id.
    #' @param acronym Project acronym.
    #' @param description Project description.
    #' @param path Project website.
    set_project = function(title, samplingDesign, captureMethod, individualAnimals,
                           observationLevel, id = NULL, acronym = NULL,
                           description = NULL, path = NULL) {
      if (!grepl("^(simpleRandom|systematicRandom|clusteredRandom|experimental|targeted|opportunistic)$", samplingDesign)) {
        stop("'samplingDesign' is invalid.")
      }
      # Accept both the camera-trap values (activityDetection, timeLapse) and the
      # bioacoustics values (continuous, recordingSchedule); the version/flavor
      # profile enforces which are valid for the chosen schema.
      if (any(!grepl("^(activityDetection|timeLapse|continuous|recordingSchedule)$", captureMethod))) {
        stop("'captureMethod' is invalid (expected activityDetection, timeLapse, continuous or recordingSchedule).")
      }
      if (!is.logical(individualAnimals)) {
        if (!grepl("^(true|false)$", individualAnimals)) stop("'individualAnimals' is invalid.")
        individualAnimals <- as.logical(individualAnimals)
      }
      if (any(!grepl("^(media|event)$", observationLevel))) stop("'observationLevel' is invalid.")
      if (!is.null(id)) self$project <- c(self$project, id = id)
      self$project <- c(self$project, title = title)
      if (!is.null(acronym)) self$project <- c(self$project, acronym = acronym)
      if (!is.null(description)) self$project <- c(self$project, description = description)
      if (!is.null(path)) self$project <- c(self$project, path = path)
      self$project <- c(self$project, samplingDesign = samplingDesign)
      self$project <- c(self$project, captureMethod = list(as.list(captureMethod)))
      self$project <- c(self$project, individualAnimals = individualAnimals)
      self$project <- c(self$project, observationLevel = list(as.list(observationLevel)))
    },

    #' @description Sets spatial and temporal coverage from the deployments.
    set_st = function() {
      if (is.null(self$data$deployments)) stop("'deployments' should be registered.")
      lat <- as.numeric(self$data$deployments$latitude)
      long <- as.numeric(self$data$deployments$longitude)
      lat.range <- range(lat, na.rm = TRUE)
      long.range <- range(long, na.rm = TRUE)
      bbox <- c(lat.range[1], long.range[1], lat.range[2], long.range[2])
      coordinates <- array(NA, dim = c(1, 5, 2))
      coordinates[1, , 1] <- lat.range[c(1, 2, 2, 1, 1)]
      coordinates[1, , 2] <- long.range[c(1, 1, 2, 2, 1)]
      self$spatial <- list(type = "Polygon", bbox = bbox, coordinates = coordinates)
      self$temporal$start <- min(self$data$deployments$deploymentStart)
      self$temporal$end <- max(self$data$deployments$deploymentEnd)
    },

    #' @description Sets taxonomic coverage from the observations. The Camtrap DP
    #'   `taxonomic` block requires a `taxonID` (e.g. a GBIF / IUCN identifier or
    #'   URI), which is looked up with `taxadb`; `taxadb` is therefore a required
    #'   dependency of the package (loaded with it, as in previous versions).
    #'   Names that cannot be matched get `taxonID = NA` (omitted from the output
    #'   rather than a bogus `<uri>NA`). A `warning()` is emitted for
    #'   `scientificName` values with unnecessary whitespace and for names with no
    #'   `taxonID` in the chosen database.
    #' @param taxonDB Taxon database passed to taxadb: \code{"gbif"} (default),
    #'   \code{"itis"} or \code{"ncbi"}.
    #' @importFrom taxadb get_ids filter_name
    #' @importFrom stats na.omit
    #' @importFrom tidyr nest unnest
    #' @importFrom purrr map
    set_taxon = function(taxonDB = "gbif") {
      if (is.null(self$data$observations)) stop("'observations' should be registered.")
      if (!is.element(taxonDB, c("gbif", "itis", "ncbi"))) {
        stop("Only supports 'gbif','itis' and 'ncbi'.")
      }
      uritemplate <- sub("[0-9].*$", "", taxadb::get_ids("Homo sapiens", taxonDB, format = "uri"))
      sciname <- self$data$observations$scientificName
      unique.sciname <- unique(sciname) %>% stats::na.omit()
      unique.sciname <- unique.sciname[!(unique.sciname == " ")]
      # Warn about unnecessary whitespace (leading/trailing/repeated spaces), a
      # common cause of failed taxon matches (and thus an empty taxonID).
      ws <- unique.sciname[unique.sciname != trimws(gsub("[[:space:]]+", " ", unique.sciname))]
      if (length(ws)) {
        warning(sprintf("set_taxon(): scientificName has unnecessary whitespace in: %s. Trimming these (e.g. trimws()) before set_taxon() is recommended.",
                        paste(sprintf("'%s'", ws), collapse = ", ")), call. = FALSE)
      }
      ntaxa <- length(unique.sciname)
      unique.sciname.clean <- sub(" sp\\.$", "", unique.sciname)
      taxonIDtable <- taxadb::filter_name(unique.sciname.clean, taxonDB) %>%
        dplyr::group_by(scientificName) %>%
        tidyr::nest() %>%
        dplyr::mutate(data2 = purrr::map(data, ~dplyr::arrange(., .$taxonomicStatus)[1, ])) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = data2) %>%
        dplyr::mutate(order = ifelse(is.na(order), " ", order)) %>%
        dplyr::mutate(family = ifelse(is.na(family), " ", family)) %>%
        dplyr::mutate(genus = ifelse(is.na(genus), " ", genus))
      colnames(taxonIDtable)[1] <- "sciname.clean"
      taxonIDtable <- tibble::tibble(sciname = unique.sciname, sciname.clean = unique.sciname.clean) %>%
        dplyr::left_join(taxonIDtable, by = "sciname.clean")
      # Unmatched names return NA from taxadb; keep taxonID as NA (avoid a bogus
      # "<uri>NA") so the empty taxonID is omitted from the output.
      raw_taxonID <- taxonIDtable$taxonID
      taxonID <- ifelse(is.na(raw_taxonID), NA_character_,
                        paste0(uritemplate, sub("^.*\\:", "", raw_taxonID)))
      taxonIDjoin <- taxonIDtable %>% dplyr::bind_cols(id = taxonID)
      # Warn about names that could not be resolved to a taxonID (left empty).
      empty <- taxonIDjoin$sciname[is.na(taxonIDjoin$id) | !nzchar(taxonIDjoin$id)]
      if (length(empty)) {
        warning(sprintf("set_taxon(): no '%s' taxonID found for %d name(s); taxonID left empty: %s. Check the spelling / whitespace, or whether the name exists in '%s'.",
                        taxonDB, length(empty), paste(sprintf("'%s'", empty), collapse = ", "), taxonDB),
                call. = FALSE)
      }
      self$taxonomic <- vector("list", ntaxa)
      for (i in 1:ntaxa) {
        self$taxonomic[[i]] <- list()
        self$taxonomic[[i]]$taxonID <- taxonIDjoin$id[i]
        self$taxonomic[[i]]$scientificName <- taxonIDjoin$sciname[i]
        self$taxonomic[[i]]$taxonRank <- taxonIDjoin$taxonRank[i]
        self$taxonomic[[i]]$kingdom <- taxonIDjoin$kingdom[i]
        self$taxonomic[[i]]$phylum <- taxonIDjoin$phylum[i]
        self$taxonomic[[i]]$class <- taxonIDjoin$class[i]
        self$taxonomic[[i]]$order <- taxonIDjoin$order[i]
        self$taxonomic[[i]]$family <- taxonIDjoin$family[i]
        self$taxonomic[[i]]$genus <- taxonIDjoin$genus[i]
      }
    },

    #' @description Adds a relatedIdentifier.
    #' @param relationType Type of relation.
    #' @param relatedIdentifier Related identifier.
    #' @param relatedIdentifierType Type of related identifier.
    #' @param resourceTypeGeneral General type of the related resource.
    add_relatedIdentifiers = function(relationType, relatedIdentifier,
                                      relatedIdentifierType, resourceTypeGeneral = NULL) {
      if (!grepl("^(IsCitedBy|Cites|IsSupplementTo|IsSupplementedBy|IsContinuedBy|Continues|IsNewVersionOf|IsPreviousVersionOf|IsPartOf|HasPart|IsPublishedIn|IsReferencedBy|References|IsDocumentedBy|Documents|IsCompiledBy|Compiles|IsVariantFormOf|IsOriginalFormOf|IsIdenticalTo|HasMetadata|IsMetadataFor|Reviews|IsReviewedBy|IsDerivedFrom|IsSourceOf|Describes|IsDescribedBy|HasVersion|IsVersionOf|Requires|IsRequiredBy|Obsoletes|IsObsoletedBy)$", relationType)) {
        stop("'relationType' is invalid.")
      }
      if (!is.null(resourceTypeGeneral)) {
        if (!grepl("^(Audiovisual|Book|BookChapter|Collection|ComputationalNotebook|ConferencePaper|ConferenceProceeding|DataPaper|Dataset|Dissertation|Event|Image|InteractiveResource|Journal|JournalArticle|Model|OutputManagementPlan|PeerReview|PhysicalObject|Preprint|Report|Service|Software|Sound|Standard|Text|Workflow|Other)$", resourceTypeGeneral)) {
          stop("'resourceTypeGeneral' is invalid.")
        }
      }
      if (!grepl("^(ARK|arXiv|bibcode|DOI|EAN13|EISSN|Handle|IGSN|ISBN|ISSN|ISTC|LISSN|LSID|PMID|PURL|UPC|URL|URN|w3id)$", relatedIdentifierType)) {
        stop("'relatedIdentifierType' is invalid.")
      }
      if (is.null(self$relatedIdentifiers)) self$relatedIdentifiers <- list()
      relatedIdentifier <- list(relationType = relationType, relatedIdentifier = relatedIdentifier)
      if (!is.null(resourceTypeGeneral)) {
        relatedIdentifier <- c(relatedIdentifier, resourceTypeGeneral = resourceTypeGeneral)
      }
      relatedIdentifier <- c(relatedIdentifier, relatedIdentifierType = relatedIdentifierType)
      self$relatedIdentifiers <- c(self$relatedIdentifiers, list(relatedIdentifier))
    },

    #' @description Adds references.
    #' @param reference Reference of data.
    add_references = function(reference) {
      self$references <- c(self$references, reference)
    },

    #' @description Load (and cache) the [MetadataProfile] for the package
    #'   profile (the JSON Schema that Frictionless validates `datapackage.json`
    #'   against). Uses `self$profile` (set by `set_properties()`), so it follows
    #'   the configured version / flavor.
    #' @param refresh Reload even if cached.
    #' @return A [MetadataProfile].
    get_profile = function(refresh = FALSE) {
      if (!refresh && !is.null(self$profile_schema)) return(self$profile_schema)
      self$profile_schema <- MetadataProfile$new(
        version = self$version, url = self$profile,
        cache_dir = self$cache_dir, use_cache = self$use_cache)
      self$profile_schema
    },

    #' @description The metadata requirements derived from the package profile:
    #'   which top-level properties are required, their type, nested required
    #'   keys, the method that creates each, and whether it is currently set.
    #' @return A tibble (`property`, `required`, `type`, `sub_required`,
    #'   `item_required`, `set_with`, `currently_set`).
    metadata_requirements = function() {
      prof <- self$get_profile()
      setters <- .ctdp_metadata_setters()
      props <- union(prof$required, names(prof$properties))
      rows <- lapply(props, function(p) {
        sub <- prof$property_required(p)
        itm <- prof$item_required(p)
        tibble::tibble(
          property      = p,
          required      = p %in% prof$required,
          type          = prof$property_type(p) %||% NA_character_,
          sub_required  = if (length(sub)) paste(sub, collapse = ", ") else NA_character_,
          item_required = if (length(itm)) paste(itm, collapse = ", ") else NA_character_,
          set_with      = unname(setters[p]) %||% NA_character_,
          currently_set = private$metadata_present(p))
      })
      out <- do.call(rbind, rows)
      out[order(!out$required, out$property), ]
    },

    #' @description Validate the current metadata against the package profile's
    #'   required structure (a fast R-side counterpart to the profile validation
    #'   that Frictionless performs). Reports missing required top-level
    #'   properties and missing nested/required item keys.
    #' @param summarize Print a summary.
    #' @return An issue table (see [ctdp_issues()]).
    check_metadata = function(summarize = TRUE) {
      prof <- tryCatch(self$get_profile(), error = function(e) {
        warning(sprintf("Could not load package profile (%s); skipping metadata check.",
                        conditionMessage(e)), call. = FALSE); NULL })
      if (is.null(prof)) return(ctdp_no_issues())
      src <- "datapackage.json"
      issues <- list()
      for (p in prof$required) {
        v <- self[[p]]
        if (!private$metadata_present(p)) {
          issues[[length(issues) + 1]] <- ctdp_issues(
            source = src, location_type = "package", field = p, constraint = "required",
            message = sprintf("Required metadata property '%s' is missing or empty (set with %s).",
                              p, .ctdp_metadata_setters()[p] %||% "the relevant method"))
          next
        }
        # array minItems (e.g. resources >= 3)
        mi <- prof$property_min_items(p)
        if (!is.null(mi) && is.list(v) && length(.ctdp_prune_empty(v) %||% list()) < mi) {
          issues[[length(issues) + 1]] <- ctdp_issues(
            source = src, location_type = "package", field = p, constraint = "minItems",
            message = sprintf("Metadata property '%s' needs at least %d item(s).", p, mi))
        }
        # nested required keys of an object property (e.g. project, temporal)
        sub <- prof$property_required(p)
        if (length(sub) && is.list(v)) {
          for (s in sub) {
            if (is.null(v[[s]]) || is.null(.ctdp_prune_empty(v[[s]]))) {
              issues[[length(issues) + 1]] <- ctdp_issues(
                source = src, location_type = "package", field = paste0(p, ".", s),
                constraint = "required",
                message = sprintf("Required sub-property '%s.%s' is missing.", p, s))
            }
          }
        }
        # required keys of each item of an array property (e.g. taxonomic.scientificName)
        itm <- prof$item_required(p)
        if (length(itm) && is.list(v) && is.null(names(v))) {
          for (i in seq_along(v)) {
            for (s in itm) {
              if (is.null(v[[i]][[s]]) || is.null(.ctdp_prune_empty(v[[i]][[s]]))) {
                issues[[length(issues) + 1]] <- ctdp_issues(
                  source = src, location_type = "package", field = paste0(p, "[", i, "].", s),
                  constraint = "required",
                  message = sprintf("Item %d of '%s' is missing required key '%s'.", i, p, s))
              }
            }
          }
        }
      }
      out <- do.call(ctdp_bind_issues, issues)
      if (summarize) ctdp_summarize_validation(out)
      out
    },

    #' @description Pre-check that the package descriptor and its table schemas
    #'   conform to the Frictionless specification, before the authoritative
    #'   Python Frictionless step. Checks the package structure (non-empty
    #'   `resources`; each resource has `name` and `path`/`data`; unique names;
    #'   `profile` set) and the well-formedness of every loaded / inline table
    #'   schema (`ctdp_check_schema()`). Optionally runs a full JSON Schema
    #'   validation of the written descriptor when `jsonschema` is given and the
    #'   `jsonvalidate` package is installed.
    #' @param check_schemas Also check each table schema's well-formedness.
    #' @param jsonschema Optional path/URL to a JSON Schema to validate the
    #'   serialized descriptor against (requires the `jsonvalidate` package).
    #' @param summarize Print a summary.
    #' @return An issue table (see [ctdp_issues()]).
    check_descriptor = function(check_schemas = TRUE, jsonschema = NULL, summarize = TRUE) {
      src <- "datapackage.json"
      issues <- list()
      add <- function(field, constraint, message, severity = "error") {
        issues[[length(issues) + 1]] <<- ctdp_issues(
          source = src, location_type = "package", field = field,
          constraint = constraint, severity = severity, message = message)
      }
      if (length(self$resources) == 0) {
        add(NA_character_, "package-structure", "Package has no resources.")
      }
      names_seen <- character(0)
      for (r in self$resources) {
        nm <- r$name
        if (is.null(nm) || !nzchar(nm)) {
          add(NA_character_, "resource-structure", "A resource is missing its 'name'.")
        } else {
          if (nm %in% names_seen) {
            add(nm, "resource-unique", sprintf("Resource name '%s' is duplicated.", nm))
          }
          names_seen <- c(names_seen, nm)
        }
        if (is.null(r$path) && is.null(r$data)) {
          add(nm %||% NA_character_, "resource-structure",
              sprintf("Resource '%s' has neither 'path' nor 'data'.", nm %||% "?"))
        }
      }
      if (is.null(self$profile)) {
        add("profile", "profile-missing", "Package 'profile' is not set.", severity = "warning")
      }
      if (check_schemas) {
        for (nm in names(self$schemas)) {
          issues[[length(issues) + 1]] <- self$schemas[[nm]]$check_schema()
        }
        for (r in self$resources) {
          if (is.list(r$schema)) issues[[length(issues) + 1]] <- ctdp_check_schema(r$schema)
        }
      }
      # Optional deep JSON Schema validation of the serialized descriptor.
      if (!is.null(jsonschema)) {
        if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
          add("jsonschema", "jsonschema",
              "Package 'jsonvalidate' is required for 'jsonschema=' validation; install it with install.packages('jsonvalidate').",
              severity = "warning")
        } else {
          js <- private$descriptor_json()
          res <- tryCatch(
            jsonvalidate::json_validate(js, jsonschema, engine = "ajv", verbose = TRUE),
            error = function(e) e)
          if (inherits(res, "error")) {
            add("jsonschema", "jsonschema",
                sprintf("JSON Schema validation could not run (%s). Note: remote $ref may not resolve offline.",
                        conditionMessage(res)), severity = "warning")
          } else if (!isTRUE(res)) {
            errs <- attr(res, "errors")
            msgs <- if (is.data.frame(errs) && nrow(errs)) errs$message else "descriptor failed JSON Schema validation"
            for (m in msgs) add("datapackage.json", "jsonschema", as.character(m))
          }
        }
      }
      out <- do.call(ctdp_bind_issues, issues)
      if (summarize) ctdp_summarize_validation(out)
      out
    },

    #' @description Warn if the package `profile` is not a Camtrap DP profile.
    #'   A package can be a valid Frictionless data package yet not be Camtrap DP
    #'   form unless its profile is the Camtrap DP profile.
    #' @param summarize Print a summary.
    #' @return An issue table (see [ctdp_issues()]).
    check_camtrap_profile = function(summarize = TRUE) {
      p <- self$profile
      is_camtrap <- !is.null(p) && grepl("camtrap-dp", p, fixed = TRUE) &&
        grepl("profile", p, fixed = TRUE)
      issues <- if (is_camtrap) ctdp_no_issues() else ctdp_issues(
        source = "datapackage.json", location_type = "package", field = "profile",
        constraint = "camtrap-profile", severity = "warning",
        message = sprintf(paste0("Package profile %s is not a Camtrap DP profile; the result may be a ",
                                 "valid Frictionless data package but not Camtrap DP form."),
                          if (is.null(p)) "(unset)" else paste0("'", p, "'")))
      if (summarize) ctdp_summarize_validation(issues)
      issues
    },

    #' @description Check primary-key and foreign-key relations across the
    #'   registered tables, driven by each table's Table Schema.
    #' @param summarize If `TRUE`, print a summary of any issues.
    #' @return An issue table (see [ctdp_issues()]).
    check_relations = function(summarize = TRUE) {
      issues <- list()
      for (rname in names(self$data)) {
        sch <- tryCatch(self$get_schema(rname), error = function(e) NULL)
        if (is.null(sch)) next
        child_df <- self$data[[rname]]
        child_src <- private$resource_path(rname) %||% paste0(rname, ".csv")
        # primary key
        pk <- .ctdp_as_vector(sch$primaryKey)
        if (!is.null(pk)) {
          issues[[length(issues) + 1]] <- private$relation_pk(child_df, pk, child_src, sch$missingValues, rname)
        }
        # foreign keys
        for (fk in sch$foreignKeys) {
          issues[[length(issues) + 1]] <- private$relation_fk(rname, child_df, child_src, fk, sch$missingValues, sch)
        }
      }
      out <- do.call(ctdp_bind_issues, issues)
      if (summarize) ctdp_summarize_validation(out)
      out
    },

    #' @description List every external (URL) reference declared across the
    #'   package: the package `profile`, each resource's `schema` URL (or the
    #'   references inside an inline schema), and the semantic / description-URL
    #'   references of every loaded table schema. Use this when adopting a new
    #'   version or schema flavor so that no URL-specified requirement is missed.
    #' @return A tibble (`resource`, `field`, `key`, `category`, `url`).
    external_references = function() {
      rows <- list()
      if (!is.null(self$profile)) {
        rows[[length(rows) + 1]] <- tibble::tibble(
          resource = "datapackage", field = NA_character_, key = "profile",
          category = "profile-ref", url = self$profile)
      }
      for (r in self$resources) {
        if (is.character(r$schema)) {
          rows[[length(rows) + 1]] <- tibble::tibble(
            resource = r$name %||% NA_character_, field = NA_character_,
            key = "schema", category = "schema-ref", url = r$schema)
        } else if (is.list(r$schema)) {
          rows[[length(rows) + 1]] <- ctdp_schema_references(r$schema)
        }
      }
      for (nm in names(self$schemas)) {
        rows[[length(rows) + 1]] <- self$schemas[[nm]]$external_references()
      }
      rows <- Filter(function(x) !is.null(x) && nrow(x) > 0, rows)
      if (length(rows) == 0) {
        return(tibble::tibble(resource = character(0), field = character(0),
                              key = character(0), category = character(0), url = character(0)))
      }
      unique(do.call(rbind, rows))
    },

    #' @description Aggregate validation: per-table schema issues collected at
    #'   build time plus cross-table relation checks. Optionally also runs the
    #'   Python Frictionless validation.
    #' @param relations Whether to run `check_relations()`.
    #' @param metadata Whether to run `check_metadata()` (profile-driven).
    #' @param conformance Whether to run the Frictionless conformance pre-checks
    #'   `check_descriptor()` and `check_camtrap_profile()`.
    #' @param frictionless Whether to also run `validate_frictionless()`.
    #' @param summarize Whether to print a summary.
    #' @param ... Passed to `validate_frictionless()`.
    #' @return An issue table.
    validate = function(relations = TRUE, metadata = FALSE, conformance = FALSE,
                        frictionless = FALSE, summarize = TRUE, ...) {
      parts <- self$validation
      if (relations) parts <- c(parts, list(self$check_relations(summarize = FALSE)))
      if (metadata) parts <- c(parts, list(self$check_metadata(summarize = FALSE)))
      if (conformance) parts <- c(parts, list(self$check_descriptor(summarize = FALSE),
                                              self$check_camtrap_profile(summarize = FALSE)))
      if (frictionless) parts <- c(parts, list(self$validate_frictionless(summarize = FALSE, ...)))
      out <- do.call(ctdp_bind_issues, parts)
      if (summarize) ctdp_summarize_validation(out)
      invisible(out)
    },

    #' @description Run the Python Frictionless validation on the written data
    #'   package and parse the report into an issue table.
    #' @param directory Directory containing (or to receive) the data package.
    #'   Defaults to a temporary directory; the package is written there first.
    #' @param python Path to the Python interpreter.
    #' @param script Path to `frictionless_validate.py`. If `NULL`, resolved
    #'   from `getOption("camtrapdp.frictionless_script")`, else the installed
    #'   copy (`system.file("python/frictionless_validate.py", package =
    #'   "R2camtrapdp")`), else the loose-source `python/frictionless_validate.py`.
    #' @param write Whether to (re)write the data package before validating.
    #' @param patch_profile If `TRUE`, work around the malformed internal
    #'   `$ref` (`#$defs/version`) in the Camtrap DP 1.0 profile by validating
    #'   against a locally corrected copy. The written `datapackage.json` keeps
    #'   the canonical profile URL; only a separate validation descriptor is
    #'   patched. Has no effect for 1.0.1 / 1.0.2 (their profiles are correct).
    #' @param summarize Whether to print a summary.
    #' @return An issue table (engine `"frictionless"`).
    validate_frictionless = function(directory = NULL, python = "python",
                                     script = NULL, write = TRUE,
                                     patch_profile = TRUE, summarize = TRUE) {
      directory <- directory %||% file.path(tempdir(),
        paste0("camtrapdp-validate-", as.integer(Sys.time())))
      # Locate the Python helper: explicit arg > user option > installed package
      # (inst/python via system.file) > loose-source relative path.
      script <- .ctdp_resolve_frictionless_script(script)
      if (write) self$out_camtrapdp(write = TRUE, directory = directory)
      descriptor <- if (patch_profile) .ctdp_patch_descriptor(directory, self$profile) else "datapackage.json"
      report <- .ctdp_run_frictionless(directory, descriptor, python, script)
      issues <- ctdp_parse_frictionless(report, resource_paths = private$resource_path_map(),
                                        descriptor = private$descriptor_list())
      if (summarize) ctdp_summarize_validation(issues)
      issues
    },

    #' @description Exports the `camtrapdp` object and (optionally) writes the
    #'   data package to disk.
    #' @param write If `TRUE`, write the data package to `directory`.
    #' @param directory Output directory.
    #' @importFrom jsonlite toJSON
    #' @importFrom readr write_csv
    #' @return A `camtrapdp` object (list).
    out_camtrapdp = function(write = FALSE, directory = NULL) {
      outname <- c("resources", "profile", "name", "id", "created", "title",
                   "contributors", "description", "version", "keywords", "image",
                   "homepage", "sources", "licenses", "bibliographicCitation",
                   "project", "coordinatePrecision", "spatial", "temporal",
                   "taxonomic", "relatedIdentifiers", "references", "directory", "data")
      nout <- length(outname)
      dp <- vector("list", nout); names(dp) <- outname
      for (i in 1:nout) dp[[outname[i]]] <- self[[outname[i]]]
      class(dp) <- c("camtrapdp", "datapackage", "list")
      attr(dp, "version") <- self$version
      if (write) {
        if (is.null(directory)) stop("'directory' is required.\n")
        if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
        write(private$descriptor_json(), file = file.path(directory, "datapackage.json"))
        # Write each resource that has a path and a matching data table.
        resources.name <- sapply(dp$resources, "[[", "name")
        for (i in seq_along(dp$resources)) {
          res <- dp$resources[[i]]
          rname <- res$name
          if (is.null(res$path) || is.null(self$data[[rname]])) next
          readr::write_csv(private$prep_for_write(self$data[[rname]]),
                           file.path(directory, res$path), na = "")
        }
        message("Datapackage was saved to disk.")
      }
      return(dp)
    },

    #' @description Imports metadata from a list.
    #' @param metadata0 List of metadata.
    import_metadata = function(metadata0) {
      listname <- names(metadata0)
      listname <- listname[listname != "resources"]
      listname.valid <- listname[is.element(listname, names(self))]
      nlist <- length(listname.valid)
      for (i in 1:nlist) self[[listname.valid[i]]] <- metadata0[[listname.valid[i]]]
    }
  ),

  private = list(
    # Shared implementation for the three standard tables.
    set_standard = function(resource, slot, data, path, profile, format, mediatype,
                            encoding, schema, mapping, datetime_merges, validate,
                            local_schema, tz) {
      # When the caller did not pass an explicit `schema=`, use the resource's
      # configured template (set via set_properties(schema_urls=)), so a flavor
      # such as bioacoustics is honoured instead of the camera-trap default.
      schema_template <- schema %||% self$schema_urls[[resource]]
      schema_resolved <- if (is.null(schema_template)) NA_character_ else
        .ctdp_sub_version(schema_template, self$version)
      sch <- tryCatch(
        self$get_schema(resource, schema_url = schema_template, local_path = local_schema),
        error = function(e) {
          warning(sprintf("Could not load %s schema (%s). Storing data without schema validation.",
                          resource, conditionMessage(e)), call. = FALSE)
          NULL
        })
      if (!is.null(sch)) {
        built <- ctdp_build_table(sch, data, mapping = mapping,
                                  datetime_merges = datetime_merges, tz = tz, source = path)
        self$data[[resource]] <- built$data
        self$validation[[resource]] <- built$issues
        if (validate) ctdp_summarize_validation(built$issues)
        schema_resolved <- private$resolve_schema(sch, built$data, schema_resolved)
      } else {
        self$data[[resource]] <- tibble::as_tibble(data)
      }
      resource_entry <- list(name = resource, path = path, profile = profile,
                             format = format, mediatype = mediatype,
                             encoding = encoding, schema = schema_resolved)
      self$resources[[slot]] <- resource_entry
      invisible(self)
    },

    # Resolve a resource's `schema` value: a URL string when the data uses only
    # official schema fields, or an inline extended Table Schema (the official
    # schema with custom column definitions appended) when the data carries
    # extra columns -- so Frictionless accepts the custom columns.
    resolve_schema = function(sch, data, url_fallback) {
      if (is.null(sch)) return(url_fallback)
      extra <- setdiff(names(data), sch$field_order)
      if (length(extra) == 0) return(sch$url %||% url_fallback)
      raw <- sch$raw
      if (is.null(raw$fields)) raw$fields <- list()
      for (col in extra) {
        raw$fields[[length(raw$fields) + 1]] <- .ctdp_infer_field(col, data[[col]])
      }
      raw
    },

    upsert_resource = function(resource) {
      names_now <- vapply(self$resources, function(r) r$name %||% NA_character_, character(1))
      idx <- match(resource$name, names_now)
      if (is.na(idx)) {
        self$resources[[length(self$resources) + 1]] <- resource
      } else {
        self$resources[[idx]] <- resource
      }
    },

    resource_path = function(name) {
      for (r in self$resources) if (identical(r$name, name)) return(r$path)
      NULL
    },

    resource_path_map = function() {
      out <- character(0)
      for (r in self$resources) {
        if (!is.null(r$name) && !is.null(r$path)) out[[r$name]] <- r$path
      }
      out
    },

    # Is a metadata property present in the package as it would be written?
    # `out_camtrapdp()` prunes empty metadata, so an empty property becomes
    # absent -- except `taxonomic`, which is always emitted (an empty array is
    # valid), so it counts as present.
    metadata_present = function(p) {
      if (identical(p, "taxonomic")) return(TRUE)
      !is.null(.ctdp_prune_empty(self[[p]]))
    },

    relation_pk = function(df, pk, src, mv, rname) {
      out <- list()
      loc <- sprintf("datapackage$data$%s", rname)
      # Primary-key column(s) entirely absent from the stored data: point the
      # user at the data object instead of silently skipping.
      for (col in setdiff(pk, names(df))) {
        warning(sprintf("%s has no column '%s' (the primary key). Check the data passed to set_%s().",
                        loc, col, rname), call. = FALSE)
        out[[length(out) + 1]] <- ctdp_issues(
          source = src, location_type = "relation", field = col, constraint = "primaryKey",
          message = sprintf("%s has no column '%s' (primary key is required).", loc, col))
      }
      have <- intersect(pk, names(df))
      if (length(have) == 0) return(do.call(ctdp_bind_issues, out))
      key <- do.call(paste, c(lapply(have, function(n) as.character(df[[n]])), sep = "\r"))
      any_missing <- Reduce(`|`, lapply(have, function(n) .ctdp_is_missing(as.character(df[[n]]), mv)))
      rows <- which(any_missing)
      if (length(rows)) out[[length(out) + 1]] <- ctdp_issues(
        source = src, location_type = "relation", field = paste(have, collapse = "+"),
        row = rows, constraint = "primaryKey",
        message = sprintf("Primary key (%s) must not be missing; see %s.",
                          paste(have, collapse = ", "), loc))
      dup <- (duplicated(key) | duplicated(key, fromLast = TRUE)) & !any_missing
      rows <- which(dup)
      if (length(rows)) out[[length(out) + 1]] <- ctdp_issues(
        source = src, location_type = "relation", field = paste(have, collapse = "+"),
        row = rows, constraint = "primaryKey",
        message = sprintf("Primary key (%s) duplicated; see %s.",
                          paste(have, collapse = ", "), loc))
      do.call(ctdp_bind_issues, out)
    },

    relation_fk = function(child_name, child_df, child_src, fk, mv, sch = NULL) {
      child_fields <- .ctdp_as_vector(fk$fields)
      parent_res <- fk$reference$resource
      parent_fields <- .ctdp_as_vector(fk$reference$fields)
      # Self-referencing FK (reference$resource == "" or same): use child table.
      if (is.null(parent_res) || !nzchar(parent_res)) parent_res <- child_name
      loc <- sprintf("datapackage$data$%s", child_name)
      # Child foreign-key column(s) absent from the stored data: report instead
      # of silently skipping, and point the user at the data object.
      absent <- setdiff(child_fields, names(child_df))
      if (length(absent)) {
        warning(sprintf("%s has no column '%s' (foreign key to '%s'). Check the data passed to set_%s().",
                        loc, paste(absent, collapse = ", "), parent_res, child_name), call. = FALSE)
        return(ctdp_issues(
          source = child_src, location_type = "relation",
          field = paste(absent, collapse = "+"), constraint = "foreignKey",
          message = sprintf("%s has no column '%s' (foreign key to '%s').",
                            loc, paste(absent, collapse = ", "), parent_res)))
      }
      # A required foreign-key column that is present but entirely missing is
      # almost always a mistake (e.g. a column-name mismatch where coerce filled
      # it with NA). Warn and point at the data; optional FKs (all-NA allowed,
      # e.g. event-level mediaID) are left to the per-row check below.
      out0 <- list()
      for (col in child_fields) {
        req <- !is.null(sch) && isTRUE(sch$fields[[col]]$constraints$required)
        if (req && all(.ctdp_is_missing(as.character(child_df[[col]]), mv))) {
          warning(sprintf("%s has '%s' entirely missing, but it is a required foreign key to '%s'. Check the data passed to set_%s() (e.g. a column-name mismatch).",
                          loc, col, parent_res, child_name), call. = FALSE)
          out0[[length(out0) + 1]] <- ctdp_issues(
            source = child_src, location_type = "relation", field = col, constraint = "foreignKey",
            message = sprintf("Required foreign key '%s' is entirely missing in %s.", col, loc))
        }
      }
      bind0 <- function(x = NULL) do.call(ctdp_bind_issues, c(out0, list(x)))
      if (is.null(self$data[[parent_res]])) {
        return(bind0(ctdp_issues(
          source = child_src, location_type = "relation",
          field = paste(child_fields, collapse = "+"), constraint = "foreignKey",
          message = sprintf("Foreign key '%s' references resource '%s', which is not registered (datapackage$data$%s is missing).",
                            paste(child_fields, collapse = "+"), parent_res, parent_res))))
      }
      parent_df <- self$data[[parent_res]]
      if (!all(parent_fields %in% names(parent_df))) {
        return(bind0(ctdp_issues(
          source = child_src, location_type = "relation",
          field = paste(child_fields, collapse = "+"), constraint = "foreignKey",
          message = sprintf("Referenced field(s) %s missing in datapackage$data$%s.",
                            paste(parent_fields, collapse = ", "), parent_res))))
      }
      child_key <- do.call(paste, c(lapply(child_fields, function(n) as.character(child_df[[n]])), sep = "\r"))
      parent_key <- do.call(paste, c(lapply(parent_fields, function(n) as.character(parent_df[[n]])), sep = "\r"))
      # A FK value of all-missing is allowed (optional reference).
      child_missing <- Reduce(`|`, lapply(child_fields, function(n) .ctdp_is_missing(as.character(child_df[[n]]), mv)))
      bad <- !child_missing & !(child_key %in% parent_key)
      rows <- which(bad)
      if (length(rows) == 0) return(bind0())
      bind0(ctdp_issues(
        source = child_src, location_type = "relation",
        field = paste(child_fields, collapse = "+"), row = rows, constraint = "foreignKey",
        value = vapply(rows, function(r) gsub("\r", "+", child_key[r]), character(1)),
        message = vapply(rows, function(r) sprintf(
          "%s '%s' has no matching %s in '%s'.",
          paste(child_fields, collapse = "+"),
          gsub("\r", "+", child_key[r]),
          paste(parent_fields, collapse = "+"), parent_res), character(1))))
    },

    prep_for_write = function(df) {
      df <- tibble::as_tibble(df)
      for (n in names(df)) {
        if (is.logical(df[[n]])) {
          df[[n]] <- ifelse(is.na(df[[n]]), NA_character_,
                            ifelse(df[[n]], "true", "false"))
        }
      }
      df
    },

    # The metadata descriptor (datapackage.json content) as a pruned list:
    # unset metadata is dropped so empty placeholders ([] / null) do not trigger
    # spurious profile-validation errors; `taxonomic` is kept (empty array is
    # valid and the profile requires its presence). Shared by out_camtrapdp() and
    # check_descriptor()'s optional JSON Schema validation.
    descriptor_list = function() {
      meta_names <- c("resources", "profile", "name", "id", "created", "title",
                      "contributors", "description", "version", "keywords", "image",
                      "homepage", "sources", "licenses", "bibliographicCitation",
                      "project", "coordinatePrecision", "spatial", "temporal",
                      "taxonomic", "relatedIdentifiers", "references")
      dp_write <- list()
      for (n in meta_names) dp_write[[n]] <- self[[n]]
      dp_write <- .ctdp_prune_empty(dp_write)
      if (is.null(dp_write$taxonomic)) dp_write$taxonomic <- list()
      dp_write
    },

    descriptor_json = function() {
      jsonlite::toJSON(private$descriptor_list(), pretty = NULL, null = "null",
                       na = "null", auto_unbox = TRUE)
    }
  )
)
