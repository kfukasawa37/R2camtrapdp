# =============================================================================
# r2camtrapdp — schema-driven example
#
# Converts an arbitrary spreadsheet into a Camtrap DP data package in four steps:
#   1. Derive shell tables + constraints from the requested version's schema
#   2. Map input columns, coerce types, validate constraints (incl. custom cols)
#   3. Build the data package (CSVs + datapackage.json)
#   4. Validate with Python Frictionless and parse the report
#
# Run from the repository root:
#   Rscript examples/example_usage.R
#
# Step 4 requires Python with frictionless installed:  pip install frictionless
# =============================================================================

source("r2camtrapdp.R")

## Any Camtrap DP version works; the schema for that version drives everything.
VERSION <- "1.0.1"
## Path to the Python interpreter that has `frictionless` installed.
PYTHON  <- "python"

# ---------------------------------------------------------------------------
# 1. Inspect the schema and create a shell table (the "殻")
# ---------------------------------------------------------------------------
dep_schema <- TableSchema$new("deployments", version = VERSION)
cat("deployments fields:\n"); print(dep_schema$field_names())
cat("required:\n");           print(dep_schema$required_field_names())
shell <- dep_schema$empty_table()          # 0-row, correctly typed, all columns
str(shell, list.len = 6)

# ---------------------------------------------------------------------------
# 2. Arbitrary input spreadsheet -> mapping -> datetime merge -> coerce -> validate
# ---------------------------------------------------------------------------
raw_dep <- data.frame(
  station   = c("A01", "A02"),               # -> deploymentID
  lat       = c(35.1, 36.2),                  # -> latitude
  lon       = c(139.5, 140.1),                # -> longitude
  start_day = c("2023-04-01", "2023-04-02"),  # -> deploymentStart (date part)
  start_clk = c("09:00:00", "10:30:00"),      #    (time part)
  end_day   = c("2023-05-01", "2023-05-02"),  # -> deploymentEnd (date part)
  end_clk   = c("09:00:00", "10:30:00"),
  myNote    = c("custom column kept as-is", "also kept"),  # custom column
  stringsAsFactors = FALSE
)

# Mapping: names are SOURCE columns, values are Camtrap DP FIELD names.
mapping <- c(station = "deploymentID", lat = "latitude", lon = "longitude")

# (Datetimes may also be passed as POSIXct, e.g. via as.POSIXct(); the package
#  then writes the schema's datetime format, including the UTC offset and any
#  required fractional seconds. Here we merge separate date/time columns.)

built <- ctdp_build_table(
  dep_schema, raw_dep, mapping = mapping,
  datetime_merges = list(
    list(date_col = "start_day", time_col = "start_clk", target = "deploymentStart"),
    list(date_col = "end_day",   time_col = "end_clk",   target = "deploymentEnd")
  ),
  tz = "Asia/Tokyo"
)
ctdp_summarize_validation(built$issues)     # R-side schema check

# ---------------------------------------------------------------------------
# 3. Assemble the data package via the R6 class (method names unchanged)
# ---------------------------------------------------------------------------
dp <- R6_CamtrapDP$new(
  title = "Example", description = "Schema-driven demo", version = VERSION,
  id = "https://example.org/dataset/1"
)

dp$set_deployments(built$data)              # schema-driven: coerces + validates

dp$set_media(data.frame(
  mediaID = "m1", deploymentID = "A01",
  timestamp = "2023-04-01T09:05:00+0900",
  filePath = "img/m1.jpg", filePublic = TRUE, fileMediatype = "image/jpeg",
  stringsAsFactors = FALSE
))

dp$set_observations(data.frame(
  observationID = "o1", deploymentID = "A01", mediaID = "m1",
  eventStart = "2023-04-01T09:05:00+0900", eventEnd = "2023-04-01T09:05:00+0900",
  observationLevel = "media", observationType = "animal",
  scientificName = "Vulpes vulpes", count = 1L,
  stringsAsFactors = FALSE
))

# Required Camtrap DP metadata
dp$add_contributors(data.frame(title = "Jane Doe", role = "contact",
                               organization = "NIES", stringsAsFactors = FALSE))
dp$add_sources(title = "Original spreadsheet")
dp$add_license(name = "CC0-1.0",   scope = "data")
dp$add_license(name = "CC-BY-4.0", scope = "media")
dp$set_project(title = "Project X", samplingDesign = "opportunistic",
               captureMethod = "activityDetection", individualAnimals = FALSE,
               observationLevel = "media")
dp$set_st()                                  # spatial + temporal from deployments

# Profile-derived required-metadata checklist (which properties the version's
# profile requires, the method that sets each, and whether it is set yet):
print(dp$metadata_requirements()[, c("property", "required", "set_with", "currently_set")])
dp$check_metadata()                          # report any missing required metadata

# Cross-table relation check (foreign keys read from each table's schema).
# If a key column is entirely missing it warns and points at datapackage$data$...
dp$check_relations()

# Write CSVs + datapackage.json
out_dir <- file.path(tempdir(), "example-camtrapdp")
dp$out_camtrapdp(write = TRUE, directory = out_dir)
cat("Wrote package to:", out_dir, "\n")

# ---------------------------------------------------------------------------
# 4. Frictionless validation (requires: pip install frictionless)
# ---------------------------------------------------------------------------
issues <- tryCatch(
  dp$validate_frictionless(directory = out_dir, python = PYTHON),
  error = function(e) { message("Skipping frictionless: ", conditionMessage(e)); NULL }
)
if (!is.null(issues)) {
  cat("Frictionless valid:", ctdp_is_valid(issues), "\n")
  # `issues` is a tidy table; for any error you get the file, column, row, rule
  # and the offending `value`:
  if (!ctdp_is_valid(issues)) print(issues[, c("source", "field", "row", "constraint", "value")])
}

# Validate-only: check a package that already exists on disk WITHOUT rewriting it
# (the method above re-writes `out_dir` first; this standalone function does not).
issues2 <- tryCatch(
  ctdp_validate_frictionless(out_dir, python = PYTHON),
  error = function(e) { message("Skipping frictionless: ", conditionMessage(e)); NULL }
)

# Aggregate the R-side checks: per-table schema issues + relations + profile
# metadata + descriptor/profile conformance (no Python needed).
invisible(dp$validate(relations = TRUE, metadata = TRUE, conformance = TRUE))
