# Helpers shared by the verification tests. They follow the steps of the
# "Verifying the installation" vignette (vignettes/Vignette_R2camtrapdp_Verify.Rmd):
# the bundled example data are converted into every supported Camtrap DP
# release and variant, then validated in R and (optionally) with Frictionless.

camera_versions <- c("1.0", "1.0.1", "1.0.2")

acoustic_base <- "https://raw.githubusercontent.com/camera-traps/bioacoustics/main/camtrap-dp/1.0.2/%s"

# Skip when the official schemas cannot be downloaded (CRAN, offline machines).
skip_if_no_schemas <- function() {
  skip_on_cran()
  ok <- tryCatch({
    con <- url("https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.2/deployments-table-schema.json")
    on.exit(close(con))
    length(readLines(con, n = 1, warn = FALSE)) > 0
  }, error = function(e) FALSE)
  if (!ok) skip("Camtrap DP schemas cannot be downloaded (offline?)")
}

# Python interpreter with the 'frictionless' package, or "" when unavailable.
# Set the environment variable R2CAMTRAPDP_PYTHON to choose a specific one.
find_frictionless_python <- function() {
  cand <- c(Sys.getenv("R2CAMTRAPDP_PYTHON"), Sys.which(c("python3", "python")))
  for (py in unique(cand[nzchar(cand)])) {
    status <- suppressWarnings(system2(py, c("-c", shQuote("import frictionless")),
                                       stdout = FALSE, stderr = FALSE))
    if (identical(status, 0L)) return(unname(py))
  }
  ""
}

skip_if_no_frictionless <- function() {
  py <- find_frictionless_python()
  if (!nzchar(py)) skip("Python with 'frictionless' is not available")
  py
}

# Minimal required metadata. taxonomic is filled from the observed scientific
# names directly, so that the tests do not need the taxadb database download.
add_example_metadata <- function(dp, observations, captureMethod, observationLevel,
                                 samplingDesign) {
  dp$add_contributors(data.frame(title = "Example contributor", role = "contact"))
  dp$set_project(title = "Installation check", samplingDesign = samplingDesign,
                 captureMethod = captureMethod, individualAnimals = FALSE,
                 observationLevel = observationLevel)
  dp$set_st()
  dp$taxonomic <- lapply(sort(unique(stats::na.omit(observations$scientificName))),
                         function(x) list(scientificName = x))
  dp$update_created()
  invisible(dp)
}

# Camera-trap package from Idep / Iobs for one Camtrap DP release.
build_camera_package <- function(version) {
  Idep <- R2camtrapdp::Idep
  Iobs <- R2camtrapdp::Iobs

  deployments <- create_deployments(
    deploymentID         = Idep$deploymentID,
    longitude            = Idep$longitude,
    latitude             = Idep$latitude,
    locationID           = Idep$locationID,
    deploymentStart_date = Idep$startDate,
    deploymentStart_time = Idep$startTime,
    deploymentEnd_date   = Idep$endDate,
    deploymentEnd_time   = Idep$endTime,
    cameraID             = Idep$cameraID,
    cameraModel          = Idep$cameraModel,
    cameraDelay          = Idep$Delay,
    cameraHeight         = Idep$Height,
    baitUse              = Idep$bait,
    setupBy              = Idep$setupBy)

  mediaID <- paste(Iobs$institutionCode, Iobs$collectionCode, Iobs$locationID,
                   as.numeric(factor(Iobs$filename)), sep = "_")
  media <- create_media(
    mediaID        = mediaID,
    deploymentID   = Iobs$deploymentID,
    timestamp_date = Iobs$date,
    timestamp_time = Iobs$time,
    filePath       = "Image",
    filePublic     = TRUE,
    fileMediatype  = paste0("image/", tolower(tools::file_ext(Iobs$filename))),
    captureMethod  = "activityDetection",
    fileName       = Iobs$filename)

  observations <- create_observations(
    observationID             = paste(mediaID, Iobs$obsID, sep = "_"),
    deploymentID              = Iobs$deploymentID,
    eventID                   = Iobs$eventID,
    eventStart                = Iobs$eventStart,
    eventEnd                  = Iobs$eventEnd,
    observationLevel          = "event",
    observationType           = ifelse(Iobs$object == "hito", "human",
                                ifelse(Iobs$object == "none", "blank",
                                ifelse(Iobs$object == "unidentifiable", "unknown", "animal"))),
    scientificName            = ifelse(is.na(Iobs$genus), Iobs$class,
                                       paste(Iobs$genus, Iobs$species)),
    count                     = Iobs$individualCount,
    classificationMethod      = "human",
    classificationProbability = 1)

  dp <- R6_CamtrapDP$new(version = version)
  dp$set_deployments(deployments, validate = FALSE)
  dp$set_media(media, validate = FALSE)
  dp$set_observations(observations, validate = FALSE)
  add_example_metadata(dp, observations, captureMethod = "activityDetection",
                       observationLevel = "event", samplingDesign = "simpleRandom")
}

# Acoustic (bioacoustics variant, 1.0.2) package from Adep / Aobs.
build_acoustic_package <- function() {
  Adep <- R2camtrapdp::Adep
  Aobs <- R2camtrapdp::Aobs

  dp <- R6_CamtrapDP$new(version = "1.0.2")
  dp$set_properties(
    version     = "1.0.2",
    profile     = sprintf(acoustic_base, "camtrap-dp-profile-acoustic.json"),
    schema_urls = list(
      deployments  = sprintf(acoustic_base, "deployments-table-schema-acoustic.json"),
      media        = sprintf(acoustic_base, "media-table-schema-acoustic.json"),
      observations = sprintf(acoustic_base, "observations-table-schema-acoustic.json")))

  deployments <- data.frame(
    deploymentID    = Adep$deploymentID,
    latitude        = Adep$latitude,
    longitude       = Adep$longitude,
    locationID      = Adep$locationID,
    deploymentStart = as.POSIXct(paste(Adep$startDate, Adep$startTime), tz = "Asia/Tokyo"),
    deploymentEnd   = as.POSIXct(paste(Adep$endDate, Adep$endTime), tz = "Asia/Tokyo"),
    deviceID        = Adep$deviceID,
    deviceModel     = Adep$deviceModel,
    setupBy         = Adep$setupBy)

  files <- Aobs[!duplicated(Aobs$filename), ]
  media <- data.frame(
    mediaID       = files$filename,
    deploymentID  = files$deploymentID,
    timestamp     = as.POSIXct(paste(files$date, files$time), tz = "Asia/Tokyo"),
    filePath      = file.path("audio", files$filename),
    filePublic    = TRUE,
    fileMediatype = paste0("audio/", tolower(tools::file_ext(files$filename))),
    duration      = files$duration)
  media <- merge(media, Adep[, c("deploymentID", "samplingFrequency", "bitDepth", "channels")],
                 by = "deploymentID", all.x = TRUE)

  observations <- data.frame(
    observationID    = paste(Aobs$deploymentID, Aobs$eventID, Aobs$obsID, sep = "_"),
    deploymentID     = Aobs$deploymentID,
    mediaID          = Aobs$filename,
    eventStart       = as.POSIXct(Aobs$eventStart, tz = "Asia/Tokyo"),
    eventEnd         = as.POSIXct(Aobs$eventEnd, tz = "Asia/Tokyo"),
    observationLevel = "media",
    observationType  = ifelse(Aobs$object == "none", "blank",
                       ifelse(Aobs$object == "hito", "human", "animal")),
    scientificName   = ifelse(is.na(Aobs$genus), Aobs$class, paste(Aobs$genus, Aobs$species)),
    count            = Aobs$individualCount,
    frequencyLow     = Aobs$frequencyLow,
    frequencyHigh    = Aobs$frequencyHigh)

  dp$set_deployments(deployments, validate = FALSE)
  dp$set_media(media, validate = FALSE)
  dp$set_observations(observations, validate = FALSE)
  add_example_metadata(dp, observations, captureMethod = "recordingSchedule",
                       observationLevel = "media", samplingDesign = "systematicRandom")
}
