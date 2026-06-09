#' Deployment data of videos which collected by a camera-trap
#' 
#' @name Vdep
#' @docType data
#' @references Originally data can be used from https://doi.org/10.34462/0002000233
#' @keywords data
#' 
NULL

#' Observation data of videos which collected by a camera-trap
#' @name Vobs
#' @docType data
#' @references Originally data can be used from https://doi.org/10.34462/0002000233
#' @keywords data
NULL

#' Camtrap DP objects have been created from Vdep and Vobs
#' @name datapackageVdata
#' @docType data
#' @references Originally data can be used from https://doi.org/10.34462/0002000233
#' @keywords data camtrapDP
NULL
#' Example deployment data (multiple camera deployments with image records)
#'
#' A small example deployment table used in the package vignettes and examples.
#' One row per camera deployment.
#'
#' @format A data frame with 10 rows and 14 variables:
#' \describe{
#'   \item{deploymentID}{Unique identifier of the deployment.}
#'   \item{longitude}{Longitude in decimal degrees (WGS84).}
#'   \item{latitude}{Latitude in decimal degrees (WGS84).}
#'   \item{locationID}{Identifier of the deployment location.}
#'   \item{startDate}{Deployment start date.}
#'   \item{startTime}{Deployment start time.}
#'   \item{endDate}{Deployment end date.}
#'   \item{endTime}{Deployment end time.}
#'   \item{cameraID}{Identifier of the camera.}
#'   \item{cameraModel}{Manufacturer and model of the camera.}
#'   \item{Delay}{Predefined duration after detection during which further
#'     activity is ignored (camera delay).}
#'   \item{Height}{Height at which the camera was deployed.}
#'   \item{bait}{Whether bait was used for the deployment.}
#'   \item{setupBy}{Name or identifier of the person/organization that deployed
#'     the camera.}
#' }
#' @seealso [Iobs]
"Idep"

#' Example observation data (image records)
#'
#' A small example observation table used in the package vignettes and examples.
#' One row per observation.
#'
#' @format A data frame with 388 rows and 17 variables:
#' \describe{
#'   \item{institutionCode}{Institution code.}
#'   \item{collectionCode}{Collection code.}
#'   \item{obsID}{Observation identifier (within an event).}
#'   \item{eventID}{Identifier of the event the observation belongs to.}
#'   \item{locationID}{Identifier of the deployment location.}
#'   \item{date}{Date the media file was recorded.}
#'   \item{time}{Time the media file was recorded.}
#'   \item{object}{Recorded object category (raw label).}
#'   \item{class}{Taxonomic class of the observed organism.}
#'   \item{genus}{Genus of the observed organism.}
#'   \item{species}{Species epithet of the observed organism.}
#'   \item{individualCount}{Number of observed individuals.}
#'   \item{SDcardID}{Identifier of the SD card.}
#'   \item{filename}{Name of the media file.}
#'   \item{deploymentID}{Identifier of the deployment the observation belongs to.}
#'   \item{eventStart}{Date and time at which the event started.}
#'   \item{eventEnd}{Date and time at which the event ended.}
#' }
#' @seealso [Idep]
"Iobs"

#' Example acoustic deployment field-notebook
#'
#' Example deployment notebook for an acoustic (audio) survey, used in the
#' acoustic vignette. One row per device deployment.
#'
#' @format A data frame with 2 rows and 14 variables:
#' \describe{
#'   \item{deploymentID}{Unique identifier of the deployment.}
#'   \item{longitude}{Longitude in decimal degrees (WGS84).}
#'   \item{latitude}{Latitude in decimal degrees (WGS84).}
#'   \item{locationID}{Identifier of the deployment location.}
#'   \item{startDate}{Deployment start date.}
#'   \item{startTime}{Deployment start time.}
#'   \item{endDate}{Deployment end date.}
#'   \item{endTime}{Deployment end time.}
#'   \item{deviceID}{Identifier of the recording device.}
#'   \item{deviceModel}{Manufacturer and model of the recording device.}
#'   \item{samplingFrequency}{Sampling frequency of the recordings (Hz).}
#'   \item{bitDepth}{Bit depth of the recordings.}
#'   \item{channels}{Number of audio channels.}
#'   \item{setupBy}{Name or identifier of the person/organization that deployed
#'     the device.}
#' }
#' @seealso [Aobs]
"Adep"

#' Example acoustic observation field-notebook
#'
#' Example observation notebook for an acoustic (audio) survey, used in the
#' acoustic vignette. One row per observation; the `filename` column is the
#' audio file from which the `media` table is derived.
#'
#' @format A data frame with 6 rows and 19 variables:
#' \describe{
#'   \item{institutionCode}{Institution code.}
#'   \item{collectionCode}{Collection code.}
#'   \item{obsID}{Observation identifier (within an event).}
#'   \item{eventID}{Identifier of the event the observation belongs to.}
#'   \item{deploymentID}{Identifier of the deployment.}
#'   \item{locationID}{Identifier of the deployment location.}
#'   \item{date}{Date the recording was made.}
#'   \item{time}{Time the recording started.}
#'   \item{filename}{Name of the audio file (used to build `media`).}
#'   \item{duration}{Duration of the recording file (seconds).}
#'   \item{object}{Recorded object category (`animal`, `none`, ...).}
#'   \item{class}{Taxonomic class of the observed organism.}
#'   \item{genus}{Genus of the observed organism.}
#'   \item{species}{Species epithet of the observed organism.}
#'   \item{individualCount}{Number of observed individuals (`NA` for blanks).}
#'   \item{frequencyLow}{Lower bound of the call frequency (Hz).}
#'   \item{frequencyHigh}{Upper bound of the call frequency (Hz).}
#'   \item{eventStart}{Date and time at which the event started.}
#'   \item{eventEnd}{Date and time at which the event ended.}
#' }
#' @seealso [Adep]
"Aobs"
