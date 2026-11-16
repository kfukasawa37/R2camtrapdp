test_that("create_deployments() keeps cameraID when cameraModel is omitted", {
  d <- create_deployments(
    deploymentID = "A01", latitude = 35, longitude = 139,
    deploymentStart = "2023-04-01 09:00:00", deploymentEnd = "2023-05-01 09:00:00",
    cameraID = "CAM-1")
  expect_equal(d$cameraID, "CAM-1")
  expect_true(is.na(d$cameraModel))
})

test_that("create_observations() validates cameraSetupType itself", {
  obs <- function(type) create_observations(
    observationID = "o1", deploymentID = "A01", mediaID = "m1",
    eventStart = "2023-04-01 09:05:00", eventEnd = "2023-04-01 09:05:00",
    observationLevel = "media", observationType = "unclassified",
    cameraSetupType = type)
  expect_equal(as.character(obs("setup")$cameraSetupType), "setup")
  expect_equal(as.character(obs("calibration")$cameraSetupType), "calibration")
  expect_error(obs("maintenance"), "'cameraSetupType' has invalid elements")
})

test_that("add_contributors() adds nothing for a 0-row data frame", {
  dp <- R6_CamtrapDP$new()
  dp$add_contributors(data.frame(title = character(0)))
  expect_length(dp$contributors, 0)
})

test_that("import_metadata() ignores a list without importable properties", {
  dp <- R6_CamtrapDP$new(title = "Kept")
  expect_error(dp$import_metadata(list(unknown = 1)), NA)
  expect_equal(dp$title, "Kept")
})

test_that("optional enumerated fields accept NA", {
  dep <- function(...) create_deployments(
    deploymentID = c("A01", "A02"), latitude = c(35, 35), longitude = c(139, 139),
    deploymentStart = rep("2023-04-01 09:00:00", 2),
    deploymentEnd = rep("2023-05-01 09:00:00", 2), ...)
  expect_equal(dep(timestampIssues = c("true", NA))$timestampIssues, c(TRUE, NA))
  expect_equal(dep(baitUse = c("false", NA))$baitUse, c(FALSE, NA))
  expect_error(dep(timestampIssues = c("yes", NA)), "'timestampIssues' has invalid elements")
  expect_error(dep(baitUse = c("yes", NA)), "'baitUse' has invalid elements")

  med <- function(...) create_media(
    mediaID = c("m1", "m2"), deploymentID = c("A01", "A01"),
    timestamp_date = rep("2023-04-01", 2), timestamp_time = c("09:05:00", "09:06:00"),
    filePath = c("img/m1.jpg", "img/m2.jpg"), filePublic = c(TRUE, TRUE),
    fileMediatype = rep("image/jpeg", 2), ...)
  expect_equal(as.character(med(captureMethod = c("timeLapse", NA))$captureMethod),
               c("timeLapse", NA))
  expect_error(med(favorite = c("true", NA)), NA)
  expect_error(med(captureMethod = c("motion", NA)), "'captureMethod' has invalid elements")
  expect_error(med(favorite = c("yes", NA)), "'favorite' has invalid elements")

  obs <- function(...) create_observations(
    observationID = c("o1", "o2"), deploymentID = c("A01", "A01"), mediaID = c("m1", "m2"),
    eventStart = rep("2023-04-01 09:05:00", 2), eventEnd = rep("2023-04-01 09:05:00", 2),
    observationLevel = rep("media", 2), observationType = rep("unclassified", 2), ...)
  expect_equal(as.character(obs(cameraSetupType = c("setup", NA))$cameraSetupType),
               c("setup", NA))
  expect_equal(as.character(obs(classificationMethod = c("machine", NA))$classificationMethod),
               c("machine", NA))
  expect_error(obs(cameraSetupType = c("maintenance", NA)), "'cameraSetupType' has invalid elements")
  expect_error(obs(classificationMethod = c("model", NA)), "'classificationMethod' has invalid elements")
})
