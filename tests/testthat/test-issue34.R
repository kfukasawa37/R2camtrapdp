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
