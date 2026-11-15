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
