test_that("create_deployments() keeps cameraID when cameraModel is omitted", {
  d <- create_deployments(
    deploymentID = "A01", latitude = 35, longitude = 139,
    deploymentStart = "2023-04-01 09:00:00", deploymentEnd = "2023-05-01 09:00:00",
    cameraID = "CAM-1")
  expect_equal(d$cameraID, "CAM-1")
  expect_true(is.na(d$cameraModel))
})
