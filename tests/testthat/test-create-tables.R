# Offline checks of the table constructors on the bundled example data.
# These need no internet access or Python and therefore also run on CRAN.

test_that("create_deployments() builds one row per deployment from Idep", {
  Idep <- R2camtrapdp::Idep
  dep <- create_deployments(
    deploymentID         = Idep$deploymentID,
    longitude            = Idep$longitude,
    latitude             = Idep$latitude,
    deploymentStart_date = Idep$startDate,
    deploymentStart_time = Idep$startTime,
    deploymentEnd_date   = Idep$endDate,
    deploymentEnd_time   = Idep$endTime)
  expect_equal(nrow(dep), nrow(Idep))
  expect_false(anyDuplicated(dep$deploymentID) > 0)
  expect_s3_class(dep$deploymentStart, "POSIXt")
  expect_true(all(dep$deploymentStart <= dep$deploymentEnd))
})

test_that("create_observations() keeps every Iobs record with unique IDs", {
  Iobs <- R2camtrapdp::Iobs
  mediaID <- paste(Iobs$institutionCode, Iobs$collectionCode, Iobs$locationID,
                   as.numeric(factor(Iobs$filename)), sep = "_")
  obs <- create_observations(
    observationID    = paste(mediaID, Iobs$obsID, sep = "_"),
    deploymentID     = Iobs$deploymentID,
    eventID          = Iobs$eventID,
    eventStart       = Iobs$eventStart,
    eventEnd         = Iobs$eventEnd,
    observationLevel = "event",
    observationType  = "animal")
  expect_equal(nrow(obs), nrow(Iobs))
  expect_false(anyDuplicated(obs$observationID) > 0)
})

test_that("create_observations() rejects values outside the schema enum", {
  expect_error(
    create_observations(observationID = "o1", deploymentID = "A01", mediaID = "m1",
                        eventStart = "2023-04-01 09:05:00", eventEnd = "2023-04-01 09:05:00",
                        observationLevel = "media", observationType = "dog"),
    "observationType")
})

test_that("create_observations() drops only rows that are identical in every column", {
  args <- list(observationID = c("o1", "o1", "o2"), deploymentID = "A01",
               mediaID = c("m1", "m1", "m2"),
               eventStart = "2023-04-01 09:05:00", eventEnd = "2023-04-01 09:05:00",
               observationLevel = "media", observationType = "animal",
               scientificName = c("Vulpes vulpes", "Vulpes vulpes", "Lepus brachyurus"))
  obs <- do.call(create_observations, args)
  expect_equal(obs$observationID, c("o1", "o2"))
  expect_error(do.call(create_observations, c(args, omitduplicate = FALSE)),
               "must be unique")
})

test_that("create_observations() rejects one observationID shared by different species", {
  expect_error(
    create_observations(observationID = c("o1", "o1"), deploymentID = "A01",
                        mediaID = "m1",
                        eventStart = "2023-04-01 09:05:00", eventEnd = "2023-04-01 09:05:00",
                        observationLevel = "media", observationType = "animal",
                        scientificName = c("Vulpes vulpes", "Lepus brachyurus")),
    "o1.*its own observationID")
})

test_that("create_observations() requires mediaID only for media-based rows", {
  base <- list(deploymentID = "A01", eventStart = "2023-04-01 09:05:00",
               eventEnd = "2023-04-01 09:05:00", observationType = "animal")
  # media-based: mediaID missing (omitted or NA) is an error
  expect_error(do.call(create_observations, c(base, observationID = "o1",
                                              observationLevel = "media")),
               "mediaID.*o1")
  expect_error(do.call(create_observations, c(base, list(observationID = c("o1", "o2"),
                                                         observationLevel = "media",
                                                         mediaID = c("m1", NA)))),
               "1 row.*o2")
  # event-based: neither mediaID nor eventID is required by Camtrap DP
  expect_equal(nrow(do.call(create_observations, c(base, observationID = "o1",
                                                   observationLevel = "event"))), 1)
  expect_equal(nrow(do.call(create_observations, c(base, observationID = "o1",
                                                   observationLevel = "event",
                                                   eventID = "e1"))), 1)
})
