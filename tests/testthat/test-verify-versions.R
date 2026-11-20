# Converts the bundled example data into every supported Camtrap DP release and
# variant, as in the "Verifying the installation" vignette. These tests download
# the official schemas, so they are skipped on CRAN and when offline; the
# Frictionless checks are additionally skipped when Python with 'frictionless'
# is not available (set R2CAMTRAPDP_PYTHON to point at a specific interpreter).

for (version in camera_versions) {
  test_that(paste("camera-trap package passes the R-side checks for Camtrap DP", version), {
    skip_if_no_schemas()
    dp <- suppressWarnings(build_camera_package(version))
    # every table was checked against its schema (not stored unvalidated)
    expect_setequal(names(dp$validation), c("deployments", "media", "observations"))
    issues <- suppressWarnings(dp$validate(relations = TRUE, metadata = TRUE, summarize = FALSE))
    expect_true(ctdp_is_valid(issues))
  })

  test_that(paste("camera-trap package passes Frictionless validation for Camtrap DP", version), {
    skip_if_no_schemas()
    python <- skip_if_no_frictionless()
    dp <- suppressWarnings(build_camera_package(version))
    path <- tempfile(paste0("camera-", version, "-"))
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    issues <- suppressMessages(suppressWarnings(
      dp$validate_frictionless(directory = path, python = python, summarize = FALSE)))
    expect_true(ctdp_is_valid(issues))
  })
}

test_that("acoustic package passes the R-side checks (bioacoustics variant 1.0.2)", {
  skip_if_no_schemas()
  dp <- suppressWarnings(build_acoustic_package())
  expect_setequal(names(dp$validation), c("deployments", "media", "observations"))
  issues <- suppressWarnings(dp$validate(relations = TRUE, metadata = TRUE, summarize = FALSE))
  expect_true(ctdp_is_valid(issues))
})

test_that("acoustic package passes Frictionless validation (bioacoustics variant 1.0.2)", {
  skip_if_no_schemas()
  python <- skip_if_no_frictionless()
  dp <- suppressWarnings(build_acoustic_package())
  path <- tempfile("acoustic-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  issues <- suppressMessages(suppressWarnings(
    dp$validate_frictionless(directory = path, python = python, summarize = FALSE)))
  expect_true(ctdp_is_valid(issues))
})

test_that("the checks detect an invalid package (duplicated observationID)", {
  skip_if_no_schemas()
  dp <- suppressWarnings(build_camera_package("1.0.2"))
  obs <- dp$data$observations
  dp$set_observations(rbind(obs, obs[1, ]), validate = FALSE)
  issues <- suppressWarnings(dp$validate(relations = TRUE, metadata = TRUE, summarize = FALSE))
  expect_false(ctdp_is_valid(issues))

  python <- skip_if_no_frictionless()
  path <- tempfile("camera-invalid-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  issues <- suppressMessages(suppressWarnings(
    dp$validate_frictionless(directory = path, python = python, summarize = FALSE)))
  expect_false(ctdp_is_valid(issues))
})
