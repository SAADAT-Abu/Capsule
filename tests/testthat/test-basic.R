# tests/testthat/test-basic.R

library(ReproFlow)


test_that("capture_session works", {
  skip_on_cran()

  # Capture session without saving
  session_info <- capture_session()

  expect_type(session_info, "list")
  expect_true("timestamp" %in% names(session_info))
  expect_true("r_version" %in% names(session_info))
  expect_true("platform" %in% names(session_info))
})

test_that("set_seed tracks seeds", {
  skip_on_cran()

  temp_dir <- tempdir()
  registry_file <- file.path(temp_dir, "seed_registry.json")

  # Set and track seed
  returned_seed <- set_seed(
    seed = 12345,
    analysis_name = "test_analysis",
    registry_file = registry_file
  )

  expect_equal(returned_seed, 12345)
  expect_true(file.exists(registry_file))

  # Clean up
  unlink(registry_file)
})

test_that("track_params stores parameters", {
  skip_on_cran()

  temp_dir <- tempdir()
  registry_file <- file.path(temp_dir, "param_registry.json")

  params <- list(
    alpha = 0.05,
    beta = 0.1,
    method = "test"
  )

  result <- track_params(
    params = params,
    analysis_name = "test_params",
    registry_file = registry_file
  )

  expect_type(result, "list")
  expect_true(file.exists(registry_file))

  # Clean up
  unlink(registry_file)
})

test_that("track_data creates checksums", {
  skip_on_cran()

  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_data.csv")
  registry_file <- file.path(temp_dir, "data_registry.json")

  # Create test file
  write.csv(data.frame(x = 1:10, y = 11:20), test_file, row.names = FALSE)

  # Track it
  result <- track_data(
    data_path = test_file,
    source = "generated",
    registry_file = registry_file
  )

  expect_type(result, "list")
  expect_true("checksum_sha256" %in% names(result))
  expect_true(file.exists(registry_file))

  # Clean up
  unlink(test_file)
  unlink(registry_file)
})

test_that("verify_data detects changes", {
  skip_on_cran()

  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_verify.csv")
  registry_file <- file.path(temp_dir, "verify_registry.json")

  # Create and track file
  write.csv(data.frame(x = 1:10), test_file, row.names = FALSE)
  track_data(test_file, source = "generated", registry_file = registry_file)

  # Verify unchanged
  result1 <- verify_data(test_file, registry_file = registry_file)
  expect_true(result1)

  # Modify file
  write.csv(data.frame(x = 1:20), test_file, row.names = FALSE)

  # Verify should fail
  result2 <- verify_data(test_file, registry_file = registry_file)
  expect_false(result2)

  # Clean up
  unlink(test_file)
  unlink(registry_file)
})
