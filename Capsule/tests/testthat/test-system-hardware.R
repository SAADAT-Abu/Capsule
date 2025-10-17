# tests/testthat/test-system-hardware.R

library(Capsule)

test_that("capture_system_libraries returns valid structure", {
  skip_on_cran()

  temp_dir <- tempdir()
  output_file <- file.path(temp_dir, "sys_libs.json")

  result <- capture_system_libraries(output_file)

  expect_type(result, "list")
  expect_true(file.exists(output_file))
  expect_true("os" %in% names(result))
  expect_true("r_blas" %in% names(result))
  expect_true("r_lapack" %in% names(result))

  # Clean up
  unlink(output_file)
})

test_that("capture_system_libraries works without output file", {
  skip_on_cran()

  result <- capture_system_libraries()

  expect_type(result, "list")
  expect_true("timestamp" %in% names(result))
  expect_type(result$os, "character")
})

test_that("capture_hardware returns valid structure", {
  skip_on_cran()

  temp_dir <- tempdir()
  output_file <- file.path(temp_dir, "hardware.json")

  result <- capture_hardware(output_file)

  expect_type(result, "list")
  expect_true(file.exists(output_file))
  expect_true("hostname" %in% names(result))
  expect_true("os" %in% names(result))
  expect_true("cpu" %in% names(result))
  expect_true("memory" %in% names(result))

  # Clean up
  unlink(output_file)
})

test_that("capture_hardware works without output file", {
  skip_on_cran()

  result <- capture_hardware()

  expect_type(result, "list")
  expect_true("timestamp" %in% names(result))
  expect_type(result$hostname, "character")
})

test_that("capture_hardware handles GPU detection gracefully", {
  skip_on_cran()

  result <- capture_hardware()

  # GPU info should be present even if no GPU
  expect_true("gpu" %in% names(result))
  expect_type(result$gpu, "list")
})
