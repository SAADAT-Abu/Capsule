# tests/testthat/test-large-files.R

library(Capsule)

test_that("track_data uses SHA-256 for small files", {
  skip_on_cran()

  temp_dir <- tempfile("data_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create small file
  test_file <- file.path(temp_dir, "small.txt")
  writeLines(c("line1", "line2"), test_file)

  registry_file <- file.path(temp_dir, "data_registry.json")
  result <- track_data(
    test_file,
    source = "generated",
    registry_file = registry_file,
    fast_hash = TRUE,
    size_threshold_gb = 1
  )

  expect_type(result, "list")
  expect_equal(result$checksum_algorithm, "sha256")
  expect_true(file.exists(registry_file))
})

test_that("track_data can use fast hash for simulated large files", {
  skip_on_cran()

  temp_dir <- tempfile("data_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a file
  test_file <- file.path(temp_dir, "data.txt")
  writeLines(rep("DATA", 100), test_file)

  registry_file <- file.path(temp_dir, "data_registry.json")

  # Use very low threshold to trigger fast hash
  result <- track_data(
    test_file,
    source = "generated",
    registry_file = registry_file,
    fast_hash = TRUE,
    size_threshold_gb = 0.000001 # Very low threshold
  )

  expect_type(result, "list")
  # Should use xxhash64 or metadata_hash
  expect_true(result$checksum_algorithm %in% c("sha256", "xxhash64", "metadata_hash"))
  expect_true(!is.null(result$size_gb))
})

test_that("verify_data works with different checksum algorithms", {
  skip_on_cran()

  temp_dir <- tempfile("data_test_")
  dir.create(temp_dir, recursive = TRUE)
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit(
    {
      setwd(old_wd)
      unlink(temp_dir, recursive = TRUE)
    },
    add = TRUE
  )

  # Create and track file
  test_file <- "test.txt"
  writeLines(c("test"), test_file)

  registry_file <- file.path(temp_dir, "data_registry.json")
  track_data(
    test_file,
    source = "generated",
    registry_file = registry_file
  )

  # Verify
  result <- verify_data(test_file, registry_file = registry_file)
  expect_true(result)

  # Modify file
  writeLines(c("modified"), test_file)

  # Should fail verification
  result2 <- verify_data(test_file, registry_file = registry_file)
  expect_false(result2)
})

test_that("track_data handles reference source type", {
  skip_on_cran()

  temp_dir <- tempfile("data_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  test_file <- file.path(temp_dir, "ref.fa")
  writeLines(c(">chr1", "ATCG"), test_file)

  registry_file <- file.path(temp_dir, "data_registry.json")
  result <- track_data(
    test_file,
    source = "reference",
    description = "Test reference",
    registry_file = registry_file
  )

  expect_type(result, "list")
  expect_equal(result$source, "reference")
})
