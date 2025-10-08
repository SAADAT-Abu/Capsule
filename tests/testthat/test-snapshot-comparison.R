# tests/testthat/test-snapshot-comparison.R

library(Capsule)

test_that("list_snapshots works with no snapshots", {
  skip_on_cran()

  temp_dir <- tempfile("snap_test_")
  dir.create(temp_dir, recursive = TRUE)
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit({
    setwd(old_wd)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)

  # Initialize without creating snapshots
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))

  result <- list_snapshots()
  expect_null(result)
})

test_that("list_snapshots returns snapshot information", {
  skip_on_cran()

  temp_dir <- tempfile("snap_test_")
  dir.create(temp_dir, recursive = TRUE)
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit({
    setwd(old_wd)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)

  # Initialize and create a snapshot
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))
  suppressMessages(
    snapshot_workflow(
      snapshot_name = "test_snap",
      analysis_name = "test",
      generate_docker = FALSE,
      generate_report = FALSE
    )
  )

  result <- list_snapshots()
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)
  expect_true("snapshot" %in% names(result))
})

test_that("compare_snapshots creates comparison report", {
  skip_on_cran()

  temp_dir <- tempfile("snap_test_")
  dir.create(temp_dir, recursive = TRUE)
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit({
    setwd(old_wd)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)

  # Initialize
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))

  # Create first snapshot
  track_params(list(alpha = 0.05), "test")
  suppressMessages(
    snapshot_workflow(
      snapshot_name = "snap1",
      analysis_name = "test",
      generate_docker = FALSE,
      generate_report = FALSE
    )
  )

  # Modify and create second snapshot
  track_params(list(alpha = 0.01), "test")  # Changed
  suppressMessages(
    snapshot_workflow(
      snapshot_name = "snap2",
      analysis_name = "test",
      generate_docker = FALSE,
      generate_report = FALSE
    )
  )

  # Compare
  output_file <- "comparison.md"
  result <- compare_snapshots("snap1", "snap2", output_file)

  expect_type(result, "list")
  expect_true(file.exists(output_file))

  # Check result structure
  expect_true("parameters" %in% names(result))
  expect_true("packages" %in% names(result))
  expect_true("data" %in% names(result))
})

test_that("compare_snapshots handles non-existent snapshots", {
  skip_on_cran()

  temp_dir <- tempfile("snap_test_")
  dir.create(temp_dir, recursive = TRUE)
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit({
    setwd(old_wd)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)

  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))

  # Try to compare non-existent snapshots
  result <- compare_snapshots("nonexistent1", "nonexistent2", "comparison.md")

  expect_null(result)
})

test_that("comparison detects parameter changes", {
  skip_on_cran()

  temp_dir <- tempfile("snap_test_")
  dir.create(temp_dir, recursive = TRUE)
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit({
    setwd(old_wd)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)

  # Initialize
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))

  # Snapshot 1
  track_params(list(x = 1, y = 2), "analysis1")
  suppressMessages(
    snapshot_workflow(
      snapshot_name = "s1",
      analysis_name = "analysis1",
      generate_docker = FALSE,
      generate_report = FALSE
    )
  )

  # Snapshot 2 with different params
  track_params(list(x = 1, y = 3), "analysis1")  # y changed
  suppressMessages(
    snapshot_workflow(
      snapshot_name = "s2",
      analysis_name = "analysis1",
      generate_docker = FALSE,
      generate_report = FALSE
    )
  )

  # Compare
  result <- compare_snapshots("s1", "s2", "comp.md")

  # Should detect parameter modification
  expect_type(result$parameters, "list")
})
