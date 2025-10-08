# tests/testthat/test-snapshot.R

library(Capsule)

context("snapshot_workflow function")

test_that("snapshot_workflow creates a complete snapshot of the workflow", {
  # 1. Set up a temporary project directory for a clean environment
  temp_proj_dir <- tempfile("capsule_snapshot_test_")
  dir.create(temp_proj_dir, recursive = TRUE)

  # Store the original working directory and set the new one
  original_wd <- getwd()
  setwd(temp_proj_dir)

  # 2. Ensure cleanup happens even if tests fail
  on.exit(
    {
      setwd(original_wd)
      unlink(temp_proj_dir, recursive = TRUE)
    },
    add = TRUE
  )

  # 3. Initialize Capsule in the temp directory (disable git/renv for speed)
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))

  # 4. Create a dummy analysis script
  analysis_script_content <- "
# Dummy Analysis Script for Testing
data <- data.frame(x = 1:10, y = stats::rnorm(10))
write.csv(data, 'output.csv')
print('Analysis complete.')
"
  analysis_script_path <- "my_analysis_script.R"
  writeLines(analysis_script_content, analysis_script_path)

  # 5. Run the snapshot_workflow function
  snapshot_name <- "test_snapshot_v1"
  suppressMessages(
    snapshot_workflow(
      snapshot_name = snapshot_name,
      analysis_name = "my_test_analysis",
      source_script = analysis_script_path,
      description = "A snapshot for testing purposes.",
      generate_report = FALSE # Disable report generation to simplify the test
    )
  )

  # 6. Verify that the snapshot was created correctly
  snapshot_dir <- file.path(".capsule/snapshots", snapshot_name)
  expect_true(dir.exists(snapshot_dir), "Snapshot directory should exist.")

  # Check for the existence of essential files
  expect_true(file.exists(file.path(snapshot_dir, "session_info.json")), "session_info.json should be created.")
  expect_true(file.exists(file.path(snapshot_dir, "packages.json")), "packages.json should be created.")
  expect_true(file.exists(file.path(snapshot_dir, "snapshot_metadata.json")), "snapshot_metadata.json should be created.")

  # Check for the reproducible script
  repro_script_path <- file.path(snapshot_dir, "my_test_analysis_reproducible.R")
  expect_true(file.exists(repro_script_path), "Reproducible script should be generated.")

  # Check for Docker configuration
  docker_dir <- file.path(snapshot_dir, "docker")
  expect_true(dir.exists(docker_dir), "Docker directory should exist.")
  expect_true(file.exists(file.path(docker_dir, "Dockerfile")), "Dockerfile should be present.")

  # 7. Verify the content of the metadata file
  metadata <- jsonlite::read_json(file.path(snapshot_dir, "snapshot_metadata.json"))
  expect_equal(metadata$snapshot_name, snapshot_name)
  expect_equal(metadata$analysis_name, "my_test_analysis")
  expect_equal(metadata$source_script, analysis_script_path)
  expect_false(is.null(metadata$capsule_version), "Capsule version should be recorded.")
})

test_that("snapshot_workflow fails gracefully with a non-existent source script", {
  # 1. Set up a temporary project directory
  temp_proj_dir <- tempfile("capsule_snapshot_fail_test_")
  dir.create(temp_proj_dir, recursive = TRUE)
  original_wd <- getwd()
  setwd(temp_proj_dir)
  on.exit(
    {
      setwd(original_wd)
      unlink(temp_proj_dir, recursive = TRUE)
    },
    add = TRUE
  )

  # 2. Initialize Capsule
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))

  # 3. Define a path to a non-existent script
  non_existent_script <- "non_existent_script.R"

  # 4. Expect snapshot_workflow to throw an error
  expect_error(
    snapshot_workflow(
      source_script = non_existent_script
    ),
    regexp = "Source script not found at path:"
  )
})
