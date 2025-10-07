# tests/testthat/test-init.R

# Load the package in the test environment.
library(ReproFlow)

context("init_reproflow function")

test_that("init_reproflow creates the correct directory structure", {
  # Create a temporary directory for the test
  temp_proj_dir <- tempfile("reproflow_test_")
  dir.create(temp_proj_dir, recursive = TRUE)
  on.exit(unlink(temp_proj_dir, recursive = TRUE), add = TRUE)

  # Run init_reproflow inside the temp directory
  suppressMessages(init_reproflow(project_path = temp_proj_dir, use_git = FALSE, use_renv = FALSE))

  # Check for the existence of the .reproflow directory and its subdirectories
  expect_true(dir.exists(file.path(temp_proj_dir, ".reproflow")))
  expect_true(dir.exists(file.path(temp_proj_dir, ".reproflow", "snapshots")))
  expect_true(dir.exists(file.path(temp_proj_dir, ".reproflow", "scripts")))
})

test_that("init_reproflow creates config and example files", {
  temp_proj_dir <- tempfile("reproflow_test_")
  dir.create(temp_proj_dir, recursive = TRUE)
  on.exit(unlink(temp_proj_dir, recursive = TRUE), add = TRUE)

  suppressMessages(init_reproflow(project_path = temp_proj_dir, use_git = FALSE, use_renv = FALSE))

  # Check for config file
  config_path <- file.path(temp_proj_dir, ".reproflow", "config.json")
  expect_true(file.exists(config_path))

  # Check for example script
  example_script_path <- file.path(temp_proj_dir, ".reproflow", "example_workflow.R")
  expect_true(file.exists(example_script_path))

  # Optionally, check the content of the config file
  config <- jsonlite::read_json(config_path)
  expect_equal(config$project_name, basename(temp_proj_dir))
  expect_true(config$track_data)
})

test_that("init_reproflow respects the create_gitignore argument", {
  temp_proj_dir <- tempfile("reproflow_test_")
  dir.create(temp_proj_dir, recursive = TRUE)
  on.exit(unlink(temp_proj_dir, recursive = TRUE), add = TRUE)

  # Test case 1: create_gitignore = TRUE (default)
  suppressMessages(init_reproflow(project_path = temp_proj_dir, use_git = FALSE, use_renv = FALSE, create_gitignore = TRUE))
  expect_true(file.exists(file.path(temp_proj_dir, ".gitignore")))
  unlink(file.path(temp_proj_dir, ".gitignore")) # Clean up for the next test

  # Test case 2: create_gitignore = FALSE
  suppressMessages(init_reproflow(project_path = temp_proj_dir, use_git = FALSE, use_renv = FALSE, create_gitignore = FALSE))
  expect_false(file.exists(file.path(temp_proj_dir, ".gitignore")))
})