# tests/testthat/test-init.R

# Load the package in the test environment.
library(Capsule)

context("init_capsule function")

test_that("init_capsule creates the correct directory structure", {
  # Create a temporary directory for the test
  temp_proj_dir <- tempfile("capsule_test_")
  dir.create(temp_proj_dir, recursive = TRUE)
  on.exit(unlink(temp_proj_dir, recursive = TRUE), add = TRUE)

  # Run init_capsule inside the temp directory
  suppressMessages(init_capsule(project_path = temp_proj_dir, use_git = FALSE, use_renv = FALSE))

  # Check for the existence of the .capsule directory and its subdirectories
  expect_true(dir.exists(file.path(temp_proj_dir, ".capsule")))
  expect_true(dir.exists(file.path(temp_proj_dir, ".capsule", "snapshots")))
  expect_true(dir.exists(file.path(temp_proj_dir, ".capsule", "scripts")))
})

test_that("init_capsule creates config and example files", {
  temp_proj_dir <- tempfile("capsule_test_")
  dir.create(temp_proj_dir, recursive = TRUE)
  on.exit(unlink(temp_proj_dir, recursive = TRUE), add = TRUE)

  suppressMessages(init_capsule(project_path = temp_proj_dir, use_git = FALSE, use_renv = FALSE))

  # Check for config file
  config_path <- file.path(temp_proj_dir, ".capsule", "config.json")
  expect_true(file.exists(config_path))

  # Check for example script
  example_script_path <- file.path(temp_proj_dir, ".capsule", "example_workflow.R")
  expect_true(file.exists(example_script_path))

  # Optionally, check the content of the config file
  config <- jsonlite::read_json(config_path)
  expect_equal(config$project_name, basename(temp_proj_dir))
  expect_true(config$track_data)
})

test_that("init_capsule respects the create_gitignore argument", {
  temp_proj_dir <- tempfile("capsule_test_")
  dir.create(temp_proj_dir, recursive = TRUE)
  on.exit(unlink(temp_proj_dir, recursive = TRUE), add = TRUE)

  # Test case 1: create_gitignore = TRUE (default)
  suppressMessages(init_capsule(project_path = temp_proj_dir, use_git = FALSE, use_renv = FALSE, create_gitignore = TRUE))
  expect_true(file.exists(file.path(temp_proj_dir, ".gitignore")))
  unlink(file.path(temp_proj_dir, ".gitignore")) # Clean up for the next test

  # Test case 2: create_gitignore = FALSE
  suppressMessages(init_capsule(project_path = temp_proj_dir, use_git = FALSE, use_renv = FALSE, create_gitignore = FALSE))
  expect_false(file.exists(file.path(temp_proj_dir, ".gitignore")))
})
