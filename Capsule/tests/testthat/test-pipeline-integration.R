# tests/testthat/test-pipeline-integration.R

library(Capsule)

test_that("export_for_nextflow creates valid JSON", {
  skip_on_cran()

  temp_dir <- tempfile("pipeline_test_")
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

  # Initialize and add some data
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))
  track_params(list(alpha = 0.05), "test_analysis")

  output_file <- file.path(temp_dir, "nextflow.json")
  result <- export_for_nextflow(output_file)

  expect_type(result, "list")
  expect_true(file.exists(output_file))
  expect_true("capsule_version" %in% names(result))
  expect_true("r_version" %in% names(result))

  # Validate JSON structure
  loaded <- jsonlite::read_json(output_file)
  expect_type(loaded, "list")
})

test_that("export_for_snakemake creates valid YAML", {
  skip_on_cran()

  temp_dir <- tempfile("pipeline_test_")
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

  # Initialize and add some data
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))
  track_params(list(n = 100), "test_analysis")

  output_file <- file.path(temp_dir, "snakemake.yaml")
  result <- export_for_snakemake(output_file)

  expect_type(result, "list")
  expect_true(file.exists(output_file))

  # Validate YAML structure
  loaded <- yaml::read_yaml(output_file)
  expect_type(loaded, "list")
  expect_true("capsule" %in% names(loaded))
})

test_that("export_for_wdl creates valid JSON", {
  skip_on_cran()

  temp_dir <- tempfile("pipeline_test_")
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

  # Initialize and add some data
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))
  track_params(list(method = "test"), "test_analysis")

  output_file <- file.path(temp_dir, "wdl.json")
  result <- export_for_wdl(output_file)

  expect_type(result, "list")
  expect_true(file.exists(output_file))

  # Validate JSON
  loaded <- jsonlite::read_json(output_file)
  expect_type(loaded, "list")
})

test_that("export_for_cwl creates valid YAML", {
  skip_on_cran()

  temp_dir <- tempfile("pipeline_test_")
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

  # Initialize and add some data
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))

  output_file <- file.path(temp_dir, "cwl.yml")
  result <- export_for_cwl(output_file)

  expect_type(result, "list")
  expect_true(file.exists(output_file))

  # Validate YAML
  loaded <- yaml::read_yaml(output_file)
  expect_type(loaded, "list")
})

test_that("pipeline exports include data files when present", {
  skip_on_cran()

  temp_dir <- tempfile("pipeline_test_")
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

  # Initialize
  suppressMessages(init_capsule(use_git = FALSE, use_renv = FALSE))

  # Create and track a data file
  test_file <- "data.txt"
  writeLines(c("test"), test_file)
  track_data(test_file, source = "generated")

  # Export
  result <- export_for_nextflow("nextflow.json")

  expect_true("data_files" %in% names(result))
  expect_true(length(result$data_files) > 0)
})
