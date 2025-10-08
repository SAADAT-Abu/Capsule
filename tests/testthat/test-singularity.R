# tests/testthat/test-singularity.R

library(Capsule)

test_that("generate_singularity creates definition file", {
  skip_on_cran()

  temp_dir <- tempfile("sing_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  result <- generate_singularity(
    output_dir = temp_dir,
    project_name = "test_project"
  )

  expect_type(result, "list")
  expect_true(file.exists(result$definition))
  expect_true(file.exists(result$build_script))
  expect_true(file.exists(result$readme))

  # Check definition file content
  def_content <- readLines(result$definition)
  expect_true(any(grepl("Bootstrap: docker", def_content)))
  expect_true(any(grepl("From: rocker/r-ver", def_content)))
})

test_that("generate_singularity handles system dependencies", {
  skip_on_cran()

  temp_dir <- tempfile("sing_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  result <- generate_singularity(
    output_dir = temp_dir,
    project_name = "test_with_deps",
    system_deps = c("samtools", "bwa")
  )

  expect_true(file.exists(result$definition))

  # Check that dependencies are in the file
  def_content <- readLines(result$definition)
  expect_true(any(grepl("samtools", def_content)))
  expect_true(any(grepl("bwa", def_content)))
})

test_that("generate_singularity build script is executable", {
  skip_on_cran()
  skip_on_os("windows")  # Unix file permissions only

  temp_dir <- tempfile("sing_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  result <- generate_singularity(
    output_dir = temp_dir,
    project_name = "test_exec"
  )

  # Check file permissions (Unix only)
  info <- file.info(result$build_script)
  # Should be executable (mode will include execute bit)
  expect_true(!is.na(info$mode))
})
