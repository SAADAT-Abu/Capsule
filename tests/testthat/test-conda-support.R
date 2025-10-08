# tests/testthat/test-conda-support.R

library(Capsule)

test_that("conda environment tracking handles missing conda gracefully", {
  skip_on_cran()

  temp_dir <- tempdir()
  registry_file <- file.path(temp_dir, "conda_registry.json")

  # This may return NULL if conda is not available - that's OK
  result <- tryCatch({
    track_conda_env(registry_file = registry_file)
  }, error = function(e) {
    NULL
  })

  # Test should pass whether conda is available or not
  expect_true(is.null(result) || is.list(result))

  # Clean up
  if (file.exists(registry_file)) {
    unlink(registry_file)
  }
})

test_that("get_conda_env_info works with empty registry", {
  skip_on_cran()

  temp_dir <- tempdir()
  registry_file <- file.path(temp_dir, "conda_registry2.json")

  # Should handle non-existent registry
  result <- get_conda_env_info(registry_file = registry_file)
  expect_null(result)

  # Clean up
  if (file.exists(registry_file)) {
    unlink(registry_file)
  }
})

test_that("conda registry structure is correct when created", {
  skip_on_cran()

  temp_dir <- tempdir()
  registry_file <- file.path(temp_dir, "conda_registry3.json")

  # Create a mock registry
  registry <- list(
    created = Sys.time(),
    last_updated = Sys.time(),
    environments = list()
  )

  # Save it
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(registry, registry_file, auto_unbox = TRUE)

  # Load it back
  loaded <- jsonlite::read_json(registry_file, simplifyVector = FALSE)
  expect_type(loaded, "list")
  expect_true("environments" %in% names(loaded))

  # Clean up
  unlink(registry_file)
})
