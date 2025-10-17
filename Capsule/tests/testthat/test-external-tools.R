# tests/testthat/test-external-tools.R

library(Capsule)

test_that("track_external_tools works with common tools", {
  skip_on_cran()

  temp_dir <- tempdir()
  registry_file <- file.path(temp_dir, "tools_registry.json")

  # Test with common Unix tools that should exist
  result <- track_external_tools(
    tools = c("ls", "cat"),
    registry_file = registry_file
  )

  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_true(file.exists(registry_file))

  # Check structure
  expect_true("ls" %in% names(result))
  expect_true(result$ls$available)
  expect_type(result$ls$version, "character")

  # Clean up
  unlink(registry_file)
})

test_that("get_tool_versions retrieves tracked tools", {
  skip_on_cran()

  temp_dir <- tempdir()
  registry_file <- file.path(temp_dir, "tools_registry2.json")

  # Track some tools first
  track_external_tools(
    tools = c("bash", "grep"),
    registry_file = registry_file
  )

  # Get all tools
  all_tools <- get_tool_versions(registry_file = registry_file)
  expect_type(all_tools, "list")
  expect_true(length(all_tools) > 0)

  # Get specific tool
  bash_info <- get_tool_versions("bash", registry_file = registry_file)
  expect_type(bash_info, "list")
  expect_true(bash_info$available)

  # Clean up
  unlink(registry_file)
})

test_that("track_external_tools handles non-existent tools gracefully", {
  skip_on_cran()

  temp_dir <- tempdir()
  registry_file <- file.path(temp_dir, "tools_registry3.json")

  # Test with a tool that definitely doesn't exist
  result <- track_external_tools(
    tools = c("nonexistent_tool_xyz123"),
    registry_file = registry_file
  )

  expect_type(result, "list")
  expect_false(result$nonexistent_tool_xyz123$available)

  # Clean up
  unlink(registry_file)
})
