# tests/testthat/test-reference-tracking.R

library(Capsule)

test_that("track_reference_genome works with FASTA file", {
  skip_on_cran()

  temp_dir <- tempfile("ref_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create dummy FASTA
  fasta_file <- file.path(temp_dir, "test.fa")
  fasta_lines <- c(
    ">chr1",
    "ATCGATCGATCG",
    ">chr2",
    "GCTAGCTAGCTA"
  )
  writeLines(fasta_lines, fasta_file)

  # Track it
  registry_file <- file.path(temp_dir, "ref_registry.json")
  result <- track_reference_genome(
    fasta_path = fasta_file,
    genome_build = "TestBuild",
    species = "Test species",
    registry_file = registry_file
  )

  expect_type(result, "list")
  expect_equal(result$genome_build, "TestBuild")
  expect_equal(result$species, "Test species")
  expect_true(file.exists(registry_file))
})

test_that("track_reference_genome works with FASTA and GTF", {
  skip_on_cran()

  temp_dir <- tempfile("ref_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create dummy FASTA
  fasta_file <- file.path(temp_dir, "test.fa")
  writeLines(c(">chr1", "ATCG"), fasta_file)

  # Create dummy GTF
  gtf_file <- file.path(temp_dir, "test.gtf")
  writeLines(c('chr1\ttest\tgene\t1\t4\t.\t+\t.\tgene_id "G1";'), gtf_file)

  # Track it
  registry_file <- file.path(temp_dir, "ref_registry.json")
  result <- track_reference_genome(
    fasta_path = fasta_file,
    gtf_path = gtf_file,
    genome_build = "TestBuild2",
    registry_file = registry_file
  )

  expect_type(result, "list")
  expect_true("gtf" %in% names(result$annotations))
  expect_equal(result$annotations$gtf, gtf_file)
})

test_that("get_reference_info retrieves tracked references", {
  skip_on_cran()

  temp_dir <- tempfile("ref_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create and track a reference
  fasta_file <- file.path(temp_dir, "test.fa")
  writeLines(c(">chr1", "ATCG"), fasta_file)

  registry_file <- file.path(temp_dir, "ref_registry.json")
  track_reference_genome(
    fasta_path = fasta_file,
    genome_build = "TestBuild3",
    registry_file = registry_file
  )

  # Retrieve it
  info <- get_reference_info("TestBuild3", registry_file = registry_file)
  expect_type(info, "list")
  expect_equal(info$genome_build, "TestBuild3")

  # Test getting all references
  all_refs <- get_reference_info(registry_file = registry_file)
  expect_type(all_refs, "list")
  expect_true("TestBuild3" %in% names(all_refs))
})

test_that("list_reference_sources runs without error", {
  skip_on_cran()

  # Should just print information
  expect_invisible(list_reference_sources())
})
