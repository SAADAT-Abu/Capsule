#' Track Data Provenance
#'
#' @description
#' Records comprehensive provenance information for data files including checksums,
#' sources, timestamps, and metadata.
#'
#' @param data_path Character. Path to data file or directory.
#' @param source Character. Source of the data (e.g., "downloaded", "generated", "manual").
#' @param source_url Character. URL if data was downloaded. Optional.
#' @param description Character. Description of the data. Optional.
#' @param metadata List. Additional metadata. Optional.
#' @param registry_file Character. Path to provenance registry. Default ".reproflow/data_registry.json".
#'
#' @return A list containing data provenance information
#'
#' @importFrom digest digest
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track a downloaded dataset
#' track_data("data/mydata.csv",
#'            source = "downloaded",
#'            source_url = "https://example.com/data.csv",
#'            description = "Customer data from API")
#'
#' # Track generated data
#' track_data("results/simulation.rds",
#'            source = "generated",
#'            description = "Monte Carlo simulation results")
#' }
track_data <- function(data_path, source = c("downloaded", "generated", "manual", "other"),
                       source_url = NULL, description = NULL, metadata = NULL,
                       registry_file = ".reproflow/data_registry.json") {

  source <- match.arg(source)

  if (!file.exists(data_path)) {
    cli::cli_alert_danger("Data file not found: {.file {data_path}}")
    return(invisible(NULL))
  }

  # Calculate file checksum
  checksum <- digest::digest(file = data_path, algo = "sha256")

  # Get file info
  file_info <- file.info(data_path)

  # Create provenance record
  provenance <- list(
    file_path = normalizePath(data_path, mustWork = TRUE),
    file_name = basename(data_path),
    checksum_sha256 = checksum,
    size_bytes = file_info$size,
    size_readable = .format_size(file_info$size),
    created = file_info$ctime,
    modified = file_info$mtime,
    accessed = file_info$atime,
    source = source,
    source_url = source_url,
    description = description,
    metadata = metadata,
    tracked_at = Sys.time(),
    tracked_by = Sys.info()["user"],
    r_version = R.version.string
  )

  # Load existing registry or create new
  registry <- .load_registry(registry_file)

  # Add to registry
  registry$data[[data_path]] <- provenance
  registry$last_updated <- Sys.time()

  # Save registry
  .save_registry(registry, registry_file)

  cli::cli_alert_success("Data provenance tracked: {.file {data_path}}")
  cli::cli_alert_info("Checksum: {.val {substr(checksum, 1, 16)}}...")

  invisible(provenance)
}


#' Verify Data Integrity
#'
#' @description
#' Verify that tracked data files have not been modified by comparing checksums
#'
#' @param data_path Character. Path to specific file, or NULL to verify all tracked files.
#' @param registry_file Character. Path to provenance registry.
#'
#' @return Logical. TRUE if data is unchanged, FALSE otherwise
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Verify specific file
#' verify_data("data/mydata.csv")
#'
#' # Verify all tracked files
#' verify_data()
#' }
verify_data <- function(data_path = NULL, registry_file = ".reproflow/data_registry.json") {

  registry <- .load_registry(registry_file)

  if (is.null(registry$data) || length(registry$data) == 0) {
    cli::cli_alert_warning("No data files tracked in registry")
    return(invisible(FALSE))
  }

  # Determine which files to verify
  if (is.null(data_path)) {
    files_to_verify <- names(registry$data)
  } else {
    files_to_verify <- data_path
  }

  all_valid <- TRUE

  for (file_path in files_to_verify) {
    if (!file_path %in% names(registry$data)) {
      cli::cli_alert_warning("File not in registry: {.file {file_path}}")
      all_valid <- FALSE
      next
    }

    if (!file.exists(file_path)) {
      cli::cli_alert_danger("File missing: {.file {file_path}}")
      all_valid <- FALSE
      next
    }

    # Calculate current checksum
    current_checksum <- digest::digest(file = file_path, algo = "sha256")
    original_checksum <- registry$data[[file_path]]$checksum_sha256

    if (current_checksum == original_checksum) {
      cli::cli_alert_success("Verified: {.file {file_path}}")
    } else {
      cli::cli_alert_danger("Modified: {.file {file_path}}")
      cli::cli_alert_info("Original: {.val {substr(original_checksum, 1, 16)}}...")
      cli::cli_alert_info("Current:  {.val {substr(current_checksum, 1, 16)}}...")
      all_valid <- FALSE
    }
  }

  invisible(all_valid)
}


#' Get Data Lineage
#'
#' @description
#' Retrieve complete lineage information for tracked data
#'
#' @param data_path Character. Path to data file. If NULL, returns all lineage.
#' @param registry_file Character. Path to provenance registry.
#'
#' @return List containing lineage information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get lineage for specific file
#' lineage <- get_data_lineage("data/mydata.csv")
#'
#' # Get all lineage
#' all_lineage <- get_data_lineage()
#' }
get_data_lineage <- function(data_path = NULL, registry_file = ".reproflow/data_registry.json") {

  registry <- .load_registry(registry_file)

  if (is.null(data_path)) {
    return(registry$data)
  }

  if (data_path %in% names(registry$data)) {
    return(registry$data[[data_path]])
  } else {
    cli::cli_alert_warning("No lineage found for: {.file {data_path}}")
    return(NULL)
  }
}


#' Load Provenance Registry
#'
#' @description
#' Internal function to load the provenance registry
#'
#' @param registry_file Character. Path to registry file
#'
#' @return List containing registry data
#' @keywords internal
.load_registry <- function(registry_file) {
  if (file.exists(registry_file)) {
    jsonlite::read_json(registry_file, simplifyVector = FALSE)
  } else {
    list(
      created = Sys.time(),
      last_updated = Sys.time(),
      data = list()
    )
  }
}


#' Save Provenance Registry
#'
#' @description
#' Internal function to save the provenance registry
#'
#' @param registry List. Registry data to save
#' @param registry_file Character. Path to registry file
#'
#' @return NULL
#' @keywords internal
.save_registry <- function(registry, registry_file) {
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(registry, registry_file, auto_unbox = TRUE, pretty = TRUE)
}


#' Format File Size
#'
#' @description
#' Internal function to format file size in human-readable format
#'
#' @param bytes Numeric. Size in bytes
#'
#' @return Character. Formatted size
#' @keywords internal
.format_size <- function(bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  size <- bytes
  unit_idx <- 1

  while (size >= 1024 && unit_idx < length(units)) {
    size <- size / 1024
    unit_idx <- unit_idx + 1
  }

  sprintf("%.2f %s", size, units[unit_idx])
}
