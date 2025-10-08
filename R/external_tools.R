#' Track External Bioinformatics Tools
#'
#' @description
#' Track versions of external command-line tools commonly used in bioinformatics
#' pipelines (e.g., samtools, STAR, BWA, etc.)
#'
#' @param tools Character vector of tool names to track. If NULL, tracks common tools.
#' @param registry_file Character. Path to tools registry. Default ".capsule/tools_registry.json"
#'
#' @return List containing tool version information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track common bioinformatics tools
#' track_external_tools()
#'
#' # Track specific tools
#' track_external_tools(c("samtools", "bwa", "STAR"))
#' }
track_external_tools <- function(tools = NULL,
                                 registry_file = ".capsule/tools_registry.json") {

  # Common bioinformatics tools
  default_tools <- c(
    "samtools", "bcftools", "bedtools", "bwa", "bowtie2",
    "STAR", "hisat2", "salmon", "kallisto", "fastqc",
    "cutadapt", "trimmomatic", "picard", "gatk",
    "subread", "featureCounts", "htseq-count", "multiqc"
  )

  if (is.null(tools)) tools <- default_tools

  cli::cli_alert_info("Tracking {length(tools)} external tool(s)...")

  tool_versions <- list()

  for (tool in tools) {
    # Try to get version
    version <- .get_tool_version(tool)

    # Get full path
    path <- .get_tool_path(tool)

    tool_versions[[tool]] <- list(
      version = version,
      path = path,
      available = !is.na(path),
      checked_at = Sys.time()
    )

    if (!is.na(path)) {
      cli::cli_alert_success("{tool}: {version}")
    } else {
      cli::cli_alert_warning("{tool}: not found")
    }
  }

  # Save to registry
  registry <- .load_tools_registry(registry_file)
  registry$tools <- tool_versions
  registry$last_updated <- Sys.time()
  registry$r_version <- R.version.string
  .save_tools_registry(registry, registry_file)

  n_available <- sum(sapply(tool_versions, function(x) x$available))
  cli::cli_alert_success("Tracked {n_available}/{length(tools)} available tool(s)")

  invisible(tool_versions)
}


#' Get External Tool Versions
#'
#' @description
#' Retrieve version information for previously tracked external tools
#'
#' @param tool_name Character. Specific tool name, or NULL for all tools
#' @param registry_file Character. Path to tools registry
#'
#' @return List of tool version information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all tracked tools
#' get_tool_versions()
#'
#' # Get specific tool
#' get_tool_versions("samtools")
#' }
get_tool_versions <- function(tool_name = NULL,
                              registry_file = ".capsule/tools_registry.json") {

  registry <- .load_tools_registry(registry_file)

  if (is.null(registry$tools) || length(registry$tools) == 0) {
    cli::cli_alert_warning("No tools tracked. Run track_external_tools() first.")
    return(NULL)
  }

  if (is.null(tool_name)) {
    return(registry$tools)
  }

  if (tool_name %in% names(registry$tools)) {
    return(registry$tools[[tool_name]])
  } else {
    cli::cli_alert_warning("Tool not found: {tool_name}")
    return(NULL)
  }
}


#' Get Tool Version String
#'
#' @description
#' Internal function to get version string from a command-line tool
#'
#' @param tool Character. Tool name
#'
#' @return Character. Version string or "not installed"
#' @keywords internal
.get_tool_version <- function(tool) {
  version <- tryCatch({
    # Try different version flags
    version_flags <- c("--version", "-v", "-version", "version", "-V")

    for (flag in version_flags) {
      result <- suppressWarnings(
        system2(tool, flag, stdout = TRUE, stderr = TRUE)
      )

      # Check if command succeeded
      status <- attr(result, "status")
      if (!is.null(status) && status != 0) next

      # If we got output, extract version
      if (length(result) > 0) {
        # Return first line that looks like it has version info
        version_line <- result[1]

        # Clean up common patterns
        version_line <- gsub("^.*version\\s+", "", version_line, ignore.case = TRUE)
        version_line <- gsub("^.*v(\\d)", "\\1", version_line)

        return(trimws(version_line))
      }
    }

    # If no version flag worked, at least confirm it's installed
    test_result <- suppressWarnings(
      system2("which", tool, stdout = TRUE, stderr = FALSE)
    )
    if (length(test_result) > 0 && !is.null(test_result)) {
      return("installed (version unknown)")
    }

    return("not installed")

  }, error = function(e) {
    return("not installed")
  })

  return(version)
}


#' Get Tool Path
#'
#' @description
#' Internal function to get the full path to a command-line tool
#'
#' @param tool Character. Tool name
#'
#' @return Character. Full path or NA
#' @keywords internal
.get_tool_path <- function(tool) {
  path <- tryCatch({
    result <- system2("which", tool, stdout = TRUE, stderr = FALSE)
    if (length(result) > 0 && !is.null(attr(result, "status")) == FALSE) {
      return(result[1])
    }
    return(NA_character_)
  }, error = function(e) {
    return(NA_character_)
  })

  return(path)
}


#' Load Tools Registry
#'
#' @description
#' Internal function to load the tools registry
#'
#' @param registry_file Character. Path to registry file
#'
#' @return List containing registry data
#' @keywords internal
.load_tools_registry <- function(registry_file) {
  if (file.exists(registry_file)) {
    jsonlite::read_json(registry_file, simplifyVector = FALSE)
  } else {
    list(
      created = Sys.time(),
      last_updated = Sys.time(),
      tools = list()
    )
  }
}


#' Save Tools Registry
#'
#' @description
#' Internal function to save the tools registry
#'
#' @param registry List. Registry data to save
#' @param registry_file Character. Path to registry file
#'
#' @return NULL
#' @keywords internal
.save_tools_registry <- function(registry, registry_file) {
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(registry, registry_file, auto_unbox = TRUE, pretty = TRUE)
}
