#' Track Conda Environment
#'
#' @description
#' Export and track a conda environment specification for reproducibility.
#' Works with both conda and mamba.
#'
#' @param env_name Character. Name of conda environment. If NULL, uses active environment.
#' @param output_file Character. Path to save environment file. Default "conda_environment.yml"
#' @param use_mamba Logical. Use mamba instead of conda. Default FALSE.
#' @param registry_file Character. Path to conda registry
#'
#' @return List containing environment information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track currently active conda environment
#' track_conda_env()
#'
#' # Track specific environment
#' track_conda_env(env_name = "bioinfo_env")
#'
#' # Use mamba instead
#' track_conda_env(use_mamba = TRUE)
#' }
track_conda_env <- function(env_name = NULL,
                           output_file = "conda_environment.yml",
                           use_mamba = FALSE,
                           registry_file = ".capsule/conda_registry.json") {

  # Determine which tool to use
  conda_cmd <- if (use_mamba) "mamba" else "conda"

  # Check if conda/mamba is available
  conda_check <- suppressWarnings(
    system2("which", conda_cmd, stdout = TRUE, stderr = FALSE)
  )

  if (length(conda_check) == 0 || is.null(conda_check)) {
    cli::cli_alert_danger("{conda_cmd} not found in PATH")
    return(invisible(NULL))
  }

  # Detect active environment if not specified
  if (is.null(env_name)) {
    env_name <- Sys.getenv("CONDA_DEFAULT_ENV")
    if (env_name == "" || env_name == "base") {
      cli::cli_alert_warning("No conda environment active (or using 'base')")
      cli::cli_alert_info("Specify env_name explicitly or activate an environment first")
      return(invisible(NULL))
    }
  }

  cli::cli_alert_info("Exporting conda environment: {env_name}")

  # Export environment
  export_cmd <- paste(conda_cmd, "env export -n", env_name)
  result <- tryCatch({
    system2("bash", c("-c", shQuote(paste(export_cmd, ">", output_file))),
            stdout = TRUE, stderr = TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export conda environment: {e$message}")
    return(NULL)
  })

  if (!file.exists(output_file)) {
    cli::cli_alert_danger("Failed to create environment file")
    return(invisible(NULL))
  }

  # Track the file with Capsule
  track_data(output_file,
             source = "generated",
             description = paste("Conda environment:", env_name))

  # Get conda version
  conda_version <- tryCatch({
    system2(conda_cmd, "--version", stdout = TRUE, stderr = TRUE)[1]
  }, error = function(e) "unknown")

  # Get environment info
  env_list <- tryCatch({
    result <- system2(conda_cmd, c("env", "list"), stdout = TRUE, stderr = TRUE)
    grep(env_name, result, value = TRUE)[1]
  }, error = function(e) "unknown location")

  # Create environment record
  env_info <- list(
    name = env_name,
    file = normalizePath(output_file, mustWork = FALSE),
    exported_at = Sys.time(),
    conda_cmd = conda_cmd,
    conda_version = conda_version,
    env_location = env_list
  )

  # Save to registry
  registry <- .load_conda_registry(registry_file)
  registry$environments[[env_name]] <- env_info
  registry$last_updated <- Sys.time()
  .save_conda_registry(registry, registry_file)

  cli::cli_alert_success("Conda environment exported: {.file {output_file}}")
  cli::cli_alert_info("Environment: {env_name}")
  cli::cli_alert_info("Conda version: {conda_version}")

  invisible(env_info)
}


#' Restore Conda Environment
#'
#' @description
#' Restore a conda environment from a previously exported environment file
#'
#' @param env_file Character. Path to environment YAML file. Default "conda_environment.yml"
#' @param env_name Character. Name for the new environment. If NULL, uses name from file.
#' @param use_mamba Logical. Use mamba instead of conda. Default FALSE.
#' @param force Logical. Remove existing environment if it exists. Default FALSE.
#'
#' @return Logical. TRUE if successful, FALSE otherwise
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Restore environment from file
#' restore_conda_env("conda_environment.yml")
#'
#' # Use mamba for faster installation
#' restore_conda_env("conda_environment.yml", use_mamba = TRUE)
#'
#' # Force recreate if exists
#' restore_conda_env("conda_environment.yml", force = TRUE)
#' }
restore_conda_env <- function(env_file = "conda_environment.yml",
                             env_name = NULL,
                             use_mamba = FALSE,
                             force = FALSE) {

  if (!file.exists(env_file)) {
    cli::cli_alert_danger("Environment file not found: {.file {env_file}}")
    return(invisible(FALSE))
  }

  # Determine which tool to use
  conda_cmd <- if (use_mamba) "mamba" else "conda"

  # Check if conda/mamba is available
  conda_check <- suppressWarnings(
    system2("which", conda_cmd, stdout = TRUE, stderr = FALSE)
  )

  if (length(conda_check) == 0 || is.null(conda_check)) {
    cli::cli_alert_danger("{conda_cmd} not found in PATH")
    return(invisible(FALSE))
  }

  # Extract name from YAML if not provided
  if (is.null(env_name)) {
    yaml_content <- tryCatch({
      yaml::read_yaml(env_file)
    }, error = function(e) {
      cli::cli_alert_danger("Could not read YAML file: {e$message}")
      return(NULL)
    })

    if (is.null(yaml_content)) return(invisible(FALSE))

    env_name <- yaml_content$name
    if (is.null(env_name)) {
      cli::cli_alert_danger("No environment name found in YAML file")
      return(invisible(FALSE))
    }
  }

  # Check if environment already exists
  env_list <- system2(conda_cmd, c("env", "list"), stdout = TRUE, stderr = TRUE)
  env_exists <- any(grepl(paste0("^", env_name, "\\s"), env_list))

  if (env_exists) {
    if (force) {
      cli::cli_alert_warning("Removing existing environment: {env_name}")
      remove_result <- system2(conda_cmd, c("env", "remove", "-n", env_name, "-y"),
                              stdout = TRUE, stderr = TRUE)
    } else {
      cli::cli_alert_danger("Environment already exists: {env_name}")
      cli::cli_alert_info("Use force = TRUE to recreate it")
      return(invisible(FALSE))
    }
  }

  # Create environment
  cli::cli_alert_info("Creating conda environment: {env_name}")
  cli::cli_alert_info("This may take several minutes...")

  cmd_args <- c("env", "create", "-f", env_file, "-n", env_name)
  result <- system2(conda_cmd, cmd_args, stdout = TRUE, stderr = TRUE)

  # Check if successful
  env_list_after <- system2(conda_cmd, c("env", "list"), stdout = TRUE, stderr = TRUE)
  success <- any(grepl(paste0("^", env_name, "\\s"), env_list_after))

  if (success) {
    cli::cli_alert_success("Environment restored: {env_name}")
    cli::cli_alert_info("Activate with: conda activate {env_name}")
    return(invisible(TRUE))
  } else {
    cli::cli_alert_danger("Failed to restore environment")
    return(invisible(FALSE))
  }
}


#' Get Conda Environment Info
#'
#' @description
#' Retrieve information about tracked conda environments
#'
#' @param env_name Character. Specific environment name, or NULL for all
#' @param registry_file Character. Path to conda registry
#'
#' @return List of environment information
#'
#' @export
get_conda_env_info <- function(env_name = NULL,
                               registry_file = ".capsule/conda_registry.json") {

  registry <- .load_conda_registry(registry_file)

  if (is.null(registry$environments) || length(registry$environments) == 0) {
    cli::cli_alert_warning("No conda environments tracked")
    return(NULL)
  }

  if (is.null(env_name)) {
    return(registry$environments)
  }

  if (env_name %in% names(registry$environments)) {
    return(registry$environments[[env_name]])
  } else {
    cli::cli_alert_warning("Environment not found: {env_name}")
    return(NULL)
  }
}


#' Load Conda Registry
#'
#' @description
#' Internal function to load the conda registry
#'
#' @param registry_file Character. Path to registry file
#'
#' @return List containing registry data
#' @keywords internal
.load_conda_registry <- function(registry_file) {
  if (file.exists(registry_file)) {
    jsonlite::read_json(registry_file, simplifyVector = FALSE)
  } else {
    list(
      created = Sys.time(),
      last_updated = Sys.time(),
      environments = list()
    )
  }
}


#' Save Conda Registry
#'
#' @description
#' Internal function to save the conda registry
#'
#' @param registry List. Registry data to save
#' @param registry_file Character. Path to registry file
#'
#' @return NULL
#' @keywords internal
.save_conda_registry <- function(registry, registry_file) {
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(registry, registry_file, auto_unbox = TRUE, pretty = TRUE)
}
