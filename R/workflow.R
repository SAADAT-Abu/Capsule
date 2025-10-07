#' Initialize ReproFlow in Project
#'
#' @description
#' Initialize ReproFlow reproducibility framework in the current project.
#' Creates necessary directory structure and configuration files.
#'
#' @param project_path Character. Path to project directory. Default is current directory.
#' @param use_renv Logical. Initialize renv for package management. Default TRUE.
#' @param use_git Logical. Initialize git if not already present. Default TRUE.
#' @param create_gitignore Logical. Create/update .gitignore. Default TRUE.
#'
#' @return Invisible NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Initialize ReproFlow in current directory
#' init_reproflow()
#'
#' # Initialize without renv
#' init_reproflow(use_renv = FALSE)
#' }
init_reproflow <- function(project_path = ".",
                          use_renv = TRUE,
                          use_git = TRUE,
                          create_gitignore = TRUE) {

  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(project_path)

  cli::cli_h1("Initializing ReproFlow")

  # Create ReproFlow directory structure
  cli::cli_alert_info("Creating directory structure...")
  dir.create(".reproflow", showWarnings = FALSE)
  dir.create(".reproflow/snapshots", showWarnings = FALSE)
  dir.create(".reproflow/scripts", showWarnings = FALSE)

  # Initialize renv if requested
  if (use_renv) {
    if (!requireNamespace("renv", quietly = TRUE)) {
      cli::cli_alert_info("Installing renv...")
      utils::install.packages("renv")
    }

    if (!file.exists("renv.lock")) {
      cli::cli_alert_info("Initializing renv...")
      renv::init(bare = TRUE)
      renv::snapshot()
    } else {
      cli::cli_alert_success("renv already initialized")
    }
  }

  # Initialize git if requested
  if (use_git && !dir.exists(".git")) {
    cli::cli_alert_info("Initializing git repository...")
    system("git init")
  }

  # Create/update .gitignore
  if (create_gitignore) {
    .create_gitignore()
  }

  # Create initial config file
  .create_config()

  # Create example workflow script
  .create_example_script()

  cli::cli_alert_success("ReproFlow initialized successfully!")
  cli::cli_h2("Next steps:")
  cli::cli_ul(c(
    "Use {.code track_data()} to track your data files",
    "Use {.code set_seed()} to set and track random seeds",
    "Use {.code track_params()} to track analysis parameters",
    "Use {.code snapshot_workflow()} to create a complete snapshot"
  ))

  invisible(NULL)
}


#' Create Complete Workflow Snapshot
#'
#' @description
#' Create a comprehensive snapshot of the entire workflow including session info,
#' packages, data, parameters, and generate all reproducibility artifacts.
#'
#' @param snapshot_name Character. Name for this snapshot. Default is timestamp.
#' @param analysis_name Character. Name of the analysis
#' @param source_script Character. Path to main analysis script
#' @param description Character. Description of this workflow
#' @param generate_docker Logical. Generate Docker configuration. Default TRUE.
#' @param generate_script Logical. Generate reproducible script. Default TRUE.
#' @param generate_report Logical. Generate reproducibility report. Default TRUE.
#'
#' @return List containing paths to generated files
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create complete workflow snapshot
#' snapshot_workflow(
#'   snapshot_name = "analysis_v1",
#'   analysis_name = "main_analysis",
#'   source_script = "analysis.R",
#'   description = "Initial analysis run"
#' )
#' }
snapshot_workflow <- function(snapshot_name = NULL,
                              analysis_name = "analysis",
                              source_script = NULL,
                              description = NULL,
                              generate_docker = TRUE,
                              generate_script = TRUE,
                              generate_report = TRUE) {

  if (is.null(snapshot_name)) {
    snapshot_name <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }

  cli::cli_h1(paste("Creating Workflow Snapshot:", snapshot_name))

  # Create snapshot directory
  snapshot_dir <- file.path(".reproflow/snapshots", snapshot_name)
  dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)

  generated_files <- list()

  # Capture session info
  cli::cli_alert_info("Capturing session information...")
  session_file <- file.path(snapshot_dir, "session_info.json")
  capture_session(session_file, format = "json")
  generated_files$session_info <- session_file

  # Capture environment
  cli::cli_alert_info("Capturing environment state...")
  env_file <- file.path(snapshot_dir, "environment.json")
  capture_environment(env_file)
  generated_files$environment <- env_file

  # Snapshot packages
  cli::cli_alert_info("Creating package manifest...")
  pkg_file <- file.path(snapshot_dir, "packages.json")
  snapshot_packages(pkg_file)
  generated_files$packages <- pkg_file

  # Create renv lockfile
  cli::cli_alert_info("Creating renv lockfile...")
  renv_file <- file.path(snapshot_dir, "renv.lock")
  tryCatch({
    if (requireNamespace("renv", quietly = TRUE)) {
      renv::snapshot(lockfile = renv_file, prompt = FALSE)
      generated_files$renv_lock <- renv_file
    }
  }, error = function(e) {
    cli::cli_alert_warning("Could not create renv lockfile: {e$message}")
  })

  # Copy registries
  cli::cli_alert_info("Copying tracking registries...")
  if (file.exists(".reproflow/data_registry.json")) {
    file.copy(".reproflow/data_registry.json",
              file.path(snapshot_dir, "data_registry.json"))
    generated_files$data_registry <- file.path(snapshot_dir, "data_registry.json")
  }
  if (file.exists(".reproflow/param_registry.json")) {
    file.copy(".reproflow/param_registry.json",
              file.path(snapshot_dir, "param_registry.json"))
    generated_files$param_registry <- file.path(snapshot_dir, "param_registry.json")
  }
  if (file.exists(".reproflow/seed_registry.json")) {
    file.copy(".reproflow/seed_registry.json",
              file.path(snapshot_dir, "seed_registry.json"))
    generated_files$seed_registry <- file.path(snapshot_dir, "seed_registry.json")
  }

  # Generate reproducible script
  if (generate_script) {
    cli::cli_alert_info("Generating reproducible script...")
    script_file <- file.path(snapshot_dir, paste0(analysis_name, "_reproducible.R"))
    generate_repro_script(
      script_file,
      source_script = source_script,
      analysis_name = analysis_name
    )
    generated_files$script <- script_file
  }

  # Generate Docker configuration
  if (generate_docker) {
    cli::cli_alert_info("Generating Docker configuration...")
    docker_dir <- file.path(snapshot_dir, "docker")
    dir.create(docker_dir, showWarnings = FALSE)
    docker_files <- generate_docker(
      output_dir = docker_dir,
      project_name = analysis_name
    )
    generated_files$docker <- docker_files
  }

  # Generate reproducibility report
  if (generate_report) {
    cli::cli_alert_info("Generating reproducibility report...")
    report_file <- file.path(snapshot_dir, "reproducibility_report.md")
    create_repro_report(
      output_file = report_file,
      analysis_name = analysis_name
    )
    generated_files$report <- report_file
  }

  # Create snapshot metadata
  metadata <- list(
    snapshot_name = snapshot_name,
    analysis_name = analysis_name,
    description = description,
    created = Sys.time(),
    created_by = Sys.info()["user"],
    r_version = R.version.string,
    reproflow_version = as.character(packageVersion("ReproFlow")),
    source_script = source_script,
    files = generated_files
  )

  metadata_file <- file.path(snapshot_dir, "snapshot_metadata.json")
  jsonlite::write_json(metadata, metadata_file, auto_unbox = TRUE, pretty = TRUE)
  generated_files$metadata <- metadata_file

  cli::cli_alert_success("Workflow snapshot created: {.file {snapshot_dir}}")
  cli::cli_h2("Generated files:")
  cli::cli_ul(names(generated_files))

  invisible(generated_files)
}


#' Create .gitignore
#' @keywords internal
.create_gitignore <- function() {
  gitignore_path <- ".gitignore"

  # Lines to add
  reproflow_lines <- c(
    "",
    "# ReproFlow",
    ".reproflow/snapshots/",
    "",
    "# R",
    ".Rhistory",
    ".RData",
    ".Rproj.user",
    "",
    "# renv",
    "renv/library/",
    "renv/local/",
    "renv/cellar/",
    "renv/lock/",
    "renv/python/",
    "renv/staging/"
  )

  # Read existing or create new
  if (file.exists(gitignore_path)) {
    existing <- readLines(gitignore_path)
    if (!any(grepl("ReproFlow", existing))) {
      writeLines(c(existing, reproflow_lines), gitignore_path)
      cli::cli_alert_success("Updated .gitignore")
    }
  } else {
    writeLines(reproflow_lines, gitignore_path)
    cli::cli_alert_success("Created .gitignore")
  }
}


#' Create ReproFlow config
#' @keywords internal
.create_config <- function() {
  config <- list(
    version = "0.1.0",
    created = Sys.time(),
    project_name = basename(getwd()),
    track_data = TRUE,
    track_params = TRUE,
    track_seeds = TRUE,
    auto_snapshot = FALSE
  )

  config_file <- ".reproflow/config.json"
  jsonlite::write_json(config, config_file, auto_unbox = TRUE, pretty = TRUE)
  cli::cli_alert_success("Created configuration file")
}


#' Create example workflow script
#' @keywords internal
.create_example_script <- function() {
  example_script <- c(
    "# ReproFlow Example Workflow",
    "# This is an example of how to use ReproFlow in your analysis",
    "",
    "library(ReproFlow)",
    "",
    "# 1. Set and track random seed",
    "set_seed(12345, analysis_name = 'example_analysis')",
    "",
    "# 2. Track analysis parameters",
    "params <- list(",
    "  n_simulations = 1000,",
    "  alpha = 0.05,",
    "  method = 'bootstrap'",
    ")",
    "track_params(params, 'example_analysis', 'Example analysis parameters')",
    "",
    "# 3. Track data files",
    "# track_data('data/mydata.csv', source = 'downloaded',",
    "#            source_url = 'https://example.com/data.csv')",
    "",
    "# 4. Your analysis code here",
    "# ...",
    "",
    "# 5. Create complete workflow snapshot",
    "# snapshot_workflow(",
    "#   snapshot_name = 'final_analysis',",
    "#   analysis_name = 'example_analysis',",
    "#   source_script = 'this_script.R',",
    "#   description = 'Complete analysis workflow'",
    "# )"
  )

  example_file <- ".reproflow/example_workflow.R"
  writeLines(example_script, example_file)
  cli::cli_alert_success("Created example workflow: {.file {example_file}}")
}
