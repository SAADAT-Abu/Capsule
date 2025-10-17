#' Export Capsule Data for Nextflow
#'
#' @description
#' Export all Capsule tracking data in a format suitable for Nextflow pipelines
#'
#' @param output_file Character. Path to save manifest. Default "capsule_manifest.json"
#' @param include_checksums Logical. Include file checksums. Default TRUE.
#'
#' @return List containing manifest data
#'
#' @importFrom utils packageVersion
#' @export
#'
#' @examples
#' \dontrun{
#' export_for_nextflow("reproflow_manifest.json")
#' }
export_for_nextflow <- function(output_file = "capsule_manifest.json",
                                include_checksums = TRUE) {
  cli::cli_alert_info("Exporting Capsule data for Nextflow...")

  # Gather all tracking data
  data_reg <- .load_registry(".capsule/data_registry.json")
  param_reg <- .load_param_registry(".capsule/param_registry.json")
  seed_reg <- .load_seed_registry(".capsule/seed_registry.json")

  # Format for Nextflow
  manifest <- list(
    capsule_version = as.character(utils::packageVersion("Capsule")),
    exported_at = Sys.time(),
    r_version = R.version.string
  )

  # Data files
  if (!is.null(data_reg$data) && length(data_reg$data) > 0) {
    manifest$data_files <- lapply(names(data_reg$data), function(name) {
      d <- data_reg$data[[name]]
      file_info <- list(
        path = d$file_path,
        source = d$source
      )
      if (include_checksums) {
        file_info$checksum <- d$checksum
        file_info$checksum_algorithm <- d$checksum_algorithm
      }
      file_info
    })
    names(manifest$data_files) <- names(data_reg$data)
  }

  # Parameters
  if (!is.null(param_reg$analyses) && length(param_reg$analyses) > 0) {
    manifest$parameters <- param_reg$analyses
  }

  # Random seeds
  if (!is.null(seed_reg$seeds) && length(seed_reg$seeds) > 0) {
    manifest$random_seeds <- seed_reg$seeds
  }

  # Write manifest
  jsonlite::write_json(manifest, output_file, auto_unbox = TRUE, pretty = TRUE)

  cli::cli_alert_success("Nextflow manifest: {.file {output_file}}")
  cli::cli_h2("Import in nextflow.config:")
  cli::cli_code("import groovy.json.JsonSlurper")
  cli::cli_code("def capsule = new JsonSlurper().parse(file('$output_file'))")
  cli::cli_code("params.capsule = capsule")

  invisible(manifest)
}


#' Export Capsule Data for Snakemake
#'
#' @description
#' Export all Capsule tracking data in YAML format for Snakemake pipelines
#'
#' @param output_file Character. Path to save config. Default "capsule_config.yaml"
#' @param include_checksums Logical. Include file checksums. Default TRUE.
#'
#' @return List containing config data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_for_snakemake("reproflow_config.yaml")
#' }
export_for_snakemake <- function(output_file = "capsule_config.yaml",
                                 include_checksums = TRUE) {
  cli::cli_alert_info("Exporting Capsule data for Snakemake...")

  # Gather all tracking data
  data_reg <- .load_registry(".capsule/data_registry.json")
  param_reg <- .load_param_registry(".capsule/param_registry.json")
  seed_reg <- .load_seed_registry(".capsule/seed_registry.json")

  # Format for Snakemake (YAML-friendly)
  config <- list(
    capsule = list(
      version = as.character(utils::packageVersion("Capsule")),
      exported_at = as.character(Sys.time()),
      r_version = R.version.string
    )
  )

  # Data files - convert to simple format
  if (!is.null(data_reg$data) && length(data_reg$data) > 0) {
    data_files <- list()
    for (name in names(data_reg$data)) {
      d <- data_reg$data[[name]]
      file_info <- list(
        path = d$file_path,
        source = d$source
      )
      if (include_checksums) {
        file_info$checksum <- d$checksum
      }
      data_files[[basename(name)]] <- file_info
    }
    config$capsule$data_files <- data_files
  }

  # Parameters - flatten for easier access
  if (!is.null(param_reg$analyses) && length(param_reg$analyses) > 0) {
    params_flat <- list()
    for (analysis_name in names(param_reg$analyses)) {
      analysis <- param_reg$analyses[[analysis_name]]
      if (!is.null(analysis$parameters)) {
        for (param in analysis$parameters) {
          params_flat[[param$name]] <- param$value
        }
      }
    }
    config$capsule$parameters <- params_flat
  }

  # Random seeds
  if (!is.null(seed_reg$seeds) && length(seed_reg$seeds) > 0) {
    seeds <- list()
    for (seed_name in names(seed_reg$seeds)) {
      seeds[[seed_name]] <- seed_reg$seeds[[seed_name]]$seed
    }
    config$capsule$random_seeds <- seeds
  }

  # Write config
  yaml::write_yaml(config, output_file)

  cli::cli_alert_success("Snakemake config: {.file {output_file}}")
  cli::cli_h2("Import in Snakefile:")
  cli::cli_code("configfile: '$output_file'")
  cli::cli_code("# Access with: config['capsule']['parameters']['param_name']")

  invisible(config)
}


#' Create WDL (Workflow Description Language) Config
#'
#' @description
#' Export Capsule data in JSON format suitable for WDL workflows
#'
#' @param output_file Character. Path to save config. Default "capsule_inputs.json"
#'
#' @return List containing config data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_for_wdl("reproflow_inputs.json")
#' }
export_for_wdl <- function(output_file = "capsule_inputs.json") {
  cli::cli_alert_info("Exporting Capsule data for WDL...")

  # Gather all tracking data
  data_reg <- .load_registry(".capsule/data_registry.json")
  param_reg <- .load_param_registry(".capsule/param_registry.json")

  # WDL uses a flat structure for inputs
  inputs <- list()

  # Add data file paths
  if (!is.null(data_reg$data) && length(data_reg$data) > 0) {
    for (name in names(data_reg$data)) {
      d <- data_reg$data[[name]]
      # Use basename as key
      key <- paste0("input_", gsub("[^a-zA-Z0-9_]", "_", basename(name)))
      inputs[[key]] <- d$file_path
    }
  }

  # Add parameters
  if (!is.null(param_reg$analyses) && length(param_reg$analyses) > 0) {
    for (analysis_name in names(param_reg$analyses)) {
      analysis <- param_reg$analyses[[analysis_name]]
      if (!is.null(analysis$parameters)) {
        for (param in analysis$parameters) {
          inputs[[param$name]] <- param$value
        }
      }
    }
  }

  # Write WDL inputs JSON
  jsonlite::write_json(inputs, output_file, auto_unbox = TRUE, pretty = TRUE)

  cli::cli_alert_success("WDL inputs: {.file {output_file}}")
  cli::cli_h2("Use with WDL:")
  cli::cli_code("cromwell run workflow.wdl -i $output_file")

  invisible(inputs)
}


#' Generate CWL (Common Workflow Language) Input
#'
#' @description
#' Export Capsule data in YAML format suitable for CWL workflows
#'
#' @param output_file Character. Path to save inputs. Default "capsule_cwl_inputs.yml"
#'
#' @return List containing input data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_for_cwl("reproflow_cwl_inputs.yml")
#' }
export_for_cwl <- function(output_file = "capsule_cwl_inputs.yml") {
  cli::cli_alert_info("Exporting Capsule data for CWL...")

  # Gather all tracking data
  data_reg <- .load_registry(".capsule/data_registry.json")
  param_reg <- .load_param_registry(".capsule/param_registry.json")

  inputs <- list()

  # Add data files in CWL format
  if (!is.null(data_reg$data) && length(data_reg$data) > 0) {
    for (name in names(data_reg$data)) {
      d <- data_reg$data[[name]]
      key <- paste0("input_", gsub("[^a-zA-Z0-9_]", "_", basename(name)))
      inputs[[key]] <- list(
        class = "File",
        path = d$file_path
      )
    }
  }

  # Add parameters
  if (!is.null(param_reg$analyses) && length(param_reg$analyses) > 0) {
    for (analysis_name in names(param_reg$analyses)) {
      analysis <- param_reg$analyses[[analysis_name]]
      if (!is.null(analysis$parameters)) {
        for (param in analysis$parameters) {
          inputs[[param$name]] <- param$value
        }
      }
    }
  }

  # Write CWL inputs
  yaml::write_yaml(inputs, output_file)

  cli::cli_alert_success("CWL inputs: {.file {output_file}}")
  cli::cli_h2("Use with CWL:")
  cli::cli_code("cwl-runner workflow.cwl $output_file")

  invisible(inputs)
}
