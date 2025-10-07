#' Track Analysis Parameters
#'
#' @description
#' Record analysis parameters and configuration settings for reproducibility
#'
#' @param params Named list of parameters to track
#' @param analysis_name Character. Name/identifier for this analysis
#' @param description Character. Description of what these parameters control
#' @param registry_file Character. Path to parameter registry
#'
#' @return List containing parameter information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track model parameters
#' params <- list(
#'   learning_rate = 0.01,
#'   epochs = 100,
#'   batch_size = 32,
#'   model_type = "neural_network"
#' )
#' track_params(params, "model_training", "Deep learning model parameters")
#' }
track_params <- function(params, analysis_name = NULL, description = NULL,
                        registry_file = ".reproflow/param_registry.json") {

  if (!is.list(params) && !is.environment(params)) {
    cli::cli_alert_danger("params must be a list or environment")
    return(invisible(NULL))
  }

  # Convert environment to list if needed
  if (is.environment(params)) {
    params <- as.list(params)
  }

  if (is.null(analysis_name)) {
    analysis_name <- paste0("analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  # Create parameter record
  param_record <- list(
    analysis_name = analysis_name,
    description = description,
    timestamp = Sys.time(),
    tracked_by = Sys.info()["user"],
    n_params = length(params),
    parameters = lapply(names(params), function(name) {
      value <- params[[name]]
      list(
        name = name,
        value = value,
        class = class(value),
        type = typeof(value)
      )
    })
  )

  names(param_record$parameters) <- names(params)

  # Load existing registry
  registry <- .load_param_registry(registry_file)

  # Add to registry
  registry$analyses[[analysis_name]] <- param_record
  registry$last_updated <- Sys.time()

  # Save registry
  .save_param_registry(registry, registry_file)

  cli::cli_alert_success("Parameters tracked for: {.val {analysis_name}}")
  cli::cli_alert_info("Tracked {.val {length(params)}} parameter(s)")

  invisible(param_record)
}


#' Set and Track Random Seed
#'
#' @description
#' Set a random seed and track it for reproducibility
#'
#' @param seed Numeric. Random seed to set. If NULL, generates random seed.
#' @param kind Character. RNG kind (see ?set.seed). Default NULL uses current.
#' @param normal.kind Character. Normal RNG kind. Default NULL uses current.
#' @param sample.kind Character. Sample RNG kind. Default NULL uses current.
#' @param analysis_name Character. Name to associate with this seed
#' @param registry_file Character. Path to seed registry
#'
#' @return The seed value (invisibly)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Set and track a specific seed
#' set_seed(12345, analysis_name = "simulation_1")
#'
#' # Generate and track a random seed
#' set_seed(analysis_name = "bootstrap_analysis")
#' }
set_seed <- function(seed = NULL, kind = NULL, normal.kind = NULL,
                     sample.kind = NULL, analysis_name = NULL,
                     registry_file = ".reproflow/seed_registry.json") {

  # Generate random seed if not provided
  if (is.null(seed)) {
    seed <- as.integer(Sys.time())
  }

  # Set the seed
  if (is.null(kind)) {
    set.seed(seed)
  } else {
    set.seed(seed, kind = kind, normal.kind = normal.kind, sample.kind = sample.kind)
  }

  # Get RNG state
  rng_state <- .Random.seed

  if (is.null(analysis_name)) {
    analysis_name <- paste0("seed_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  # Create seed record
  seed_record <- list(
    analysis_name = analysis_name,
    seed = seed,
    kind = if (!is.null(kind)) kind else RNGkind()[1],
    normal.kind = if (!is.null(normal.kind)) normal.kind else RNGkind()[2],
    sample.kind = if (!is.null(sample.kind)) sample.kind else RNGkind()[3],
    rng_state = rng_state,
    timestamp = Sys.time(),
    tracked_by = Sys.info()["user"],
    r_version = R.version.string
  )

  # Load existing registry
  registry <- .load_seed_registry(registry_file)

  # Add to registry
  registry$seeds[[analysis_name]] <- seed_record
  registry$last_updated <- Sys.time()

  # Save registry
  .save_seed_registry(registry, registry_file)

  cli::cli_alert_success("Random seed set and tracked: {.val {seed}}")
  cli::cli_alert_info("Analysis: {.val {analysis_name}}")

  invisible(seed)
}


#' Restore Random Seed
#'
#' @description
#' Restore a previously tracked random seed
#'
#' @param analysis_name Character. Name of analysis to restore seed from
#' @param registry_file Character. Path to seed registry
#'
#' @return The seed value (invisibly)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Restore previously tracked seed
#' restore_seed("simulation_1")
#' }
restore_seed <- function(analysis_name, registry_file = ".reproflow/seed_registry.json") {

  registry <- .load_seed_registry(registry_file)

  if (!analysis_name %in% names(registry$seeds)) {
    cli::cli_alert_danger("Seed not found for analysis: {.val {analysis_name}}")
    return(invisible(NULL))
  }

  seed_record <- registry$seeds[[analysis_name]]

  # Restore RNG state
  .Random.seed <<- seed_record$rng_state

  cli::cli_alert_success("Random seed restored: {.val {seed_record$seed}}")
  cli::cli_alert_info("Analysis: {.val {analysis_name}}")

  invisible(seed_record$seed)
}


#' Get Parameter History
#'
#' @description
#' Retrieve parameter tracking history
#'
#' @param analysis_name Character. Specific analysis name, or NULL for all
#' @param registry_file Character. Path to parameter registry
#'
#' @return List of parameter records
#'
#' @export
get_param_history <- function(analysis_name = NULL,
                              registry_file = ".reproflow/param_registry.json") {

  registry <- .load_param_registry(registry_file)

  if (is.null(analysis_name)) {
    return(registry$analyses)
  }

  if (analysis_name %in% names(registry$analyses)) {
    return(registry$analyses[[analysis_name]])
  } else {
    cli::cli_alert_warning("No parameters found for: {.val {analysis_name}}")
    return(NULL)
  }
}


#' Get Seed History
#'
#' @description
#' Retrieve seed tracking history
#'
#' @param analysis_name Character. Specific analysis name, or NULL for all
#' @param registry_file Character. Path to seed registry
#'
#' @return List of seed records
#'
#' @export
get_seed_history <- function(analysis_name = NULL,
                             registry_file = ".reproflow/seed_registry.json") {

  registry <- .load_seed_registry(registry_file)

  if (is.null(analysis_name)) {
    return(registry$seeds)
  }

  if (analysis_name %in% names(registry$seeds)) {
    return(registry$seeds[[analysis_name]])
  } else {
    cli::cli_alert_warning("No seed found for: {.val {analysis_name}}")
    return(NULL)
  }
}


#' Load Parameter Registry
#' @keywords internal
.load_param_registry <- function(registry_file) {
  if (file.exists(registry_file)) {
    jsonlite::read_json(registry_file, simplifyVector = FALSE)
  } else {
    list(
      created = Sys.time(),
      last_updated = Sys.time(),
      analyses = list()
    )
  }
}


#' Save Parameter Registry
#' @keywords internal
.save_param_registry <- function(registry, registry_file) {
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(registry, registry_file, auto_unbox = TRUE, pretty = TRUE)
}


#' Load Seed Registry
#' @keywords internal
.load_seed_registry <- function(registry_file) {
  if (file.exists(registry_file)) {
    jsonlite::read_json(registry_file, simplifyVector = FALSE)
  } else {
    list(
      created = Sys.time(),
      last_updated = Sys.time(),
      seeds = list()
    )
  }
}


#' Save Seed Registry
#' @keywords internal
.save_seed_registry <- function(registry, registry_file) {
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(registry, registry_file, auto_unbox = TRUE, pretty = TRUE)
}
