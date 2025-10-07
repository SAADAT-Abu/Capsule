#' Capture Complete Session Information
#'
#' @description
#' Captures comprehensive R session information including R version, platform,
#' loaded packages, system information, and locale settings.
#'
#' @param output_file Character. Path to save the session info. If NULL, returns as list.
#' @param format Character. Output format: "json", "yaml", or "rds". Default is "json".
#'
#' @return A list containing session information, invisibly returned
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Capture session info to JSON
#' capture_session("session_info.json")
#'
#' # Capture and return as list
#' info <- capture_session()
#' }
capture_session <- function(output_file = NULL, format = c("json", "yaml", "rds")) {
  format <- match.arg(format)

  # Get basic session info
  si <- utils::sessionInfo()

  # Capture comprehensive information
  session_info <- list(
    timestamp = Sys.time(),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    r_version_full = R.version.string,
    platform = si$platform,
    running = si$running,
    locale = Sys.getlocale(),
    system = list(
      sysname = Sys.info()["sysname"],
      release = Sys.info()["release"],
      version = Sys.info()["version"],
      nodename = Sys.info()["nodename"],
      machine = Sys.info()["machine"],
      user = Sys.info()["user"]
    ),
    base_packages = si$basePkgs,
    loaded_packages = .extract_package_info(si$otherPkgs),
    attached_packages = .extract_package_info(si$loadedOnly),
    matrix_products = si$matprod,
    blas = si$BLAS,
    lapack = si$LAPACK,
    search_path = search(),
    libpaths = .libPaths(),
    environment_vars = list(
      R_LIBS_USER = Sys.getenv("R_LIBS_USER"),
      R_LIBS_SITE = Sys.getenv("R_LIBS_SITE"),
      R_HOME = R.home()
    )
  )

  # Save if output file specified
  if (!is.null(output_file)) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

    switch(format,
           json = jsonlite::write_json(session_info, output_file,
                                       auto_unbox = TRUE, pretty = TRUE),
           yaml = yaml::write_yaml(session_info, output_file),
           rds = saveRDS(session_info, output_file)
    )

    cli::cli_alert_success("Session info saved to {.file {output_file}}")
  }

  invisible(session_info)
}


#' Extract Package Information
#'
#' @description
#' Internal function to extract detailed package information
#'
#' @param pkg_list List of package info from sessionInfo()
#'
#' @return List of package details
#' @keywords internal
.extract_package_info <- function(pkg_list) {
  if (is.null(pkg_list)) return(list())

  lapply(pkg_list, function(pkg) {
    list(
      package = pkg$Package,
      version = as.character(pkg$Version),
      date = pkg$Date,
      built = pkg$Built,
      repository = pkg$Repository,
      path = pkg$LibPath
    )
  })
}


#' Capture Environment State
#'
#' @description
#' Captures the current global environment state including objects and their types
#'
#' @param output_file Character. Path to save environment info. If NULL, returns as list.
#' @param include_values Logical. Whether to include object values (for small objects). Default FALSE.
#' @param max_size Numeric. Maximum object size (in bytes) to include values. Default 1MB.
#'
#' @return A list containing environment information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- 1:10
#' y <- "test"
#' capture_environment("env_state.json")
#' }
capture_environment <- function(output_file = NULL, include_values = FALSE,
                                max_size = 1024 * 1024) {

  # Get all objects in global environment
  obj_names <- ls(envir = .GlobalEnv, all.names = FALSE)

  env_info <- list(
    timestamp = Sys.time(),
    n_objects = length(obj_names),
    objects = lapply(obj_names, function(name) {
      obj <- get(name, envir = .GlobalEnv)
      obj_size <- object.size(obj)

      info <- list(
        name = name,
        class = class(obj),
        type = typeof(obj),
        size_bytes = as.numeric(obj_size),
        size_readable = format(obj_size, units = "auto"),
        dimensions = if (is.null(dim(obj))) length(obj) else dim(obj)
      )

      # Include values for small objects if requested
      if (include_values && as.numeric(obj_size) <= max_size) {
        info$value <- obj
      }

      info
    })
  )

  names(env_info$objects) <- obj_names

  # Save if output file specified
  if (!is.null(output_file)) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(env_info, output_file, auto_unbox = TRUE, pretty = TRUE)
    cli::cli_alert_success("Environment state saved to {.file {output_file}}")
  }

  invisible(env_info)
}
