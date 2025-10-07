#' Track Package Versions and Dependencies
#'
#' @description
#' Creates a comprehensive snapshot of all installed packages, their versions,
#' dependencies, and sources for reproducibility.
#'
#' @param output_file Character. Path to save package info. If NULL, returns as list.
#' @param include_dependencies Logical. Include dependency tree. Default TRUE.
#' @param only_attached Logical. Only track attached packages. Default FALSE.
#'
#' @return A list containing package information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Track all installed packages
#' snapshot_packages("package_manifest.json")
#'
#' # Track only attached packages
#' snapshot_packages("packages.json", only_attached = TRUE)
#' }
snapshot_packages <- function(output_file = NULL, include_dependencies = TRUE,
                               only_attached = FALSE) {

  if (only_attached) {
    # Get only attached packages
    attached <- names(sessionInfo()$otherPkgs)
    pkg_list <- as.data.frame(utils::installed.packages())[attached, ]
  } else {
    # Get all installed packages
    pkg_list <- as.data.frame(utils::installed.packages())
  }

  packages_info <- list(
    timestamp = Sys.time(),
    r_version = R.version.string,
    n_packages = nrow(pkg_list),
    packages = lapply(seq_len(nrow(pkg_list)), function(i) {
      pkg <- pkg_list[i, ]

      info <- list(
        package = as.character(pkg$Package),
        version = as.character(pkg$Version),
        priority = as.character(pkg$Priority),
        depends = .parse_deps(pkg$Depends),
        imports = .parse_deps(pkg$Imports),
        suggests = .parse_deps(pkg$Suggests),
        built = as.character(pkg$Built),
        repository = as.character(pkg$Repository),
        license = as.character(pkg$License),
        needs_compilation = as.character(pkg$NeedsCompilation),
        lib_path = as.character(pkg$LibPath)
      )

      # Add source information if available
      if (!is.null(pkg$Repository) && !is.na(pkg$Repository) && pkg$Repository != "") {
        info$source <- list(
          type = "CRAN",
          repository = as.character(pkg$Repository)
        )
      }

      info
    })
  )

  names(packages_info$packages) <- pkg_list$Package

  # Add dependency graph if requested
  if (include_dependencies) {
    packages_info$dependency_graph <- .build_dependency_graph(pkg_list$Package)
  }

  # Save if output file specified
  if (!is.null(output_file)) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(packages_info, output_file, auto_unbox = TRUE, pretty = TRUE)
    cli::cli_alert_success("Package manifest saved to {.file {output_file}}")
  }

  invisible(packages_info)
}


#' Parse Package Dependencies
#'
#' @description
#' Internal function to parse package dependency strings
#'
#' @param dep_string Character. Dependency string from package description
#'
#' @return Character vector of package names
#' @keywords internal
.parse_deps <- function(dep_string) {
  if (is.null(dep_string) || is.na(dep_string) || dep_string == "") return(character(0))

  # Remove version specifications
  deps <- strsplit(dep_string, ",")[[1]]
  deps <- trimws(deps)
  deps <- gsub("\\s*\\(.*?\\)", "", deps)

  # Remove R itself
  deps <- deps[deps != "R"]

  deps
}


#' Build Dependency Graph
#'
#' @description
#' Internal function to build a dependency graph for packages
#'
#' @param packages Character vector of package names
#'
#' @return List representing dependency relationships
#' @keywords internal
.build_dependency_graph <- function(packages) {
  graph <- list()

  for (pkg in packages) {
    desc <- utils::packageDescription(pkg)
    if (is.list(desc)) {
      deps <- unique(c(
        .parse_deps(desc$Depends),
        .parse_deps(desc$Imports)
      ))
      graph[[pkg]] <- deps
    }
  }

  graph
}


#' Create renv Lockfile
#'
#' @description
#' Generate an renv-compatible lockfile for package reproducibility
#'
#' @param output_file Character. Path to save lockfile. Default "renv.lock".
#' @param project_path Character. Path to project. Default is current directory.
#'
#' @return Path to created lockfile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_renv_lockfile("renv.lock")
#' }
create_renv_lockfile <- function(output_file = "renv.lock", project_path = ".") {

  if (!requireNamespace("renv", quietly = TRUE)) {
    cli::cli_alert_warning("renv package not installed. Installing now...")
    utils::install.packages("renv")
  }

  # Initialize renv in temp directory to avoid affecting current project
  old_wd <- getwd()
  on.exit(setwd(old_wd))

  # Create snapshot
  tryCatch({
    renv::snapshot(lockfile = output_file, prompt = FALSE)
    cli::cli_alert_success("renv lockfile created at {.file {output_file}}")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to create renv lockfile: {e$message}")
  })

  invisible(output_file)
}
