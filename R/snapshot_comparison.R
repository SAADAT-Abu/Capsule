#' Compare Two Workflow Snapshots
#'
#' @description
#' Compare two Capsule snapshots to identify differences in packages,
#' parameters, data files, and other tracked components
#'
#' @param snapshot1 Character. Name of first snapshot
#' @param snapshot2 Character. Name of second snapshot
#' @param output_file Character. Path to save comparison report. Default "snapshot_comparison.md"
#'
#' @return List containing comparison results
#'
#' @export
#'
#' @examples
#' \dontrun{
#' compare_snapshots("analysis_v1", "analysis_v2")
#' }
compare_snapshots <- function(snapshot1, snapshot2,
                              output_file = "snapshot_comparison.md") {
  cli::cli_alert_info("Comparing snapshots: {snapshot1} vs {snapshot2}")

  snap1_dir <- file.path(".capsule/snapshots", snapshot1)
  snap2_dir <- file.path(".capsule/snapshots", snapshot2)

  # Check if snapshots exist
  if (!dir.exists(snap1_dir)) {
    cli::cli_alert_danger("Snapshot not found: {snapshot1}")
    return(invisible(NULL))
  }

  if (!dir.exists(snap2_dir)) {
    cli::cli_alert_danger("Snapshot not found: {snapshot2}")
    return(invisible(NULL))
  }

  # Load metadata
  meta1 <- jsonlite::read_json(file.path(snap1_dir, "snapshot_metadata.json"),
    simplifyVector = FALSE
  )
  meta2 <- jsonlite::read_json(file.path(snap2_dir, "snapshot_metadata.json"),
    simplifyVector = FALSE
  )

  # Compare packages
  pkg1 <- jsonlite::read_json(file.path(snap1_dir, "packages.json"),
    simplifyVector = FALSE
  )
  pkg2 <- jsonlite::read_json(file.path(snap2_dir, "packages.json"),
    simplifyVector = FALSE
  )

  pkg_diff <- .compare_packages(pkg1, pkg2)

  # Compare parameters
  param_diff <- .compare_registries(
    file.path(snap1_dir, "param_registry.json"),
    file.path(snap2_dir, "param_registry.json"),
    "parameters"
  )

  # Compare data files
  data_diff <- .compare_registries(
    file.path(snap1_dir, "data_registry.json"),
    file.path(snap2_dir, "data_registry.json"),
    "data"
  )

  # Compare seeds
  seed_diff <- .compare_registries(
    file.path(snap1_dir, "seed_registry.json"),
    file.path(snap2_dir, "seed_registry.json"),
    "seeds"
  )

  # Generate report
  report <- .generate_comparison_report(
    snapshot1, snapshot2,
    meta1, meta2,
    pkg_diff, param_diff, data_diff, seed_diff
  )

  writeLines(report, output_file)

  cli::cli_alert_success("Comparison saved: {.file {output_file}}")

  # Return structured results
  results <- list(
    packages = pkg_diff,
    parameters = param_diff,
    data = data_diff,
    seeds = seed_diff
  )

  invisible(results)
}


#' Compare Package Lists
#'
#' @description
#' Internal function to compare package versions between two snapshots
#'
#' @keywords internal
.compare_packages <- function(pkg1, pkg2) {
  pkgs1 <- names(pkg1$packages)
  pkgs2 <- names(pkg2$packages)

  # Find added, removed, and changed packages
  added <- setdiff(pkgs2, pkgs1)
  removed <- setdiff(pkgs1, pkgs2)
  common <- intersect(pkgs1, pkgs2)

  changed <- character()
  for (pkg in common) {
    v1 <- pkg1$packages[[pkg]]$version
    v2 <- pkg2$packages[[pkg]]$version
    if (!identical(v1, v2)) {
      changed <- c(changed, pkg)
    }
  }

  list(
    added = added,
    removed = removed,
    changed = changed,
    details = lapply(changed, function(pkg) {
      list(
        package = pkg,
        v1 = pkg1$packages[[pkg]]$version,
        v2 = pkg2$packages[[pkg]]$version
      )
    })
  )
}


#' Compare Registry Files
#'
#' @description
#' Internal function to compare registry files
#'
#' @keywords internal
.compare_registries <- function(file1, file2, type) {
  if (!file.exists(file1) || !file.exists(file2)) {
    return(list(status = "one or both files missing"))
  }

  reg1 <- jsonlite::read_json(file1, simplifyVector = FALSE)
  reg2 <- jsonlite::read_json(file2, simplifyVector = FALSE)

  # Extract the relevant section based on type
  data1 <- switch(type,
    "parameters" = reg1$analyses,
    "data" = reg1$data,
    "seeds" = reg1$seeds,
    reg1
  )

  data2 <- switch(type,
    "parameters" = reg2$analyses,
    "data" = reg2$data,
    "seeds" = reg2$seeds,
    reg2
  )

  if (is.null(data1)) data1 <- list()
  if (is.null(data2)) data2 <- list()

  items1 <- names(data1)
  items2 <- names(data2)

  added <- setdiff(items2, items1)
  removed <- setdiff(items1, items2)
  common <- intersect(items1, items2)

  # Check for modifications in common items
  modified <- character()
  for (item in common) {
    # Simple comparison - could be more sophisticated
    if (!identical(data1[[item]], data2[[item]])) {
      modified <- c(modified, item)
    }
  }

  list(
    added = if (length(added) > 0) added else character(0),
    removed = if (length(removed) > 0) removed else character(0),
    modified = if (length(modified) > 0) modified else character(0),
    n_added = length(added),
    n_removed = length(removed),
    n_modified = length(modified)
  )
}


#' Generate Comparison Report
#'
#' @description
#' Internal function to generate markdown comparison report
#'
#' @keywords internal
.generate_comparison_report <- function(snap1, snap2, meta1, meta2,
                                        pkg_diff, param_diff, data_diff, seed_diff) {
  report <- c(
    paste("#", "Snapshot Comparison"),
    "",
    paste("**Snapshot 1:**", snap1),
    paste("- Created:", meta1$created),
    paste("- R version:", meta1$r_version),
    "",
    paste("**Snapshot 2:**", snap2),
    paste("- Created:", meta2$created),
    paste("- R version:", meta2$r_version),
    "",
    "---",
    "",
    "## Summary",
    ""
  )

  # Summary table
  report <- c(
    report,
    "| Component | Added | Removed | Changed |",
    "|-----------|-------|---------|---------|",
    paste(
      "|", "Packages", "|",
      length(pkg_diff$added), "|",
      length(pkg_diff$removed), "|",
      length(pkg_diff$changed), "|"
    ),
    paste(
      "|", "Parameters", "|",
      param_diff$n_added, "|",
      param_diff$n_removed, "|",
      param_diff$n_modified, "|"
    ),
    paste(
      "|", "Data Files", "|",
      data_diff$n_added, "|",
      data_diff$n_removed, "|",
      data_diff$n_modified, "|"
    ),
    paste(
      "|", "Seeds", "|",
      seed_diff$n_added, "|",
      seed_diff$n_removed, "|",
      seed_diff$n_modified, "|"
    ),
    "",
    "---",
    ""
  )

  # Package changes
  report <- c(
    report,
    "## Package Changes",
    ""
  )

  if (length(pkg_diff$added) > 0) {
    report <- c(
      report,
      "### Added Packages",
      "",
      paste("-", pkg_diff$added),
      ""
    )
  }

  if (length(pkg_diff$removed) > 0) {
    report <- c(
      report,
      "### Removed Packages",
      "",
      paste("-", pkg_diff$removed),
      ""
    )
  }

  if (length(pkg_diff$changed) > 0) {
    report <- c(
      report,
      "### Version Changes",
      ""
    )
    for (detail in pkg_diff$details) {
      report <- c(
        report,
        paste("-", detail$package, ":", detail$v1, "->", detail$v2)
      )
    }
    report <- c(report, "")
  }

  if (length(pkg_diff$added) == 0 && length(pkg_diff$removed) == 0 && length(pkg_diff$changed) == 0) {
    report <- c(report, "*No package changes*", "")
  }

  # Parameter changes
  report <- c(
    report,
    "## Parameter Changes",
    ""
  )

  # Safe check for changes
  has_param_changes <- !is.null(param_diff$n_added) && !is.null(param_diff$n_removed) && !is.null(param_diff$n_modified) &&
    (param_diff$n_added > 0 || param_diff$n_removed > 0 || param_diff$n_modified > 0)

  if (isTRUE(has_param_changes)) {
    if (length(param_diff$added) > 0) {
      report <- c(report, "**Added:**", paste("-", param_diff$added), "")
    }
    if (length(param_diff$removed) > 0) {
      report <- c(report, "**Removed:**", paste("-", param_diff$removed), "")
    }
    if (length(param_diff$modified) > 0) {
      report <- c(report, "**Modified:**", paste("-", param_diff$modified), "")
    }
  } else {
    report <- c(report, "*No parameter changes*", "")
  }

  # Data file changes
  report <- c(
    report,
    "## Data File Changes",
    ""
  )

  # Safe check for changes
  has_data_changes <- !is.null(data_diff$n_added) && !is.null(data_diff$n_removed) && !is.null(data_diff$n_modified) &&
    (data_diff$n_added > 0 || data_diff$n_removed > 0 || data_diff$n_modified > 0)

  if (isTRUE(has_data_changes)) {
    if (length(data_diff$added) > 0) {
      report <- c(report, "**Added:**", paste("-", data_diff$added), "")
    }
    if (length(data_diff$removed) > 0) {
      report <- c(report, "**Removed:**", paste("-", data_diff$removed), "")
    }
    if (length(data_diff$modified) > 0) {
      report <- c(report, "**Modified:**", paste("-", data_diff$modified), "")
    }
  } else {
    report <- c(report, "*No data file changes*", "")
  }

  # Random seed changes
  report <- c(
    report,
    "## Random Seed Changes",
    ""
  )

  # Safe check for changes
  has_seed_changes <- !is.null(seed_diff$n_added) && !is.null(seed_diff$n_removed) && !is.null(seed_diff$n_modified) &&
    (seed_diff$n_added > 0 || seed_diff$n_removed > 0 || seed_diff$n_modified > 0)

  if (isTRUE(has_seed_changes)) {
    if (length(seed_diff$added) > 0) {
      report <- c(report, "**Added:**", paste("-", seed_diff$added), "")
    }
    if (length(seed_diff$removed) > 0) {
      report <- c(report, "**Removed:**", paste("-", seed_diff$removed), "")
    }
    if (length(seed_diff$modified) > 0) {
      report <- c(report, "**Modified:**", paste("-", seed_diff$modified), "")
    }
  } else {
    report <- c(report, "*No seed changes*", "")
  }

  return(report)
}


#' List Available Snapshots
#'
#' @description
#' List all available snapshots with basic metadata
#'
#' @return Data frame with snapshot information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_snapshots()
#' }
list_snapshots <- function() {
  snapshots_dir <- ".capsule/snapshots"

  if (!dir.exists(snapshots_dir)) {
    cli::cli_alert_warning("No snapshots directory found")
    return(invisible(NULL))
  }

  snapshot_dirs <- list.dirs(snapshots_dir, full.names = FALSE, recursive = FALSE)

  if (length(snapshot_dirs) == 0) {
    cli::cli_alert_info("No snapshots found")
    return(invisible(NULL))
  }

  # Gather metadata for each snapshot
  snapshot_info <- lapply(snapshot_dirs, function(snap) {
    meta_file <- file.path(snapshots_dir, snap, "snapshot_metadata.json")

    if (file.exists(meta_file)) {
      meta <- jsonlite::read_json(meta_file, simplifyVector = TRUE)
      data.frame(
        snapshot = snap,
        created = as.character(meta$created),
        analysis = meta$analysis_name,
        r_version = meta$r_version,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        snapshot = snap,
        created = NA,
        analysis = NA,
        r_version = NA,
        stringsAsFactors = FALSE
      )
    }
  })

  result <- do.call(rbind, snapshot_info)

  cli::cli_alert_success("Found {nrow(result)} snapshot(s)")
  print(result)

  invisible(result)
}
