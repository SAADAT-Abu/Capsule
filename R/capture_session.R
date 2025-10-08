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
#' @importFrom utils sessionInfo object.size
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
capture_session <- function(output_file = NULL,
                            format = c("json", "yaml", "rds")) {
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
    dir.create(dirname(output_file),
               recursive = TRUE,
               showWarnings = FALSE)
    switch(
      format,
      json = jsonlite::write_json(
        session_info,
        output_file,
        auto_unbox = TRUE,
        pretty = TRUE
      ),
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
  if (is.null(pkg_list)) {
    return(list())
  }
  
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
capture_environment <- function(output_file = NULL,
                                include_values = FALSE,
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
        dimensions = if (is.null(dim(obj)))
          length(obj)
        else
          dim(obj)
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
    dir.create(dirname(output_file),
               recursive = TRUE,
               showWarnings = FALSE)
    jsonlite::write_json(env_info,
                         output_file,
                         auto_unbox = TRUE,
                         pretty = TRUE)
    cli::cli_alert_success("Environment state saved to {.file {output_file}}")
  }
  
  invisible(env_info)
}


#' Capture System Libraries
#'
#' @description
#' Capture version information for system libraries that R packages depend on
#' (e.g., libcurl, libxml2, BLAS/LAPACK implementations)
#'
#' @param output_file Character. Path to save library info. If NULL, returns as list.
#'
#' @return List containing system library information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' capture_system_libraries("system_libs.json")
#' }
capture_system_libraries <- function(output_file = NULL) {
  sys_libs <- list(timestamp = Sys.time(),
                   os = Sys.info()["sysname"],
                   os_release = Sys.info()["release"])
  
  # Get R's linked BLAS and LAPACK
  sys_libs$r_blas <- tryCatch(
    as.character(extSoftVersion()["BLAS"]),
    error = function(e)
      "unknown"
  )
  
  sys_libs$r_lapack <- tryCatch(
    as.character(extSoftVersion()["LAPACK"]),
    error = function(e)
      "unknown"
  )
  
  # Linux/Mac library detection
  if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
    # Check common libraries using pkg-config
    libs_to_check <- c("libcurl",
                       "libxml-2.0",
                       "openssl",
                       "libcrypto",
                       "zlib",
                       "liblzma",
                       "bzip2")
    
    for (lib in libs_to_check) {
      version <- tryCatch({
        result <- system2("pkg-config",
                          c("--modversion", lib),
                          stdout = TRUE,
                          stderr = FALSE)
        if (!is.null(attr(result, "status")) &&
            attr(result, "status") != 0) {
          "not found via pkg-config"
        } else {
          result[1]
        }
      }, error = function(e) {
        "unknown"
      })
      
      # Clean lib name for list element
      clean_name <- gsub("[.-]", "_", lib)
      sys_libs[[clean_name]] <- version
    }
    
    # Try ldconfig for additional detection on Linux
    if (Sys.info()["sysname"] == "Linux") {
      ldconfig_libs <- c("libopenblas", "libmkl", "libatlas")
      
      for (lib in ldconfig_libs) {
        detected <- tryCatch({
          result <- system2("ldconfig",
                            c("-p"),
                            stdout = TRUE,
                            stderr = FALSE)
          matches <- grep(lib, result, value = TRUE)
          if (length(matches) > 0)
            "installed"
          else
            "not found"
        }, error = function(e)
          "unknown")
        
        clean_name <- gsub("[.-]", "_", lib)
        sys_libs[[clean_name]] <- detected
      }
    }
  }
  
  # Windows detection
  if (Sys.info()["sysname"] == "Windows") {
    sys_libs$note <- "Windows system library detection limited"
  }
  
  # Compiler information
  sys_libs$compiler <- tryCatch({
    R.version$cxx
  }, error = function(e)
    "unknown")
  
  if (!is.null(output_file)) {
    dir.create(dirname(output_file),
               recursive = TRUE,
               showWarnings = FALSE)
    jsonlite::write_json(sys_libs,
                         output_file,
                         auto_unbox = TRUE,
                         pretty = TRUE)
    cli::cli_alert_success("System libraries saved: {.file {output_file}}")
  }
  
  invisible(sys_libs)
}


#' Capture Hardware Information
#'
#' @description
#' Capture hardware specifications including CPU, RAM, and GPU information.
#' Useful for documenting computational resources used in analysis.
#'
#' @param output_file Character. Path to save hardware info. If NULL, returns as list.
#'
#' @return List containing hardware information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' capture_hardware("hardware_info.json")
#' }
capture_hardware <- function(output_file = NULL) {
  hw_info <- list(timestamp = Sys.time(),
                  hostname = Sys.info()["nodename"],
                  os = Sys.info()["sysname"])
  
  # CPU info - Linux
  if (Sys.info()["sysname"] == "Linux") {
    cpu_info <- tryCatch({
      if (file.exists("/proc/cpuinfo")) {
        cpuinfo <- readLines("/proc/cpuinfo")
        
        # Model name
        model_line <- grep("model name", cpuinfo, value = TRUE)
        model <- if (length(model_line) > 0) {
          gsub(".*:\\s+", "", model_line[1])
        } else {
          "unknown"
        }
        
        # Core count
        cores <- tryCatch({
          as.numeric(system("nproc", intern = TRUE))
        }, error = function(e) {
          length(grep("processor", cpuinfo))
        })
        
        list(model = model,
             cores = cores,
             threads = length(grep("processor", cpuinfo)))
      } else {
        list(error = "Could not read /proc/cpuinfo")
      }
    }, error = function(e) {
      list(error = paste("CPU detection failed:", e$message))
    })
    
    hw_info$cpu <- cpu_info
    
    # Memory info
    mem_info <- tryCatch({
      if (file.exists("/proc/meminfo")) {
        meminfo <- readLines("/proc/meminfo")
        total_line <- grep("MemTotal", meminfo, value = TRUE)
        total_kb <- as.numeric(gsub(".*:\\s+(\\d+).*", "\\1", total_line))
        
        list(total_kb = total_kb,
             total_gb = round(total_kb / (1024^2), 2))
      } else {
        list(error = "Could not read /proc/meminfo")
      }
    }, error = function(e) {
      list(error = paste("Memory detection failed:", e$message))
    })
    
    hw_info$memory <- mem_info
  }
  
  # macOS
  if (Sys.info()["sysname"] == "Darwin") {
    cpu_info <- tryCatch({
      list(
        model = system("sysctl -n machdep.cpu.brand_string", intern = TRUE),
        cores = as.numeric(system("sysctl -n hw.physicalcpu", intern = TRUE)),
        threads = as.numeric(system("sysctl -n hw.logicalcpu", intern = TRUE))
      )
    }, error = function(e) {
      list(error = paste("CPU detection failed:", e$message))
    })
    
    hw_info$cpu <- cpu_info
    
    mem_info <- tryCatch({
      mem_bytes <- as.numeric(system("sysctl -n hw.memsize", intern = TRUE))
      list(total_bytes = mem_bytes,
           total_gb = round(mem_bytes / (1024^3), 2))
    }, error = function(e) {
      list(error = paste("Memory detection failed:", e$message))
    })
    
    hw_info$memory <- mem_info
  }
  
  # Windows
  if (Sys.info()["sysname"] == "Windows") {
    hw_info$cpu <- list(note = "Windows CPU detection via wmic not implemented")
    hw_info$memory <- list(note = "Windows memory detection via wmic not implemented")
  }
  
  # GPU detection (NVIDIA)
  gpu_info <- tryCatch({
    nvidia_smi <- system2(
      "nvidia-smi",
      c(
        "--query-gpu=name,driver_version,memory.total",
        "--format=csv,noheader"
      ),
      stdout = TRUE,
      stderr = FALSE
    )
    
    status <- attr(nvidia_smi, "status")
    if (!is.null(status) && status != 0) {
      return(list(nvidia = "not detected"))
    }
    
    if (length(nvidia_smi) > 0) {
      # Parse CSV output
      gpus <- lapply(nvidia_smi, function(line) {
        parts <- strsplit(line, ",")[[1]]
        if (length(parts) >= 3) {
          list(
            name = trimws(parts[1]),
            driver_version = trimws(parts[2]),
            memory_total = trimws(parts[3])
          )
        } else {
          line
        }
      })
      
      list(nvidia = gpus, count = length(nvidia_smi))
    } else {
      list(nvidia = "not detected")
    }
  }, error = function(e) {
    list(nvidia = "not detected or nvidia-smi not available")
  })
  
  hw_info$gpu <- gpu_info
  
  # Disk info
  hw_info$disk_space <- tryCatch({
    df_output <- system2("df", c("-h", "."), stdout = TRUE, stderr = FALSE)
    if (length(df_output) > 1) {
      # Parse df output
      header <- df_output[1]
      data <- df_output[2]
      list(df_current_dir = data)
    } else {
      "unknown"
    }
  }, error = function(e)
    "unknown")
  
  if (!is.null(output_file)) {
    dir.create(dirname(output_file),
               recursive = TRUE,
               showWarnings = FALSE)
    jsonlite::write_json(hw_info,
                         output_file,
                         auto_unbox = TRUE,
                         pretty = TRUE)
    cli::cli_alert_success("Hardware info saved: {.file {output_file}}")
  }
  
  invisible(hw_info)
}
