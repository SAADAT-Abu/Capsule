#' Track Reference Genome
#'
#' @description
#' Track reference genome files, annotations, and indices for reproducibility.
#' This is critical for genomics/transcriptomics pipelines where the exact
#' reference version affects results.
#'
#' @param fasta_path Character. Path to reference genome FASTA file
#' @param gtf_path Character. Path to GTF annotation file. Optional.
#' @param gff_path Character. Path to GFF annotation file. Optional.
#' @param genome_build Character. Genome build identifier (e.g., "GRCh38", "mm10")
#' @param species Character. Species name (e.g., "Homo sapiens", "Mus musculus")
#' @param source_url Character. URL where reference was downloaded from
#' @param indices Named list. Paths to aligner indices (STAR, BWA, etc.)
#' @param metadata List. Additional metadata about the reference
#' @param registry_file Character. Path to reference registry
#'
#' @return List containing reference genome information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' track_reference_genome(
#'   fasta_path = "ref/GRCh38.fa",
#'   gtf_path = "ref/gencode.v38.annotation.gtf",
#'   genome_build = "GRCh38",
#'   species = "Homo sapiens",
#'   source_url = "https://www.gencodegenes.org/",
#'   indices = list(
#'     star = "ref/STAR_index/",
#'     bwa = "ref/bwa_index/GRCh38"
#'   )
#' )
#' }
track_reference_genome <- function(
    fasta_path,
    gtf_path = NULL,
    gff_path = NULL,
    genome_build = NULL,
    species = NULL,
    source_url = NULL,
    indices = list(),
    metadata = list(),
    registry_file = ".capsule/reference_registry.json") {
  if (!file.exists(fasta_path)) {
    cli::cli_alert_danger("FASTA file not found: {.file {fasta_path}}")
    return(invisible(NULL))
  }

  if (is.null(genome_build)) {
    genome_build <- paste0("reference_", format(Sys.time(), "%Y%m%d"))
    cli::cli_alert_warning("No genome_build specified, using: {genome_build}")
  }

  cli::cli_alert_info("Tracking reference genome: {genome_build}")

  # Track FASTA
  cli::cli_alert_info("Tracking FASTA file...")
  track_data(fasta_path,
    source = "reference",
    source_url = source_url,
    description = paste("Reference genome FASTA:", genome_build)
  )

  # Track annotation files
  annotation_files <- list()

  if (!is.null(gtf_path)) {
    if (file.exists(gtf_path)) {
      cli::cli_alert_info("Tracking GTF annotation...")
      track_data(gtf_path,
        source = "reference",
        source_url = source_url,
        description = paste("Gene annotation GTF:", genome_build)
      )
      annotation_files$gtf <- gtf_path
    } else {
      cli::cli_alert_warning("GTF file not found: {.file {gtf_path}}")
    }
  }

  if (!is.null(gff_path)) {
    if (file.exists(gff_path)) {
      cli::cli_alert_info("Tracking GFF annotation...")
      track_data(gff_path,
        source = "reference",
        source_url = source_url,
        description = paste("Gene annotation GFF:", genome_build)
      )
      annotation_files$gff <- gff_path
    } else {
      cli::cli_alert_warning("GFF file not found: {.file {gff_path}}")
    }
  }

  # Track indices
  tracked_indices <- list()

  if (length(indices) > 0) {
    cli::cli_alert_info("Tracking {length(indices)} index/indices...")

    for (idx_name in names(indices)) {
      idx_path <- indices[[idx_name]]

      # Handle both files and directories
      if (file.exists(idx_path) || dir.exists(idx_path)) {
        track_data(idx_path,
          source = "reference",
          description = paste0(idx_name, " index for ", genome_build)
        )
        tracked_indices[[idx_name]] <- idx_path
        cli::cli_alert_success("Tracked {idx_name} index")
      } else {
        cli::cli_alert_warning("{idx_name} index not found: {.file {idx_path}}")
      }
    }
  }

  # Get FASTA statistics
  fasta_stats <- .get_fasta_stats(fasta_path)

  # Create reference record
  ref_record <- list(
    genome_build = genome_build,
    species = species,
    fasta = list(
      path = normalizePath(fasta_path, mustWork = TRUE),
      stats = fasta_stats
    ),
    annotations = annotation_files,
    indices = tracked_indices,
    source_url = source_url,
    metadata = metadata,
    tracked_at = Sys.time(),
    tracked_by = Sys.info()["user"]
  )

  # Save to reference registry
  registry <- .load_reference_registry(registry_file)
  registry$references[[genome_build]] <- ref_record
  registry$last_updated <- Sys.time()
  .save_reference_registry(registry, registry_file)

  cli::cli_alert_success("Reference genome tracked: {genome_build}")
  if (!is.null(species)) {
    cli::cli_alert_info("Species: {species}")
  }
  if (!is.null(fasta_stats$n_sequences)) {
    cli::cli_alert_info("Sequences: {fasta_stats$n_sequences}")
  }

  invisible(ref_record)
}


#' Get Reference Genome Information
#'
#' @description
#' Retrieve information about tracked reference genomes
#'
#' @param genome_build Character. Specific genome build, or NULL for all
#' @param registry_file Character. Path to reference registry
#'
#' @return List of reference genome information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all tracked references
#' get_reference_info()
#'
#' # Get specific reference
#' get_reference_info("GRCh38")
#' }
get_reference_info <- function(genome_build = NULL,
                               registry_file = ".capsule/reference_registry.json") {
  registry <- .load_reference_registry(registry_file)

  if (is.null(registry$references) || length(registry$references) == 0) {
    cli::cli_alert_warning("No reference genomes tracked")
    return(NULL)
  }

  if (is.null(genome_build)) {
    return(registry$references)
  }

  if (genome_build %in% names(registry$references)) {
    return(registry$references[[genome_build]])
  } else {
    cli::cli_alert_warning("Reference not found: {genome_build}")
    return(NULL)
  }
}


#' Get FASTA Statistics
#'
#' @description
#' Internal function to extract basic statistics from a FASTA file
#'
#' @param fasta_path Character. Path to FASTA file
#'
#' @return List containing FASTA statistics
#' @keywords internal
.get_fasta_stats <- function(fasta_path) {
  stats <- tryCatch(
    {
      # Read first few lines to get a sense of the file
      lines <- readLines(fasta_path, n = 10000, warn = FALSE)

      # Count sequences (lines starting with >)
      n_sequences <- sum(grepl("^>", lines))

      # If we read all sequences in first 10000 lines, we're good
      # Otherwise, count all headers
      if (lines[length(lines)] != "" && !grepl("^>", lines[length(lines)])) {
        # Need to count all headers
        n_sequences <- as.numeric(
          system2("grep", c("-c", "'^>'", shQuote(fasta_path)),
            stdout = TRUE, stderr = FALSE
          )
        )
      }

      # Get file size
      file_size <- file.info(fasta_path)$size

      list(
        n_sequences = n_sequences,
        file_size_bytes = file_size,
        file_size_readable = .format_size(file_size)
      )
    },
    error = function(e) {
      list(
        n_sequences = NA,
        file_size_bytes = file.info(fasta_path)$size,
        file_size_readable = .format_size(file.info(fasta_path)$size),
        error = "Could not parse FASTA statistics"
      )
    }
  )

  return(stats)
}


#' List Common Reference Genome Sources
#'
#' @description
#' Display a helpful list of common reference genome sources
#'
#' @export
#'
#' @examples
#' list_reference_sources()
list_reference_sources <- function() {
  cli::cli_h1("Common Reference Genome Sources")

  cli::cli_h2("Human")
  cli::cli_ul(c(
    "GENCODE: https://www.gencodegenes.org/",
    "Ensembl: https://www.ensembl.org/",
    "UCSC: https://hgdownload.soe.ucsc.edu/goldenPath/",
    "NCBI RefSeq: https://www.ncbi.nlm.nih.gov/refseq/"
  ))

  cli::cli_h2("Mouse")
  cli::cli_ul(c(
    "GENCODE (M): https://www.gencodegenes.org/mouse/",
    "Ensembl: https://www.ensembl.org/",
    "UCSC: https://hgdownload.soe.ucsc.edu/goldenPath/"
  ))

  cli::cli_h2("Other Model Organisms")
  cli::cli_ul(c(
    "Ensembl (multi-species): https://www.ensembl.org/",
    "NCBI Genome: https://www.ncbi.nlm.nih.gov/genome/",
    "Phytozome (plants): https://phytozome-next.jgi.doe.gov/"
  ))

  cli::cli_h2("Pre-built Indices")
  cli::cli_ul(c(
    "Illumina iGenomes: https://support.illumina.com/sequencing/sequencing_software/igenome.html",
    "10x Genomics: https://support.10xgenomics.com/single-cell-gene-expression/software/downloads/latest"
  ))

  invisible(NULL)
}


#' Load Reference Registry
#'
#' @description
#' Internal function to load the reference registry
#'
#' @param registry_file Character. Path to registry file
#'
#' @return List containing registry data
#' @keywords internal
.load_reference_registry <- function(registry_file) {
  if (file.exists(registry_file)) {
    jsonlite::read_json(registry_file, simplifyVector = FALSE)
  } else {
    list(
      created = Sys.time(),
      last_updated = Sys.time(),
      references = list()
    )
  }
}


#' Save Reference Registry
#'
#' @description
#' Internal function to save the reference registry
#'
#' @param registry List. Registry data to save
#' @param registry_file Character. Path to registry file
#'
#' @return NULL
#' @keywords internal
.save_reference_registry <- function(registry, registry_file) {
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(registry, registry_file, auto_unbox = TRUE, pretty = TRUE)
}
