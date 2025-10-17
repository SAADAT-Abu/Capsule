# Capsule 0.2.0

## Major Update: Bioinformatics Focus

This release adds comprehensive support for bioinformatics and computational biology workflows, with enhancements specifically designed for NGS analysis, HPC environments, and large-scale data processing.

### New Features - Critical for Bioinformatics

* **External Tool Version Tracking**
  - `track_external_tools()`: Track versions of command-line tools (samtools, STAR, BWA, etc.)
  - `get_tool_versions()`: Retrieve tracked tool versions
  - Automatically detects and tracks 18+ common bioinformatics tools

* **Conda/Mamba Environment Support**
  - `track_conda_env()`: Export and track conda environments
  - `restore_conda_env()`: Restore conda environments from YAML
  - `get_conda_env_info()`: Retrieve conda environment information
  - Full support for both conda and mamba

* **Reference Genome Tracking**
  - `track_reference_genome()`: Track reference genomes, annotations, and indices
  - `get_reference_info()`: Retrieve reference genome information
  - `list_reference_sources()`: Display common reference genome sources
  - Tracks FASTA files, GTF/GFF annotations, and aligner indices (STAR, BWA, etc.)

### New Features - High Priority

* **Large File Handling**
  - Enhanced `track_data()` with smart checksumming for large files (>1GB)
  - xxHash64 support for 10-100x faster checksumming of BAM/FASTQ files
  - Metadata-based fingerprinting for very large files
  - Automatic algorithm selection based on file size

* **System Library Detection**
  - `capture_system_libraries()`: Detect system library versions
  - Tracks libcurl, libxml2, BLAS/LAPACK implementations
  - Essential for documenting system dependencies

* **Hardware Information Capture**
  - `capture_hardware()`: Capture CPU, RAM, and GPU specifications
  - NVIDIA GPU detection via nvidia-smi
  - Cross-platform support (Linux, macOS, Windows)
  - Essential for HPC job documentation

### New Features - Containerization & HPC

* **Singularity/Apptainer Support**
  - `generate_singularity()`: Generate Singularity definition files
  - Full support for HPC environments where Docker is unavailable
  - Automatic build script generation
  - Conda environment integration

### New Features - Pipeline Integration

* **Workflow Manager Integration**
  - `export_for_nextflow()`: Export data for Nextflow pipelines
  - `export_for_snakemake()`: Export data for Snakemake workflows
  - `export_for_wdl()`: Export data for WDL workflows
  - `export_for_cwl()`: Export data for CWL workflows
  - Seamless integration with major workflow managers

### New Features - Snapshot Management

* **Snapshot Comparison**
  - `compare_snapshots()`: Compare two workflow snapshots
  - `list_snapshots()`: List all available snapshots with metadata
  - Detailed diff reports showing package, parameter, and data changes
  - Markdown report generation

### Enhancements

* Updated DESCRIPTION with bioinformatics focus
* Enhanced documentation with bioinformatics examples
* Improved error handling and user feedback
* Better cross-platform compatibility
* Added utils to imports for better compatibility

### Bug Fixes

* Fixed issue with checksum verification for legacy tracked files
* Improved handling of missing files in verification
* Better error messages for conda/mamba detection

---

# Capsule 0.1.0

## Initial Release

### Features

* **Session Tracking**: Comprehensive R session information capture
  - `capture_session()`: Capture R version, platform, and system info
  - `capture_environment()`: Capture global environment state

* **Package Management**: Complete package version tracking
  - `snapshot_packages()`: Create detailed package manifests
  - `create_renv_lockfile()`: Generate renv lockfiles
  - Automatic dependency graph creation

* **Data Provenance**: Track data files with integrity verification
  - `track_data()`: Record data source, checksums, and metadata
  - `verify_data()`: Verify data integrity via SHA-256 checksums
  - `get_data_lineage()`: Retrieve complete data provenance

* **Parameter Tracking**: Document analysis parameters
  - `track_params()`: Store analysis parameters with metadata
  - `get_param_history()`: Retrieve parameter history

* **Random Seed Management**: Reproducible random number generation
  - `set_seed()`: Set and track random seeds
  - `restore_seed()`: Restore previously tracked seeds
  - Complete RNG state tracking

* **Script Generation**: Create reproducible analysis scripts
  - `generate_repro_script()`: Generate executable R scripts
  - `create_repro_report()`: Generate markdown reports
  - Automatic integration of all tracked components

* **Docker Support**: Containerization for perfect reproducibility
  - `generate_docker()`: Generate Dockerfile and docker-compose.yml
  - RStudio Server support
  - Automatic system dependency configuration

* **Workflow Management**: Complete workflow orchestration
  - `init_capsule()`: Initialize Capsule in projects
  - `snapshot_workflow()`: Create complete workflow snapshots
  - Automatic artifact generation

### Documentation

* Comprehensive README with quick start guide
* Complete function documentation with examples
* Example workflow demonstrating all features
* Docker usage instructions

### Infrastructure

* MIT License
* Complete test suite
* Package structure following R best practices
