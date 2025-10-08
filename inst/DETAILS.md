# Capsule

## Overview

Capsule is a comprehensive reproducibility framework specifically designed for **bioinformatics and computational biology workflows**. It automatically captures your entire analysis environment and generates everything needed to reproduce your research—from Docker containers to pipeline configurations.


## Installation

```r
# Install from GitHub
devtools::install_github("SAADAT-Abu/Capsule")

# Or install locally
install.packages("path/to/Capsule", repos = NULL, type = "source")
```

### System Requirements

- **R** ≥ 4.0.0
- **Optional but recommended:**
  - conda/mamba (for environment tracking)
  - Singularity/Apptainer (for HPC containers)
  - Workflow manager: Nextflow, Snakemake, WDL, or CWL

---

## Quick Start

### Basic Workflow

```r
library(Capsule)

# 1. Initialize Capsule
init_capsule()

# 2. Track external tools (bioinformatics pipelines)
track_external_tools(c("samtools", "bwa", "STAR"))

# 3. Track conda environment
track_conda_env()

# 4. Track reference genome
track_reference_genome(
  fasta_path = "ref/GRCh38.fa",
  gtf_path = "ref/gencode.v38.annotation.gtf",
  genome_build = "GRCh38",
  species = "Homo sapiens",
  indices = list(
    star = "ref/STAR_index/",
    bwa = "ref/bwa_index/"
  )
)

# 5. Set random seed
set_seed(12345, analysis_name = "rna_seq_analysis")

# 6. Track analysis parameters
params <- list(
  threads = 8,
  min_mapping_quality = 30,
  p_value_threshold = 0.05
)
track_params(params, "rna_seq_analysis", "RNA-seq pipeline parameters")

# 7. Track data files (fast checksumming for large files)
track_data("data/sample1.fastq.gz", source = "downloaded",
           source_url = "https://example.com/data/sample1.fastq.gz")

# 8. Run your analysis...
# [Your bioinformatics pipeline code here]

# 9. Create complete snapshot
snapshot_workflow(
  snapshot_name = "rna_seq_v1",
  analysis_name = "rna_seq_analysis",
  source_script = "analysis.R",
  description = "RNA-seq differential expression analysis"
)

# 10. Export for workflow managers
export_for_nextflow("nextflow_manifest.json")
export_for_snakemake("snakemake_config.yaml")
```

---

## Complete Bioinformatics Pipeline Example

Here's a complete example with simulated RNA-seq data:

```r
library(Capsule)

# =====================================
# RNA-Seq Analysis with Capsule
# Author: Abu Saadat
# =====================================

# Initialize project
init_capsule()

# Track computational environment
capture_hardware("hardware_info.json")
capture_system_libraries("system_libs.json")

# Track external bioinformatics tools
track_external_tools(c(
  "samtools", "bwa", "STAR", "featureCounts",
  "fastqc", "multiqc", "cutadapt"
))

# Track conda environment (if using conda)
track_conda_env()

# Set random seed for reproducibility
set_seed(42, analysis_name = "rna_seq_de")

# Define and track analysis parameters
analysis_params <- list(
  # Alignment parameters
  star_threads = 8,
  star_genome_dir = "ref/STAR_index",
  star_out_filter_mismatch_n_max = 10,

  # Quantification parameters
  feature_counts_threads = 8,
  min_mapping_quality = 30,

  # Differential expression parameters
  min_count = 10,
  p_value_threshold = 0.05,
  log2fc_threshold = 1.0,

  # Sample groups
  treatment_samples = c("treated_1", "treated_2", "treated_3"),
  control_samples = c("control_1", "control_2", "control_3")
)

track_params(
  analysis_params,
  "rna_seq_de",
  "RNA-seq differential expression analysis parameters"
)

# Track reference genome
track_reference_genome(
  fasta_path = "ref/GRCh38.primary_assembly.genome.fa",
  gtf_path = "ref/gencode.v38.primary_assembly.annotation.gtf",
  genome_build = "GRCh38_gencode_v38",
  species = "Homo sapiens",
  source_url = "https://www.gencodegenes.org/human/release_38.html",
  indices = list(
    star = "ref/STAR_index_GRCh38",
    bwa = "ref/bwa_index_GRCh38"
  )
)

# Simulate RNA-seq data for demonstration
print("\n=== Simulating RNA-seq Data ===\n")

# Create directories
dir.create("data", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# Simulate count matrix
set.seed(42)
n_genes <- 1000
n_samples <- 6

# Gene names
genes <- paste0("GENE_", sprintf("%04d", 1:n_genes))

# Simulate counts with some differentially expressed genes
base_counts <- matrix(
  rnbinom(n_genes * n_samples, mu = 100, size = 10),
  nrow = n_genes,
  ncol = n_samples
)

# Add differential expression for first 100 genes
de_genes_idx <- 1:100
fold_change <- 4
base_counts[de_genes_idx, 4:6] <- base_counts[de_genes_idx, 4:6] * fold_change

# Create count matrix
count_matrix <- data.frame(
  gene_id = genes,
  base_counts
)
colnames(count_matrix) <- c(
  "gene_id",
  paste0("control_", 1:3),
  paste0("treated_", 1:3)
)

# Save count matrix
count_file <- "data/gene_counts.csv"
write.csv(count_matrix, count_file, row.names = FALSE)

# Track the count matrix
track_data(
  count_file,
  source = "generated",
  description = "Simulated RNA-seq gene count matrix",
  metadata = list(
    n_genes = n_genes,
    n_samples = n_samples,
    n_de_genes = length(de_genes_idx),
    simulation_seed = 42
  )
)

print("Simulated count matrix with", n_genes, "genes and", n_samples, "samples\n")
print("Including", length(de_genes_idx), "differentially expressed genes\n\n")

# Perform differential expression analysis (simplified)
print("=== Differential Expression Analysis ===\n")

# Calculate means
control_mean <- rowMeans(count_matrix[, 2:4])
treated_mean <- rowMeans(count_matrix[, 5:7])

# Calculate log2 fold change
log2fc <- log2((treated_mean + 1) / (control_mean + 1))

# Simple t-test (for demonstration)
p_values <- apply(count_matrix[, 2:7], 1, function(x) {
  t.test(x[1:3], x[4:6])$p.value
})

# Create results table
de_results <- data.frame(
  gene_id = count_matrix$gene_id,
  control_mean = control_mean,
  treated_mean = treated_mean,
  log2_fold_change = log2fc,
  p_value = p_values,
  significant = p_values < analysis_params$p_value_threshold &
                abs(log2fc) > analysis_params$log2fc_threshold
)

# Save results
results_file <- "results/differential_expression_results.csv"
write.csv(de_results, results_file, row.names = FALSE)

# Track results
track_data(
  results_file,
  source = "generated",
  description = "Differential expression analysis results"
)

# Summary statistics
n_significant <- sum(de_results$significant)
n_upregulated <- sum(de_results$significant & de_results$log2_fold_change > 0)
n_downregulated <- sum(de_results$significant & de_results$log2_fold_change < 0)

print("\nResults Summary:\n")
print("  Total genes analyzed:", n_genes, "\n")
print("  Significant genes:", n_significant, "\n")
print("  Upregulated:", n_upregulated, "\n")
print("  Downregulated:", n_downregulated, "\n\n")

# Create visualization (simple volcano plot data)
volcano_data <- data.frame(
  log2fc = de_results$log2_fold_change,
  neg_log10_p = -log10(de_results$p_value),
  significant = de_results$significant
)

plot_file <- "results/volcano_plot.pdf"
pdf(plot_file, width = 8, height = 6)
plot(
  volcano_data$log2fc,
  volcano_data$neg_log10_p,
  col = ifelse(volcano_data$significant, "red", "grey"),
  pch = 20,
  xlab = "Log2 Fold Change",
  ylab = "-Log10 P-value",
  main = "Volcano Plot: Differential Expression"
)
abline(h = -log10(analysis_params$p_value_threshold), lty = 2)
abline(v = c(-analysis_params$log2fc_threshold, analysis_params$log2fc_threshold), lty = 2)
dev.off()

print("Volcano plot saved to:", plot_file, "\n\n")

# Create comprehensive snapshot
print("=== Creating Workflow Snapshot ===\n")
snapshot_files <- snapshot_workflow(
  snapshot_name = "rna_seq_analysis_v1",
  analysis_name = "rna_seq_de",
  source_script = "rna_seq_pipeline.R",
  description = "RNA-seq differential expression analysis with simulated data",
  generate_docker = TRUE,
  generate_script = TRUE,
  generate_report = TRUE
)

print("\n=== Exporting for Workflow Managers ===\n")

# Export for Nextflow
export_for_nextflow("nextflow_manifest.json")

# Export for Snakemake
export_for_snakemake("snakemake_config.yaml")

# Export for WDL
export_for_wdl("wdl_inputs.json")

# Generate Singularity container (for HPC)
print("\n=== Generating HPC Container ===\n")
generate_singularity(
  output_dir = "singularity",
  project_name = "rna_seq_analysis",
  system_deps = c("samtools", "bwa")
)

print("\n" , rep("=", 60), "\n", sep = "")
print("RNA-seq Analysis Complete!\n")
print(rep("=", 60), "\n", sep = "")
print("\nAll reproducibility artifacts generated in:\n")
print("  .capsule/snapshots/rna_seq_analysis_v1/\n\n")

print("To reproduce this analysis:\n")
print("  1. Use Docker:\n")
print("     cd .capsule/snapshots/rna_seq_analysis_v1/docker\n")
print("     docker-compose up\n\n")
print("  2. Use Singularity (HPC):\n")
print("     cd singularity\n")
print("     sudo bash build_singularity.sh\n\n")
print("  3. Use Nextflow:\n")
print("     nextflow run pipeline.nf -params-file nextflow_manifest.json\n\n")

# Verify all tracked data
print("=== Data Integrity Check ===\n")
verify_data()

print("\n✓ Analysis complete with full reproducibility!\n\n")
```

---

## Core Features

### 🧬 Bioinformatics-Specific Features

#### 1. External Tool Tracking

Track versions of command-line tools used in your pipeline:

```r
track_external_tools(c(
  "samtools", "bcftools", "bedtools",
  "bwa", "bowtie2", "STAR", "hisat2",
  "salmon", "kallisto", "fastqc", "multiqc"
))

# Get tracked tool versions
versions <- get_tool_versions()
```

#### 2. Conda Environment Management

```r
# Export current environment
track_conda_env()

# Restore environment elsewhere
restore_conda_env("conda_environment.yml")

# Use mamba for faster installation
track_conda_env(use_mamba = TRUE)
```

#### 3. Reference Genome Tracking

```r
track_reference_genome(
  fasta_path = "ref/GRCh38.fa",
  gtf_path = "ref/gencode.v38.annotation.gtf",
  genome_build = "GRCh38",
  species = "Homo sapiens",
  source_url = "https://www.gencodegenes.org/",
  indices = list(
    star = "ref/STAR_index/",
    bwa = "ref/bwa_index/",
    salmon = "ref/salmon_index/"
  )
)

# List common reference sources
list_reference_sources()
```

#### 4. Large File Handling

Fast checksumming for BAM/FASTQ/VCF files:

```r
# Automatically uses xxHash64 for files >1GB (10-100x faster)
track_data(
  "data/sample1.bam",
  source = "generated",
  fast_hash = TRUE
)
```

### 🔧 Core Features

#### Session & Environment Tracking

```r
capture_session("session_info.json")
capture_environment("environment.json")
capture_system_libraries("system_libs.json")
capture_hardware("hardware_info.json")  # CPU, RAM, GPU
```

#### Parameter & Seed Tracking

```r
# Track parameters
params <- list(alpha = 0.05, n_iter = 1000)
track_params(params, "analysis_1")

# Track random seeds
set_seed(12345, analysis_name = "simulation")
restore_seed("simulation")  # Restore later
```

#### Package Management

```r
snapshot_packages("packages.json")
create_renv_lockfile("renv.lock")
```

### 🐳 Containerization

#### Docker

```r
generate_docker(
  output_dir = ".",
  project_name = "my_analysis",
  include_rstudio = TRUE
)
```

#### Singularity (HPC)

```r
generate_singularity(
  output_dir = ".",
  project_name = "my_analysis",
  system_deps = c("samtools", "bwa", "STAR")
)
```

### 🔄 Workflow Manager Integration

```r
# Nextflow
export_for_nextflow("manifest.json")

# Snakemake
export_for_snakemake("config.yaml")

# WDL (Cromwell)
export_for_wdl("inputs.json")

# CWL
export_for_cwl("inputs.yml")
```

### 📊 Snapshot Management

```r
# Create snapshot
snapshot_workflow(
  snapshot_name = "analysis_v1",
  analysis_name = "main_analysis"
)

# List snapshots
list_snapshots()

# Compare snapshots
compare_snapshots("analysis_v1", "analysis_v2", "comparison.md")
```

---

## Output Structure

After running `snapshot_workflow()`, you'll have:

```
.capsule/
├── snapshots/
│   └── analysis_v1/
│       ├── session_info.json          # R session details
│       ├── environment.json           # Environment state
│       ├── packages.json              # Package manifest
│       ├── renv.lock                  # renv lockfile
│       ├── data_registry.json         # Data provenance
│       ├── param_registry.json        # Parameters
│       ├── seed_registry.json         # Random seeds
│       ├── tools_registry.json        # External tools ← NEW
│       ├── conda_registry.json        # Conda environments ← NEW
│       ├── reference_registry.json    # Reference genomes ← NEW
│       ├── system_libs.json           # System libraries ← NEW
│       ├── hardware.json              # Hardware specs ← NEW
│       ├── analysis_reproducible.R    # Executable script
│       ├── reproducibility_report.md  # Human-readable report
│       ├── snapshot_metadata.json     # Snapshot metadata
│       └── docker/                    # Docker configuration
│           ├── Dockerfile
│           ├── docker-compose.yml
│           └── DOCKER_README.md
├── tools_registry.json
├── conda_registry.json
└── reference_registry.json
```

---

## Use Cases

### 1. NGS Pipeline Reproducibility

```r
# Track all components of an NGS pipeline
track_external_tools(c("bwa", "samtools", "gatk", "picard"))
track_conda_env()
track_reference_genome("GRCh38.fa", genome_build = "GRCh38")
track_data("sample.fastq.gz", source = "downloaded")
```

### 2. HPC Job Documentation

```r
# Capture hardware used for benchmarking
hardware <- capture_hardware()
# Generate Singularity container
generate_singularity(project_name = "hpc_analysis")
```

### 3. Multi-Pipeline Integration

```r
# Export to all major workflow managers
export_for_nextflow()
export_for_snakemake()
export_for_wdl()
export_for_cwl()
```

### 4. Collaborative Research

```r
# Create snapshot and share
snapshot_workflow(snapshot_name = "pub_version")
# Collaborators can compare their results
compare_snapshots("pub_version", "validation_run")
```
