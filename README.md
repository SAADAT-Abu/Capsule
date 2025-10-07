# ReproFlow

> A comprehensive reproducibility framework for R that automatically captures your entire analysis environment

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

## Overview

ReproFlow addresses a critical bottleneck in research: **researchers lack the time to properly document their workflows**. This package automatically captures everything needed to reproduce your analysis, including:

- ✅ R session information and platform details
- ✅ Package versions and dependencies
- ✅ Data provenance and checksums
- ✅ Analysis parameters
- ✅ Random seeds and RNG states
- ✅ Executable reproduction scripts
- ✅ Docker configurations
- ✅ renv lockfiles

## Installation

```r
# Install from source
devtools::install_github("yourusername/ReproFlow")

# Or install locally
install.packages("path/to/ReproFlow", repos = NULL, type = "source")
```

## Quick Start

### 1. Initialize ReproFlow in your project

```r
library(ReproFlow)

# Initialize in current directory
init_reproflow()
```

This creates:
- `.reproflow/` directory for tracking files
- `renv.lock` for package management
- `.gitignore` with sensible defaults
- Example workflow script

### 2. Track your analysis components

```r
# Set and track random seed
set_seed(12345, analysis_name = "simulation_study")

# Track analysis parameters
params <- list(
  n_iterations = 1000,
  alpha = 0.05,
  method = "bootstrap"
)
track_params(params, "simulation_study", "Monte Carlo simulation parameters")

# Track data files
track_data(
  "data/experiment_data.csv",
  source = "downloaded",
  source_url = "https://example.com/data.csv",
  description = "Experimental measurements from Lab A"
)
```

### 3. Run your analysis

```r
# Your analysis code with tracked components
set_seed(12345, analysis_name = "simulation_study")

# Load parameters
params <- list(n_iterations = 1000, alpha = 0.05)

# Run analysis
results <- run_simulation(params)
```

### 4. Create a complete snapshot

```r
# Generate all reproducibility artifacts
snapshot_workflow(
  snapshot_name = "final_analysis_v1",
  analysis_name = "simulation_study",
  source_script = "analysis.R",
  description = "Final analysis with corrected parameters"
)
```

This generates:
- Session and environment snapshots
- Package manifests
- Reproducible R script
- Docker configuration
- Comprehensive reproducibility report

## Core Functions

### Initialization

- `init_reproflow()` - Initialize ReproFlow in a project

### Tracking Functions

- `set_seed()` - Set and track random seeds
- `track_params()` - Track analysis parameters
- `track_data()` - Track data files with checksums
- `capture_session()` - Capture R session information
- `capture_environment()` - Capture environment state

### Package Management

- `snapshot_packages()` - Create package version manifest
- `create_renv_lockfile()` - Generate renv lockfile

### Data Verification

- `verify_data()` - Verify data integrity via checksums
- `get_data_lineage()` - Retrieve data provenance information

### Code Generation

- `generate_repro_script()` - Generate executable reproduction script
- `generate_docker()` - Generate Docker configuration
- `create_repro_report()` - Create reproducibility report

### Workflow

- `snapshot_workflow()` - Create complete workflow snapshot

## Example Workflow

```r
library(ReproFlow)

# 1. Initialize project
init_reproflow()

# 2. Set up reproducibility components
set_seed(42, analysis_name = "main_analysis")

params <- list(
  model = "linear",
  cross_validation_folds = 10,
  feature_selection = TRUE
)
track_params(params, "main_analysis", "Model training parameters")

# 3. Track input data
track_data(
  "data/training_data.csv",
  source = "generated",
  description = "Training dataset with 1000 samples"
)

# 4. Run your analysis
model <- train_model(params)
results <- evaluate_model(model)

# 5. Create complete snapshot
snapshot_workflow(
  snapshot_name = "publication_version",
  analysis_name = "main_analysis",
  source_script = "train_model.R",
  description = "Analysis for publication submission"
)
```

## Docker Workflow

ReproFlow can generate complete Docker configurations:

```r
# Generate Docker files
generate_docker(
  output_dir = ".",
  project_name = "my_analysis",
  include_rstudio = TRUE,
  system_deps = c("libcurl4-openssl-dev", "libxml2-dev")
)
```

Then use Docker to ensure perfect reproducibility:

```bash
# Build container
docker-compose build

# Run RStudio Server
docker-compose up

# Or run script directly
docker-compose run --rm my_analysis Rscript analysis.R
```

## Output Structure

After running `snapshot_workflow()`, you'll have:

```
.reproflow/
├── snapshots/
│   └── final_analysis_v1/
│       ├── session_info.json          # R session details
│       ├── environment.json           # Environment state
│       ├── packages.json              # Package manifest
│       ├── renv.lock                  # renv lockfile
│       ├── data_registry.json         # Data provenance
│       ├── param_registry.json        # Parameters
│       ├── seed_registry.json         # Random seeds
│       ├── analysis_reproducible.R    # Executable script
│       ├── reproducibility_report.md  # Human-readable report
│       ├── snapshot_metadata.json     # Snapshot metadata
│       └── docker/                    # Docker configuration
│           ├── Dockerfile
│           ├── docker-compose.yml
│           └── DOCKER_README.md
```

## Best Practices

1. **Initialize early**: Run `init_reproflow()` at the start of your project
2. **Track incrementally**: Track data and parameters as you use them
3. **Set seeds explicitly**: Always use `set_seed()` for stochastic analyses
4. **Snapshot regularly**: Create snapshots at key milestones
5. **Verify data**: Regularly run `verify_data()` to ensure data integrity
6. **Use version control**: Commit your `.reproflow/` registries to git

## Why ReproFlow?

### The Problem

Reproducibility is critical in research, but researchers face:
- ⏰ Limited time to document workflows
- 📦 Difficulty tracking package versions
- 🔢 Lost parameter configurations
- 📊 Unknown data provenance
- 🎲 Forgotten random seeds
- 🐳 Complex containerization setup

### The Solution

ReproFlow **automatically** handles all of this:
- Zero-overhead tracking as you work
- Comprehensive environment capture
- One-function workflow snapshots
- Ready-to-use Docker configurations
- Human-readable reports

## Requirements

- R >= 4.0.0
- Suggested packages: `renv`, `jsonlite`, `digest`, `yaml`, `cli`, `rlang`

## License

MIT License - see LICENSE file for details

## Contributing

Contributions welcome! Please feel free to submit issues and pull requests.

## Citation

If you use ReproFlow in your research, please cite:

```
@software{reproflow,
  title = {ReproFlow: Comprehensive Reproducibility Framework for R},
  author = {Your Name},
  year = {2025},
  url = {https://github.com/yourusername/ReproFlow}
}
```

## Support

- 📖 Documentation: [package documentation]
- 🐛 Bug reports: [GitHub Issues]
- 💬 Questions: [GitHub Discussions]
