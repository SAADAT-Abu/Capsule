# ReproFlow Complete Workflow Example
# =====================================
# This example demonstrates a complete analysis workflow using ReproFlow

library(ReproFlow)

# Step 1: Initialize ReproFlow
# -----------------------------
cat("Step 1: Initializing ReproFlow...\n")
init_reproflow()

# Step 2: Set Random Seed
# ------------------------
cat("\nStep 2: Setting random seed...\n")
set_seed(
  seed = 42,
  analysis_name = "example_simulation",
  registry_file = ".reproflow/seed_registry.json"
)

# Step 3: Define and Track Parameters
# ------------------------------------
cat("\nStep 3: Tracking analysis parameters...\n")
analysis_params <- list(
  n_samples = 1000,
  n_features = 50,
  noise_level = 0.1,
  test_split = 0.2,
  model_type = "linear_regression",
  regularization = "ridge",
  alpha = 0.05
)

track_params(
  params = analysis_params,
  analysis_name = "example_simulation",
  description = "Parameters for synthetic data generation and model training"
)

# Step 4: Generate Synthetic Data
# --------------------------------
cat("\nStep 4: Generating synthetic data...\n")

# Create synthetic dataset
n <- analysis_params$n_samples
p <- analysis_params$n_features

X <- matrix(rnorm(n * p), nrow = n, ncol = p)
true_beta <- rnorm(p)
y <- X %*% true_beta + rnorm(n, sd = analysis_params$noise_level)

# Create data directory and save
dir.create("data", showWarnings = FALSE)
synthetic_data <- data.frame(y = y, X)
write.csv(synthetic_data, "data/synthetic_data.csv", row.names = FALSE)

# Step 5: Track Data File
# ------------------------
cat("\nStep 5: Tracking data file...\n")
track_data(
  data_path = "data/synthetic_data.csv",
  source = "generated",
  description = "Synthetic dataset for regression analysis",
  metadata = list(
    n_samples = n,
    n_features = p,
    noise_sd = analysis_params$noise_level
  )
)

# Step 6: Train Model
# --------------------
cat("\nStep 6: Training model...\n")

# Split data
set.seed(42)
train_idx <- sample(1:n, size = floor(n * (1 - analysis_params$test_split)))
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

# Train simple linear model
model <- lm(y_train ~ X_train)

# Evaluate
predictions <- cbind(1, X_test) %*% coef(model)
mse <- mean((y_test - predictions)^2)
r_squared <- cor(y_test, predictions)^2

cat(sprintf("  Model MSE: %.4f\n", mse))
cat(sprintf("  Model R²: %.4f\n", r_squared))

# Step 7: Save Results
# --------------------
cat("\nStep 7: Saving results...\n")
dir.create("results", showWarnings = FALSE)

results <- list(
  model_coefficients = coef(model),
  mse = mse,
  r_squared = r_squared,
  n_train = length(train_idx),
  n_test = n - length(train_idx)
)

saveRDS(results, "results/model_results.rds")

# Track results file
track_data(
  data_path = "results/model_results.rds",
  source = "generated",
  description = "Model training results and performance metrics"
)

# Step 8: Verify Data Integrity
# ------------------------------
cat("\nStep 8: Verifying data integrity...\n")
verify_data()

# Step 9: Capture Session Information
# ------------------------------------
cat("\nStep 9: Capturing session information...\n")
capture_session(
  output_file = "session_info.json",
  format = "json"
)

# Step 10: Create Complete Workflow Snapshot
# -------------------------------------------
cat("\nStep 10: Creating workflow snapshot...\n")
snapshot_files <- snapshot_workflow(
  snapshot_name = "example_complete",
  analysis_name = "example_simulation",
  source_script = "inst/examples/complete_workflow.R",
  description = "Complete example workflow with synthetic data and linear regression",
  generate_docker = TRUE,
  generate_script = TRUE,
  generate_report = TRUE
)

# Summary
# -------
cat("\n" , rep("=", 60), "\n", sep = "")
cat("ReproFlow Workflow Complete!\n")
cat(rep("=", 60), "\n", sep = "")
cat("\nGenerated files:\n")
for (name in names(snapshot_files)) {
  if (name != "docker") {
    cat(sprintf("  - %s: %s\n", name, snapshot_files[[name]]))
  }
}

cat("\nTo reproduce this analysis:\n")
cat("  1. Navigate to the snapshot directory\n")
cat("  2. Run the reproducible script:\n")
cat(sprintf("     Rscript %s\n", snapshot_files$script))
cat("  3. Or use Docker:\n")
cat("     docker-compose -f ", snapshot_files$docker$compose, " up\n")

cat("\nReproducibility report available at:\n")
cat(sprintf("  %s\n", snapshot_files$report))
