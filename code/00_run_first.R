# ============================================================
# 00_run_first.R
# ============================================================
# Recommended execution entry for peer review.
#
# This script orchestrates the full analysis pipeline in a
# deterministic and reproducible manner. All required intermediate
# datasets are precomputed and provided under `data/` to ensure
# efficient runtime during peer evaluation.
#
# The script:
#   - establishes a clean execution context,
#   - creates a dedicated results directory for this run,
#   - executes analysis and figure scripts in a fixed order,
#   - avoids cross-script environment side effects.
#
# Running this file is sufficient to regenerate Figures 1, 2,
# and 6 as presented in the manuscript.
# ============================================================

CODE_DIR <- "/code"
assign("CODE_DIR", CODE_DIR, envir = .GlobalEnv)

cat("CODE_DIR set to:\n")
print(CODE_DIR)
cat("\n")

run_id  <- format(Sys.time(), "%Y%m%d_%H%M%S")
run_dir <- file.path(CODE_DIR, "..", "results", paste0("run_", run_id))
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

assign("run_dir", run_dir, envir = .GlobalEnv)

cat("Outputs will be saved in:\n")
cat(run_dir, "\n\n")

cat("Running ARCH sensitivity analysis...\n")

source(file.path(CODE_DIR, "04_ARCH_sensitivity.R"), local = new.env())
source(file.path(CODE_DIR, "Fig1_map_CE.R"), local = new.env())
source(file.path(CODE_DIR, "Fig1_trend.R"), local = new.env())
source(file.path(CODE_DIR, "Fig1_map_US.R"), local = new.env())

source(file.path(CODE_DIR, "Fig2a_map_CE.R"), local = new.env())
source(file.path(CODE_DIR, "Fig2a_map_US.R"), local = new.env())

source(file.path(CODE_DIR, "Fig2b.R"), local = new.env())
source(file.path(CODE_DIR, "Fig2c.R"), local = new.env())
source(file.path(CODE_DIR, "Fig2d.R"), local = new.env())

source(file.path(CODE_DIR, "Fig6_ARCH_example.R"), local = new.env())

cat("\n============================================\n")
cat("All scripts completed successfully.\n")
cat("============================================\n")