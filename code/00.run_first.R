# 00.run_first.R

# ============================================================
# Entry point for reviewers (Code Ocean capsule)
#
# Purpose
# - This script is the recommended starting point for peer review.
# - It runs Scripts 04–06 using the precomputed intermediate outputs
#   already provided in `data/`.

setwd("..")
cat("Running ARCH / RHW / AHW workflow (entry point)...\n")

# 1) Create a unique output folder for this run
run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path("results", paste0("run_", run_id))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("Outputs will be written to: ", out_dir, "\n")

# 2) Switch working directory so any relative outputs go into this folder
old_wd <- getwd()
setwd(out_dir)
on.exit(setwd(old_wd), add = TRUE)

# 3) Run analysis + visualization (scripts still read from ../data)
source(file.path("..", "..", "code", "04.analyze_RHW_AHW_trend.R"))
source(file.path("..", "..", "code", "05.analyze_ARCH_trend.R"))
source(file.path("..", "..", "code", "06.plot_ARCH_example.R"))

cat("Done.\n")