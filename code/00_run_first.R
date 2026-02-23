# ============================================================
# 00_run_first.R
# Stable entry point for ARCH workflow (Code Ocean safe)
# ============================================================

cat("============================================\n")
cat("Running ARCH workflow...\n")
cat("============================================\n\n")

# 🔥 强制切换到 /code 目录
setwd("/code")

cat("Working directory:\n")
print(getwd())
cat("\n")

# ============================================================
# 创建输出目录
# ============================================================

run_id  <- format(Sys.time(), "%Y%m%d_%H%M%S")
run_dir <- file.path("..", "results", paste0("run_", run_id))
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

assign("run_dir", run_dir, envir = .GlobalEnv)

cat("Outputs will be saved in:\n")
cat(run_dir, "\n\n")

# ============================================================
# 运行敏感性分析
# ============================================================

cat("Running ARCH sensitivity analysis...\n")
source("04_ARCH_sensitivity.R")

# ============================================================
# 按顺序运行所有 Figure
# ============================================================

cat("Generating figures...\n")

source("Fig1_CE_map.R")
source("Fig1_trend.R")
source("Fig1_US_map.R")
source("Fig2a_CE_map.R")
source("Fig2a_US_map.R")
source("Fig2b.R")
source("Fig2c.R")
source("Fig2d.R")
source("Fig6_ARCH_example.R")

cat("\n============================================\n")
cat("All scripts completed successfully.\n")
cat("============================================\n")