library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)
library(readr)

# ===============================
# 1. 读取已经整理好的汇总数据
# ===============================

ARCH_frequency_elevation_mean_std <- read_csv(
  "D:/论文1/Github/data/ARCH data/ARCH_freq_by_elevation.csv",
  show_col_types = FALSE
)

cat("数据维度：", nrow(ARCH_frequency_elevation_mean_std), "行，",
    ncol(ARCH_frequency_elevation_mean_std), "列\n")
print(names(ARCH_frequency_elevation_mean_std))

# ===============================
# 2. 从汇总数据中提取5个分组
# ===============================

make_group_df <- function(df, mean_col, sd_col) {
  df %>%
    transmute(
      Year = year,
      MEAN = .data[[mean_col]],
      STD  = .data[[sd_col]]
    ) %>%
    filter(Year > 1980) %>%
    mutate(
      date = as.Date(paste0(Year, "-01-01")),
      date = ymd_hms(paste(date, "00:00:00"))
    )
}

data_total <- make_group_df(
  ARCH_frequency_elevation_mean_std,
  "total_mean",
  "total_sd"
)

data_range_1 <- make_group_df(
  ARCH_frequency_elevation_mean_std,
  "elevation_0_1000m_mean",
  "elevation_0_1000m_sd"
)

data_range_2 <- make_group_df(
  ARCH_frequency_elevation_mean_std,
  "elevation_1000_2000m_mean",
  "elevation_1000_2000m_sd"
)

data_range_3 <- make_group_df(
  ARCH_frequency_elevation_mean_std,
  "elevation_2000_3000m_mean",
  "elevation_2000_3000m_sd"
)

data_range_4 <- make_group_df(
  ARCH_frequency_elevation_mean_std,
  "elevation_above_3000m_mean",
  "elevation_above_3000m_sd"
)

# ===============================
# 3. Theil-Sen 趋势计算函数
# ===============================

process_data <- function(data_frame) {
  
  fit_data <- data_frame %>%
    select(date, Year, MEAN, STD)
  
  sen1 <- TheilSen(
    fit_data,
    pollutant = "MEAN",
    avg.time = "year",
    deseason = TRUE,
    date.format = "%Y",
    alpha = 0.05,
    plot = FALSE
  )
  
  trend_info <- data.frame(
    slope = sen1$data$res2$slope,
    slope_percent = sen1$data$res2$slope.percent,
    p_stars = sen1$data$res2$p.stars
  )
  
  coef_info <- sen1$data$res2
  
  return(list(
    data = fit_data,
    trend = trend_info,
    coef = coef_info
  ))
}

# ===============================
# 4. 分组趋势计算
# ===============================

res1 <- process_data(data_range_1)
res2 <- process_data(data_range_2)
res3 <- process_data(data_range_3)
res4 <- process_data(data_range_4)
resT <- process_data(data_total)

combined_data <- bind_rows(
  mutate(res1$data, land_type = "0–1000 m"),
  mutate(res2$data, land_type = "1000–2000 m"),
  mutate(res3$data, land_type = "2000–3000 m"),
  mutate(res4$data, land_type = "≥3000 m"),
  mutate(resT$data, land_type = "Total")
)

combined_data$land_type <- factor(
  combined_data$land_type,
  levels = c("0–1000 m", "1000–2000 m", "2000–3000 m", "≥3000 m", "Total")
)

trend_text <- paste0(
  "0–1000 m: Slope = ", round(res1$trend$slope, 3),
  res1$trend$p_stars,
  " (", round(res1$trend$slope_percent, 3), "% yr⁻¹)\n",
  
  "≥3000 m: Slope = ", round(res4$trend$slope, 3),
  res4$trend$p_stars,
  " (", round(res4$trend$slope_percent, 3), "% yr⁻¹)\n",
  
  "Total: Slope = ", round(resT$trend$slope, 3),
  resT$trend$p_stars,
  " (", round(resT$trend$slope_percent, 3), "% yr⁻¹)"
)

# ===============================
# 5. 提取 Total 的 Theil-Sen 系数
# ===============================

coefT <- resT$coef

cat("\n===== Theil-Sen coefficient unit check =====\n")
cat("coefT$b * 365.25 =", coefT$b[1] * 365.25, "\n")
cat("reported slope   =", coefT$slope[1], "\n")
cat("difference       =", coefT$b[1] * 365.25 - coefT$slope[1], "\n")

# ===============================
# 6. 提取 Total 和 elevation 分组数据
# ===============================

total_data <- combined_data %>%
  filter(land_type == "Total")

elevation_data <- combined_data %>%
  filter(land_type != "Total")

# ===============================
# 7. 计算 Total mean 的 Theil-Sen 拟合线
# ===============================

total_fit_line <- total_data %>%
  mutate(
    date_for_fit = as.Date(paste0(Year, "-01-01")),
    date_num = as.numeric(date_for_fit),
    trend_fit = coefT$a[1] + coefT$b[1] * date_num
  ) %>%
  select(Year, trend_fit)

# ===============================
# 8. 配色
# ===============================

plot_cols <- c(
  "0–1000 m"     = "#B8E186",
  "1000–2000 m" = "#66BD63",
  "2000–3000 m" = "#1A9850",
  "≥3000 m"     = "#006837",
  "Total"       = "#5E3C99"
)

# ===============================
# 9. 绘图
# ===============================

p1 <- ggplot() +
  
  geom_ribbon(
    data = total_data,
    aes(x = Year, ymin = MEAN - STD, ymax = MEAN + STD),
    fill = "#5E3C99",
    alpha = 0.18
  ) +
  
  geom_line(
    data = elevation_data,
    aes(x = Year, y = MEAN, color = land_type),
    linewidth = 0.95
  ) +
  
  geom_line(
    data = total_data,
    aes(x = Year, y = MEAN, color = land_type),
    linewidth = 1.15
  ) +
  
  geom_line(
    data = total_fit_line,
    aes(x = Year, y = trend_fit),
    color = "#5E3C99",
    linewidth = 1.2
  ) +
  
  annotate(
    "text",
    x = 2001,
    y = 5.15,
    label = trend_text,
    size = 4.6,
    fontface = "bold",
    hjust = 0.5
  ) +
  
  scale_color_manual(values = plot_cols) +
  
  scale_y_continuous(
    limits = c(-0.4, 5.5),
    breaks = c(1, 3, 5),
    expand = c(0, 0)
  ) +
  
  scale_x_continuous(
    limits = c(1980, 2020),
    breaks = c(1980, 1990, 2000, 2010, 2020),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Year",
    y = "ARCH Frequency",
    color = "Elevation"
  ) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 22, color = "black"),
    axis.text  = element_text(size = 17, color = "black"),
    
    legend.position = "right",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text  = element_text(size = 17),
    
    panel.grid = element_blank()
  )

print(p1)

# ===============================
# 10. 计算 Total 拟合线起点和终点的 y 值
# ===============================

total_fit_start_end <- total_data %>%
  mutate(
    date_for_fit = as.Date(paste0(Year, "-01-01")),
    date_num = as.numeric(date_for_fit),
    
    trend_fit = coefT$a[1] + coefT$b[1] * date_num,
    
    line1 = coefT$upper.a[1] + coefT$upper.b[1] * date_num,
    line2 = coefT$lower.a[1] + coefT$lower.b[1] * date_num,
    
    trend_lower = pmin(line1, line2),
    trend_upper = pmax(line1, line2),
    
    lower_error = trend_fit - trend_lower,
    upper_error = trend_upper - trend_fit,
    error_mean = (lower_error + upper_error) / 2,
    
    fit_pm_text = paste0(
      sprintf("%.2f", trend_fit),
      " ± ",
      sprintf("%.2f", error_mean)
    ),
    
    fit_ci_text = paste0(
      sprintf("%.2f", trend_fit),
      " [",
      sprintf("%.2f", trend_lower),
      ", ",
      sprintf("%.2f", trend_upper),
      "]"
    )
  ) %>%
  filter(Year %in% c(min(Year, na.rm = TRUE), max(Year, na.rm = TRUE))) %>%
  select(
    Year,
    MEAN,
    trend_fit,
    trend_lower,
    trend_upper,
    lower_error,
    upper_error,
    error_mean,
    fit_pm_text,
    fit_ci_text
  )

cat("\n===== Total fitted start and end values =====\n")
print(total_fit_start_end)

fit_change <- total_fit_start_end %>%
  summarise(
    start_year = min(Year),
    end_year = max(Year),
    start_y = trend_fit[Year == start_year],
    end_y = trend_fit[Year == end_year],
    absolute_change = end_y - start_y,
    relative_change_percent = (end_y - start_y) / start_y * 100,
    fold_change = end_y / start_y
  )

cat("\n===== Total fitted change =====\n")
print(fit_change)

# ===============================
# 11. 保存 EPS
# ===============================

# out_dir <- "D:/论文1/代码图片终版/plot-v01/plot2"
# dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# 
# ggsave(
#   filename = file.path(out_dir, "Fig2d.eps"),
#   plot = p1,
#   device = cairo_ps,
#   width = 9,
#   height = 6,
#   units = "in"
# )
