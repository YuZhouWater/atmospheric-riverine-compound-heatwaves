# ============================================================
# Figure a–f: AHW vs RHW (Count, Duration, Intensity) + Growth bars
#
# Methods:
# - Theil–Sen trend estimation: openair::TheilSen (annual aggregation)
# - Time series panels: (a)(c)(e) with mean ± SD ribbon and trend line
# - Growth bars: (b)(d)(f), comparing 1981–1990 vs 2010–2019 (% change)

setwd("..")
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(openair)
  library(patchwork)
  library(scales)  
  library(grid)    
})


in_dir  <- "../data"
dir.create("results/Fig1", recursive = TRUE, showWarnings = FALSE)
out_dir <- "../results/Fig1"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 1) safe read
# -----------------------------
read_csv_safe <- function(path) {
  if (!file.exists(path)) stop("Missing input file: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE)
}

# -----------------------------
# 2) Read inputs (relative paths; Code Ocean)
# -----------------------------
ahw_dir <- file.path(in_dir, "Heatwave_attributes_AHW")
rhw_dir <- file.path(in_dir, "Heatwave_attributes_RHW")

Ahw_count     <- read_csv_safe(file.path(ahw_dir, "AHW_annual_frequency.csv"))
Rhw_count     <- read_csv_safe(file.path(rhw_dir, "RHW_annual_frequency.csv"))

Ahw_duration  <- read_csv_safe(file.path(ahw_dir, "AHW_annual_duration.csv"))
Rhw_duration  <- read_csv_safe(file.path(rhw_dir, "RHW_annual_duration.csv"))

Ahw_intensity <- read_csv_safe(file.path(ahw_dir, "AHW_annual_mean_intensity.csv"))
Rhw_intensity <- read_csv_safe(file.path(rhw_dir, "RHW_annual_mean_intensity.csv"))

# -----------------------------
# 3) Build annual table for openair (one row per year)
#    - 'date' is converted to POSIXct for TheilSen
# -----------------------------
temp_model <- data.frame(
  year          = Rhw_count$year,
  Rhw_count     = Rhw_count$AVERAGE,
  Ahw_count     = Ahw_count$MEAN,
  Rhw_duration  = Rhw_duration$MEAN,
  Ahw_duration  = Ahw_duration$MEAN,
  Rhw_intensity = Rhw_intensity$mean,
  Ahw_intensity = Ahw_intensity$mean
)

# Use Jan-01 as the annual timestamp
temp_model$date <- ymd_hms(paste0(temp_model$year, "-01-01 00:00:00"))

# A Date vector used later for joins/ribbons
date_key <- as.Date(paste0(temp_model$year, "-01-01"))

# -----------------------------
# 4) Theil–Sen trends (annual scale)
#    - openair returns $data$main.data with columns including:
#      date, conc, slope, intercept (and others)
# -----------------------------
sen1 <- TheilSen(temp_model, pollutant = "Rhw_count",     avg.time = "year", deseason = TRUE, date.format = "%Y", slope.percent = FALSE, dec.place = 3)
sen2 <- TheilSen(temp_model, pollutant = "Ahw_count",     avg.time = "year", deseason = TRUE, date.format = "%Y", slope.percent = FALSE, dec.place = 3)
sen3 <- TheilSen(temp_model, pollutant = "Rhw_duration",  avg.time = "year", deseason = TRUE, date.format = "%Y", slope.percent = FALSE, dec.place = 3)
sen4 <- TheilSen(temp_model, pollutant = "Ahw_duration",  avg.time = "year", deseason = TRUE, date.format = "%Y", slope.percent = FALSE, dec.place = 3)
sen5 <- TheilSen(temp_model, pollutant = "Rhw_intensity", avg.time = "year", deseason = TRUE, date.format = "%Y", slope.percent = FALSE, dec.place = 3)
sen6 <- TheilSen(temp_model, pollutant = "Ahw_intensity", avg.time = "year", deseason = TRUE, date.format = "%Y", slope.percent = FALSE, dec.place = 3)

data1 <- sen1$data$main.data
data2 <- sen2$data$main.data
data3 <- sen3$data$main.data
data4 <- sen4$data$main.data
data5 <- sen5$data$main.data
data6 <- sen6$data$main.data

# -----------------------------
# 5) Attach annual SD for uncertainty ribbons
# -----------------------------
new_df_count <- data.frame(
  date = date_key,
  Rhw.count.std = Rhw_count$STDEVP,
  Ahw.count.std = Ahw_count$STDEVP
)
data1 <- left_join(data1, new_df_count, by = "date")
data2 <- left_join(data2, new_df_count, by = "date")

new_df_duration <- data.frame(
  date = date_key,
  Rhw.duration.std = Rhw_duration$STDEVP,
  Ahw.duration.std = Ahw_duration$STDEVP
)
data3 <- left_join(data3, new_df_duration, by = "date")
data4 <- left_join(data4, new_df_duration, by = "date")

new_df_intensity <- data.frame(
  date = date_key,
  Rhw.intensity.std = Rhw_intensity$STDEVP,
  Ahw.intensity.std = Ahw_intensity$STDEVP
)
data5 <- left_join(data5, new_df_intensity, by = "date")
data6 <- left_join(data6, new_df_intensity, by = "date")

# -----------------------------
# 6) Growth rate (% change):
#    compare mean(1981–1990) vs mean(2010–2019)
# -----------------------------
calculate_means_and_growth <- function(df) {
  df$year <- as.numeric(format(df$date, "%Y"))
  df_first10 <- df[df$year >= 1981 & df$year <= 1990, ]
  df_last10  <- df[df$year >= 2010 & df$year <= 2019, ]
  mean_first10 <- mean(df_first10$conc, na.rm = TRUE)
  mean_last10  <- mean(df_last10$conc,  na.rm = TRUE)
  growth_rate <- ((mean_last10 - mean_first10) / mean_first10) * 100
  list(mean_first10 = mean_first10, mean_last10 = mean_last10, growth_rate = growth_rate)
}

result_data1 <- calculate_means_and_growth(data1)
result_data2 <- calculate_means_and_growth(data2)
result_data3 <- calculate_means_and_growth(data3)
result_data4 <- calculate_means_and_growth(data4)
result_data5 <- calculate_means_and_growth(data5)
result_data6 <- calculate_means_and_growth(data6)

# ============================================================
# 7) Plot settings (consistent axis + theme)
# ============================================================
xscale_1981_2019 <- scale_x_date(
  limits = c(as.Date("1981-01-01"), as.Date("2019-01-01")),
  breaks = seq(as.Date("1981-01-01"), as.Date("2019-01-01"), by = "9 years"),
  date_labels = "%Y",
  expand = expansion(mult = c(0.01, 0.01))
)

common_time_theme <- theme_bw() +
  theme(
    panel.border       = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_rect(fill = "white"),
    legend.position    = "none",
    plot.margin        = margin(5, 5, 5, 5),
    axis.title.x       = element_blank(),
    axis.title.y       = element_text(color = "black", size = 18, margin = margin(r = 10)),
    axis.text          = element_text(size = 16, color = "black"),
    axis.line          = element_line(color = "black", linewidth = 0.8),
    axis.ticks         = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length  = unit(0.15, "cm")
  )

bar_theme <- theme_bw() +
  theme(
    panel.border       = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_rect(fill = "white"),
    legend.position    = "none",
    plot.margin        = margin(5, 5, 5, 5),
    axis.title.x       = element_blank(),
    axis.title.y       = element_text(color = "black", size = 18, margin = margin(r = 10)),
    axis.text          = element_text(size = 16, color = "black"),
    axis.line          = element_line(color = "black", linewidth = 0.8),
    axis.ticks         = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length  = unit(0.15, "cm"),
    axis.ticks.x       = element_blank()
  )

# Styling knobs
ribbon_alpha <- 0.10
line_w       <- 0.9
pt_size      <- 2.2
trend_w      <- 1.2
trend_alpha  <- 0.55
lab_size     <- 5.5
panel_size   <- 6.5

# ============================================================
# 8) Panels (a)(c)(e): time series
# ============================================================

# (a) Count
p1 <- ggplot() +
  geom_ribbon(data = data1,
              aes(x = date, ymin = conc - Rhw.count.std, ymax = conc + Rhw.count.std),
              fill = "blue", alpha = ribbon_alpha) +
  geom_line(data = data1, aes(x = date, y = conc), color = "blue", linewidth = line_w) +
  geom_point(data = data1, aes(x = date, y = conc), color = "blue", size = pt_size, shape = 16) +
  geom_abline(intercept = data1$intercept, slope = data1$slope / 365,
              color = "blue", alpha = trend_alpha, linewidth = trend_w) +

  geom_ribbon(data = data2,
              aes(x = date, ymin = conc - Ahw.count.std, ymax = conc + Ahw.count.std),
              fill = "red", alpha = ribbon_alpha) +
  geom_line(data = data2, aes(x = date, y = conc), color = "red", linewidth = line_w) +
  geom_point(data = data2, aes(x = date, y = conc), color = "red", size = pt_size, shape = 16) +
  geom_abline(intercept = data2$intercept, slope = data2$slope / 365,
              color = "red", alpha = trend_alpha, linewidth = trend_w) +

  annotate("text", x = as.Date("1981-01-01"), y = 9,
           label = "AHW: 0.49 [0.18,0.79] counts/dec*",
           size = lab_size, color = "red", hjust = 0) +
  annotate("text", x = as.Date("1981-01-01"), y = 7.7,
           label = "RHW: 0.59 [0.37,0.88] counts/dec***",
           size = lab_size, color = "blue", hjust = 0) +
  annotate("text", x = as.Date("1981-10-01"), y = 10,
           label = "(a)", size = panel_size, color = "black",
           fontface = "bold", hjust = 0) +

  labs(x = NULL, y = "Count") +
  coord_cartesian(xlim = c(as.Date("1981-01-01"), as.Date("2019-01-01"))) +
  xscale_1981_2019 +
  common_time_theme

# (c) Duration
p2 <- ggplot() +
  geom_ribbon(data = data3,
              aes(x = date, ymin = conc - Rhw.duration.std, ymax = conc + Rhw.duration.std),
              fill = "blue", alpha = ribbon_alpha) +
  geom_line(data = data3, aes(x = date, y = conc), color = "blue", linewidth = line_w) +
  geom_point(data = data3, aes(x = date, y = conc), color = "blue", size = pt_size, shape = 16) +
  geom_abline(intercept = data3$intercept, slope = data3$slope / 365,
              color = "blue", alpha = trend_alpha, linewidth = trend_w) +

  geom_ribbon(data = data4,
              aes(x = date, ymin = conc - Ahw.duration.std, ymax = conc + Ahw.duration.std),
              fill = "red", alpha = ribbon_alpha) +
  geom_line(data = data4, aes(x = date, y = conc), color = "red", linewidth = line_w) +
  geom_point(data = data4, aes(x = date, y = conc), color = "red", size = pt_size, shape = 16) +
  geom_abline(intercept = data4$intercept, slope = data4$slope / 365,
              color = "red", alpha = trend_alpha, linewidth = trend_w) +

  annotate("text", x = as.Date("1981-01-01"), y = 76,
           label = "AHW: 2.59 [0.80,4.68] days/dec**",
           size = lab_size, color = "red", hjust = 0) +
  annotate("text", x = as.Date("1981-01-01"), y = 66,
           label = "RHW: 5.92 [3.39,8.63] days/dec***",
           size = lab_size, color = "blue", hjust = 0) +
  annotate("text", x = as.Date("1981-10-01"), y = 86,
           label = "(c)", size = panel_size, color = "black",
           fontface = "bold", hjust = 0) +

  labs(x = NULL, y = "Duration (d)") +
  coord_cartesian(xlim = c(as.Date("1981-01-01"), as.Date("2019-01-01"))) +
  xscale_1981_2019 +
  common_time_theme

# (e) Intensity
p3 <- ggplot() +
  geom_ribbon(data = data5,
              aes(x = date, ymin = conc - Rhw.intensity.std, ymax = conc + Rhw.intensity.std),
              fill = "blue", alpha = ribbon_alpha) +
  geom_line(data = data5, aes(x = date, y = conc), color = "blue", linewidth = line_w) +
  geom_point(data = data5, aes(x = date, y = conc), color = "blue", size = pt_size, shape = 16) +
  geom_abline(intercept = data5$intercept, slope = data5$slope / 365,
              color = "blue", alpha = trend_alpha, linewidth = trend_w) +

  geom_ribbon(data = data6,
              aes(x = date, ymin = conc - Ahw.intensity.std, ymax = conc + Ahw.intensity.std),
              fill = "red", alpha = ribbon_alpha) +
  geom_line(data = data6, aes(x = date, y = conc), color = "red", linewidth = line_w) +
  geom_point(data = data6, aes(x = date, y = conc), color = "red", size = pt_size, shape = 16) +
  geom_abline(intercept = data6$intercept, slope = data6$slope / 365,
              color = "red", alpha = trend_alpha, linewidth = trend_w) +

  annotate("text", x = as.Date("1981-01-01"), y = 76,
           label = "AHW: 3.36 [0.18,0.79] ℃/dec*",
           size = lab_size, color = "red", hjust = 0) +
  annotate("text", x = as.Date("1981-01-01"), y = 66,
           label = "RHW: 1.54 [0.67,2.85] ℃/dec***",
           size = lab_size, color = "blue", hjust = 0) +
  annotate("text", x = as.Date("1981-10-01"), y = 86,
           label = "(e)", size = panel_size, color = "black",
           fontface = "bold", hjust = 0) +

  labs(x = "Year", y = "Intensity (℃)") +
  coord_cartesian(xlim = c(as.Date("1981-01-01"), as.Date("2019-01-01"))) +
  xscale_1981_2019 +
  common_time_theme +
  theme(axis.title.x = element_text(color = "black", size = 18, margin = margin(t = 8)))

# ============================================================
# 9) Panels (b)(d)(f): growth bars
# ============================================================
make_growth_bar <- function(mean_vals, ylab, ylim_top, panel_tag){
  df <- data.frame(land = c("Rhw", "Ahw"), mean_value = mean_vals)

  ggplot(df, aes(x = land, y = mean_value, fill = land)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(sprintf("%.1f", mean_value), "%"), color = land),
              vjust = -0.5, size = 5) +
    labs(x = "Heatwave Type", y = ylab) +
    scale_fill_manual(values = c("Ahw" = alpha("red", 0.55),
                                 "Rhw" = alpha("blue", 0.55))) +
    scale_color_manual(values = c("Ahw" = "red", "Rhw" = "blue")) +
    coord_cartesian(ylim = c(0, ylim_top)) +
    annotate("text", x = 0.5, y = ylim_top,
             label = panel_tag, size = panel_size, color = "black",
             fontface = "bold", hjust = 0) +
    bar_theme
}

p4 <- make_growth_bar(
  mean_vals = c(result_data1$growth_rate, result_data2$growth_rate),
  ylab      = "Count Trend (%/dec)",
  ylim_top  = 150,
  panel_tag = "(b)"
)

p5 <- make_growth_bar(
  mean_vals = c(result_data3$growth_rate, result_data4$growth_rate),
  ylab      = "Duration Trend (%/dec)",
  ylim_top  = 200,
  panel_tag = "(d)"
)

p6 <- make_growth_bar(
  mean_vals = c(result_data5$growth_rate, result_data6$growth_rate),
  ylab      = "Intensity Trend (%/dec)",
  ylim_top  = 150,
  panel_tag = "(f)"
)

# ============================================================
# 10) Assemble a–f and save
# ============================================================
combined_plot1 <- (p1 | p4) + plot_layout(widths = c(7.5, 2.5))
combined_plot2 <- (p2 | p5) + plot_layout(widths = c(7.5, 2.5))
combined_plot3 <- (p3 | p6) + plot_layout(widths = c(7.5, 2.5))

final_combined_plot <- combined_plot1 / combined_plot2 / combined_plot3

ggsave(
  filename = file.path(out_dir, "Fig1_trend_plot.png"),
  plot     = final_combined_plot,
  dpi      = 600,
  width    = 12,
  height   = 12
)

final_combined_plot

calculate_growth_and_stats <- function(sen_data, slope_column, pstars_column, df) {

  slope <- sen_data$slope
  p_stars <- sen_data$p.stars
  

  df$year <- as.numeric(format(df$date, "%Y"))
  df_first10 <- df[df$year >= 1981 & df$year <= 1990, ]
  df_last10  <- df[df$year >= 2010 & df$year <= 2019, ]
  mean_first10 <- mean(df_first10$conc, na.rm = TRUE)
  mean_last10  <- mean(df_last10$conc,  na.rm = TRUE)
  growth_rate <- ((mean_last10 - mean_first10) / mean_first10) * 100
  
  list(
    slope = slope,
    p_stars = p_stars,
    growth_rate = growth_rate
  )
}


# ---- 1) Get stats (existing)
growth_data1 <- calculate_growth_and_stats(sen1$data$res2, "slope", "p.stars", data1)
growth_data2 <- calculate_growth_and_stats(sen2$data$res2, "slope", "p.stars", data2)
growth_data3 <- calculate_growth_and_stats(sen3$data$res2, "slope", "p.stars", data3)
growth_data4 <- calculate_growth_and_stats(sen4$data$res2, "slope", "p.stars", data4)
growth_data5 <- calculate_growth_and_stats(sen5$data$res2, "slope", "p.stars", data5)
growth_data6 <- calculate_growth_and_stats(sen6$data$res2, "slope", "p.stars", data6)

# ---- 2) Fixed summary table with slope ×10 (per decade) and units
metric_levels <- c(
  "RHW Count",
  "AHW Count",
  "RHW Duration",
  "AHW Duration",
  "RHW Intensity",
  "AHW Intensity"
)

slope_dec <- c(
  growth_data1$slope, growth_data2$slope, growth_data3$slope,
  growth_data4$slope, growth_data5$slope, growth_data6$slope
) * 10

unit_vec <- c(
  "counts/dec", "counts/dec",
  "days/dec",   "days/dec",
  "℃/dec",      "℃/dec"
)

output_df <- tibble::tibble(
  Metric = factor(metric_levels, levels = metric_levels),
  Slope_per_decade = round(slope_dec, 2),
  Slope_unit = unit_vec,
  Significance = c(
    growth_data1$p_stars, growth_data2$p_stars, growth_data3$p_stars,
    growth_data4$p_stars, growth_data5$p_stars, growth_data6$p_stars
  ),
  Relative_Percent_Change_1980s_to_2010s = round(c(
    growth_data1$growth_rate, growth_data2$growth_rate, growth_data3$growth_rate,
    growth_data4$growth_rate, growth_data5$growth_rate, growth_data6$growth_rate
  ), 1)
) %>%
  dplyr::arrange(Metric)

# ---- 3) Save
readr::write_csv(output_df, file.path(out_dir, "Fig1_trend_summary.csv"))
