# ============================================================
# This script calculates and visualizes the trend in ARCH frequency.
# It plots the annual mean frequency with uncertainty and reports fitted changes.
# ============================================================

library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)
library(readr)


data <- read_csv(
  "D:/论文1/Github/data/ARCH_frequency_by_time_gap/time_gap=5.csv",
  show_col_types = FALSE
)

years <- 1981:2019

data_total <- data %>%
  mutate(
    MEAN = apply(select(., -1), 1, mean, na.rm = TRUE),
    STD  = apply(select(., -1), 1, sd, na.rm = TRUE),
    Year = years
  ) %>%
  select(Year, MEAN, STD) %>%
  filter(Year > 1980) %>%
  mutate(
    date = as.Date(paste0(Year, "-01-01")),
    date = ymd_hms(paste(date, "00:00:00"))
  )

fit_data <- data_total %>%
  select(date, Year, MEAN, STD)

sen_total <- TheilSen(
  fit_data,
  pollutant = "MEAN",
  avg.time = "year",
  deseason = TRUE,
  date.format = "%Y",
  alpha = 0.05,
  plot = FALSE
)

trend_total <- data.frame(
  slope = sen_total$data$res2$slope,
  slope_percent = sen_total$data$res2$slope.percent,
  p_stars = sen_total$data$res2$p.stars
)

coefT <- sen_total$data$res2

cat("\n===== Total ARCH frequency trend =====\n")
cat(
  "Slope =",
  round(trend_total$slope, 3),
  trend_total$p_stars,
  "(",
  round(trend_total$slope_percent, 3),
  "% yr⁻¹)\n"
)

total_fit_line <- data_total %>%
  mutate(
    date_for_fit = as.Date(paste0(Year, "-01-01")),
    date_num = as.numeric(date_for_fit),
    trend_fit = coefT$a[1] + coefT$b[1] * date_num
  ) %>%
  select(Year, trend_fit)

trend_text <- paste0(
  "Slope = ",
  round(trend_total$slope, 3),
  trend_total$p_stars,
  " (",
  round(trend_total$slope_percent, 3),
  "% yr⁻¹)"
)

p1 <- ggplot() +
  
  geom_ribbon(
    data = data_total,
    aes(x = Year, ymin = MEAN - STD, ymax = MEAN + STD),
    fill = "#5E3C99",
    alpha = 0.18
  ) +
  
  geom_line(
    data = data_total,
    aes(x = Year, y = MEAN),
    color = "#5E3C99",
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
    x = 2000,
    y = 5.15,
    label = trend_text,
    size = 5,
    fontface = "bold",
    hjust = 0.5
  ) +
  
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
    y = "ARCH Frequency"
  ) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 22, color = "black"),
    axis.text  = element_text(size = 17, color = "black"),
    panel.grid = element_blank()
  )

print(p1)


total_fit_start_end <- data_total %>%
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
