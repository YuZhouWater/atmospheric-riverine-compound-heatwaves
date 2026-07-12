# ============================================================
# This script calculates and visualizes ARCH sensitivity to different compound-event time windows.
# It compares ARCH frequency trends across time gaps from 1 to 20 days
# and reports the Theil–Sen trend metrics for each time window.
# ============================================================

rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(lubridate)
  library(openair)
  library(ggplot2)
  library(ggpubr)
})

data_dir <- "D:/论文1/Github/data"

input_path <- file.path(
  data_dir,
  "ARCH_frequency_by_time_gap.csv"
)

out_dir <- file.path(data_dir, "results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

data_all <- read_csv(
  input_path,
  show_col_types = FALSE
)

# Ensure the first column is named year
names(data_all)[1] <- "year"

plot_data <- data_all %>%
  pivot_longer(
    cols = -year,
    names_to = c("gap", "statistic"),
    names_pattern = "gap(\\d+)_(mean|sd)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  rename(
    MEAN = mean,
    STD  = sd
  ) %>%
  mutate(
    gap = as.numeric(gap),
    
    time_gap = factor(
      paste0("time_gap=", gap),
      levels = paste0(
        "time_gap=",
        c(1, 3, 5, 10, 15, 20)
      )
    )
  ) %>%
  arrange(gap, year)

# Check the processed data
print(head(plot_data))

calculate_theil_sen <- function(df) {
  
  gap_value <- unique(df$gap)
  
  fit_data <- df %>%
    transmute(
      date = ymd_hms(
        paste0(year, "-01-01 00:00:00")
      ),
      MEAN = MEAN
    ) %>%
    filter(
      !is.na(date),
      !is.na(MEAN)
    )
  
  sen_result <- TheilSen(
    fit_data,
    pollutant = "MEAN",
    ylab = "ARCH frequency",
    avg.time = "year",
    deseason = FALSE,
    date.format = "%Y",
    slope.percent = FALSE,
    dec.place = 3
  )
  
  slope_year <- as.numeric(
    sen_result$data$res2$slope
  )
  
  tibble(
    time_gap_days = gap_value,
    slope_year = slope_year,
    slope_decade = slope_year * 10,
    p_stars = sen_result$data$res2$p.stars,
    slope_percent = sen_result$data$res2$slope.percent
  )
}

metrics_table <- plot_data %>%
  group_split(gap) %>%
  map_dfr(calculate_theil_sen) %>%
  arrange(time_gap_days)

print(metrics_table)

p1 <- ggplot(
  plot_data,
  aes(
    x = year,
    y = MEAN,
    colour = time_gap,
    fill = time_gap,
    group = time_gap
  )
) +
  
  # Mean ± standard deviation
  geom_ribbon(
    aes(
      ymin = pmax(MEAN - STD, 0),
      ymax = MEAN + STD
    ),
    alpha = 0.08,
    colour = NA
  ) +
  
  # Annual mean frequency
  geom_line(
    linewidth = 1
  ) +
  
  scale_x_continuous(
    limits = c(1980, 2020),
    breaks = seq(1980, 2020, by = 20),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Year",
    y = expression("ARCH frequency (events yr"^{-1}*")"),
    colour = "Time window",
    fill = "Time window"
  ) +
  
  theme_pubr() +
  
  theme(
    axis.title.x = element_text(
      colour = "black",
      size = 20
    ),
    
    axis.title.y = element_text(
      colour = "black",
      size = 20,
      margin = margin(r = 15)
    ),
    
    axis.text = element_text(
      colour = "black",
      size = 20
    ),
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.8
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.8
    ),
    
    axis.ticks.x = element_blank(),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      size = 18,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 18
    ),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.margin = margin(
      5, 15, 5, 5
    )
  )

print(p1)

