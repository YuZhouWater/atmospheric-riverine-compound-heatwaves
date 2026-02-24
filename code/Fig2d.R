setwd("..")
library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)
library(readr)
library(ggpubr)

data <- read_csv("data/ARCH_frequency_by_time_gap/time_gap=5.csv")
data2 <- read_csv("data/Watershed_attraibutes/arrtibute796.csv")

new_row <- as.data.frame(t(data2$ele_mt_sav))
colnames(new_row) <- colnames(data)[2:797]

data_with_new_row <- rbind(data[, 2:797], new_row)
elevation_data <- data_with_new_row[nrow(data_with_new_row), ]

range_1 <- which(elevation_data >= 0 & elevation_data <= 1000)
range_2 <- which(elevation_data > 1000 & elevation_data <= 2000)
range_3 <- which(elevation_data > 2000 & elevation_data <= 3000)
range_4 <- which(elevation_data > 3000)

years <- 1981:2019

prepare_range <- function(index_vec) {
  df <- data_with_new_row[, c(1, index_vec)]
  df <- df[-nrow(df), ]
  df$MEAN <- apply(df, 1, mean, na.rm = TRUE)
  df$STD  <- apply(df, 1, sd, na.rm = TRUE)
  df$year <- years
  return(df)
}

data_range_1 <- prepare_range(range_1)
data_range_2 <- prepare_range(range_2)
data_range_3 <- prepare_range(range_3)
data_range_4 <- prepare_range(range_4)

# Total
data_total <- data
data_total$MEAN <- apply(data_total[, -1], 1, mean, na.rm = TRUE)
data_total$STD  <- apply(data_total[, -1], 1, sd, na.rm = TRUE)
data_total$year <- years

# ===============================
process_data <- function(data_frame) {

  new_dataset <- data_frame %>%
    select(year, STD, MEAN)

  colnames(new_dataset) <- c("Year", "STD", "MEAN")

  fit_data <- new_dataset %>% filter(Year > 1980)

  fit_data$date <- as.Date(paste(fit_data$Year, "-01-01", sep = ""))
  fit_data$date <- ymd_hms(paste(fit_data$date, "00:00:00"))

  sen1 <- TheilSen(
    fit_data,
    pollutant = "MEAN",
    avg.time = 'year',
    deseason = TRUE,
    date.format = "%Y"
  )

  data1 <- sen1$data$main.data
  data1 <- left_join(data1, fit_data, by = "date")

  trend_info <- data.frame(
    slope = sen1$data$res2$slope,
    slope_percent = sen1$data$res2$slope.percent,
    p_stars = sen1$data$res2$p.stars
  )

  return(list(data = data1, trend = trend_info))
}


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
  "0–1000 m: Slope = ", round(res1$trend$slope,3),
  res1$trend$p_stars,
  " (", round(res1$trend$slope_percent,2), "% yr⁻¹)\n",
  
  "≥3000 m: Slope = ", round(res4$trend$slope,3),
  res4$trend$p_stars,
  " (", round(res4$trend$slope_percent,2), "% yr⁻¹)\n",
  
  "Total: Slope = ", round(resT$trend$slope,3),
  resT$trend$p_stars,
  " (", round(resT$trend$slope_percent,2), "% yr⁻¹)"
)


p1 <- ggplot(combined_data, aes(x = Year, y = MEAN, color = land_type, fill = land_type)) +
  geom_ribbon(aes(ymin = MEAN - STD, ymax = MEAN + STD), alpha = 0.2) +
  geom_line(linewidth = 1) +
  annotate(
    "text",
    x = 2000,
    y = 5,
    label = trend_text,
    size = 5,
    fontface = "bold",
    hjust = 0.5
  ) +
  theme_classic() +
  scale_y_continuous(
    limits = c(-1, 5.5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(1980, 2020),
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "ARCH Frequency",
    color = "Elevation",
    fill = "Elevation"
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    panel.grid = element_blank()
  )

print(p1)
dir.create("results/Fig2", recursive = TRUE, showWarnings = FALSE)
ggsave("results/Fig2/Fig2d.png", p1, dpi = 600, width = 9, height = 6)
