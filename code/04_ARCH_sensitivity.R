setwd("..")
# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)
library(readr)
library(ggpubr)
# Read the CSV files from the specified folder
data1 <- read_csv("data/ARCH_frequency_by_time_gap/time_gap=1.csv")
data2 <- read_csv("data/ARCH_frequency_by_time_gap/time_gap=3.csv")
data3 <- read_csv("data/ARCH_frequency_by_time_gap/time_gap=5.csv")
data4 <- read_csv("data/ARCH_frequency_by_time_gap/time_gap=10.csv")
data5 <- read_csv("data/ARCH_frequency_by_time_gap/time_gap=15.csv")
data6 <- read_csv("data/ARCH_frequency_by_time_gap/time_gap=20.csv")

years <- 1981:2019
data1 <- data1[, -1]
data2 <- data2[, -1]
data3 <- data3[, -1]
data4 <- data4[, -1]
data5 <- data5[, -1]
data6 <- data6[, -1]
# 计算每行的平均值和标准差（不包括时间列）
data1$MEAN <- apply(data1, 1, mean, na.rm = TRUE)
data1$STD <- apply(data1, 1, sd, na.rm = TRUE)
data1$year <- years  # 添加时间列

data2$MEAN <- apply(data2, 1, mean, na.rm = TRUE)
data2$STD <- apply(data2, 1, sd, na.rm = TRUE)
data2$year <- years  # 添加时间列

data3$MEAN <- apply(data3, 1, mean, na.rm = TRUE)
data3$STD <- apply(data3, 1, sd, na.rm = TRUE)
data3$year <- years  # 添加时间列

data4$MEAN <- apply(data4, 1, mean, na.rm = TRUE)
data4$STD<- apply(data4, 1, sd, na.rm = TRUE)
data4$year <- years  # 添加时间列

data5$MEAN <- apply(data5, 1, mean, na.rm = TRUE)
data5$STD<- apply(data5, 1, sd, na.rm = TRUE)
data5$year <- years  # 添加时间列

data6$MEAN <- apply(data6, 1, mean, na.rm = TRUE)
data6$STD<- apply(data6, 1, sd, na.rm = TRUE)
data6$year <- years  # 添加时间列
data_frame <- data6

# Function to process each dataset and calculate mean and std
process_data <- function(data_frame) {
  new_dataset <- data_frame %>%
    select(year, STD, MEAN)  # Select relevant columns
  
  colnames(new_dataset) <- c("Year", "STD", "MEAN")  # Rename for consistency
  
  # Filter data from 1980 onwards
  fit_data <- new_dataset %>% filter(Year > 1980)
  
  # Create date column for TheilSen analysis
  fit_data$date <- ymd_hms(paste0(fit_data$Year, "-01-01 00:00:00"))
  
  # TheilSen method for trend analysis
  sen1 <- TheilSen(
    fit_data,
    pollutant = "MEAN",
    ylab = "ARCH_count",
    avg.time = 'year',
    deseason = TRUE,
    date.format = "%Y",
    slope.percent = TRUE,
    dec.place = 3
  )
  
  # Return processed data and metrics
  plot_data <- sen1$data$main.data %>%
    left_join(fit_data, by = "date")
  
  metrics <- tibble(
    slope = sen1$data$res2$slope,
    p_stars = sen1$data$res2$p.stars,
    slope_percent = sen1$data$res2$slope.percent
  )
  
  return(list(plot = plot_data, metrics = metrics))
}

# Process all dataframes
res1 <- process_data(data1)
res2 <- process_data(data2)
res3 <- process_data(data3)
res4 <- process_data(data4)
res5 <- process_data(data5)
res6 <- process_data(data6)

# Combine data for plotting
combined_data <- bind_rows(
  mutate(res1$plot, land_type = "time_gap=1"),
  mutate(res2$plot, land_type = "time_gap=3"),
  mutate(res3$plot, land_type = "time_gap=5"),
  mutate(res4$plot, land_type = "time_gap=10"),
  mutate(res5$plot, land_type = "time_gap=15"),
  mutate(res6$plot, land_type = "time_gap=20")
)

# Create the plot for trends
p1 <- ggplot(combined_data, aes(x = Year, y = MEAN, color = land_type, fill = land_type)) +
  geom_line(size = 1) +  # Plot trend line
  theme_pubr() +  # Use a clean theme
  scale_x_continuous(
    limits = c(1980, 2020),  # Set x-axis limits
    breaks = seq(1980, 2020, by = 20),  # Set breaks every 20 years
    expand = c(0, 0)  # Remove extra space
  ) +
  theme(
    axis.title.x = element_text(color = "black", size = 20),
    axis.title.y = element_text(color = "black", size = 20, margin = ggplot2::margin(r = 15)),
    axis.text = element_text(size = 20),
    axis.line = element_line(color = "black", size = 0.8),
    plot.title = element_text(hjust = 0, face = "bold", size = 30, margin = ggplot2::margin(b = 15)),
    legend.position = "bottom",  # Place the legend at the bottom
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.margin = ggplot2::margin(5, 15, 5, 5),
    axis.ticks.length = unit(-0.25, "cm"),
    axis.ticks.x = element_blank(),
    axis.ticks = element_line(color = "black", size = 0.8)
  ) +
  labs(
    x = "Year",
    y = "ARCH frequency"
  )

# Display the plot
print(p1)
ggsave(
  filename = "results/ARCH_frequency_trend.png",
  plot = p1,
  dpi = 300,
  width = 10,
  height = 6
)
# Save the final metrics table
metrics_table <- bind_rows(
  mutate(res1$metrics, dataset = "time_gap=1"),
  mutate(res2$metrics, dataset = "time_gap=3"),
  mutate(res3$metrics, dataset = "time_gap=5"),
  mutate(res4$metrics, dataset = "time_gap=10"),
  mutate(res5$metrics, dataset = "time_gap=15"),
  mutate(res6$metrics, dataset = "time_gap=20")
)

# Print the metrics table
print(metrics_table)

# Save the metrics table as CSV
write_csv(
  metrics_table,
  "results/ARCH_frequency_summary.csv"  # Save the metrics table to the 'result' folder
)