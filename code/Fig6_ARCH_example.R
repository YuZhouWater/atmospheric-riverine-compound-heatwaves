setwd("..")
# Load necessary libraries
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(heatwaveR)
  library(ggplot2)
  library(scales)
  library(gridExtra)
  library(cowplot)
})

data_RHW_path <- "data/temp_WT.csv"
data_AHW_path <- "data/Tmax_US_LamaH.csv"

main_plot_path <- "results/ARCH_main_plot.png"
zoom_plot_path <- "results/ARCH_zoom_plot.png"

read_wide_temp <- function(path) {
  dat <- read_csv(
    path,
    col_names = TRUE,
    cols(.default = col_double(), t = col_date(format = "%Y/%m/%d")),
    show_col_types = FALSE
  ) %>%
    as.data.frame()

  if (!"t" %in% names(dat)) stop("Missing column 't' in: ", path, call. = FALSE)
  if (ncol(dat) < 2) stop("Expect at least 2 columns (t + series) in: ", path, call. = FALSE)

  dat
}

# Read RHW and AHW data
data_RHW <- read_wide_temp(data_RHW_path)
data_AHW <- read_wide_temp(data_AHW_path)

# Preprocess data for plotting (using the 'CE3' column as temperature data)
prepare_plotdata <- function(data) {
  # Use the column named "CE3" as the temperature column
  plotdata <- data[, c("t", "CE3")]  # Use the 't' column for dates and 'CE3' for temperature
  names(plotdata)[2] <- 'temp'  # Rename 'CE3' to 'temp'
  plotdata$model <- "CE3"  # Assign the model name "CE3"
  
  return(plotdata)
}

plotdata_RHW <- prepare_plotdata(data_RHW)
plotdata_AHW <- prepare_plotdata(data_AHW)

# -----------------------------
# 3) Helper: Detect Events
# -----------------------------
detect_heatwave_events <- function(data, pctile = 90, min_duration = 5) {
  detected_events <- detect_event(ts2clm(data, pctile = pctile, climatologyPeriod = c("1981-01-01", "2019-12-31")),
                                  minDuration = min_duration)
  return(detected_events)
}

RHW_events <- detect_heatwave_events(plotdata_RHW, min_duration = 5)
AHW_events <- detect_heatwave_events(plotdata_AHW, min_duration = 3)

# Filter data for 2019 (5-year focus period)
filter_by_year <- function(data, start_date, end_date) {
  filtered_data <- data %>% filter(t >= as.Date(start_date) & t <= as.Date(end_date))
  return(filtered_data)
}

focus_year_start <- "2019-01-01"
focus_year_end <- "2019-12-31"
RHW_2019 <- filter_by_year(RHW_events$climatology, focus_year_start, focus_year_end)
AHW_2019 <- filter_by_year(AHW_events$climatology, focus_year_start, focus_year_end)

# Add event type for distinction
RHW_2019 <- RHW_2019 %>% mutate(event_type = "RHW")
AHW_2019 <- AHW_2019 %>% mutate(event_type = "AHW")

# -----------------------------
# 4) Main Plot: Temperature and Events
# -----------------------------
# Updated line style to use `linewidth` instead of `size`
main_plot <- ggplot() +
  geom_flame(data = AHW_2019, n = 3, n_gap = 2, aes(x = t, y = temp, y2 = thresh, fill = event_type)) +
  geom_flame(data = RHW_2019, n = 5, n_gap = 2, aes(x = t, y = temp, y2 = thresh, fill = event_type)) +
  
  # Set fill colors for events (red for AHW, blue for RHW)
  scale_fill_manual(name = "Event Type", values = c("AHW" = "darkred", "RHW" = "#1E90FF")) +
  
  # Temperature lines (thicker for better visibility)
  geom_line(data = AHW_2019, aes(x = t, y = temp, colour = "AT"), size = 2) +
  geom_line(data = RHW_2019, aes(x = t, y = temp, colour = "WT"), size = 2) +
  
  # Seasonal and threshold lines (more prominent with dashed style)
  geom_line(data = AHW_2019, aes(x = t, y = seas, colour = "Seasonal (AHW)"), size = 1.5) +
  geom_line(data = RHW_2019, aes(x = t, y = seas, colour = "Seasonal (RHW)"), size = 1.5) +
  geom_line(data = AHW_2019, aes(x = t, y = thresh, colour = "Threshold (AHW)"), linetype = "dashed", size = 1.5) +
  geom_line(data = RHW_2019, aes(x = t, y = thresh, colour = "Threshold (RHW)"), linetype = "dashed", size = 1.5) +
  
  xlim(as.Date(focus_year_start), as.Date(focus_year_end)) +
  
  # Line color settings
  scale_colour_manual(name = "Line Type", 
                      values = c("AT" = "red", "WT" = "#87CEFA", 
                                 "Seasonal (RHW)" = "#1E90FF", "Seasonal (AHW)" = "red", 
                                 "Threshold (RHW)" = "blue", "Threshold (AHW)" = "darkred")) +
  
  # Axis labels and title (adjusted for readability)
  labs(title = "(a)", y = expression(paste("Temperature [", degree, "C]")), x = "Date") +
  
  # Theme adjustments for cleaner look
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 20), 
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 16),
    panel.border = element_rect(color = "black", linewidth = 1.5),  # Change size to linewidth
    plot.title = element_text(hjust = 0, face = "bold", size = 30),
    plot.margin = margin(10, 10, 40, 10)  # Add margin to fit legend
  ) +
  
  # Highlight specific period with a grey rectangle
  annotate("rect", xmin = as.Date("2019-06-21"), xmax = as.Date("2019-07-07"), ymin = 8, ymax = 28, 
           alpha = 0.2, color = "black", fill = NA, size = 1) +
  
  # X-axis date formatting for better readability
  scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("1 month")) +
  
  # Adjust legend line size
  guides(colour = guide_legend(override.aes = list(size = 12))) 

# -----------------------------
# Print the plot
print(main_plot)

# -----------------------------
# 5) Zoom Plot for Detailed Focus
# -----------------------------
zoom_plot <- ggplot() +
  geom_flame(data = AHW_2019, n = 3, n_gap = 2, aes(x = t, y = temp, y2 = thresh, fill = event_type), alpha = 1) +
  geom_flame(data = RHW_2019, n = 5, n_gap = 2, aes(x = t, y = temp, y2 = thresh, fill = event_type), alpha = 1) +
  
  # Fill colors
  scale_fill_manual(name = "Event Type", values = c("AHW" = "darkred", "RHW" = "#1E90FF")) +
  
  # Zoomed-in window for 2019 summer
  xlim(as.Date("2019-06-21"), as.Date("2019-07-07")) +
  ylim(5, 28) +
  
  # Temperature lines (more prominent)
  geom_line(data = AHW_2019, aes(x = t, y = temp, colour = "AT"), size = 2) +
  geom_line(data = RHW_2019, aes(x = t, y = temp, colour = "WT"), size = 2) +
  
  # Seasonal and threshold lines (dashed)
  geom_line(data = AHW_2019, aes(x = t, y = seas, colour = "Seasonal (AHW)"), size = 1.5) +
  geom_line(data = RHW_2019, aes(x = t, y = seas, colour = "Seasonal (RHW)"), size = 1.5) +
  geom_line(data = AHW_2019, aes(x = t, y = thresh, colour = "Threshold (AHW)"), linetype = "dashed", size = 1.5) +
  geom_line(data = RHW_2019, aes(x = t, y = thresh, colour = "Threshold (RHW)"), linetype = "dashed", size = 1.5) +
  
  scale_colour_manual(name = "Line Type", values = c("AT" = "red", "WT" = "#87CEFA", "Seasonal (AHW)" = "red", "Seasonal (RHW)" = "#1E90FF", "Threshold (AHW)" = "darkred", "Threshold (RHW)" = "blue")) +
  theme_bw() +
  theme(
    legend.position = "right", 
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 18),
    legend.key.size = unit(2, "lines"),
    legend.spacing.y = unit(0.5, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 16),
    panel.border = element_rect(color = "black", size = 1.5),
    plot.title = element_text(hjust = 0, face = "bold", size = 22)
  ) +
  
  guides(colour = guide_legend(override.aes = list(size = 15))) 

# -----------------------------
# 6) Save main plot and zoom plot as separate images
# -----------------------------
ggsave(main_plot_path, main_plot, dpi = 300, width = 16, height = 10)
ggsave(zoom_plot_path, zoom_plot, dpi = 300, width = 16, height = 8)

# Optionally, display the plots
print(main_plot)
print(zoom_plot)