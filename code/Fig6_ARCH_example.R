setwd("..")
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
dir.create("results/Fig6", recursive = TRUE, showWarnings = FALSE)
main_plot_path <- "results/Fig6/Fig6_ARCH_main.png"
zoom_plot_path <- "results/Fig6/Fig6_ARCH_zoom.png"

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

data_RHW <- read_wide_temp(data_RHW_path)
data_AHW <- read_wide_temp(data_AHW_path)

prepare_plotdata <- function(data) {
  plotdata <- data[, c("t", "CE3")] 
  names(plotdata)[2] <- 'temp' 
  plotdata$model <- "CE3" 
  
  return(plotdata)
}

plotdata_RHW <- prepare_plotdata(data_RHW)
plotdata_AHW <- prepare_plotdata(data_AHW)


detect_heatwave_events <- function(data, pctile = 90, min_duration = 5) {
  detected_events <- detect_event(ts2clm(data, pctile = pctile, climatologyPeriod = c("1981-01-01", "2019-12-31")),
                                  minDuration = min_duration)
  return(detected_events)
}

RHW_events <- detect_heatwave_events(plotdata_RHW, min_duration = 5)
AHW_events <- detect_heatwave_events(plotdata_AHW, min_duration = 3)


filter_by_year <- function(data, start_date, end_date) {
  filtered_data <- data %>% filter(t >= as.Date(start_date) & t <= as.Date(end_date))
  return(filtered_data)
}

focus_year_start <- "2019-01-01"
focus_year_end <- "2019-12-31"
RHW_2019 <- filter_by_year(RHW_events$climatology, focus_year_start, focus_year_end)
AHW_2019 <- filter_by_year(AHW_events$climatology, focus_year_start, focus_year_end)

RHW_2019 <- RHW_2019 %>% mutate(event_type = "RHW")
AHW_2019 <- AHW_2019 %>% mutate(event_type = "AHW")

main_plot <- ggplot() +
  geom_flame(data = AHW_2019, n = 3, n_gap = 2, aes(x = t, y = temp, y2 = thresh, fill = event_type)) +
  geom_flame(data = RHW_2019, n = 5, n_gap = 2, aes(x = t, y = temp, y2 = thresh, fill = event_type)) +
  
  scale_fill_manual(name = "Event Type", values = c("AHW" = "darkred", "RHW" = "#1E90FF")) +

  geom_line(data = AHW_2019, aes(x = t, y = temp, colour = "AT"), size = 2) +
  geom_line(data = RHW_2019, aes(x = t, y = temp, colour = "WT"), size = 2) +
  

  geom_line(data = AHW_2019, aes(x = t, y = seas, colour = "Seasonal (AHW)"), size = 1.5) +
  geom_line(data = RHW_2019, aes(x = t, y = seas, colour = "Seasonal (RHW)"), size = 1.5) +
  geom_line(data = AHW_2019, aes(x = t, y = thresh, colour = "Threshold (AHW)"), linetype = "dashed", size = 1.5) +
  geom_line(data = RHW_2019, aes(x = t, y = thresh, colour = "Threshold (RHW)"), linetype = "dashed", size = 1.5) +
  
  xlim(as.Date(focus_year_start), as.Date(focus_year_end)) +
  
  scale_colour_manual(name = "Line Type", 
                      values = c("AT" = "red", "WT" = "#87CEFA", 
                                 "Seasonal (RHW)" = "#1E90FF", "Seasonal (AHW)" = "red", 
                                 "Threshold (RHW)" = "blue", "Threshold (AHW)" = "darkred")) +
  
 
  labs(title = "(a)", y = expression(paste("Temperature [", degree, "C]")), x = "Date") +
  

  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom", 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 20), 
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 16),
    panel.border = element_rect(color = "black", linewidth = 1.5), 
    plot.title = element_text(hjust = 0, face = "bold", size = 30),
    plot.margin = margin(10, 10, 40, 10) 
  ) +
  
  annotate("rect", xmin = as.Date("2019-06-21"), xmax = as.Date("2019-07-07"), ymin = 8, ymax = 28, 
           alpha = 0.2, color = "black", fill = NA, size = 1) +

  scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("1 month")) +
  
  guides(colour = guide_legend(override.aes = list(size = 12))) 

print(main_plot)

zoom_plot <- ggplot() +
  geom_flame(data = AHW_2019, n = 3, n_gap = 2, aes(x = t, y = temp, y2 = thresh, fill = event_type), alpha = 1) +
  geom_flame(data = RHW_2019, n = 5, n_gap = 2, aes(x = t, y = temp, y2 = thresh, fill = event_type), alpha = 1) +

  scale_fill_manual(name = "Event Type", values = c("AHW" = "darkred", "RHW" = "#1E90FF")) +

  xlim(as.Date("2019-06-21"), as.Date("2019-07-07")) +
  ylim(5, 28) +

  geom_line(data = AHW_2019, aes(x = t, y = temp, colour = "AT"), size = 2) +
  geom_line(data = RHW_2019, aes(x = t, y = temp, colour = "WT"), size = 2) +

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

ggsave(main_plot_path, main_plot, dpi = 300, width = 16, height = 10)
ggsave(zoom_plot_path, zoom_plot, dpi = 300, width = 16, height = 8)