# ============================================================
# This script performs atmospheric heatwave detection for 796 rivers.
# Running the full dataset may take approximately 10–20 minutes
# on standard CPU resources provided by Code Ocean.
#
# Outputs:
#   1) Annual event count (frequency)
#   2) Annual total duration (days)
#   3) Annual mean intensity (℃)
# ============================================================

setwd("..")
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(heatwaveR)
})

# -----------------------------
# 0) Paths (Code Ocean friendly)
# -----------------------------
outdir <- "results"
dir.create(outdir, showWarnings = FALSE)

# -----------------------------
# 1) Read data
# -----------------------------
# Expect: first column is date (t), other columns are temperature series (models)
data1 <- read_csv("data/Tmax_US_LamaH.csv")

# Standardize first column name to "t"
names(data1)[1] <- "t"

# Convert wide -> long: columns [2:n] become different "model"
plotdata <- data1 %>%
  pivot_longer(
    cols = -t,
    names_to = "model",
    values_to = "temp"
  ) %>%
  mutate(t = as.Date(t))

# -----------------------------
# 2) allocate event duration across years
# -----------------------------
process_heatwave_events <- function(event_df, years) {
  results <- data.frame(year = years, duration = rep(0, length(years)))

  event_df <- event_df %>%
    mutate(
      date_start = as.Date(date_start),
      date_end   = as.Date(date_end)
    )

  for (row in seq_len(nrow(event_df))) {

    event_start <- event_df$date_start[row]
    event_end   <- event_df$date_end[row]
    start_year  <- lubridate::year(event_start)
    end_year    <- lubridate::year(event_end)

    if (start_year == end_year) {
      results$duration[results$year == start_year] <-
        results$duration[results$year == start_year] + event_df$duration[row]
    } else {
      for (yr in seq(start_year, end_year)) {
        if (yr == start_year) {
          year_end <- as.Date(paste0(yr, "-12-31"))
          days_in_year <- as.numeric(difftime(year_end, event_start, units = "days")) + 1
        } else if (yr == end_year) {
          year_start <- as.Date(paste0(yr, "-01-01"))
          days_in_year <- as.numeric(difftime(event_end, year_start, units = "days")) + 1
        } else {
          # Middle years: assume full year; leap years are negligible here
          days_in_year <- 365
        }

        results$duration[results$year == yr] <-
          results$duration[results$year == yr] + days_in_year
      }
    }
  }

  results
}

# -----------------------------
# 3) Study period
# -----------------------------
years <- 1981:2019
full_years <- tibble(year = as.character(years))

Ahw_annual_frequency      <- tibble(year = years)
Ahw_annual_duration       <- tibble(year = years)
Ahw_annual_mean_intensity <- tibble(year = years)

models <- unique(plotdata$model)

# -----------------------------
# 4) Main loop over models
# -----------------------------
for (m in models) {

  df_for <- plotdata %>%
    filter(model == m) %>%
    select(t, temp)

  # Detect events using 90th percentile threshold (climatology 1981–2019)
  Ahw <- detect_event(
    ts2clm(
      df_for,
      pctile = 90,
      climatologyPeriod = c("1981-01-01", "2019-12-31")
    ),
    maxGap = 2,
    minDuration = 3
  )

  # 4.1 Annual event frequency: number of events per year
  yearly_freq <- Ahw$event %>%
    mutate(year = year(as.Date(date_start))) %>%
    count(year, name = "event_count") %>%
    mutate(year = as.integer(year))

  Ahw_annual_frequency <- Ahw_annual_frequency %>%
    left_join(yearly_freq, by = "year") %>%
    mutate(event_count = replace_na(event_count, 0)) %>%
    select(-event_count) %>%
    mutate(!!m := yearly_freq$event_count[match(year, yearly_freq$year)] %>% replace_na(0))

  # 4.2 Annual mean intensity (℃)
  yearly_int <- Ahw$event %>%
    mutate(
      year = format(as.Date(date_start), "%Y"),
      intensity_mean = as.numeric(intensity_mean)
    ) %>%
    group_by(year) %>%
    summarise(anom_mean = sum(intensity_mean, na.rm = TRUE), .groups = "drop")

  yearly_int_full <- full_years %>%
    left_join(yearly_int, by = "year") %>%
    mutate(anom_mean = replace_na(anom_mean, 0))

  Ahw_annual_mean_intensity[[m]] <- yearly_int_full$anom_mean

  # 4.3 Annual duration (days): allocate event duration across years
  dur_year <- process_heatwave_events(Ahw$event, years)
  Ahw_annual_duration[[m]] <- dur_year$duration
}

# -----------------------------
# 5) Save outputs
# -----------------------------
write_csv(Ahw_annual_frequency,      file.path(outdir, "AHW_annual_frequency.csv"))
write_csv(Ahw_annual_duration,       file.path(outdir, "AHW_annual_duration.csv"))
write_csv(Ahw_annual_mean_intensity, file.path(outdir, "AHW_annual_mean_intensity.csv"))
