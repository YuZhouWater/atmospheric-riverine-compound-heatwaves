# ============================================================
# Task:
# 1) Detect RHW (riverine heatwaves)
# 2) Detect AHW (atmospheric heatwaves)
# 3) Identify ARCH events (RHW overlapping AHW within time_gap_days)
# 4) Export annual ARCH frequency for each series
# ============================================================
setwd("..")
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(heatwaveR)
})

# ============================================================
# 1) Read input data (wide format: first column = date)
# ============================================================
data1 <- read_csv(
  "data/temp_WT.csv",
  col_names = TRUE,
  cols(.default = col_double(), t = col_date(format = "%Y/%m/%d"))
)
data1 <- data.frame(data1)

data2 <- read_csv(
  "data/Tmax_US_LamaH.csv",
  col_names = TRUE,
  cols(.default = col_double(), t = col_date(format = "%Y/%m/%d"))
)
data2 <- data.frame(data2)

# ============================================================
# 2) Convert wide format to long format (t, temp, model)
# ============================================================

plotdata <- data1[, 1:2]
names(plotdata)[2] <- "temp"
plotdata$model <- names(data1)[2]

for (i in 3:797) {
  onedata <- data1[, c(1, i)]
  names(onedata)[2] <- "temp"
  onedata$model <- names(data1)[i]
  plotdata <- rbind(plotdata, onedata)
}

plotdata2 <- data2[, 1:2]
names(plotdata2)[2] <- "temp"
plotdata2$model <- names(data2)[2]

for (i in 3:797) {
  onedata2 <- data2[, c(1, i)]
  names(onedata2)[2] <- "temp"
  onedata2$model <- names(data2)[i]
  plotdata2 <- rbind(plotdata2, onedata2)
}

# ============================================================
# 3) Initialize containers
# ============================================================
RR_events <- list()
years <- 1981:2019
df <- data.frame(year = years)

# ============================================================
# 4) Main loop across sites
# ============================================================

for (i in 1:796) {
  
  df_for  <- plotdata  %>% filter(model == unique(plotdata$model)[i])
  df_for2 <- plotdata2 %>% filter(model == unique(plotdata2$model)[i])
  
  # Detect riverine heatwaves (RHW)
  RHW <- detect_event(
    ts2clm(df_for, pctile = 90,
           climatologyPeriod = c("1981-01-01", "2019-12-31")),
    minDuration = 5
  )
  
  # Detect atmospheric heatwaves (AHW)
  AHW <- detect_event(
    ts2clm(df_for2, pctile = 90,
           climatologyPeriod = c("1981-01-01", "2019-12-31")),
    minDuration = 3
  )
  
  stAHWEvent <- AHW$event$date_start
  edAHWEvent <- AHW$event$date_end
  
  nRunoffEvent <- length(RHW$event$event_no)
  
  nMRE   <- rep(NA, nRunoffEvent)
  idxMRE <- matrix(NA, nrow = nRunoffEvent, ncol = 10)
  
  for (j in 1:nRunoffEvent) {
    
    curr_RHW <- RHW$event[j, ]
    
    # Search window: 120 hours before RHW start to RHW end
    swb <- curr_RHW$date_start - hours(120)
    swe <- curr_RHW$date_end
    
    # Determine overlap between AHW and search window
    col_1 <- as.numeric(sign(as.Date(stAHWEvent) - as.Date(swb)) +
                          sign(as.Date(stAHWEvent) - as.Date(swe)))
    
    col_2 <- as.numeric(sign(as.Date(edAHWEvent) - as.Date(swb)) +
                          sign(as.Date(edAHWEvent) - as.Date(swe)))
    
    idx <- col_1 == 0 | col_2 == 0 | (col_1 * col_2) < 0
    tmp <- which(idx)
    
    nMRE[j] <- length(tmp)
    
    if (nMRE[j] > 0) {
      idxMRE[j, 1:nMRE[j]] <- tmp
    }
    
    curr_AHW <- AHW$event[idx, ]
    curr_AHW <- bind_rows(curr_AHW)
    
    if (length(curr_AHW) > 1) {
      RR_events[[j]] <- list(AHW = curr_AHW, RHW = curr_RHW)
    } else {
      RR_events[[j]] <- list(AHW = NA, RHW = curr_RHW)
    }
  }
  
  # ============================================================
  # 5) Compute annual ARCH frequency
  # ============================================================
  
  idxMRE <- as.data.frame(idxMRE)
  non_na_indices <- which(!is.na(idxMRE$V1))
  
  ARCHW_data_start <- RHW$event$date_start[non_na_indices]
  ARCHW_df <- data.frame(date_start = ARCHW_data_start)
  
  yearly_frequency <- ARCHW_df %>%
    mutate(year = format(date_start, "%Y")) %>%
    group_by(year) %>%
    summarise(frequency = n(), .groups = "drop")
  
  df[i + 1] <- rep(0, length(years))
  
  for (year in yearly_frequency$year) {
    year_idx <- which(df$year == as.numeric(year))
    if (length(year_idx) > 0) {
      df[year_idx, i + 1] <-
        yearly_frequency$frequency[yearly_frequency$year == year]
    }
  }
}

# ============================================================
# 6) Rename columns
# ============================================================
colnames(df)[1] <- "year"
colnames(df)[2:581]  <- paste0("US", 1:580)
colnames(df)[582:797] <- paste0("CE", 1:216)

# ============================================================
# 7) Save output
# ============================================================
file_path <- "results/ARCH_frequency_time_gap=5.csv"
write.csv(df, file = file_path, row.names = FALSE)