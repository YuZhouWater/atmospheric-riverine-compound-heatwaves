# ============================================================
# This script visualizes future heatwave duration under SSP2-4.5 and SSP5-8.5,
# and calculates compound event proportions for 1981–1990 and 2091–2100.
# ============================================================
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(egg)

repo_dir <- "D:/论文1/Github"

data_path <- file.path(
  repo_dir,
  "data",
  "future_projection",
  "future_heatwave_duration_summary_1981_2100.csv"
)

duration_summary <- read_csv(data_path, show_col_types = FALSE)

duration_summary <- duration_summary %>%
  mutate(
    event_type = recode(
      event_type,
      "AHW_duration" = "AHWs_duration",
      "RHW_duration" = "RHWs_duration",
      "ARCH_AHW_duration" = "Compound_AHWs_duration",
      "ARCH_RHW_duration" = "Compound_RHWs_duration"
    ),
    scenario = recode(
      scenario,
      "ssp245" = "SSP2-4.5",
      "ssp585" = "SSP5-8.5"
    )
  )

years <- sort(unique(duration_summary$year))

get_series <- function(df, event_name, scenario_name) {
  df %>%
    filter(event_type == event_name, scenario == scenario_name) %>%
    arrange(year) %>%
    select(year, mean_value = mean_duration)
}


data_atmospheric_ssp245 <- get_series(
  duration_summary,
  event_name = "AHWs_duration",
  scenario_name = "SSP2-4.5"
)

data_atmospheric_ssp585 <- get_series(
  duration_summary,
  event_name = "AHWs_duration",
  scenario_name = "SSP5-8.5"
)

data_coupled_Ahw_ssp245 <- get_series(
  duration_summary,
  event_name = "Compound_AHWs_duration",
  scenario_name = "SSP2-4.5"
)

data_coupled_Ahw_ssp585 <- get_series(
  duration_summary,
  event_name = "Compound_AHWs_duration",
  scenario_name = "SSP5-8.5"
)

stacked_data585 <- data_atmospheric_ssp585 %>%
  rename(Atmospheric = mean_value) %>%
  left_join(
    data_coupled_Ahw_ssp585 %>%
      rename(Coupled = mean_value),
    by = "year"
  ) %>%
  mutate(
    Coupled = ifelse(is.na(Coupled), 0, Coupled),
    Overlap_SSP5_8.5 = pmin(Atmospheric, Coupled),
    Atmospheric_Only_SSP5_8.5 = Atmospheric - Overlap_SSP5_8.5
  )

stacked_data245 <- data_atmospheric_ssp245 %>%
  rename(Atmospheric = mean_value) %>%
  left_join(
    data_coupled_Ahw_ssp245 %>%
      rename(Coupled = mean_value),
    by = "year"
  ) %>%
  mutate(
    Coupled = ifelse(is.na(Coupled), 0, Coupled),
    Overlap_SSP2_4.5 = pmin(Atmospheric, Coupled),
    Atmospheric_Only_SSP2_4.5 = Atmospheric - Overlap_SSP2_4.5
  )

stacked_data_long585 <- stacked_data585 %>%
  pivot_longer(
    cols = c("Atmospheric_Only_SSP5_8.5", "Overlap_SSP5_8.5"),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  mutate(Scenario = "SSP5-8.5")

stacked_data_long245 <- stacked_data245 %>%
  pivot_longer(
    cols = c("Atmospheric_Only_SSP2_4.5", "Overlap_SSP2_4.5"),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  mutate(Scenario = "SSP2-4.5")

stacked_data_long <- bind_rows(stacked_data_long585, stacked_data_long245)

p2 <- ggplot() +
  geom_bar(
    data = stacked_data_long %>% filter(Scenario == "SSP5-8.5"),
    aes(x = year, y = Value, fill = Category),
    stat = "identity",
    position = "stack",
    color = "black",
    width = 0.95,
    alpha = 0.85
  ) +
  geom_bar(
    data = stacked_data_long %>% filter(Scenario == "SSP2-4.5"),
    aes(x = year, y = Value, fill = Category),
    stat = "identity",
    position = "stack",
    color = "black",
    width = 0.95,
    alpha = 0.85
  ) +
  scale_fill_manual(
    values = c(
      "Atmospheric_Only_SSP2_4.5" = "#FFDAB9",
      "Overlap_SSP2_4.5" = "#FF6347",
      "Atmospheric_Only_SSP5_8.5" = "pink",
      "Overlap_SSP5_8.5" = "#8B0000"
    ),
    labels = c(
      "Atmospheric_Only_SSP2_4.5" = "AHWs_SSP2_4.5",
      "Overlap_SSP2_4.5" = "Compound_AHWs_SSP2_4.5",
      "Atmospheric_Only_SSP5_8.5" = "AHWs_SSP5_8.5",
      "Overlap_SSP5_8.5" = "Compound_AHWs_SSP5_8.5"
    )
  ) +
  annotate(
    "text",
    x = 1980,
    y = 250,
    label = "(b)",
    size = 10,
    color = "black",
    fontface = "bold",
    hjust = 0
  ) +
  scale_x_continuous(
    limits = c(1980, 2100),
    breaks = seq(1980, 2100, by = 20),
    expand = c(0, 0)
  ) +
  labs(
    title = NULL,
    x = "Year",
    y = "Duration (d/a)",
    fill = NULL
  ) +
  theme_article() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.title.x = element_text(face = "bold", size = 24),
    axis.title.y = element_text(face = "bold", color = "black", size = 24),
    axis.text = element_text(size = 24, color = "black"),
    axis.line = element_line(color = "black", size = 0.8),
    legend.position = c(0.3, 0.8),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.key.spacing.y = unit(15, "pt"),
    plot.margin = ggplot2::margin(20, 20, 20, 20)
  )


rm(stacked_data585, stacked_data245, stacked_data_long585, stacked_data_long245, stacked_data_long)

data_river_ssp245 <- get_series(
  duration_summary,
  event_name = "RHWs_duration",
  scenario_name = "SSP2-4.5"
)

data_river_ssp585 <- get_series(
  duration_summary,
  event_name = "RHWs_duration",
  scenario_name = "SSP5-8.5"
)

data_coupled_Rhw_ssp245 <- get_series(
  duration_summary,
  event_name = "Compound_RHWs_duration",
  scenario_name = "SSP2-4.5"
)

data_coupled_Rhw_ssp585 <- get_series(
  duration_summary,
  event_name = "Compound_RHWs_duration",
  scenario_name = "SSP5-8.5"
)

stacked_data585 <- data_river_ssp585 %>%
  rename(Atmospheric = mean_value) %>%
  left_join(
    data_coupled_Rhw_ssp585 %>%
      rename(Coupled = mean_value),
    by = "year"
  ) %>%
  mutate(
    Coupled = ifelse(is.na(Coupled), 0, Coupled),
    Overlap_SSP5_8.5 = pmin(Atmospheric, Coupled),
    Atmospheric_Only_SSP5_8.5 = Atmospheric - Overlap_SSP5_8.5
  )

stacked_data245 <- data_river_ssp245 %>%
  rename(Atmospheric = mean_value) %>%
  left_join(
    data_coupled_Rhw_ssp245 %>%
      rename(Coupled = mean_value),
    by = "year"
  ) %>%
  mutate(
    Coupled = ifelse(is.na(Coupled), 0, Coupled),
    Overlap_SSP2_4.5 = pmin(Atmospheric, Coupled),
    Atmospheric_Only_SSP2_4.5 = Atmospheric - Overlap_SSP2_4.5
  )

stacked_data_long585 <- stacked_data585 %>%
  pivot_longer(
    cols = c("Atmospheric_Only_SSP5_8.5", "Overlap_SSP5_8.5"),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  mutate(Scenario = "SSP5-8.5")

stacked_data_long245 <- stacked_data245 %>%
  pivot_longer(
    cols = c("Atmospheric_Only_SSP2_4.5", "Overlap_SSP2_4.5"),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  mutate(Scenario = "SSP2-4.5")

stacked_data_long <- bind_rows(stacked_data_long585, stacked_data_long245)

p4 <- ggplot() +
  geom_bar(
    data = stacked_data_long %>% filter(Scenario == "SSP5-8.5"),
    aes(x = year, y = Value, fill = Category),
    stat = "identity",
    position = "stack",
    color = "black",
    width = 0.95,
    alpha = 0.85
  ) +
  geom_bar(
    data = stacked_data_long %>% filter(Scenario == "SSP2-4.5"),
    aes(x = year, y = Value, fill = Category),
    stat = "identity",
    position = "stack",
    color = "black",
    width = 0.95,
    alpha = 0.85
  ) +
  scale_fill_manual(
    values = c(
      "Atmospheric_Only_SSP2_4.5" = "#BBDEFB",
      "Overlap_SSP2_4.5" = "#1976D2",
      "Atmospheric_Only_SSP5_8.5" = "#64B5F6",
      "Overlap_SSP5_8.5" = "darkblue"
    ),
    labels = c(
      "Atmospheric_Only_SSP2_4.5" = "RHWs_SSP2_4.5",
      "Overlap_SSP2_4.5" = "Compound_RHWs_SSP2_4.5",
      "Atmospheric_Only_SSP5_8.5" = "RHWs_SSP5_8.5",
      "Overlap_SSP5_8.5" = "Compound_RHWs_SSP5_8.5"
    )
  ) +
  annotate(
    "text",
    x = 1980,
    y = 300,
    label = "(d)",
    size = 10,
    color = "black",
    fontface = "bold",
    hjust = 0
  ) +
  scale_x_continuous(
    limits = c(1980, 2100),
    breaks = seq(1980, 2100, by = 20),
    expand = c(0, 0)
  ) +
  labs(
    title = NULL,
    x = "Year",
    y = "Duration (d/a)",
    fill = NULL
  ) +
  theme_article() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.title.x = element_text(face = "bold", size = 24),
    axis.title.y = element_text(face = "bold", color = "black", size = 24),
    axis.text = element_text(size = 24, color = "black"),
    axis.line = element_line(color = "black", size = 0.8),
    legend.position = c(0.3, 0.8),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    legend.spacing.y = unit(0.4, "cm"),
    legend.key.size = unit(1.5, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.spacing.y = unit(15, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.margin = ggplot2::margin(20, 20, 20, 20)
  )

p2
p4

pct_result <- duration_summary %>%
  filter(
    (year >= 1981 & year <= 1990) |
      (year >= 2091 & year <= 2100)
  ) %>%
  mutate(
    period = case_when(
      year >= 1981 & year <= 1990 ~ "1981-1990",
      year >= 2091 & year <= 2100 ~ "2091-2100"
    )
  ) %>%
  select(year, scenario, period, event_type, mean_duration) %>%
  pivot_wider(
    names_from = event_type,
    values_from = mean_duration
  ) %>%
  group_by(scenario, period) %>%
  summarise(
    AHW_pct = round(
      sum(Compound_AHWs_duration, na.rm = TRUE) /
        sum(AHWs_duration, na.rm = TRUE) * 100, 1
    ),
    RHW_pct = round(
      sum(Compound_RHWs_duration, na.rm = TRUE) /
        sum(RHWs_duration, na.rm = TRUE) * 100, 1
    ),
    .groups = "drop"
  )

print(pct_result)
