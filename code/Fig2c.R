setwd("..")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

data1 <- read_csv(
  "data/Watershed_attraibutes/arrtibute796.csv",
  show_col_types = FALSE
)

data <- read_csv(
  "data/ARCH_frequency_by_time_gap/time_gap=5.csv",
  show_col_types = FALSE
)

data2 <- data %>%
  select(-any_of(c("year", "MEAN", "STD")))

col_means <- sapply(data2, function(x) mean(x, na.rm = TRUE))

merged_data <- data.frame(
  ele_mt_sav = data1$ele_mt_sav,
  annual_couple_event = as.numeric(col_means)
)

merged_data <- merged_data %>%
  mutate(
    ele_mt_sav_group = cut(
      ele_mt_sav,
      breaks = c(0, 1000, 2000, 3000, Inf),
      labels = c("0–1000 m", "1000–2000 m", "2000–3000 m", "≥3000 m"),
      right = FALSE
    )
  ) %>%
  filter(
    !is.na(ele_mt_sav_group),
    !is.na(annual_couple_event)
  )

summary_values <- merged_data %>%
  group_by(ele_mt_sav_group) %>%
  summarise(
    n = n(),
    mean_event = mean(annual_couple_event, na.rm = TRUE),
    median_event = median(annual_couple_event, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_values)

p1 <- ggplot(
  merged_data,
  aes(
    x = ele_mt_sav_group,
    y = annual_couple_event,
    fill = ele_mt_sav_group
  )
) +
  geom_boxplot(
    width = 0.62,
    alpha = 0.75,
    linewidth = 0.9,
    outlier.shape = 21,
    outlier.size = 1.8,
    outlier.alpha = 0.65
  ) +
  geom_jitter(
    aes(color = ele_mt_sav_group),
    width = 0.16,
    size = 1.4,
    alpha = 0.35,
    show.legend = FALSE
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 4,
    fill = "white",
    color = "black",
    stroke = 0.9,
    show.legend = FALSE
  ) +
  geom_text(
    data = summary_values,
    aes(
      x = ele_mt_sav_group,
      y = 2.58,
      label = paste0("n = ", n)
    ),
    inherit.aes = FALSE,
    size = 6,
    color = "black"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Elevation (m)",
    y = "Annual compound event frequency",
    fill = "Elevation (m)"
  ) +
  coord_cartesian(
    ylim = c(0, 2.7)
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(face = "bold", size = 24),
    axis.title.y = element_text(face = "bold", size = 24),
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 24, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.16, "cm"),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(20, 20, 20, 20)
  )

p1

dir.create("results/Fig2", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "results/Fig2/Fig2c_boxplot.png",
  plot = p1,
  dpi = 600,
  width = 12,
  height = 10,
  bg = "white"
)
