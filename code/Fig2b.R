setwd("..")
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 读取数据
data <- read_csv("data/ARCH_frequency_by_time_gap/time_gap=5.csv") %>%
  select(-MEAN, -STD)

# 1. 按年份划分阶段
data_period <- data %>%
  mutate(
    Period = case_when(
      year >= 1981 & year <= 1990 ~ "1981–1990",
      year >= 1991 & year <= 2000 ~ "1991–2000",
      year >= 2001 & year <= 2010 ~ "2001–2010",
      year >= 2011 & year <= 2019 ~ "2011–2019",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Period))

# 2. 转成长表：保留每年每个站点的原始频率
data_long <- data_period %>%
  pivot_longer(
    cols = -c(year, Period),
    names_to = "Site",
    values_to = "ARCH_freq"
  ) %>%
  filter(!is.na(ARCH_freq))

# 3. 按区间分类
data_long <- data_long %>%
  mutate(
    Group = case_when(
      ARCH_freq >= 0 & ARCH_freq < 1 ~ "0–1",
      ARCH_freq >= 1 & ARCH_freq < 2 ~ "1–2",
      ARCH_freq >= 2 & ARCH_freq < 3 ~ "2–3",
      ARCH_freq >= 3 ~ "3+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Group))

# 4. 统计每个时期内，各区间所占比例
plot_data <- data_long %>%
  group_by(Period, Group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Period) %>%
  mutate(
    Percentage = n / sum(n) * 100,
    label = paste0(round(Percentage, 1), "%")
  ) %>%
  ungroup()

# 5. 设置顺序
plot_data$Period <- factor(
  plot_data$Period,
  levels = c("1981–1990", "1991–2000", "2001–2010", "2011–2019")
)

plot_data$Group <- factor(
  plot_data$Group,
  levels = c("0–1", "1–2", "2–3", "3+")
)

# 6. 绘图
p <- ggplot(plot_data, aes(x = Period, y = Percentage, fill = Group)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.6) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "0–1" = "#d9d9e8",
      "1–2" = "#b4b4d8",
      "2–3" = "#7e6bb3",
      "3+"  = "#4b007d"
    ),
    name = "Annual\nARCH\nfrequency"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    x = "Period",
    y = "Percentage (%)"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

print(p)

# 保存
dir.create("results/Fig2", recursive = TRUE, showWarnings = FALSE)
ggsave("results/Fig2/Fig2b.png", p, width = 8, height = 6, dpi = 600)
