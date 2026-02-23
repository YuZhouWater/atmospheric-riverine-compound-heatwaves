setwd("..")
library(tidyverse)

# 读取数据
df <- read_delim("data/ARCH_frequency_by_time_gap/ARCH_distribution.txt", delim = "\t")

# 设置 period 顺序
df$Period <- factor(
  df$Period,
  levels = c("1981–1990", "1991–2000", "2001–2010", "2011–2019")
)

# 🔥 关键：深色放下面（从下往上堆叠）
df$FreqClass <- factor(
  df$FreqClass,
  levels = c("0–1", "1–2", "2–3", "3+")
)

# 绘图
p <- ggplot(df, aes(x = Period, y = Percentage, fill = FreqClass)) +
  geom_bar(stat = "identity", width = 0.8, color = "white") +
  geom_text(
    aes(label = sprintf("%.1f%%", Percentage)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "3+"  = "#5A189A",
      "2–3" = "#7B68EE",
      "1–2" = "#B8B8D8",
      "0–1" = "#D9D9E8"
    ),
    name = "Annual\nARCH\nfrequency"
  ) +
  labs(
    x = "Period",
    y = "Percentage (%)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    legend.position = "right"
  )

print(p)

# 保存
ggsave("results/ARCH_distribution.png", p, width = 8, height = 6, dpi = 600)