setwd("..")
library(readr) 
library(dplyr)
library(ggplot2)

# ===============================
# 读取数据（Code Ocean 路径）
# ===============================
data1 <- read_csv("data/attributes/arrtibute796.csv")

data = read_csv("data/ARCH_frequency_by_time_gap/time_gap=5.csv")
data2 <- dplyr::select(data, -year, -MEAN, -STD)

# 2) 计算剩余每列的均值（忽略NA）
col_means <- sapply(data2, function(x) mean(x, na.rm = TRUE))
# 合并所需变量
merged_data <- data.frame(
  ele_mt_sav = data1$ele_mt_sav,
  annual_couple_event = col_means
)

# ===============================
# 海拔分组
# ===============================
merged_data <- merged_data %>%
  mutate(
    ele_mt_sav_group = cut(
      ele_mt_sav,
      breaks = c(0, 1000, 2000, 3000, Inf),
      labels = c("0–1000 m", "1000–2000 m", "2000–3000 m", "≥3000 m"),
      right = FALSE
    )
  )

# 计算每个海拔区间的均值
mean_values <- merged_data %>%
  group_by(ele_mt_sav_group) %>%
  summarise(
    mean_event = mean(annual_couple_event, na.rm = TRUE),
    .groups = "drop"
  )

# ===============================
# 绘图
# ===============================
p1 <- ggplot(merged_data, aes(x = annual_couple_event, fill = ele_mt_sav_group)) +
  geom_density(alpha = 0.6) +
  geom_vline(
    data = mean_values,
    aes(xintercept = mean_event, color = ele_mt_sav_group),
    linetype = "dashed",
    size = 1
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Annual Couple Event",
    y = "Density",
    fill = "Elevation (m)"
  ) +
  scale_y_continuous(
    limits = c(0, 2.2),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 2.7),
    expand = c(0, 0)
  ) +
  theme_classic() +
theme(
  axis.title.x = element_text(face = "bold", size = 24),
  axis.title.y = element_text(face = "bold", size = 24),
  axis.text = element_text(size = 24, color = "black"),
  axis.line = element_line(color = "black", linewidth = 0.8),

  legend.title = element_text(size = 22, face = "bold"),   # ← 调大这里
  legend.text = element_text(size = 18),
  legend.key.size = unit(2, "lines"),

  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.margin = margin(20, 20, 20, 20)
)
# ===============================
# 保存到 Code Ocean 输出目录
# ===============================
ggsave("/results/ARCH_elevation_density.png",
       p1,
       dpi = 600,
       width = 12,
       height = 10)
