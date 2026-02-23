## map DO mean
## US + LamaH
## 2021-12-22

setwd("..")
library(readr)
library(dplyr)
library(sf)
sf::sf_use_s2(FALSE)
library(tmap)
library(tigris)

# global setting
options(stringsAsFactors = FALSE)

# clear variable and console
rm(list = ls())  
cat("\014")      

temporal_AT = read_csv("data/Tmax_US_LamaH.csv", 
                       col_names = TRUE, cols(.default = col_double(), t = col_date("%Y/%m/%d")))

data = read_csv("data/ARCH_frequency_by_time_gap/time_gap=5.csv")

ID_US = colnames(temporal_AT[2:581])
ID_LamaH = colnames(temporal_AT[582:797])

data2 <- dplyr::select(data, -year, -MEAN, -STD)

# 2) 计算剩余每列的均值（忽略NA）
col_means <- sapply(data2, function(x) mean(x, na.rm = TRUE))


mean_US = data.frame(ID = ID_US, mean_T = col_means[1:580])
mean_LamaH = data.frame(ID = ID_LamaH, mean_T = col_means[581:796])

# loading topo ------------------------------------------------------------
topo_US = read_csv("data/attributes/attribute_topo_site580.csv", 
                   col_names = TRUE, col_types = cols(.default = "d", ID = col_character()))

topo_LamaH = read_csv("data/attributes/attribute_topo_site216.csv", 
                      col_names = TRUE, col_types = cols(.default = "d", ID = col_character()))

# add coordinates and mean_T
map_US = left_join(topo_US, mean_US, by = "ID")
map_LamaH = left_join(topo_LamaH, mean_LamaH, by = "ID")

# 保存经纬度信息作为普通列，然后再创建sf对象
gage_US = map_US %>% 
  mutate(
    lon_deg_save = lon_deg,
    lat_deg_save = lat_deg
  ) %>%
  st_as_sf(coords = c("lon_deg", "lat_deg"), crs = 4269)

gage_LamaH = map_LamaH %>% 
  mutate(
    lon_deg_save = lon_deg,
    lat_deg_save = lat_deg
  ) %>%
  st_as_sf(coords = c("lon_deg", "lat_deg"), crs = 4269)

# mapping US --------------------------------------------------------------
# NAD83, EPSG = 4269 (unprojected)
# USA Contiguous Albers Equal Area Conic, EPSG = 5070 (projected)
# change the tempdir() location

tempdir <- function() "D:/Rtemp"
unlockBinding("tempdir", baseenv())
utils::assignInNamespace("tempdir", tempdir, ns = "base", envir = baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())

us_geo <- tigris::states(class = "sf")

contiguous_states <- us_geo %>% 
  filter(REGION != 9) %>% 
  filter(STUSPS != "AK") %>% 
  filter(STUSPS != "HI")

data2 <- read_csv("data/attributes/arrtibute796.csv")

# 合并列 - 保持sf对象，只添加新列
gage_US$ele_mt_sav <- data2$ele_mt_sav[1:580]
gage_LamaH$ele_mt_sav <- data2$ele_mt_sav[581:796]

# 划分区间并增加新列
gage_US <- gage_US %>%
  mutate(ele_mt_sav_category = case_when(
    ele_mt_sav <= 1000 ~ 1,        
    ele_mt_sav > 1000 & ele_mt_sav <= 2000 ~ 2,
    ele_mt_sav > 2000 & ele_mt_sav <= 3000 ~ 4,
    ele_mt_sav > 3000 ~ 6
  ))

gage_LamaH <- gage_LamaH %>%
  mutate(ele_mt_sav_category = case_when(
    ele_mt_sav <= 1000 ~ 1,        
    ele_mt_sav > 1000 & ele_mt_sav <= 2000 ~ 2,
    ele_mt_sav > 2000 & ele_mt_sav <= 3000 ~ 4,
    ele_mt_sav > 3000 ~ 6
  ))

# 查看新增列
head(gage_US)

map1 = tm_shape(contiguous_states, projection = 5070) +
  tm_fill(col = "#F2F2F2", alpha = 1) +
  tm_borders(col = "black", lwd = 1, alpha = 1) +
  tm_shape(gage_US, projection = 5070) +
  tm_bubbles(
    size = "ele_mt_sav_category",
    scale = 3,
    col = "mean_T",
    style = "fixed",
    breaks = c(0, 1, 1.5, 2, 3),
    labels = c("0 to 1", "1 to 1.5", "1.5 to 2", "2 to 3"),
    palette = c("#dadaeb", "#bcbddc", "#756bb1", "#4a1486"),
    border.col = "black",
    legend.size.is.portrait = TRUE,
    title.col = "Annual\nfrequency",
    title.size = "Elevation (m)"
  ) +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.stack = "horizontal",
    legend.text.size = 1.2,
    legend.title.size = 1.5,
    legend.width = 1.2,
    legend.show = FALSE
  )

map1
tmap_save(map1, filename = "results/map_US_ARCH.png", dpi = 600)
