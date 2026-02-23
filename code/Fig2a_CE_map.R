setwd("..")
library(readr)
library(dplyr)
library(sf)
sf::sf_use_s2(FALSE)
library(tmap)
library(tigris)

options(stringsAsFactors = FALSE)

rm(list = ls())  
cat("\014")      

temporal_AT = read_csv("data/Tmax_US_LamaH.csv", 
                       col_names = TRUE, cols(.default = col_double(), t = col_date("%Y/%m/%d")))

data = read_csv("data/ARCH_frequency_by_time_gap/time_gap=5.csv")

ID_US = colnames(temporal_AT[2:581])
ID_LamaH = colnames(temporal_AT[582:797])

data2 <- dplyr::select(data, -year, -MEAN, -STD)

col_means <- sapply(data2, function(x) mean(x, na.rm = TRUE))


mean_US = data.frame(ID = ID_US, mean_T = col_means[1:580])
mean_LamaH = data.frame(ID = ID_LamaH, mean_T = col_means[581:796])

topo_US = read_csv("data/attributes/attribute_topo_site580.csv", 
                   col_names = TRUE, col_types = cols(.default = "d", ID = col_character()))

topo_LamaH = read_csv("data/attributes/attribute_topo_site216.csv", 
                      col_names = TRUE, col_types = cols(.default = "d", ID = col_character()))

map_US = left_join(topo_US, mean_US, by = "ID")
map_LamaH = left_join(topo_LamaH, mean_LamaH, by = "ID")

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

gage_US$ele_mt_sav <- data2$ele_mt_sav[1:580]
gage_LamaH$ele_mt_sav <- data2$ele_mt_sav[581:796]

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


sf::sf_use_s2(FALSE)
tmap_mode("plot")


basemap_dir <- "/data/LamaH/basemap"
gpkg_files <- list.files(basemap_dir, pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE)
if (length(gpkg_files) == 0) stop("No .gpkg found in /data/LamaH/basemap")

gpkg_path <- gpkg_files[1]
layers <- st_layers(gpkg_path)$name

read_layer <- function(layer_name) {
  if (layer_name %in% layers) {
    st_read(gpkg_path, layer = layer_name, quiet = TRUE)
  } else {
    stop(paste("Layer", layer_name, "not found in gpkg"))
  }
}

shp_basin <- read_layer("basin")
shp_line  <- read_layer("country_line")

shp_basin <- st_make_valid(st_transform(shp_basin, 3035))
shp_line  <- st_make_valid(st_transform(shp_line, 3035))

# ----------------------------
gage_LamaH_europe <- gage_LamaH %>%
  filter(
    is.finite(lon_deg_save), is.finite(lat_deg_save),
    lon_deg_save >= -15, lon_deg_save <= 35,
    lat_deg_save >= 35,  lat_deg_save <= 60
  )


# ----------------------------
gage_LamaH_3035 <- st_transform(gage_LamaH_europe, 3035)

# ----------------------------
europe_bbox_ll <- st_as_sfc(
  st_bbox(c(xmin = -15, xmax = 35, ymin = 35, ymax = 60), crs = 4326)
)

europe_bbox_3035 <- st_transform(europe_bbox_ll, 3035)

shp_basin_crop <- st_crop(shp_basin, europe_bbox_3035)
shp_line_crop  <- st_crop(shp_line,  europe_bbox_3035)

# ----------------------------
# 4️⃣ 绘图
# ----------------------------
map2 <- tm_shape(shp_basin_crop) +
  tm_fill(col = "#F2F2F2", alpha = 1) +
  
  tm_shape(shp_line_crop) +
  tm_lines(col = "black", lwd = 1, alpha = 1) +
  
  tm_shape(gage_LamaH_3035) +
  tm_bubbles(
    size = "ele_mt_sav_category",
    scale = 3,
    col = "mean_T",
    style = "fixed",
    breaks = c(0, 1, 1.5, 2, 3),
    labels = c("0–1", "1–1.5", "1.5–2", "2–3"),
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

map2
dir.create("results/Fig2", recursive = TRUE, showWarnings = FALSE)
tmap_save(map2, filename = "results/Fig2/Fig2_map_CE_ARCH.png", dpi = 600)
