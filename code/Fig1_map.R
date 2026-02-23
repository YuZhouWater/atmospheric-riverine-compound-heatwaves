# =========================================================
# Code Ocean-ready: read RHW tables -> build site_summary -> map US & CE (3 metrics)
# Output: 6 PNG files saved to ./results
# =========================================================
setwd("..")
library(readr)
library(dplyr)
library(sf)
library(tmap)
library(tigris)

options(tigris_use_cache = TRUE)

# -----------------------------
# 0) Safe temp directory (container-friendly)
# -----------------------------
dir.create("tmp", showWarnings = FALSE, recursive = TRUE)
Sys.setenv(TMPDIR = normalizePath("tmp"))

# -----------------------------
# 1) Paths (based on your Code Ocean data tree)
# -----------------------------
# RHW tables (you renamed them)
p_count     <- "data/RHW_attributes/RHW_annual_frequency.csv"
p_duration  <- "data/RHW_attributes/RHW_annual_duration.csv"
p_intensity <- "data/RHW_attributes/RHW_annual_mean_intensity.csv"

p_topo_us <- "data/attributes/attribute_topo_site580.csv"
p_topo_ce <- "data/attributes/attribute_topo_site216.csv"

# CE shapefiles (exactly following your folder structure in the screenshot)
p_ce_basin <- "data/LamaH/LamaH in Central Europe/LamaH-CE_daily/A_basins_total_upstrm/3_shapefiles/Basins_A.shp"
p_ce_border <- "data/LamaH/LamaH in Central Europe/ref-countries-2020-01m/CNTR_RG_01M_2020_3035_modified/modified_lines.shp"

# -----------------------------
# 2) Read RHW datasets (first column is time)
# -----------------------------
Rhw_count     <- read_csv(p_count,     show_col_types = FALSE)
Rhw_duration  <- read_csv(p_duration,  show_col_types = FALSE)
Rhw_intensity <- read_csv(p_intensity, show_col_types = FALSE)

# -----------------------------
# 3) Helper: drop summary cols -> compute column means per site
#    (Uses the current site column names directly; no temporal_US renaming here)
# -----------------------------
prep_and_colmean <- function(df, value_name) {

  # Drop summary columns if present (case-insensitive support)
  drop_cols <- names(df)[tolower(names(df)) %in% c("stdevp", "mean", "average")]
  df2 <- df %>% select(-all_of(drop_cols))

  # Site columns = everything except the first (time) column
  site_cols <- names(df2)[-1]

  tibble(
    site = site_cols,
    !!value_name := sapply(df2[site_cols], function(x) mean(x, na.rm = TRUE))
  )
}

count_mean_tbl     <- prep_and_colmean(Rhw_count,     "mean_count")
duration_mean_tbl  <- prep_and_colmean(Rhw_duration,  "mean_duration")
intensity_mean_tbl <- prep_and_colmean(Rhw_intensity, "mean_intensity")

site_summary <- count_mean_tbl %>%
  left_join(duration_mean_tbl,  by = "site") %>%
  left_join(intensity_mean_tbl, by = "site") %>%
  arrange(site)

# -----------------------------
# 4) Build US / CE metric tables (prefix filter)
# -----------------------------
mean_US_all <- site_summary %>%
  filter(grepl("^US", site)) %>%
  transmute(ID = site, freq = mean_count, dur = mean_duration, inten = mean_intensity)

mean_CE_all <- site_summary %>%
  filter(grepl("^CE", site)) %>%
  transmute(ID = site, freq = mean_count, dur = mean_duration, inten = mean_intensity)

# -----------------------------
# 5) Read topo tables and build sf points
# -----------------------------
topo_US <- read_csv(
  p_topo_us,
  col_names = TRUE,
  col_types = cols(.default = "d", ID = col_character()),
  show_col_types = FALSE
)

topo_CE <- read_csv(
  p_topo_ce,
  col_names = TRUE,
  col_types = cols(.default = "d", ID = col_character()),
  show_col_types = FALSE
)

gage_US <- left_join(topo_US, mean_US_all, by = "ID") %>%
  st_as_sf(coords = c("lon_deg", "lat_deg"), crs = 4269)

gage_CE <- left_join(topo_CE, mean_CE_all, by = "ID") %>%
  st_as_sf(coords = c("lon_deg", "lat_deg"), crs = 4269)

# -----------------------------
# 6) Basemaps
# -----------------------------
# US states (requires internet; if offline, replace with a local shapefile)
us_geo <- tigris::states(class = "sf")
contiguous_states <- us_geo %>% filter(REGION != 9, STUSPS != "AK", STUSPS != "HI")

# CE basemap (local)
shp_basin  <- st_read(p_ce_basin,  quiet = TRUE)
shp_border <- st_read(p_ce_border, quiet = TRUE)
gage_CE_3035 <- st_transform(gage_CE, 3035)

# -----------------------------
# 7) Map builders
# -----------------------------
make_us_map <- function(sf_points, value_col, breaks, labels, palette, title_col) {
  tm_shape(contiguous_states, projection = 5070) +
    tm_fill(col = "#F2F2F2", alpha = 1) +
    tm_borders(col = "black", lwd = 1, alpha = 1) +
    tm_shape(sf_points, projection = 5070) +
    tm_bubbles(size = 3, col = value_col, style = "fixed",
               breaks = breaks, labels = labels, palette = palette,
               border.col = "black", title.col = title_col) +
    tm_layout(frame = FALSE, legend.show = TRUE,
              legend.outside = TRUE, legend.outside.position = "right",
              legend.text.size = 1.2, legend.title.size = 1.5, legend.width = 1.2)
}

make_ce_map <- function(sf_points_3035, value_col, breaks, labels, palette4, title_col) {
  tm_shape(shp_basin, projection = 3035) +
    tm_fill(col = "#F2F2F2", alpha = 1) +
    tm_shape(shp_border, projection = 3035) +
    tm_lines(col = "black", lwd = 1, alpha = 1) +
    tm_shape(sf_points_3035, projection = 3035) +
    tm_bubbles(size = 3, col = value_col, style = "fixed",
               breaks = breaks, labels = labels, palette = palette4,
               border.col = "black", title.col = title_col) +
    tm_layout(frame = FALSE, legend.show = TRUE,
              legend.outside = TRUE, legend.outside.position = "right",
              legend.text.size = 1.2, legend.title.size = 1.5, legend.width = 1.2)
}

# -----------------------------
# 8) Build 6 maps (strict bins as you required)
# -----------------------------
# US frequency: 1–1.5, 1.5–2, 2–2.5, 2.5–3 (4 bins / 4 colors)
map_US_freq <- make_us_map(
  gage_US, "freq",
  breaks = c(1, 1.5, 2, 2.5, 3),
  labels = c("1–1.5", "1.5–2", "2–2.5", "2.5–3"),
  palette = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c"),
  title_col = "Annual\nfrequency"
)

# US duration: 9–15, 15–20, 20–25, 25+ (4 bins / 4 colors)
map_US_dur <- make_us_map(
  gage_US, "dur",
  breaks = c(9, 15, 20, 25, Inf),
  labels = c("9–15", "15–20", "20–25", "25+"),
  palette = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c"),
  title_col = "Duration\n(days)"
)

# US intensity: keep your original bins (0–4, 4–7, 7–10, 10+ => 4 bins)
map_US_inten <- make_us_map(
  gage_US, "inten",
  breaks = c(0, 4, 7, 10, Inf),
  labels = c("0–4", "4–7", "7–10", "10+"),
  palette = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c"),
  title_col = "Mean\nintensity"
)

# CE frequency (same bins)
map_CE_freq <- make_ce_map(
  gage_CE_3035, "freq",
  breaks = c(1, 1.5, 2, 2.5, 3),
  labels = c("1–1.5", "1.5–2", "2–2.5", "2.5–3"),
  palette4 = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c"),
  title_col = "Annual\nfrequency"
)

# CE duration (same bins)
map_CE_dur <- make_ce_map(
  gage_CE_3035, "dur",
  breaks = c(9, 15, 20, 25, Inf),
  labels = c("9–15", "15–20", "20–25", "25+"),
  palette4 = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c"),
  title_col = "Duration\n(days)"
)

# CE intensity (4 bins)
map_CE_inten <- make_ce_map(
  gage_CE_3035, "inten",
  breaks = c(0, 4, 7, 10, Inf),
  labels = c("0–4", "4–7", "7–10", "10+"),
  palette4 = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c"),
  title_col = "Mean\nintensity"
)

# -----------------------------
# 9) Save 6 figures to ./results
# -----------------------------
dir.create("results", showWarnings = FALSE)

tmap_save(map_US_freq,  "results/US_RHW_frequency.png",  dpi = 600, width = 8, height = 5)
tmap_save(map_US_dur,   "results/US_RHW_duration.png",   dpi = 600, width = 8, height = 5)
tmap_save(map_US_inten, "results/US_RHW_intensity.png",  dpi = 600, width = 8, height = 5)

tmap_save(map_CE_freq,  "results/CE_RHW_frequency.png",  dpi = 600, width = 8, height = 5)
tmap_save(map_CE_dur,   "results/CE_RHW_duration.png",   dpi = 600, width = 8, height = 5)
tmap_save(map_CE_inten, "results/CE_RHW_intensity.png",  dpi = 600, width = 8, height = 5)

