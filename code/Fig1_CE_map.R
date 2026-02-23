setwd("..")
library(readr)
library(dplyr)
library(sf)
sf::sf_use_s2(FALSE)
library(tmap)

dir.create("tmp", showWarnings = FALSE, recursive = TRUE)
tempdir <- function() normalizePath("tmp")
unlockBinding("tempdir", baseenv())
utils::assignInNamespace("tempdir", tempdir, ns = "base", envir = baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())

basemap_dir <- "/data/LamaH/basemap"
results_dir <- "/results"
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

temporal_path <- "data/Tmax_US_LamaH.csv"

freq_path  <- "data/RHW_attributes/RHW_annual_frequency.csv"
dur_path   <- "data/RHW_attributes/RHW_annual_duration.csv"
inten_path <- "data/RHW_attributes/RHW_annual_mean_intensity.csv"

topo_path  <- "data/attributes/attribute_topo_site216.csv"

temporal_US <- read_csv(
  temporal_path,
  col_names = TRUE,
  col_types = cols(.default = col_double(), t = col_date("%Y/%m/%d")),
  show_col_types = FALSE
)
target_site_names <- names(temporal_US)[-1]

Rhw_count     <- read_csv(freq_path,  show_col_types = FALSE)
Rhw_duration  <- read_csv(dur_path,   show_col_types = FALSE)
Rhw_intensity <- read_csv(inten_path, show_col_types = FALSE)

prep_and_colmean <- function(df, value_name, target_site_names) {

  drop_cols <- intersect(names(df), c("STDEVP", "MEAN", "mean", "AVERAGE"))
  df2 <- df %>% select(-all_of(drop_cols))

  cur_site_cols <- names(df2)[-1]
  if (length(cur_site_cols) != length(target_site_names)) {
    stop(sprintf(
      "Site column count mismatch: current=%d, reference temporal=%d. Check extra summary columns.",
      length(cur_site_cols), length(target_site_names)
    ))
  }

  names(df2) <- c(names(df2)[1], target_site_names)

  tibble(
    site = target_site_names,
    !!value_name := sapply(df2[target_site_names], function(x) mean(x, na.rm = TRUE))
  )
}

count_mean_tbl     <- prep_and_colmean(Rhw_count,     "mean_count",     target_site_names)
duration_mean_tbl  <- prep_and_colmean(Rhw_duration,  "mean_duration",  target_site_names)
intensity_mean_tbl <- prep_and_colmean(Rhw_intensity, "mean_intensity", target_site_names)

site_summary <- count_mean_tbl %>%
  left_join(duration_mean_tbl,  by = "site") %>%
  left_join(intensity_mean_tbl, by = "site") %>%
  arrange(site)


mean_CE_all <- site_summary %>%
  filter(grepl("^CE", site)) %>%
  transmute(
    ID    = site,
    freq  = mean_count,
    dur   = mean_duration,
    inten = mean_intensity
  )

topo_LamaH <- read_csv(
  topo_path,
  col_names = TRUE,
  col_types = cols(.default = "d", ID = col_character()),
  show_col_types = FALSE
)

map_CE_all <- left_join(topo_LamaH, mean_CE_all, by = "ID")

map_CE_europe_ll <- map_CE_all %>%
  filter(
    is.finite(lon_deg), is.finite(lat_deg),
    lon_deg >= -15, lon_deg <= 35,
    lat_deg >= 35,  lat_deg <= 60
  )

gage_CE_ll <- st_as_sf(map_CE_europe_ll, coords = c("lon_deg", "lat_deg"), crs = 4326, remove = FALSE)
gage_CE_3035 <- st_transform(gage_CE_ll, 3035)

gpkg_files <- list.files(basemap_dir, pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE)
if (length(gpkg_files) == 0) stop("No .gpkg found in /data/LamaH/basemap")

gpkg_path <- gpkg_files[1]
layers <- st_layers(gpkg_path)$name
cat("Using basemap:", gpkg_path, "\n")
cat("Layers:\n"); print(layers)

read_layer <- function(layer_name) {
  if (layer_name %in% layers) st_read(gpkg_path, layer = layer_name, quiet = TRUE) else NULL
}

shp_basin <- read_layer("basin")
shp_line  <- read_layer("country_line")

if (is.null(shp_basin) || is.null(shp_line)) {
  stop("Basemap gpkg must contain layers: 'basin' and 'country_line'.")
}

shp_basin <- st_make_valid(st_transform(shp_basin, 3035))
shp_line  <- st_make_valid(st_transform(shp_line,  3035))

bb <- st_bbox(shp_basin)

gage_CE_3035_in <- gage_CE_3035 %>%
  filter(
    st_coordinates(.)[,1] >= bb["xmin"], st_coordinates(.)[,1] <= bb["xmax"],
    st_coordinates(.)[,2] >= bb["ymin"], st_coordinates(.)[,2] <= bb["ymax"]
  )

cat("Points before lon/lat filter:", nrow(map_CE_all), "\n")
cat("After lon/lat filter:", nrow(gage_CE_3035), "\n")
cat("After basemap bbox filter:", nrow(gage_CE_3035_in), "\n")

tmap_mode("plot")

make_ce_map <- function(sf_points_3035, value_col, breaks, labels, palette4, title_col) {
  
  sf_points_clean <- sf_points_3035 %>%
    filter(!is.na(.data[[value_col]]))

  sf_points_clean <- sf_points_clean %>%
    mutate(
      value_cat = cut(
        .data[[value_col]],
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE,
        right = FALSE
      )
    )

  tm_shape(shp_basin) +
    tm_fill(col = "#F2F2F2", alpha = 1) +
    
    tm_shape(shp_line) +
    tm_lines(col = "black", lwd = 1, alpha = 1) +
    
    tm_shape(sf_points_clean) +
    tm_bubbles(
      size = 0.9,
      col = "value_cat", 
      palette = palette4,
      border.col = "black",
      title.col = title_col,
      legend.show = FALSE,  
      legend.na.show = FALSE
    ) +
  
    tm_add_legend(
      type = "symbol",  
      labels = labels,
      col = palette4,
      border.col = "black",
      size = 1,
      title = title_col
    ) +
    
    tm_layout(
      frame = FALSE,
      legend.show = TRUE, 
      legend.outside = TRUE,
      legend.outside.position = "right",
      legend.text.size = 1.2,
      legend.title.size = 0.6,
      legend.width = 3
    )
}

palette4 <- c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c")

map_freq_CE <- make_ce_map(
  sf_points_3035 = gage_CE_3035_in,
  value_col = "freq",
  breaks  = c(1, 1.5, 2, 2.5, 3),
  labels  = c("1–1.5", "1.5–2", "2–2.5", "2.5–3"),
  palette4 = palette4,
  title_col = "Annual\nfrequency"
)

map_dur_CE <- make_ce_map(
  sf_points_3035 = gage_CE_3035_in,
  value_col = "dur",
  breaks  = c(9, 15, 20, 25, Inf),
  labels  = c("9–15", "15–20", "20–25", "25+"),
  palette4 = palette4,
  title_col = "Duration\n(days)"
)

map_inten_CE <- make_ce_map(
  sf_points_3035 = gage_CE_3035_in,
  value_col = "inten",
  breaks  = c(0, 4, 7, 10, Inf),
  labels  = c("0–4", "4–7", "7–10", "10+"),
  palette4 = palette4,
  title_col = "Mean\nintensity"
)

tmap_save(map_freq_CE,  file.path(results_dir, "CE_RHW_frequency.png"),  dpi = 600, width = 8, height = 5, units = "in")
tmap_save(map_dur_CE,   file.path(results_dir, "CE_RHW_duration.png"),   dpi = 600, width = 8, height = 5, units = "in")
tmap_save(map_inten_CE, file.path(results_dir, "CE_RHW_intensity.png"),  dpi = 600, width = 8, height = 5, units = "in")


cat("Saved:\n",
    file.path(results_dir, "CE_RHW_frequency.png"), "\n",
    file.path(results_dir, "CE_RHW_duration.png"), "\n",
    file.path(results_dir, "CE_RHW_intensity.png"), "\n")