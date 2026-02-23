setwd("..")
library(readr)
library(dplyr)
library(sf)
sf::sf_use_s2(FALSE)
library(tmap)
library(tigris)

options(tigris_use_cache = TRUE)
dir.create("tmp", showWarnings = FALSE, recursive = TRUE)
tempdir <- function() normalizePath("tmp")
unlockBinding("tempdir", baseenv())
utils::assignInNamespace("tempdir", tempdir, ns = "base", envir = baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())

temporal_US <- read_csv(
  "data/Tmax_US_LamaH.csv",
  col_names = TRUE,
  cols(.default = col_double(), t = col_date("%Y/%m/%d")),
  show_col_types = FALSE
)
target_site_names <- names(temporal_US)[-1]

Rhw_count <- read_csv("data/RHW_attributes/RHW_annual_frequency.csv", show_col_types = FALSE)
Rhw_duration <- read_csv("data/RHW_attributes/RHW_annual_duration.csv", show_col_types = FALSE)
Rhw_intensity <- read_csv("data/RHW_attributes/RHW_annual_mean_intensity.csv", show_col_types = FALSE)

prep_and_colmean <- function(df, value_name, target_site_names) {

  drop_cols <- intersect(names(df), c("STDEVP", "MEAN", "mean", "AVERAGE"))
  df2 <- df %>% select(-all_of(drop_cols))

  cur_site_cols <- names(df2)[-1]
  if (length(cur_site_cols) != length(target_site_names)) {
    stop(sprintf(
      "Site column count mismatch: current=%d, temporal_US=%d. Check extra summary columns.",
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

mean_US_all <- site_summary %>%
  filter(grepl("^US", site)) %>%
  transmute(
    ID    = site,
    freq  = mean_count,
    dur   = mean_duration,
    inten = mean_intensity
  )

topo_US <- read_csv(
  "data/attributes/attribute_topo_site580.csv",
  col_names = TRUE,
  col_types = cols(.default = "d", ID = col_character()),
  show_col_types = FALSE
)

map_US_all <- left_join(topo_US, mean_US_all, by = "ID") %>%
  filter(
    !is.na(lon_deg), !is.na(lat_deg),
    !is.na(freq), !is.na(dur), !is.na(inten)
  )

gage_US_all <- map_US_all %>%
  st_as_sf(coords = c("lon_deg", "lat_deg"), crs = 4269)

us_geo <- tigris::states(class = "sf")
contiguous_states <- us_geo %>%
  filter(REGION != 9, STUSPS != "AK", STUSPS != "HI")

make_us_map_simple <- function(sf_points, value_col, breaks, labels, palette, title_col) {

  sf_points_clean <- sf_points %>%
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

  map <- tm_shape(contiguous_states, projection = 5070) +
    tm_fill(col = "#F2F2F2") +
    tm_borders(col = "black", lwd = 1) +
    
    tm_shape(sf_points_clean, projection = 5070) +
    tm_symbols(  
      size = 0.9,
      col = "value_cat",
      palette = palette,
      border.col = "black",
      legend.show = FALSE
    ) +
    tm_add_legend(
      type = "symbol",
      labels = labels,
      col = palette,
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
  
  return(map)
}

map_freq_US <- make_us_map_simple(
  sf_points = gage_US_all,
  value_col = "freq",
  breaks = c(1, 1.5, 2, 2.5, Inf),
  labels = c("1–1.5", "1.5–2", "2–2.5", "2.5+"),
  palette = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c"),
  title_col = "Annual\nfrequency"
)

map_dur_US <- make_us_map_simple(
  sf_points = gage_US_all,
  value_col = "dur",
  breaks = c(9, 15, 20, 25, Inf),
  labels = c("9–15", "15–20", "20–25", "25+"),
  palette = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c"),
  title_col = "Duration\n(days)"
)

map_inten_US <- make_us_map_simple(
  sf_points = gage_US_all,
  value_col = "inten",
  breaks = c(0, 4, 7, 10, Inf),
  labels = c("0–4", "4–7", "7–10", "10+"),
  palette = c("#eff3ff", "#bdd7e7", "#6baed6", "#08519c"),
  title_col = "Mean\nintensity"
)

dir.create("results", showWarnings = FALSE)

tmap_save(map_freq_US,  "results/US_RHW_frequency.png",  dpi = 600, width = 8, height = 5)
tmap_save(map_dur_US,   "results/US_RHW_duration.png",   dpi = 600, width = 8, height = 5)
tmap_save(map_inten_US, "results/US_RHW_intensity.png",  dpi = 600, width = 8, height = 5)