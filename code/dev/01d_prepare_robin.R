# ============================================================================
# Prepare ROBIN runoff data and catchment-grid matching for the TWC change
# workflow.
#
# This script:
#   1) reads ROBIN catchment geometries and station metadata
#   2) converts daily runoff from cumecs to mm/day
#   3) aggregates runoff to monthly and yearly scales
#   4) matches ROBIN catchments to the common TWC complete grid
#   5) saves runoff products and catchment-grid lookup tables
# ============================================================================

# Libraries ==================================================================

source("source/twc_change.R")

library(dplyr)

# Constants & Variables =======================================================

FILE_ROBIN_SHP <- "~/shared/data/geodata/robin_v1_Jan2025/ROBIN_V1_Shapefiles_Jan2025.shp"
FILE_ROBIN_META <- "~/shared/data/stations/robin_v1/supporting-documents/robin_station_metadata_public_v1-1.csv"
PATH_ROBIN_SOURCE <- "~/shared/data/stations/robin_v1/source"
PATH_ROBIN_RAW <- "~/shared/data/stations/robin_v1/raw"

FILE_ROBIN_DAILY <- file.path(
  PATH_ROBIN_RAW,
  "robin-v1_q_mm_land_18630101_20221231_station_daily.rds"
)

FILE_ROBIN_MONTHLY <- file.path(
  PATH_ROBIN_RAW,
  "robin-v1_q_mm_land_18630101_20221231_station_monthly.rds"
)

FILE_ROBIN_YEARLY <- file.path(
  PATH_ROBIN_RAW,
  "robin-v1_q_mm_land_18630101_20221231_station_yearly.rds"
)

dir.create(PATH_ROBIN_RAW, recursive = TRUE, showWarnings = FALSE)

twc_complete_grid <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_complete_grid.Rds")
)

# Analysis ===================================================================

# ROBIN geometries and metadata -----------------------------------------------

runoff_robin_shp <- st_read(FILE_ROBIN_SHP, quiet = TRUE)
runoff_robin_shp <- st_make_valid(runoff_robin_shp)
runoff_robin_shp <- runoff_robin_shp[st_is_valid(runoff_robin_shp), ]

runoff_robin_meta <- fread(FILE_ROBIN_META)

# ROBIN station time series ---------------------------------------------------

csv_files <- list.files(
  PATH_ROBIN_SOURCE,
  pattern = "\\.csv$",
  full.names = TRUE
)

runoff_robin <- rbindlist(
  lapply(csv_files, fread),
  use.names = TRUE,
  fill = TRUE
)

runoff_robin_day <- merge(
  runoff_robin,
  runoff_robin_meta[, .(robin_id = ROBIN_ID, area = AREA)],
  by = "robin_id",
  all.x = TRUE
)

runoff_robin_day[
  ,
  flow := (flow_cumecs * SEC_IN_DAY / (area * 10^6)) * 1000
]

runoff_robin_day[, flow := round(flow, 2)]
runoff_robin_day[, c("flow_cumecs", "area") := NULL]
runoff_robin_day[, date := as.Date(date)]
runoff_robin_day[, year := as.integer(format(date, "%Y"))]
runoff_robin_day[, month := as.integer(format(date, "%m"))]

setcolorder(
  runoff_robin_day,
  c("robin_id", "date", "year", "month", "flow")
)

runoff_robin_month <- runoff_robin_day[
  ,
  .(
    flow = sum(flow, na.rm = TRUE),
    n_missing = sum(is.na(flow))
  ),
  by = .(robin_id, year, month)
]

runoff_robin_year <- runoff_robin_day[
  ,
  .(
    flow = sum(flow, na.rm = TRUE),
    n_missing = sum(is.na(flow))
  ),
  by = .(robin_id, year)
]

# Match TWC complete grid to ROBIN catchments ---------------------------------

twc_complete_grid_sf <- st_as_sf(
  twc_complete_grid,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

twc_grid_in_robin <- st_join(
  twc_complete_grid_sf,
  runoff_robin_shp,
  left = FALSE
)

twc_grid_in_robin$lon <- st_coordinates(twc_grid_in_robin)[, 1]
twc_grid_in_robin$lat <- st_coordinates(twc_grid_in_robin)[, 2]

twc_grid_in_robin <- as.data.table(twc_grid_in_robin)
twc_grid_in_robin <- twc_grid_in_robin[, .(lon, lat, robin_id = ROBIN_ID)]

# Catchments with no direct intersections -------------------------------------

runoff_rep <- st_point_on_surface(runoff_robin_shp)

twc_complete_grid_sf <- twc_complete_grid_sf %>%
  mutate(grid_id = row_number())

idx <- st_nearest_feature(runoff_rep, twc_complete_grid_sf)

runoff_with_grid <- runoff_robin_shp %>%
  mutate(grid_id = twc_complete_grid_sf$grid_id[idx])

twc_grid_in_robin_small <- left_join(
  st_drop_geometry(runoff_with_grid),
  st_drop_geometry(twc_complete_grid_sf),
  by = "grid_id"
)

twc_grid_in_robin_small <- as.data.table(twc_grid_in_robin_small)
twc_grid_in_robin_small <- twc_grid_in_robin_small[, .(lon, lat, robin_id = ROBIN_ID)]

# Unified basin-grid lookup ---------------------------------------------------

robin_coords <- merge(
  twc_grid_in_robin_small,
  twc_grid_in_robin,
  by = c("lon", "lat", "robin_id"),
  all = TRUE,
  allow.cartesian = TRUE
)

basin_ids_with_flow_data <- unique(runoff_robin_day$robin_id)

robin_coords_with_flow <- robin_coords[
  robin_id %in% basin_ids_with_flow_data
]

# Outputs ====================================================================

saveRDS(runoff_robin_day, FILE_ROBIN_DAILY)
saveRDS(runoff_robin_month, FILE_ROBIN_MONTHLY)
saveRDS(runoff_robin_year, FILE_ROBIN_YEARLY)

saveRDS(
  robin_coords_with_flow,
  file.path(PATH_OUTPUT_RAW, "robin_coords.rds")
)

saveRDS(
  twc_grid_in_robin,
  file.path(PATH_OUTPUT_RAW, "twc_grid_in_robin.rds")
)

# Validation =================================================================

robin_n_cells <- robin_coords_with_flow[
  ,
  .(n_cells = .N),
  by = .(robin_id)
]

p_robin_n_cells <- ggplot(
  robin_n_cells,
  aes(x = n_cells)
) +
  geom_histogram(bins = 3) +
  theme_bw() +
  labs(
    x = "Number of matched TWC grid cells",
    y = "Count",
    title = "Distribution of matched TWC grid cells per ROBIN catchment"
  )

## Map of matched TWC grid cells 

twc_grid_in_robin_plot <- unique(
  robin_coords_with_flow[, .(lon, lat, robin_id)]
)

p_robin_grid_map <- ggplot() +
  geom_sf(
    data = runoff_robin_shp,
    fill = NA,
    color = "grey40",
    linewidth = 0.2
  ) +
  geom_point(
    data = twc_grid_in_robin_plot,
    aes(x = lon, y = lat),
    size = 0.15,
    alpha = 0.5
  ) +
  coord_sf() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Matched TWC grid cells within ROBIN catchments"
  )


## Random yearly runoff series 

set.seed(1979)

random_robin_ids <- unique(runoff_robin_year$robin_id)[
  sample(
    length(unique(runoff_robin_year$robin_id)),
    min(9, length(unique(runoff_robin_year$robin_id)))
  )
]

runoff_robin_year_plot <- runoff_robin_year[
  robin_id %in% random_robin_ids
]

runoff_robin_year_plot[
  ,
  facet_label := paste0("ROBIN ID: ", robin_id)
]

p_robin_year_ts <- ggplot(
  runoff_robin_year_plot,
  aes(x = year, y = flow, group = robin_id)
) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ facet_label, scales = "free_y", ncol = 3) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Yearly runoff [mm]",
    title = "Validation of yearly ROBIN runoff at 9 random catchments"
  )

