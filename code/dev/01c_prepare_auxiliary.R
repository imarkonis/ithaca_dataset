# ============================================================================
# Prepare auxiliary yearly GRACE and ESA CCI datasets for the TWC change
# workflow.
#
# This script prepares yearly GRACE terrestrial water storage and ESA CCI soil
# moisture products, aligns them to the common TWC grid, and saves cleaned
# outputs for downstream analysis.
# ============================================================================

# Libraries ==================================================================

source("source/twc_change.R")

library(lubridate)
library(pRecipe)

# Inputs ======================================================================
FILE_GRACE <- "~/shared/data/obs/other/waterstorage/raw/grace-gfz_ws_mm_global_200204_202112_025_monthly.nc"
FILE_CCI <- "~/shared/data/obs/soilmoisture/raw/esa-cci-sm-v07-1_swv_m3m-3_land_197811_202112_025_yearly.nc"

twc_grid <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_complete_grid.Rds")
)

# Functions ==================================================================

annual_mean_from_monthly_brick <- function(file) {
  dt <- as.data.table(tabular(brick(file)))
  dt[, date := as.Date(date)]
  
  dt[
    ,
    .(value = mean(value, na.rm = TRUE)),
    by = .(lon, lat, year = year(date))
  ]
}

yearly_brick_to_dt <- function(file, period) {
  r <- subset_data(file, yrs = period)
  dt <- as.data.table(tabular(r))
  dt[, date := as.Date(date)]
  
  return(dt)
}

prepare_auxiliary_dt <- function(dt, dataset_name, grid_mask) {
  dt <- merge(
    dt,
    grid_mask,
    by = c("lon", "lat")
  )
  
  dt <- dt[
    ,
    if (all(is.na(value))) NULL else .SD,
    by = .(lon, lat)
  ]
  
  dt[, dataset := dataset_name]
  
  setcolorder(
    dt,
    c("lon", "lat", "year", "dataset", "value")
  )
  
  setorder(dt, lon, lat, year)
  
  return(dt)
}

keep_complete_time_series <- function(dt, full_years) {
  dt <- dt[year %in% full_years]
  
  dt[
    ,
    if (
      uniqueN(year) == length(full_years) &&
      all(sort(unique(year)) == full_years)
    ) .SD,
    by = .(lon, lat)
  ]
}

# Analysis ===================================================================

## GRACE water storage =======================================================
grace_dt <- annual_mean_from_monthly_brick(FILE_GRACE)

grace_prepared <- prepare_auxiliary_dt(
  dt = grace_dt,
  dataset_name = 'GRACE',
  grid_mask = twc_grid
)

grace_years <- sort(unique(grace_prepared$year))

grace_prepared <- keep_complete_time_series(
  dt = grace_prepared,
  full_years = grace_years
)

## ESA CCI soil moisture =====================================================
cci_dt <- yearly_brick_to_dt(
  file = FILE_CCI,
  period = FULL_PERIOD
)

cci_dt[, year := year(date)]
cci_dt[, date := NULL]

cci_prepared <- prepare_auxiliary_dt(
  dt = cci_dt,
  dataset_name = 'CCI',
  grid_mask = twc_grid
)

cci_years <- 1992:2021
cci_prepared <- keep_complete_time_series(
  dt = cci_prepared,
  full_years = cci_years
)
# Outputs ====================================================================

saveRDS(
  grace_prepared,
  file.path(PATH_OUTPUT_RAW_OTHER, "grace_yearly.Rds")
)

saveRDS(
  cci_prepared,
  file.path(PATH_OUTPUT_RAW_OTHER, "cci_yearly.Rds")
)

# Validation =================================================================

grace_mean_map <- grace_prepared[
  ,
  .(mean_value = mean(value, na.rm = TRUE)),
  by = .(lon, lat, dataset)
]

cci_mean_map <- cci_prepared[
  ,
  .(mean_value = mean(value, na.rm = TRUE)),
  by = .(lon, lat, dataset)
]

p_grace_mean <- ggplot(
  grace_mean_map,
  aes(x = lon, y = lat, fill = mean_value)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Mean",
    title = "Mean GRACE terrestrial water storage"
  )

p_cci_mean <- ggplot(
  cci_mean_map,
  aes(x = lon, y = lat, fill = mean_value)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Mean",
    title = "Mean CCI soil moisture"
  )

set.seed(1979)

grace_random_points <- unique(grace_prepared[, .(lon, lat)])[
  sample(.N, min(9, .N))
]

grace_plot_dt <- merge(
  grace_prepared,
  grace_random_points,
  by = c("lon", "lat")
)

grace_plot_dt[
  ,
  facet_label := paste0(
    "lon=", round(lon, 2),
    ", lat=", round(lat, 2)
  )
]

p_grace_ts <- ggplot(
  grace_plot_dt,
  aes(x = year, y = value, group = 1)
) +
  geom_line(linewidth = 0.5, alpha = 0.9) +
  facet_wrap(~ facet_label, scales = "free_y", ncol = 3) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Value",
    title = "Validation of yearly GRACE time series at 9 random grid cells"
  )

cci_random_points <- unique(cci_prepared[, .(lon, lat)])[
  sample(.N, min(9, .N))
]

cci_plot_dt <- merge(
  cci_prepared,
  cci_random_points,
  by = c("lon", "lat")
)

cci_plot_dt[
  ,
  facet_label := paste0(
    "lon=", round(lon, 2),
    ", lat=", round(lat, 2)
  )
]

p_cci_ts <- ggplot(
  cci_plot_dt,
  aes(x = year, y = value, group = 1)
) +
  geom_line(linewidth = 0.5, alpha = 0.9) +
  facet_wrap(~ facet_label, scales = "free_y", ncol = 3) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Value",
    title = "Validation of yearly CCI time series at 9 random grid cells"
  )
