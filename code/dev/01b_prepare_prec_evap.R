# ============================================================================
# Prepare raw yearly precipitation and evaporation datasets for the TWC change
# workflow.
#
# This script subsets the selected gridded precipitation and evaporation
# datasets to 1982-2021, converts them to tabular format, and saves a combined
# raw table for downstream analysis.
# ============================================================================

# Libraries ==================================================================

source("source/twc_change.R")
source("source/data_registry.R")

library(lubridate)
library(raster)
library(pRecipe)
library(doParallel)

# Parallel setup ==============================================================

registerDoParallel(max(N_DATASETS_PREC, N_DATASETS_EVAP))

# Constants & Variables =======================================================

PATH_OUTPUT_RAW_PREC <- file.path(PATH_OUTPUT_RAW, "prec")
PATH_OUTPUT_RAW_EVAP <- file.path(PATH_OUTPUT_RAW, "evap")

dir.create(PATH_OUTPUT_RAW_PREC, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_OUTPUT_RAW_EVAP, recursive = TRUE, showWarnings = FALSE)

# Functions ==================================================================

build_dataset_table <- function(datasets, names_analysis, names_ensemble, path_out) {
  names_used <- unique(c(names_ensemble, names_analysis))
  
  datasets_used <- datasets[fname %in% names_used, .(
    name,
    fname,
    file_raw = file,
    file = file.path(path_out, paste0(fname, "_yearly.nc"))
  )]
  
  return(datasets_used[])
}

subset_and_save_dataset_files <- function(datasets_used, period) {
  foreach(
    dataset_count = seq_len(nrow(datasets_used)),
    .packages = c("raster", "pRecipe", "lubridate")
  ) %dopar% {
    result <- subset_data(
      datasets_used$file_raw[dataset_count],
      yrs = period
    )
    
    saveNC(result, datasets_used$file[dataset_count])
  }
  
  invisible(NULL)
}

grid_files_to_dt <- function(datasets_used, variable_name) {
  dt_list <- vector("list", nrow(datasets_used))
  
  for (dataset_count in seq_len(nrow(datasets_used))) {
    message(datasets_used$name[dataset_count])
    
    dataset_brick <- brick(datasets_used$file[dataset_count])
    dataset_dt <- as.data.table(tabular(dataset_brick))
    dataset_dt[, date := as.Date(date)]
    dataset_dt[, variable := variable_name]
    dataset_dt[, dataset := datasets_used$name[dataset_count]]
    
    dt_list[[dataset_count]] <- dataset_dt
  }
  
  return(rbindlist(dt_list, use.names = TRUE))
}

# Analysis ===================================================================

## Precipitation ==============================================================

prec_datasets <- filter_datasets(
  var = "precip",
  tstep = "yearly",
  area = "land"
)

prec_names_ensemble <- prec_datasets[name %in% PREC_ENSEMBLE_NAMES_SHORT]$fname
prec_names_analysis <- prec_datasets[name %in% PREC_NAMES_SHORT]$fname

prec_datasets_used <- build_dataset_table(
  datasets = prec_datasets,
  names_analysis = prec_names_analysis,
  names_ensemble = prec_names_ensemble,
  path_out = PATH_OUTPUT_RAW_PREC
)

subset_and_save_dataset_files(
  datasets_used = prec_datasets_used,
  period = FULL_PERIOD
)

prec_dt <- grid_files_to_dt(
  datasets_used = prec_datasets_used,
  variable_name = "prec"
)

## Evaporation ================================================================

evap_datasets <- filter_datasets(
  var = "evap",
  var2 = "e",
  tstep = "yearly",
  area = "land"
)

evap_names_ensemble <- evap_datasets[name %in% EVAP_ENSEMBLE_NAMES_SHORT]$fname
evap_names_analysis <- evap_datasets[name %in% EVAP_NAMES_SHORT]$fname

evap_datasets_used <- build_dataset_table(
  datasets = evap_datasets,
  names_analysis = evap_names_analysis,
  names_ensemble = evap_names_ensemble,
  path_out = PATH_OUTPUT_RAW_EVAP
)

subset_and_save_dataset_files(
  datasets_used = evap_datasets_used,
  period = FULL_PERIOD
)

evap_dt <- grid_files_to_dt(
  datasets_used = evap_datasets_used,
  variable_name = "evap"
)

## Merging ===================================================================
grid_mean_dt <- prec_evap_raw[
  ,
  .(mean_value = mean(value, na.rm = TRUE)),
  by = .(lon, lat, variable, dataset)
]

complete_grids <- grid_mean_dt[
  !is.na(mean_value),
  .(n_datasets = uniqueN(dataset)),
  by = .(lon, lat)
][
  n_datasets == max(n_datasets),
  .(lon, lat)
]

prec_evap_raw <- merge(
  prec_evap_raw,
  complete_grids,
  by = c("lon", "lat")
)

prec_evap_raw[, `:=`(
  year = year(date),
  date = NULL
)]

setcolorder(
  prec_evap_raw,
  c("lon", "lat", "year", "dataset", "variable", "value")
)

setorder(
  prec_evap_raw,
  variable, dataset, lon, lat, year
)

prec_evap_raw[, value := round(value, 0)]

# Outputs ====================================================================

write_fst(
  prec_evap_raw, 
  file.path(PATH_OUTPUT_RAW, "prec_evap_raw.fst")
)

saveRDS(
  complete_grids,
  file.path(PATH_OUTPUT_DATA, "twc_complete_grid.Rds")
)

#saveRDS(
#  prec_datasets_used,
#  file.path(PATH_OUTPUT_DATA, "prec_datasets_used.Rds")
#)

#saveRDS(
#  evap_datasets_used,
#  file.path(PATH_OUTPUT_DATA, "evap_datasets_used.Rds")
#)

# Validation =================================================================

set.seed(1979)

random_points <- unique(prec_evap_raw[, .(lon, lat)])[
  sample(.N, min(9, .N))
]

plot_dt <- merge(
  prec_evap_raw,
  random_points,
  by = c("lon", "lat")
)

plot_dt[
  ,
  facet_label := paste0(
    "lon=", round(lon, 2),
    ", lat=", round(lat, 2)
  )
]

ggplot(
  plot_dt,
  aes(x = date, y = value, color = dataset, group = dataset)
) +
  geom_line(linewidth = 0.5, alpha = 0.9) +
  facet_grid(variable ~ facet_label, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Year",
    y = "Value",
    color = "Dataset",
    title = "Validation of yearly dataset time series at 9 random grid cells"
  )

# Mean fields by dataset ------------------------------------------------------

prec_evap_mean_maps <- prec_evap_raw[
  ,
  .(mean_value = mean(value, na.rm = TRUE)),
  by = .(lon, lat, variable, dataset)
]

plot_mean_maps <- function(dt_plot, var_name) {
  
  ggplot(
    dt_plot[variable == var_name],
    aes(x = lon, y = lat, fill = mean_value)
  ) +
    geom_raster() +
    facet_wrap(~ dataset, ncol = 4) +
    coord_equal() +
    scale_fill_viridis_c(na.value = "grey90") +
    theme_bw() +
    labs(
      x = "Longitude",
      y = "Latitude",
      fill = "Mean",
      title = paste("Mean", var_name, "across datasets")
    )
}

plot_mean_maps(
  dt_plot = prec_evap_mean_maps,
  var_name = "prec"
)

plot_mean_maps(
  dt_plot = prec_evap_mean_maps,
  var_name = "evap"
)
