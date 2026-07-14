# ============================================================================
# Prepare and widen the cropped yearly precipitation and evaporation datasets
# for the TWC change workflow.
#
# This script reads the cropped yearly land datasets produced by
# 01a_crop_data (in PATH_OUTPUT_INPUT), converts them to tabular format, and
# builds a combined raw long table (saved as prec_evap_raw.fst). It then keeps
# only the selected analysis datasets, applies the intended MSWEP-GLEAM pairing,
# reshapes to wide format, and saves the combined precipitation-evaporation
# table (prec_evap.Rds). All outputs are written to PATH_OUTPUT_OUTPUT.
#
# (Merges the former 01b_prepare_prec_evap.R and 01e_widen_prec_evap.R.)
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(lubridate)
library(pRecipe)

# Constants & Variables ======================================================

# Map the dataset token in each Zenodo filename (the part before the first "_")
# to the short dataset name used across the workflow. Mirrors the dataset names
# downloaded in 00b_data_download.
DATASET_NAME_BY_TOKEN <- c(
  "cpc"          = "CPC",
  "gpcc"         = "GPCC",
  "em-earth"     = "EARTH",
  "era5-land"    = "ERA5L",
  "fldas"        = "FLDAS",
  "merra"        = "MERRA",
  "precl"        = "PRECL",
  "terraclimate" = "TERRA",
  "mswep"        = "MSWEP",
  "bess"         = "BESS",
  "etmonitor"    = "ETMON",
  "etsynthesis"  = "ETSYN",
  "gleam-v4-1a"  = "GLEAM"
)

# Functions ==================================================================

resolve_dataset_names <- function(files) {
  tokens <- names(DATASET_NAME_BY_TOKEN)

  vapply(basename(files), function(fname) {
    hits <- tokens[vapply(tokens, grepl, logical(1), x = fname, fixed = TRUE)]
    if (length(hits) != 1) return(NA_character_)
    DATASET_NAME_BY_TOKEN[[hits]]
  }, character(1), USE.NAMES = FALSE)
}

build_input_table <- function(files) {
  data.table(
    name = resolve_dataset_names(files),
    file = files
  )
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

# Inputs =====================================================================
#
# Cropped yearly NetCDFs written by 01a_crop_data under PATH_OUTPUT_INPUT, kept
# under their original Zenodo filenames. Evaporation files carry the "_e_"
# variable marker; the remaining files are precipitation.

input_files <- list.files(
  PATH_OUTPUT_INPUT,
  pattern = "\\.nc$",
  full.names = TRUE
)

if (length(input_files) == 0) {
  stop(
    "No cropped .nc files found in ", PATH_OUTPUT_INPUT,
    "\nRun 01a_crop_data first.",
    call. = FALSE
  )
}

evap_files <- input_files[grepl("_e_", basename(input_files), fixed = TRUE)]
prec_files <- setdiff(input_files, evap_files)

prec_datasets_used <- build_input_table(prec_files)
evap_datasets_used <- build_input_table(evap_files)

## Fail if any file could not be mapped to a short dataset name.
unresolved <- rbind(prec_datasets_used, evap_datasets_used)[is.na(name)]$file

if (length(unresolved) > 0) {
  stop(
    "Could not map these input files to a dataset name:\n",
    paste(unresolved, collapse = "\n"),
    call. = FALSE
  )
}

# Prepare raw long table =====================================================

prec_dt <- grid_files_to_dt(
  datasets_used = prec_datasets_used,
  variable_name = "prec"
)

evap_dt <- grid_files_to_dt(
  datasets_used = evap_datasets_used,
  variable_name = "evap"
)

prec_evap_raw <- rbindlist(
  list(prec_dt, evap_dt),
  use.names = TRUE
)

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

# Widen selected analysis datasets ===========================================

prec_evap_analysis <- copy(
  prec_evap_raw[
    (variable == "prec" & dataset %in% PREC_NAMES_SHORT) |
      (variable == "evap" & dataset %in% EVAP_NAMES_SHORT)
  ]
)

## Pair MSWEP precipitation with GLEAM evaporation
prec_evap_analysis[
  variable == "prec" & dataset == "MSWEP",
  dataset := "GLEAM"
]

## Reshape to wide format
prec_evap <- dcast(
  prec_evap_analysis,
  lon + lat + year + dataset ~ variable,
  value.var = "value"
)

setcolorder(
  prec_evap,
  c("lon", "lat", "year", "dataset", "prec", "evap")
)

prec_evap <- prec_evap[complete.cases(prec_evap)]

setorder(
  prec_evap,
  dataset, lon, lat, year
)

# Outputs ====================================================================

write_fst(
  prec_evap_raw,
  file.path(PATH_OUTPUT_OUTPUT, "prec_evap_raw.fst")
)

saveRDS(
  complete_grids,
  file.path(PATH_OUTPUT_OUTPUT, "twc_complete_grid.Rds")
)

saveRDS(
  prec_evap,
  file.path(PATH_OUTPUT_OUTPUT, "prec_evap.Rds")
)

#saveRDS(
#  prec_datasets_used,
#  file.path(PATH_OUTPUT_OUTPUT, "prec_datasets_used.Rds")
#)

#saveRDS(
#  evap_datasets_used,
#  file.path(PATH_OUTPUT_OUTPUT, "evap_datasets_used.Rds")
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
  aes(x = year, y = value, color = dataset, group = dataset)
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

plot_mean_maps(
  dt_plot = prec_evap_mean_maps,
  var_name = "prec"
)

plot_mean_maps(
  dt_plot = prec_evap_mean_maps,
  var_name = "evap"
)
