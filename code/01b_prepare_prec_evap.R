# ============================================================================
# Prepare and widen the cropped yearly precipitation and evaporation datasets
# for the TWC change workflow.
#
# Reads the cropped yearly land NetCDFs produced by 01a_crop_data (in
# PATH_OUTPUT_INPUT) and converts each to a long table one layer (year) at a
# time, dropping ocean/masked (NA) cells as it goes, so peak memory stays at a
# single global 2-D field rather than a full multi-year cube. It builds a
# combined raw long table (prec_evap_raw.fst), restricts to grid cells covered
# by every dataset, keeps the selected analysis datasets, applies the MSWEP-
# GLEAM pairing, reshapes to wide format, and saves the combined precipitation-
# evaporation table (prec_evap.Rds). All outputs go to PATH_OUTPUT_OUTPUT.
#
# (Merges the former 01b_prepare_prec_evap.R and 01e_widen_prec_evap.R.)
# Ingestion is serial and land-only to keep peak memory bounded; see issue #40.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(raster)

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

build_input_table <- function(files, variable_name) {
  data.table(
    name     = resolve_dataset_names(files),
    file     = files,
    variable = variable_name
  )
}

# Read one cropped NetCDF into a compact long data.table WITHOUT materialising
# the whole cube. Each layer (year) is read on its own, ocean/masked (NA) cells
# are dropped immediately, and the value is rounded, so peak memory is a single
# global field plus the accumulating land-only rows.
read_grid_file <- function(file, dataset_name, variable_name) {
  dataset_brick <- brick(file)
  
  ## Layer dates -> integer years. Prefer the z (time) dimension; fall back to
  ## parsing layer names (e.g. "X1982.01.01") if z is not set.
  z <- getZ(dataset_brick)
  layer_years <- if (!is.null(z)) {
    year(as.Date(z))
  } else {
    year(as.Date(gsub("^X", "", names(dataset_brick)), format = "%Y.%m.%d"))
  }
  if (anyNA(layer_years)) {
    stop("Could not derive layer years for: ", basename(file), call. = FALSE)
  }
  
  ## lon/lat for every grid cell, computed once and indexed per layer.
  cell_xy <- xyFromCell(dataset_brick, seq_len(ncell(dataset_brick)))
  
  layer_list <- vector("list", nlayers(dataset_brick))
  
  for (l in seq_len(nlayers(dataset_brick))) {
    layer_values <- values(dataset_brick[[l]])   # one global field (~1M cells)
    keep <- which(!is.na(layer_values))
    if (length(keep) == 0L) next
    
    layer_list[[l]] <- data.table(
      lon   = cell_xy[keep, 1L],
      lat   = cell_xy[keep, 2L],
      year  = layer_years[l],
      value = round(layer_values[keep], 0)
    )
  }
  
  rm(dataset_brick)                              # release the raster handle
  
  dataset_dt <- rbindlist(layer_list, use.names = TRUE)
  dataset_dt[, `:=`(
    variable = variable_name,
    dataset  = dataset_name
  )]
  dataset_dt[]
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

# Analysis ===================================================================

# Resolve each input file to a short dataset name and tag its variable.
prec_datasets_used <- build_input_table(prec_files, "prec")
evap_datasets_used <- build_input_table(evap_files, "evap")

input_dt <- rbindlist(
  list(prec_datasets_used, evap_datasets_used),
  use.names = TRUE
)

## Fail if any file could not be mapped to a short dataset name.
unresolved <- input_dt[is.na(name)]$file

if (length(unresolved) > 0) {
  stop(
    "Could not map these input files to a dataset name:\n",
    paste(unresolved, collapse = "\n"),
    call. = FALSE
  )
}

# Ingest one file at a time. read_grid_file drops NA (ocean) cells and releases
# the brick before the next file, so peak memory is a single global field plus
# the accumulating land-only rows -- never all datasets or a full cube at once.
dt_list <- vector("list", nrow(input_dt))

for (i in seq_len(nrow(input_dt))) {
  message(input_dt$name[i])
  dt_list[[i]] <- read_grid_file(
    file          = input_dt$file[i],
    dataset_name  = input_dt$name[i],
    variable_name = input_dt$variable[i]
  )
}

prec_evap_raw <- rbindlist(dt_list, use.names = TRUE)
rm(dt_list)

# Keep only grid cells covered by every dataset. NA cells were already dropped,
# so a cell's distinct-dataset count is its coverage.
complete_grids <- prec_evap_raw[
  ,
  .(n_datasets = uniqueN(dataset)),
  by = .(lon, lat)
][
  n_datasets == max(n_datasets),
  .(lon, lat)
]

prec_evap_raw <- prec_evap_raw[complete_grids, on = .(lon, lat), nomatch = 0L]

setcolorder(
  prec_evap_raw,
  c("lon", "lat", "year", "dataset", "variable", "value")
)

setorder(
  prec_evap_raw,
  variable, dataset, lon, lat, year
)

# Widen selected analysis datasets -------------------------------------------

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

write_fst(
  prec_evap,
  file.path(PATH_OUTPUT_OUTPUT, "prec_evap.fst")
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

## No NA values should remain in the land-only raw table.
stopifnot(prec_evap_raw[is.na(value), .N] == 0)

## Every analysis dataset should survive to the wide table.
stopifnot(all(EVAP_NAMES_SHORT %in% prec_evap$dataset))

# Yearly time series at 9 random grid cells ----------------------------------

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

# Mean fields by dataset -----------------------------------------------------

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
