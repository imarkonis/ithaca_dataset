# ============================================================================
# Crop the raw precipitation and evaporation datasets to the study period.
#
# Reads the raw yearly land datasets downloaded locally by 00b_data_download,
# subsets each of them to FULL_PERIOD (1982-2021), and saves one cropped
# <fname>_yearly.nc per dataset under PATH_OUTPUT_RAW_PREC / PATH_OUTPUT_RAW_EVAP
# for the downstream preparation step (01b).
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(pRecipe)
library(doParallel)

# Parallel setup =============================================================

registerDoParallel(max(N_DATASETS_PREC, N_DATASETS_EVAP))

# Inputs =====================================================================
#
# Raw yearly land datasets downloaded locally by 00b_data_download:
#   * precipitation & evaporation collections obtained via pRecipe
#     (located through filter_datasets()$file);
#   * GPCC (v2022) obtained from the ITHACA Zenodo repository and stored as a
#     stand-alone NetCDF under PATH_OUTPUT_RAW_PREC.

# Constants & Variables ======================================================

GPCC_RAW_FILE <- file.path(
  PATH_OUTPUT_RAW_PREC,
  "gpcc-v2022_tp_mm_land_198101_202012_025_yearly.nc"
)

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

# Analysis ===================================================================

## Precipitation =============================================================

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

## GPCC is downloaded from Zenodo (not pRecipe), so point its raw input at the
## local Zenodo file while keeping the pRecipe-based cropped output name.
prec_datasets_used[name == "GPCC", file_raw := GPCC_RAW_FILE]

## Evaporation ===============================================================

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

## Check that every raw input downloaded by 00b is available before cropping.
raw_inputs <- c(prec_datasets_used$file_raw, evap_datasets_used$file_raw)
missing_inputs <- raw_inputs[!file.exists(raw_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing raw input files (run 00b_data_download first):\n",
    paste(missing_inputs, collapse = "\n"),
    call. = FALSE
  )
}

# Outputs ====================================================================

subset_and_save_dataset_files(
  datasets_used = prec_datasets_used,
  period = FULL_PERIOD
)

subset_and_save_dataset_files(
  datasets_used = evap_datasets_used,
  period = FULL_PERIOD
)

# Validation =================================================================

expected_files <- c(prec_datasets_used$file, evap_datasets_used$file)
missing_files <- expected_files[!file.exists(expected_files)]

if (length(missing_files) > 0) {
  stop(
    "Cropping did not produce all expected files:\n",
    paste(missing_files, collapse = "\n"),
    call. = FALSE
  )
}

message(
  "Cropped ", length(expected_files), " datasets to ",
  FULL_PERIOD["START"], "-", FULL_PERIOD["END"],
  " (", nrow(prec_datasets_used), " prec, ",
  nrow(evap_datasets_used), " evap)."
)
