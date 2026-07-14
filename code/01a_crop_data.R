# ============================================================================
# Crop the raw precipitation and evaporation datasets to the study period.
#
# Lists the raw yearly NetCDF files that 00b_data_download places in
# PATH_OUTPUT_RAW_PREC / PATH_OUTPUT_RAW_EVAP, subsets each of them to
# FULL_PERIOD (1982-2021), and saves the cropped files under PATH_OUTPUT_INPUT
# for the downstream preparation step (01b). The raw files are left untouched.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(pRecipe)
library(doParallel)

# Parallel setup =============================================================

registerDoParallel(max(N_DATASETS_PREC, N_DATASETS_EVAP))

# Inputs =====================================================================
#
# Every yearly NetCDF downloaded by 00b_data_download, discovered by listing the
# raw precipitation and evaporation folders (Zenodo filenames are kept as-is).

prec_files <- list.files(
  PATH_OUTPUT_RAW_PREC,
  pattern = "\\.nc$",
  full.names = TRUE
)

evap_files <- list.files(
  PATH_OUTPUT_RAW_EVAP,
  pattern = "\\.nc$",
  full.names = TRUE
)

raw_files <- c(prec_files, evap_files)

if (length(raw_files) == 0) {
  stop(
    "No raw .nc files found in\n  ", PATH_OUTPUT_RAW_PREC,
    "\n  ", PATH_OUTPUT_RAW_EVAP,
    "\nRun 00b_data_download first.",
    call. = FALSE
  )
}

# Functions ==================================================================

crop_and_save_file <- function(file, path_out, period) {
  result <- subset_data(file, yrs = period)
  saveNC(result, file.path(path_out, basename(file)))

  invisible(file)
}

# Analysis ===================================================================

foreach(
  file_count = seq_along(raw_files),
  .packages = c("raster", "pRecipe", "lubridate")
) %dopar% {
  crop_and_save_file(raw_files[file_count], PATH_OUTPUT_INPUT, FULL_PERIOD)
}

# Outputs ====================================================================
#
# One cropped NetCDF per raw dataset, written under PATH_OUTPUT_INPUT with the
# original filename. The raw files remain untouched.

# Validation =================================================================

cropped_files <- file.path(PATH_OUTPUT_INPUT, basename(raw_files))
invalid_files <- cropped_files[!file.exists(cropped_files) | file.size(cropped_files) == 0]

if (length(invalid_files) > 0) {
  stop(
    "Cropping did not produce valid files:\n",
    paste(invalid_files, collapse = "\n"),
    call. = FALSE
  )
}

message(
  "Cropped ", length(raw_files), " datasets to ",
  FULL_PERIOD["START"], "-", FULL_PERIOD["END"],
  " (", length(prec_files), " prec, ", length(evap_files), " evap) into ",
  PATH_OUTPUT_INPUT, "."
)
