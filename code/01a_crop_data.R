# ============================================================================
# Crop the raw precipitation and evaporation datasets to the study period.
#
# Lists the raw yearly NetCDF files that 00b_data_download places in
# PATH_OUTPUT_RAW_PREC / PATH_OUTPUT_RAW_EVAP, subsets each of them to
# FULL_PERIOD (1982-2021), and writes the cropped file back in place for the
# downstream preparation step (01b).
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

crop_file_in_place <- function(file, period) {
  result <- subset_data(file, yrs = period)

  ## Write to a temporary file first, then replace, so the source NetCDF is
  ## never truncated while it is still being read.
  tmp_file <- file.path(dirname(file), paste0(".tmp_", basename(file)))
  saveNC(result, tmp_file)
  file.copy(tmp_file, file, overwrite = TRUE)
  unlink(tmp_file)

  invisible(file)
}

# Analysis ===================================================================

foreach(
  file_count = seq_along(raw_files),
  .packages = c("raster", "pRecipe", "lubridate")
) %dopar% {
  crop_file_in_place(raw_files[file_count], FULL_PERIOD)
}

# Outputs ====================================================================
#
# The cropped datasets replace the raw files in place under
# PATH_OUTPUT_RAW_PREC / PATH_OUTPUT_RAW_EVAP.

# Validation =================================================================

invalid_files <- raw_files[!file.exists(raw_files) | file.size(raw_files) == 0]

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
  " (", length(prec_files), " prec, ", length(evap_files), " evap)."
)
