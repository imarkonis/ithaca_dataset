# ============================================================================
# Crop the raw precipitation and evaporation datasets to the study period.
#
# Lists the raw yearly NetCDF files that 00b_data_download places in
# PATH_OUTPUT_RAW_PREC / PATH_OUTPUT_RAW_EVAP, subsets each of them to
# FULL_PERIOD (1982-2021), and saves the cropped files under PATH_OUTPUT_INPUT
# for the downstream preparation step (01b). The raw files are left untouched.
#
# The yearly date axis of each dataset is rebuilt from the period token in its
# filename rather than taken from the file, because the products do not encode
# time consistently (see yearly_dates_from_filename).
#
# The cropped files keep the Zenodo naming convention, but their period token is
# rewritten to the cropped period, so that a cropped file is never mistaken for
# the original download:
#   gpcc-v2022_tp_mm_land_198101_202012_025_yearly.nc  (raw)
#   gpcc-v2022_tp_mm_land_198201_202112_025_yearly.nc  (cropped)
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(doParallel)

# Parallel setup =============================================================

registerDoParallel(max(N_DATASETS_PREC, N_DATASETS_EVAP))

# Inputs =====================================================================

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

## First and last year a dataset covers, read from the period token in its
## filename, e.g. "..._198001_202312_..." -> c(1980, 2023).
dataset_period <- function(file) {
  period_token <- regmatches(
    basename(file),
    regexpr("[0-9]{6}_[0-9]{6}", basename(file))
  )

  if (length(period_token) == 0) {
    stop("No period token in filename: ", basename(file), call. = FALSE)
  }

  c(
    as.integer(substr(period_token, 1, 4)),
    as.integer(substr(period_token, 8, 11))
  )
}

## Years a dataset retains once cropped: its own coverage clipped to the study
## period. Datasets need not span the whole study period (GPCC ends in 2020).
cropped_period <- function(file, period) {
  dataset_years <- dataset_period(file)

  c(
    max(dataset_years[1], period[["START"]]),
    min(dataset_years[2], period[["END"]])
  )
}

## Name a cropped file after the period it actually contains, so it can never be
## confused with the raw download, e.g.
##   gpcc-v2022_tp_mm_land_198101_202012_025_yearly.nc  (raw, 1981-2020)
##   gpcc-v2022_tp_mm_land_198201_202012_025_yearly.nc  (cropped, 1982-2020)
cropped_basename <- function(file, period) {
  vapply(file, function(dataset_file) {
    cropped_years <- cropped_period(dataset_file, period)

    sub(
      "_[0-9]{6}_[0-9]{6}_",
      paste0("_", cropped_years[1], "01_", cropped_years[2], "12_"),
      basename(dataset_file)
    )
  }, character(1), USE.NAMES = FALSE)
}

## Build the yearly date axis of a dataset from its filename period.
##
## We do not trust the time coordinate stored in the file: GLEAM writes it as
## plain layer indices (0, 1, ... n-1) rather than a CF date axis. pRecipe's
## aux_date() then reads those as "days since 1970-01-01", placing every layer
## in 1970, so the FULL_PERIOD selection matches nothing and raster::subset()
## fails with "incorrect number of layer names". Rebuilding the axis from the
## filename gives every product the same, correct yearly axis.
yearly_dates_from_filename <- function(file, n_layers) {
  dataset_years <- do.call(seq, as.list(dataset_period(file)))

  if (length(dataset_years) != n_layers) {
    stop(
      basename(file), " has ", n_layers, " layers but its filename spans ",
      dataset_years[1], "-", dataset_years[length(dataset_years)],
      " (", length(dataset_years), " years).",
      call. = FALSE
    )
  }

  as.Date(paste0(dataset_years, "-01-01"))
}

crop_and_save_file <- function(file, path_out, period) {
  dataset_brick <- brick(file)

  dataset_brick <- setZ(
    dataset_brick,
    yearly_dates_from_filename(file, nlayers(dataset_brick))
  )

  result <- subset_data(dataset_brick, yrs = period)
  saveNC(result, file.path(path_out, cropped_basename(file, period)))

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

cropped_files <- file.path(
  PATH_OUTPUT_INPUT,
  cropped_basename(raw_files, FULL_PERIOD)
)

# Validation =================================================================

invalid_files <- cropped_files[
  !file.exists(cropped_files) | file.size(cropped_files) == 0
]

if (length(invalid_files) > 0) {
  stop(
    "Cropping did not produce valid files:\n",
    paste(invalid_files, collapse = "\n"),
    call. = FALSE
  )
}

## Report datasets that do not cover the whole study period, so partial coverage
## is a stated fact rather than a surprise downstream.
partial_files <- raw_files[
  vapply(
    raw_files,
    function(dataset_file) {
      !identical(
        as.integer(cropped_period(dataset_file, FULL_PERIOD)),
        as.integer(c(FULL_PERIOD[["START"]], FULL_PERIOD[["END"]]))
      )
    },
    logical(1),
    USE.NAMES = FALSE
  )
]

if (length(partial_files) > 0) {
  message(
    "Datasets not covering ", FULL_PERIOD["START"], "-", FULL_PERIOD["END"], ":\n",
    paste0("  ", cropped_basename(partial_files, FULL_PERIOD), collapse = "\n")
  )
}

message(
  "Cropped ", length(raw_files), " datasets to ",
  FULL_PERIOD["START"], "-", FULL_PERIOD["END"],
  " (", length(prec_files), " prec, ", length(evap_files), " evap) into ",
  PATH_OUTPUT_INPUT, "."
)
