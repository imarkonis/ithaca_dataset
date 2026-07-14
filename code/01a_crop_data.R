# ============================================================================
# Crop the raw precipitation and evaporation datasets to the study period.
#
# Lists the raw yearly NetCDF files that 00b_data_download places in
# PATH_OUTPUT_RAW_PREC / PATH_OUTPUT_RAW_EVAP, subsets each of them to
# FULL_PERIOD (1982-2021), and saves the cropped files under PATH_OUTPUT_INPUT
# for the downstream preparation step (01b). The raw files are left untouched.
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

## Rewrite the "<start><end>" period token of a Zenodo filename to the cropped
## period, e.g. "_198101_202012_" -> "_198201_202112_". Filenames that do not
## carry the token are returned unchanged.
cropped_basename <- function(file, period) {
  sub(
    "_[0-9]{6}_[0-9]{6}_",
    paste0("_", period[["START"]], "01_", period[["END"]], "12_"),
    basename(file)
  )
}

## raster::brick() picks the data variable automatically. Some products (GLEAM)
## ship extra variables (e.g. time bounds), which makes that guess fail with
## "incorrect number of layer names". Fall back to naming the data variable
## explicitly: the first 3-D variable that is not a bounds/CRS helper.
read_dataset_brick <- function(file) {
  dataset_brick <- try(brick(file), silent = TRUE)

  if (!inherits(dataset_brick, "try-error")) {
    return(dataset_brick)
  }

  nc <- nc_open(file)
  on.exit(nc_close(nc), add = TRUE)

  var_n_dims <- vapply(nc$var, function(x) x$ndims, integer(1))

  data_vars <- names(var_n_dims)[
    var_n_dims >= 3 &
      !grepl("bnds|bounds|crs|spatial_ref", names(var_n_dims), ignore.case = TRUE)
  ]

  if (length(data_vars) == 0) {
    stop("No 3-D data variable found in ", basename(file), call. = FALSE)
  }

  brick(file, varname = data_vars[1])
}

crop_and_save_file <- function(file, path_out, period) {
  result <- subset_data(read_dataset_brick(file), yrs = period)
  saveNC(result, file.path(path_out, cropped_basename(file, period)))

  invisible(file)
}

# Analysis ===================================================================

foreach(
  file_count = seq_along(raw_files),
  .packages = c("raster", "pRecipe", "lubridate", "ncdf4")
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

message(
  "Cropped ", length(raw_files), " datasets to ",
  FULL_PERIOD["START"], "-", FULL_PERIOD["END"],
  " (", length(prec_files), " prec, ", length(evap_files), " evap) into ",
  PATH_OUTPUT_INPUT, "."
)
