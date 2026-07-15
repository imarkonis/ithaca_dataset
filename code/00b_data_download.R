# ============================================================================
# Download raw precipitation, evaporation, and PET data from Zenodo
#
# This script downloads the yearly land datasets required by the ITHACA
# workflow. Most precipitation and evaporation files come from the pRecipe and
# evapoRe Zenodo records. GPCC and the MERRA-2/MSWX PET product come from the
# ITHACA dataset Zenodo record. Existing files are left untouched.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(jsonlite)

# Inputs =====================================================================

PRECIPE_RECORD_ID <- "14290970"
EVAPORE_RECORD_ID <- "21369628"
ITHACA_RECORD_ID <- "21353180"

# Constants & Variables ======================================================

PREC_NAMES_SHORT_LCASE <- c(
  "era5-land",
  "fldas",
  "merra",
  "terraclimate",
  "mswep"
)

# GPCC is deliberately omitted because it is downloaded from ITHACA_RECORD_ID.
PREC_ENSEMBLE_NAMES_SHORT_LCASE <- c(
  "cpc",
  "em-earth",
  "era5-land",
  "fldas",
  "merra",
  "precl",
  "terraclimate"
)

PREC_ALL_NAMES_SHORT_LCASE <- unique(c(
  PREC_NAMES_SHORT_LCASE,
  PREC_ENSEMBLE_NAMES_SHORT_LCASE
))

PREC_NAMES_PATTERN <- paste0(
  PREC_ALL_NAMES_SHORT_LCASE,
  collapse = "|"
)

EVAP_ENSEMBLE_NAMES_SHORT_LCASE <- c(
  "bess",
  "era5-land",
  "etmonitor",
  "etsynthesis",
  "fldas",
  "gleam-v4-1a",
  "merra",
  "terraclimate"
)

EVAP_NAMES_PATTERN <- paste0(
  EVAP_ENSEMBLE_NAMES_SHORT_LCASE,
  collapse = "|"
)

ITHACA_FILE_DESTINATIONS <- c(
  "gpcc-v2022_tp_mm_land_198101_202012_025_yearly.nc" =
    PATH_OUTPUT_RAW_PREC,
  "merra2_mswx_pet_mm_1980_2024_yearly.fst" =
    PATH_OUTPUT_RAW_OTHER
)

options(timeout = max(600, getOption("timeout")))

# Functions ==================================================================

get_zenodo_files <- function(record_id) {
  api_url <- paste0("https://zenodo.org/api/records/", record_id)
  record <- fromJSON(api_url, simplifyVector = TRUE)

  record$files
}

build_download_manifest <- function(files, output_dir) {
  data.frame(
    file_name = files$key,
    download_url = files$links$self,
    output_dir = rep(output_dir, length.out = nrow(files)),
    expected_size = files$size,
    stringsAsFactors = FALSE
  )
}

download_manifest_files <- function(manifest) {
  for (i in seq_len(nrow(manifest))) {
    out_path <- file.path(
      manifest$output_dir[i],
      manifest$file_name[i]
    )

    if (file.exists(out_path)) {
      message("Already exists, skipping: ", manifest$file_name[i])
      next
    }

    message("Downloading: ", manifest$file_name[i])
    download.file(
      url = manifest$download_url[i],
      destfile = out_path,
      mode = "wb",
      quiet = FALSE
    )
  }

  invisible(manifest)
}

# Analysis ===================================================================

precip_files <- get_zenodo_files(PRECIPE_RECORD_ID)

selected_files_prec <- precip_files[
  grepl("land", precip_files$key) &
    grepl("yearly", precip_files$key) &
    grepl(PREC_NAMES_PATTERN, precip_files$key),
]

evap_files <- get_zenodo_files(EVAPORE_RECORD_ID)

selected_files_evap <- evap_files[
  grepl("land", evap_files$key) &
    grepl("yearly", evap_files$key) &
    grepl("_e_", evap_files$key) &
    grepl(EVAP_NAMES_PATTERN, evap_files$key),
]

ithaca_files <- get_zenodo_files(ITHACA_RECORD_ID)
ithaca_file_index <- match(
  names(ITHACA_FILE_DESTINATIONS),
  ithaca_files$key
)

if (anyNA(ithaca_file_index)) {
  stop(
    "ITHACA Zenodo record is missing expected files:\n",
    paste(
      names(ITHACA_FILE_DESTINATIONS)[is.na(ithaca_file_index)],
      collapse = "\n"
    ),
    call. = FALSE
  )
}

selected_files_ithaca <- ithaca_files[ithaca_file_index,]

download_manifest <- rbind(
  build_download_manifest(
    selected_files_prec,
    PATH_OUTPUT_RAW_PREC
  ),
  build_download_manifest(
    selected_files_evap,
    PATH_OUTPUT_RAW_EVAP
  ),
  build_download_manifest(
    selected_files_ithaca,
    unname(ITHACA_FILE_DESTINATIONS)
  )
)

download_manifest_files(download_manifest)

# Outputs ====================================================================

# Raw precipitation NetCDF files are written to PATH_OUTPUT_RAW_PREC, raw
# evaporation NetCDF files to PATH_OUTPUT_RAW_EVAP, and the MERRA-2/MSWX PET
# FST file to PATH_OUTPUT_RAW_OTHER. The complete expected output list is held
# in download_manifest.

# Validation =================================================================

download_manifest$file_path <- file.path(
  download_manifest$output_dir,
  download_manifest$file_name
)

missing_files <- download_manifest$file_path[
  !file.exists(download_manifest$file_path)
]

if (length(missing_files) > 0) {
  stop(
    "Expected downloads are missing:\n",
    paste(missing_files, collapse = "\n"),
    call. = FALSE
  )
}

actual_sizes <- file.size(download_manifest$file_path)
invalid_size <- is.na(actual_sizes) |
  actual_sizes != download_manifest$expected_size

if (any(invalid_size)) {
  stop(
    "Downloaded files do not match their expected Zenodo sizes:\n",
    paste(download_manifest$file_path[invalid_size], collapse = "\n"),
    call. = FALSE
  )
}

expected_prec_files <- download_manifest$file_name[
  download_manifest$output_dir == PATH_OUTPUT_RAW_PREC
]
expected_evap_files <- download_manifest$file_name[
  download_manifest$output_dir == PATH_OUTPUT_RAW_EVAP
]

actual_prec_files <- list.files(
  PATH_OUTPUT_RAW_PREC,
  pattern = "\\.nc$"
)
actual_evap_files <- list.files(
  PATH_OUTPUT_RAW_EVAP,
  pattern = "\\.nc$"
)

unexpected_files <- c(
  file.path(
    PATH_OUTPUT_RAW_PREC,
    setdiff(actual_prec_files, expected_prec_files)
  ),
  file.path(
    PATH_OUTPUT_RAW_EVAP,
    setdiff(actual_evap_files, expected_evap_files)
  )
)

if (length(unexpected_files) > 0) {
  warning(
    "Unexpected raw NetCDF files are present:\n",
    paste(unexpected_files, collapse = "\n"),
    call. = FALSE
  )
}

if (any(grepl("^gpcc", selected_files_prec$key))) {
  stop(
    "GPCC must not be selected from the pRecipe Zenodo record.",
    call. = FALSE
  )
}

message(
  "Validated ", nrow(download_manifest), " Zenodo downloads: ",
  length(expected_prec_files), " precipitation, ",
  length(expected_evap_files), " evaporation, and 1 PET file."
)
