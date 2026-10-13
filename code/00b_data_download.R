# ============================================================================
# Download the raw input data of the ITHACA workflow from the frozen Zenodo
# input snapshot
#
# The analysis uses one frozen input snapshot: version v3 of the ITHACA dataset
# record on Zenodo, https://doi.org/10.5281/zenodo.23193962. This script
# downloads exactly the 35 NetCDF files named below from that record and checks
# every file against the size and md5 checksum that Zenodo lists for it:
#
#   9 yearly land precipitation files (incl. GPCC) -> PATH_OUTPUT_RAW_PREC
#   8 yearly land evaporation files                -> PATH_OUTPUT_RAW_EVAP
#  18 monthly PET forcing files                    -> PATH_OUTPUT_RAW_OTHER
#
# The pinned record is the reproducibility source for the paper. The pRecipe,
# evapoRe and tempeR records, and the MERRA-2, MSWX-Past and ERA5-Land products
# the files were made from, are provenance sources, not runtime dependencies of
# this script. PET itself is not downloaded: 01c_prepare_pet_forcing.R and
# 01d_estimate_pet.R derive it from the forcing files. Files that already exist
# are not downloaded again, but they must pass the same checks. A file that
# exists but differs from the snapshot stops the script, unless
# ARCHIVE_REPLACED_FILES is TRUE: it is then moved to an rchive folder inside
# its raw folder and the snapshot copy is downloaded.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(jsonlite)

# Inputs =====================================================================

# Version record of the frozen snapshot. Do not use the concept DOI
# (10.5281/zenodo.21353179) or Zenodo's "latest" redirect: the script must stay
# on the exact version used by the paper.
ITHACA_RECORD_ID <- "23193962"

# Constants & Variables ======================================================

# The exact files of the snapshot that the workflow reads. Names are not
# matched by pattern: this list is the input contract. The precipitation and
# evaporation names are the ones 01a and 01b expect, and the forcing names must
# match MERRA2_FILES and MSWX_FILES in 01c_prepare_pet_forcing.R.
PREC_FILES <- c(
  "cpc-global_tp_mm_land_197901_202309_025_yearly.nc",
  "em-earth_tp_mm_land_195001_201912_025_yearly.nc",
  "era5-land_tp_mm_land_195001_202112_025_yearly.nc",
  "fldas_tp_mm_land_198201_202410_025_yearly.nc",
  "gpcc-v2022_tp_mm_land_198101_202012_025_yearly.nc",
  "merra-2_tp_mm_land_198001_202410_025_yearly.nc",
  "mswep-v2-8_tp_mm_land_197901_202411_025_yearly.nc",
  "precl_tp_mm_land_194801_202410_025_yearly.nc",
  "terraclimate_tp_mm_land_195801_202312_025_yearly.nc"
)

EVAP_FILES <- c(
  "bess_e_mm_land_198201_201912_025_yearly.nc",
  "era5-land_e_mm_land_195001_202112_025_yearly.nc",
  "etmonitor_e_mm_land_200006_201912_025_yearly.nc",
  "etsynthesis_e_mm_land_200001_201912_025_yearly.nc",
  "fldas_e_mm_land_198201_202212_025_yearly.nc",
  "gleam-v4-1a_e_mm_land_198001_202312_025_yearly.nc",
  "merra2_e_mm_land_198001_202301_025_yearly.nc",
  "terraclimate_e_mm_land_195801_202112_025_yearly.nc"
)

PET_FORCING_FILES <- c(
  "era5-land_albedo_198001_202501_025_monthly.nc",
  "merra2_2sh_kgkg-1_land_198001_202511_025_monthly.nc",
  "merra2_sp_Pa_land_198001_202511_025_monthly.nc",
  "merra2_ssrd_wm-2_land_198001_202511_025_monthly.nc",
  "merra2_strd_wm-2_land_198001_202511_025_monthly.nc",
  "merra2_t2m_degC_land_198001_202511_025_monthly.nc",
  "merra2_tmax_degC_land_198001_202512_025_monthly.nc",
  "merra2_tmin_degC_land_198001_202512_025_monthly.nc",
  "merra2_u2m_ms-1_land_198001_202511_025_monthly.nc",
  "merra2_v2m_ms-1_land_198001_202511_025_monthly.nc",
  "mswx-past_r_pct_land_197901_202512_025_monthly.nc",
  "mswx-past_sp_pa_land_197901_202412_025_monthly.nc",
  "mswx-past_ssrd_Wm-2_land_197902_202512_025_monthly.nc",
  "mswx-past_strd_Wm-2_land_197902_202512_025_monthly.nc",
  "mswx-past_t2m_degC_land_197901_202512_025_monthly.nc",
  "mswx-past_tmax_degC_land_197901_202512_025_monthly.nc",
  "mswx-past_tmin_degC_land_197901_202512_025_monthly.nc",
  "mswx-past_u10_ms-1_land_197901_202412_025_monthly.nc"
)

# Existing files that differ from the snapshot (wrong size or md5): FALSE stops
# the script and lists them, TRUE moves them to ARCHIVE_FOLDER (a subfolder of
# their raw folder, which 01a does not read) and downloads the snapshot copy.
ARCHIVE_REPLACED_FILES <- FALSE
ARCHIVE_FOLDER <- "archive"

# The largest files are 460-600 MB, so allow up to an hour per file.
options(timeout = max(3600, getOption("timeout")))

# Functions ==================================================================

# Files of a Zenodo record with name, size, md5 checksum and download link.
# Zenodo sometimes answers with a transient gateway error, so retry.
get_zenodo_files <- function(record_id, attempts = 3) {
  api_url <- paste0("https://zenodo.org/api/records/", record_id)

  for (attempt in seq_len(attempts)) {
    record <- tryCatch(
      fromJSON(api_url, simplifyVector = TRUE),
      error = function(e) e
    )

    if (!inherits(record, "error")) {
      return(record$files)
    }

    message("Zenodo request failed (attempt ", attempt, "): ", conditionMessage(record))
    Sys.sleep(5 * attempt)
  }

  stop("Could not read Zenodo record ", record_id, ".", call. = FALSE)
}

# One row per expected file, taken from the record. A name that is not in the
# record is an error: the record defines the input contract.
build_download_manifest <- function(file_names, output_dir, record_files) {
  index <- match(file_names, record_files$key)

  if (anyNA(index)) {
    stop(
      "Zenodo record ", ITHACA_RECORD_ID, " is missing expected files:\n",
      paste(file_names[is.na(index)], collapse = "\n"),
      call. = FALSE
    )
  }

  data.frame(
    file_name = file_names,
    download_url = record_files$links$self[index],
    output_dir = output_dir,
    expected_size = record_files$size[index],
    expected_md5 = sub("^md5:", "", record_files$checksum[index]),
    stringsAsFactors = FALSE
  )
}

# Move every existing file that differs from the snapshot into ARCHIVE_FOLDER,
# next to where it was. The file keeps its name; if that name is already taken in
# the archive, a timestamp is added. Returns the new paths of the archived files.
archive_mismatched_files <- function(manifest) {
  archived <- character(0)

  for (i in seq_len(nrow(manifest))) {
    path <- file.path(manifest$output_dir[i], manifest$file_name[i])

    if (!file.exists(path)) {
      next
    }

    is_valid <- file.size(path) == manifest$expected_size[i] &&
      unname(tools::md5sum(path)) == manifest$expected_md5[i]

    if (is_valid) {
      next
    }

    archive_dir <- file.path(manifest$output_dir[i], ARCHIVE_FOLDER)
    dir.create(archive_dir, showWarnings = FALSE)
    target <- file.path(archive_dir, manifest$file_name[i])

    if (file.exists(target)) {
      stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      target <- file.path(
        archive_dir,
        sub("\\.nc$", paste0("_", stamp, ".nc"), manifest$file_name[i])
      )
    }

    if (!file.rename(path, target)) {
      stop("Could not move ", path, " to ", target, call. = FALSE)
    }

    message("Archived (differs from the snapshot): ", manifest$file_name[i], " -> ", target)
    archived <- c(archived, target)
  }

  invisible(archived)
}

# A file is downloaded under a temporary name and renamed once it has the
# expected size, so an interrupted download is never mistaken for a finished one.
download_manifest_files <- function(manifest) {
  for (i in seq_len(nrow(manifest))) {
    out_path <- file.path(manifest$output_dir[i], manifest$file_name[i])

    if (file.exists(out_path)) {
      message("Already exists, skipping: ", manifest$file_name[i])
      next
    }

    message("Downloading: ", manifest$file_name[i])
    part_path <- paste0(out_path, ".part")

    download.file(
      url = manifest$download_url[i],
      destfile = part_path,
      mode = "wb",
      quiet = FALSE
    )

    if (file.size(part_path) != manifest$expected_size[i]) {
      file.remove(part_path)
      stop("Incomplete download: ", manifest$file_name[i], call. = FALSE)
    }

    file.rename(part_path, out_path)
  }

  invisible(manifest)
}

# Analysis ===================================================================

record_files <- get_zenodo_files(ITHACA_RECORD_ID)

download_manifest <- rbind(
  build_download_manifest(PREC_FILES, PATH_OUTPUT_RAW_PREC, record_files),
  build_download_manifest(EVAP_FILES, PATH_OUTPUT_RAW_EVAP, record_files),
  build_download_manifest(PET_FORCING_FILES, PATH_OUTPUT_RAW_OTHER, record_files)
)

stopifnot(!anyDuplicated(download_manifest$file_name))

archived_files <- character(0)

if (ARCHIVE_REPLACED_FILES) {
  archived_files <- archive_mismatched_files(download_manifest)
}

download_manifest_files(download_manifest)

# Outputs ====================================================================

# The 35 NetCDF files are in place: precipitation in PATH_OUTPUT_RAW_PREC,
# evaporation in PATH_OUTPUT_RAW_EVAP and PET forcing in PATH_OUTPUT_RAW_OTHER.
# The complete list is held in download_manifest.

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
    "Files do not match their Zenodo sizes (delete them and run again, or set ",
    "ARCHIVE_REPLACED_FILES <- TRUE to move them to an archive folder):\n",
    paste(download_manifest$file_path[invalid_size], collapse = "\n"),
    call. = FALSE
  )
}

actual_md5 <- unname(tools::md5sum(download_manifest$file_path))
invalid_md5 <- is.na(actual_md5) | actual_md5 != download_manifest$expected_md5

if (any(invalid_md5)) {
  stop(
    "Files do not match their Zenodo md5 checksums (delete them and run again, or ",
    "set ARCHIVE_REPLACED_FILES <- TRUE to move them to an archive folder):\n",
    paste(download_manifest$file_path[invalid_md5], collapse = "\n"),
    call. = FALSE
  )
}

# NetCDF files in the raw folders that are not part of the snapshot. 01a crops
# every .nc in the precipitation and evaporation folders, so they would be used.
unexpected_files <- unlist(lapply(
  unique(download_manifest$output_dir),
  function(folder) {
    expected <- download_manifest$file_name[download_manifest$output_dir == folder]
    found <- list.files(folder, pattern = "\\.nc$")

    file.path(folder, setdiff(found, expected))
  }
))

if (length(unexpected_files) > 0) {
  warning(
    "Unexpected raw NetCDF files are present:\n",
    paste(unexpected_files, collapse = "\n"),
    call. = FALSE
  )
}

message(
  "Validated ", nrow(download_manifest), " files from Zenodo record ",
  ITHACA_RECORD_ID, " (size and md5): ",
  length(PREC_FILES), " precipitation, ",
  length(EVAP_FILES), " evaporation and ",
  length(PET_FORCING_FILES), " PET forcing",
  if (length(archived_files) > 0) {
    paste0("; archived ", length(archived_files), " replaced file(s) in ", ARCHIVE_FOLDER)
  },
  "."
)
