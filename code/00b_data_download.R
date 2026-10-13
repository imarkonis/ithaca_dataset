# ============================================================================
# Download the raw input data of the ITHACA workflow from the frozen Zenodo
# input snapshot
#
# The analysis uses one frozen input snapshot: version v3 of the ITHACA dataset
# record on Zenodo, https://doi.org/10.5281/zenodo.23193962. This script
# downloads every NetCDF file of that record (35 in v3) and checks each one
# against the size and md5 checksum that Zenodo lists for it. The record, not
# this script, defines which files are the inputs: a new snapshot only needs a
# new ITHACA_RECORD_ID. The folder of a file follows from its name:
#
#   *_monthly.nc       PET forcing               -> PATH_OUTPUT_RAW_OTHER
#   *_e_*_yearly.nc    yearly land evaporation   -> PATH_OUTPUT_RAW_EVAP
#   *_tp_*_yearly.nc   yearly land precipitation -> PATH_OUTPUT_RAW_PREC
#
# A NetCDF file that fits none of these names, or a folder left without any
# file, stops the script. Files that are not NetCDF are not downloaded: the
# record also holds the derived PET table merra2_mswx_pet_mm_1980_2024_yearly.fst,
# but PET is derived locally from the forcing files by
# 01c_prepare_pet_forcing.R and 01d_estimate_pet.R.
#
# The pinned record is the reproducibility source for the paper. The pRecipe,
# evapoRe and tempeR records, and the MERRA-2, MSWX-Past and ERA5-Land products
# the files were made from, are provenance sources, not runtime dependencies of
# this script. Files that already exist are not downloaded again, but they must
# pass the same checks. A file that exists but differs from the snapshot stops
# the script, unless ARCHIVE_REPLACED_FILES is TRUE: it is then moved to an
# "archive" folder inside its raw folder and the snapshot copy is downloaded.
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

# Raw folder of a NetCDF file, decided by its name; NA if the name fits no kind.
raw_folder_for <- function(file_names) {
  folder <- rep(NA_character_, length(file_names))

  folder[grepl("_tp_.*_yearly\\.nc$", file_names)] <- PATH_OUTPUT_RAW_PREC
  folder[grepl("_e_.*_yearly\\.nc$", file_names)] <- PATH_OUTPUT_RAW_EVAP
  folder[grepl("_monthly\\.nc$", file_names)] <- PATH_OUTPUT_RAW_OTHER

  folder
}

# One row per NetCDF file of the record, with the size, md5 and link Zenodo
# lists for it. A name that fits no raw folder, or a raw folder that would stay
# empty, is an error: the downstream scripts could not work with that record.
build_download_manifest <- function(record_files) {
  record_files <- record_files[grepl("\\.nc$", record_files$key), ]
  output_dir <- raw_folder_for(record_files$key)

  if (anyNA(output_dir)) {
    stop(
      "Zenodo record ", ITHACA_RECORD_ID, " has NetCDF files that fit no raw folder ",
      "(expected *_tp_*_yearly.nc, *_e_*_yearly.nc or *_monthly.nc):\n",
      paste(record_files$key[is.na(output_dir)], collapse = "\n"),
      call. = FALSE
    )
  }

  empty_folders <- setdiff(
    c(PATH_OUTPUT_RAW_PREC, PATH_OUTPUT_RAW_EVAP, PATH_OUTPUT_RAW_OTHER),
    output_dir
  )

  if (length(empty_folders) > 0) {
    stop(
      "Zenodo record ", ITHACA_RECORD_ID, " has no files for:\n",
      paste(empty_folders, collapse = "\n"),
      call. = FALSE
    )
  }

  data.frame(
    file_name = record_files$key,
    download_url = record_files$links$self,
    output_dir = output_dir,
    expected_size = record_files$size,
    expected_md5 = sub("^md5:", "", record_files$checksum),
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

not_netcdf <- record_files$key[!grepl("\\.nc$", record_files$key)]

if (length(not_netcdf) > 0) {
  message("Not downloaded (not a NetCDF file): ", paste(not_netcdf, collapse = ", "))
}

download_manifest <- build_download_manifest(record_files)

archived_files <- character(0)

if (ARCHIVE_REPLACED_FILES) {
  archived_files <- archive_mismatched_files(download_manifest)
}

download_manifest_files(download_manifest)

# Outputs ====================================================================

# Every NetCDF file of the record is in place: precipitation in
# PATH_OUTPUT_RAW_PREC, evaporation in PATH_OUTPUT_RAW_EVAP and PET forcing in
# PATH_OUTPUT_RAW_OTHER. The complete list is held in download_manifest.

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
  sum(download_manifest$output_dir == PATH_OUTPUT_RAW_PREC), " precipitation, ",
  sum(download_manifest$output_dir == PATH_OUTPUT_RAW_EVAP), " evaporation and ",
  sum(download_manifest$output_dir == PATH_OUTPUT_RAW_OTHER), " PET forcing",
  if (length(archived_files) > 0) {
    paste0("; archived ", length(archived_files), " replaced file(s) in ", ARCHIVE_FOLDER)
  },
  "."
)
