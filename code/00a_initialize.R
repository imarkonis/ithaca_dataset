# ============================================================================
# Initialize project paths for the ithaca dataset workflow
#
# This script defines the input and output directories for the ithaca dataset
# project, creates the required folder structure, and saves all path objects
# to paths.Rdata for downstream scripts.
# ============================================================================

# Libraries ==================================================================

source("code/_machine_paths.R") # defines PATH_SAVE for this machine (see MACHINE_PATHS / ACTIVE_MACHINE there)

# Constants & Variables ======================================================

if (!dir.exists(dirname(PATH_SAVE))) {
  stop("Parent folder '", dirname(PATH_SAVE), "' does not exist. ",
       "Edit ACTIVE_MACHINE (or add a MACHINE_PATHS row) in code/_machine_paths.R.", call. = FALSE)
}

PATH_OUTPUT <- file.path(PATH_SAVE, "ithaca_dataset")
PATH_OUTPUT_DATA <- file.path(PATH_OUTPUT, "data")
PATH_OUTPUT_RAW <- file.path(PATH_OUTPUT_DATA, "raw")
PATH_OUTPUT_RAW_PREC <-  file.path(PATH_OUTPUT_RAW, "prec") 
PATH_OUTPUT_RAW_EVAP <-  file.path(PATH_OUTPUT_RAW, "evap") 
PATH_OUTPUT_RAW_OTHER <- file.path(PATH_OUTPUT_RAW, "other")
PATH_OUTPUT_INPUT <- file.path(PATH_OUTPUT_DATA, "input")
PATH_OUTPUT_OUTPUT <- file.path(PATH_OUTPUT_DATA, "output")
PATH_OUTPUT_DATASET <- file.path(PATH_OUTPUT_DATA, "dataset")
PATH_OUTPUT_FIGURES <- file.path(PATH_OUTPUT, "figures")
PATH_OUTPUT_TABLES <- file.path(PATH_OUTPUT, "tables")

# Analysis ===================================================================

paths_to_create <- c(
  PATH_OUTPUT,
  PATH_OUTPUT_DATA,
  PATH_OUTPUT_RAW,
  PATH_OUTPUT_RAW_PREC, 
  PATH_OUTPUT_RAW_EVAP,
  PATH_OUTPUT_RAW_OTHER,
  PATH_OUTPUT_INPUT,
  PATH_OUTPUT_OUTPUT,
  PATH_OUTPUT_DATASET,
  PATH_OUTPUT_FIGURES,
  PATH_OUTPUT_TABLES
)

invisible(lapply(
  X = paths_to_create,
  FUN = dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

# Validation ==================================================================

if (!all(dir.exists(paths_to_create))) {
  stop("Could not create output folders. Edit PATH_SAVE at the top of this script.",
       call. = FALSE)
}

# Outputs ====================================================================

save(
  PATH_OUTPUT,
  PATH_OUTPUT_DATA,
  PATH_OUTPUT_RAW,
  PATH_OUTPUT_RAW_PREC,
  PATH_OUTPUT_RAW_EVAP,
  PATH_OUTPUT_RAW_OTHER,
  PATH_OUTPUT_INPUT,
  PATH_OUTPUT_OUTPUT,
  PATH_OUTPUT_DATASET,
  PATH_OUTPUT_FIGURES,
  PATH_OUTPUT_TABLES,
  file = file.path(PATH_OUTPUT, "paths.Rdata")
)