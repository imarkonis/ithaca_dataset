# ============================================================================
# Initialize project paths for the ithaca dataset workflow
#
# This script defines the input and output directories for the ithaca dataset
# project, creates the required folder structure, and saves all path objects
# to paths.Rdata for downstream scripts.
# ============================================================================

# Constants & Variables ======================================================

PATH_SAVE <-  "C:/Users/imark/ownCloud/Yannis/15_Data/R/" # Change this to the local path for saving the script output 

if (!dir.exists(dirname(PATH_SAVE))) {
  stop("Parent folder '", dirname(PATH_SAVE), "' does not exist. ",
       "Edit PATH_SAVE at the top of this script.", call. = FALSE)
}

PATH_OUTPUT <- file.path(PATH_SAVE, "ithaca_dataset")
PATH_OUTPUT_DATA <- file.path(PATH_OUTPUT, "data")
PATH_OUTPUT_RAW <- file.path(PATH_OUTPUT_DATA, "raw")
PATH_OUTPUT_RAW_PREC <-  file.path(PATH_OUTPUT_RAW, "prec") 
PATH_OUTPUT_RAW_EVAP <-  file.path(PATH_OUTPUT_RAW, "evap") 
PATH_OUTPUT_RAW_OTHER <- file.path(PATH_OUTPUT_RAW, "other")
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
  PATH_OUTPUT_FIGURES,
  PATH_OUTPUT_TABLES,
  file = file.path(PATH_OUTPUT, "paths.Rdata")
)
