# ============================================================================
# Initialize project paths for the TWC change workflow
#
# This script defines the input and output directories for the TWC change
# project, creates the required folder structure, and saves all path objects
# to paths.Rdata for downstream scripts.
# ============================================================================

# Constants & Variables ======================================================

PATH_OUTPUT <- file.path(PATH_SAVE, "twc_change")
PATH_OUTPUT_DATA <- file.path(PATH_OUTPUT, "data")
PATH_OUTPUT_RAW <- file.path(PATH_OUTPUT_DATA, "raw")
PATH_OUTPUT_FIGURES <- file.path(PATH_OUTPUT, "figures")
PATH_OUTPUT_TABLES <- file.path(PATH_OUTPUT, "tables")
PATH_OUTPUT_RAW_OTHER <- file.path(PATH_OUTPUT_RAW, "other")

# Analysis ===================================================================

paths_to_create <- c(
  PATH_OUTPUT,
  PATH_OUTPUT_DATA,
  PATH_OUTPUT_RAW,
  PATH_OUTPUT_FIGURES,
  PATH_OUTPUT_TABLES,
  PATH_OUTPUT_RAW_OTHER
)

invisible(lapply(
  X = paths_to_create,
  FUN = dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

# Outputs ====================================================================

save(
  PATH_OUTPUT,
  PATH_OUTPUT_DATA,
  PATH_OUTPUT_RAW,
  PATH_OUTPUT_FIGURES,
  PATH_OUTPUT_TABLES,
  PATH_OUTPUT_RAW_OTHER,
  file = file.path(PATH_OUTPUT, "paths.Rdata")
)
