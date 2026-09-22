# ============================================================================
# Machine-specific base path for the ithaca_dataset workflow.
#
# This is the ONLY place PATH_SAVE is defined. Both 00a_initialize.R (which
# creates paths.Rdata) and _source.R (which every other script loads it
# from) source this file, so they can no longer disagree about where the
# pipeline's data folder lives -- add a row per computer below, then change
# ACTIVE_MACHINE to switch.
# ============================================================================

MACHINE_PATHS <- data.frame(
  name = c("markonis_noa", 
           "markonis_home"),
  path = c("D:/research/", 
           "C:/Users/markonis/Documents/Data/"),
  stringsAsFactors = FALSE
)

ACTIVE_MACHINE <- "markonis_home"  # <-- change this when switching computers

PATH_SAVE <- MACHINE_PATHS$path[MACHINE_PATHS$name == ACTIVE_MACHINE]

if (length(PATH_SAVE) != 1) {
  stop(
    "ACTIVE_MACHINE '", ACTIVE_MACHINE, "' is not listed exactly once in ",
    "MACHINE_PATHS. Add it as a (name, path) row, or fix ACTIVE_MACHINE, ",
    "in code/_machine_paths.R.",
    call. = FALSE
  )
}
