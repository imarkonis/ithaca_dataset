# ============================================================================
# Combine selected precipitation and evaporation datasets into a common wide
# table for TWC analysis.
#
# This script reads the prepared long-format precipitation and evaporation table
# from the raw TWC workflow, keeps only the selected analysis datasets, applies
# the intended GLEAM-MSWEP pairing, reshapes to wide format, and saves the
# combined precipitation-evaporation table.
# ============================================================================

# Libraries ==================================================================

source("source/twc_change.R")

# Inputs =====================================================================

prec_evap_raw <- read_fst(
  file.path(PATH_OUTPUT_RAW, "other/prec_evap_raw.fst"),
  as.data.table = TRUE
)

# Analysis ===================================================================

# Constants & variables ======================================================

prec_evap_analysis <- copy(
  prec_evap_raw[
    (variable == "prec" & dataset %in% PREC_NAMES_SHORT) |
      (variable == "evap" & dataset %in% EVAP_NAMES_SHORT)
  ]
)

## Pair MSWEP precipitation with GLEAM evaporation 
prec_evap_analysis[
  variable == "prec" & dataset == "MSWEP",
  dataset := "GLEAM"
]

## Reshape to wide format 
prec_evap <- dcast(
  prec_evap_analysis,
  lon + lat + year + dataset ~ variable,
  value.var = "value"
)

setcolorder(
  prec_evap,
  c("lon", "lat", "year", "dataset", "prec", "evap")
)

prec_evap <- prec_evap[complete.cases(prec_evap)]

setorder(
  prec_evap,
  dataset, lon, lat, year
)

# Outputs ====================================================================

saveRDS(
  prec_evap,
  file.path(PATH_OUTPUT_DATA, "prec_evap.Rds")
)
