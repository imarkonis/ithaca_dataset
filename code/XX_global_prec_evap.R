# ============================================================================
# Estimate dataset global annual means
#
# This script:
#   1. merges precipitation and evaporation datasets with the analysis grid
#   2. estimates area-weighted annual global mean P and E per dataset
#   3. adds annual global availability and flux
#   4. saves the aggregated object for observational dataset plotting
#
# Grid-cell area weights are calculated upstream in
# twc_grid_classes.Rds and sum to one over the complete analysis grid.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

# Inputs =====================================================================

prec_evap <- read_fst(
  file.path(PATH_OUTPUT_OUTPUT, "prec_evap.fst"),
  as.data.table = TRUE
)

grid_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
)

# Constants & Variables =======================================================

OUTPUT_FILE <- file.path(
  PATH_OUTPUT_OUTPUT,
  "dataset_global_year.Rds"
)

# Analysis ===================================================================

grid_weights <- grid_classes[
  ,
  .(
    lon,
    lat,
    area_weight
  )
]

prec_evap_global <- merge(
  prec_evap,
  grid_weights,
  by = c("lon", "lat"),
  all = FALSE
)

dataset_global_year_full <- prec_evap_global[
  ,
  .(
    prec = weighted.mean(
      prec,
      area_weight,
      na.rm = TRUE
    ),
    evap = weighted.mean(
      evap,
      area_weight,
      na.rm = TRUE
    ),
    area_weight_prec = sum(
      area_weight[is.finite(prec)],
      na.rm = TRUE
    ),
    area_weight_evap = sum(
      area_weight[is.finite(evap)],
      na.rm = TRUE
    ),
    area_weight_both = sum(
      area_weight[
        is.finite(prec) &
          is.finite(evap)
      ],
      na.rm = TRUE
    ),
    n_cells = .N,
    n_valid_prec = sum(is.finite(prec)),
    n_valid_evap = sum(is.finite(evap)),
    n_valid_both = sum(
      is.finite(prec) &
        is.finite(evap)
    )
  ),
  by = .(dataset, year)
]

setcolorder(
  dataset_global_year_full,
  c(
    "dataset",
    "year",
    "prec",
    "evap",
    "avail",
    "flux",
    "area_weight_prec",
    "area_weight_evap",
    "area_weight_both",
    "n_cells",
    "n_valid_prec",
    "n_valid_evap",
    "n_valid_both"
  )
)

setkey(
  dataset_global_year_full,
  dataset,
  year
)

# Outputs ====================================================================

dataset_global_year <- dataset_global_year_full[
  ,
  .(
    dataset,
    year,
    prec,
    evap
  )
]

saveRDS(
  dataset_global_year,
  OUTPUT_FILE
)

# Validation =================================================================

cat("\nOutput structure:\n")
print(str(dataset_global_year))

cat("\nPreview:\n")
print(dataset_global_year)

cat("\nNumber of datasets:\n")
print(dataset_global_year[, uniqueN(dataset)])

cat("\nYear range:\n")
print(dataset_global_year[, range(year, na.rm = TRUE)])

cat("\nMissing values by variable:\n")
print(
  dataset_global_year[
    ,
    .(
      n_missing_prec = sum(!is.finite(prec)),
      n_missing_evap = sum(!is.finite(evap))
    )
  ]
)

cat("\nSmallest spatial coverage:\n")
print(
  dataset_global_year_full[
    order(area_weight_both)
  ][
    1:min(20L, .N),
    .(
      dataset,
      year,
      area_weight_both,
      n_cells,
      n_valid_both,
      prec,
      evap,
      avail,
      flux
    )
  ]
)

cat("\nSaved RDS:\n")
cat(OUTPUT_FILE, "\n")

cat("\nFinished global dataset annual aggregation.\n")