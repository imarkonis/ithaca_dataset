# ============================================================================
# Estimate dataset annual means by IPCC region and biome
#
# This script:
#   1. merges precipitation and evaporation datasets with spatial classes
#      and grid-cell weights
#   2. estimates area-weighted annual mean P and E per
#      dataset x region x biome
#   3. saves the aggregated object for Monte Carlo sampling
#
# Grid-cell area weights, IPCC regions, biome classes, and the common analysis
# grid are defined upstream in twc_grid_classes.Rds.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(fst)

# Inputs =====================================================================

prec_evap <- read_fst(
  file.path(PATH_OUTPUT_OUTPUT, "prec_evap.fst"),
  as.data.table = TRUE
)

grid_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "twc_grid_classes.Rds")
)

# Constants & Variables ======================================================

OUTPUT_FILE <- file.path(
  PATH_OUTPUT_OUTPUT,
  "dataset_region_biome_year.Rds"
)

# Analysis ===================================================================

grid_classes <- grid_classes[
  ,
  .(
    lon,
    lat,
    region,
    biome,
    area_weight
  )
]

prec_evap_cf <- merge(
  prec_evap,
  grid_classes,
  by = c("lon", "lat"),
  all = FALSE
)

dataset_region_biome_year <- prec_evap_cf[
  ,
  .(
    prec = weighted.mean(
      prec,
      area_weight
    ),
    evap = weighted.mean(
      evap,
      area_weight
    )
  ),
  by = .(
    dataset,
    region,
    biome,
    year
  )
]

setcolorder(
  dataset_region_biome_year,
  c(
    "dataset",
    "region",
    "biome",
    "year",
    "prec",
    "evap"
  )
)

setkey(
  dataset_region_biome_year,
  dataset,
  region,
  biome,
  year
)

# Outputs ====================================================================

saveRDS(
  dataset_region_biome_year,
  OUTPUT_FILE
)

# Validation =================================================================

cat("\nOutput structure:\n")
print(str(dataset_region_biome_year))

cat("\nPreview:\n")
print(dataset_region_biome_year)

cat("\nNumber of datasets:\n")
print(
  dataset_region_biome_year[
    ,
    uniqueN(dataset)
  ]
)

cat("\nNumber of regions:\n")
print(
  dataset_region_biome_year[
    ,
    uniqueN(region)
  ]
)

cat("\nNumber of region-biome combinations:\n")
print(
  dataset_region_biome_year[
    ,
    uniqueN(paste(region, biome))
  ]
)

cat("\nYear range:\n")
print(
  dataset_region_biome_year[
    ,
    range(year, na.rm = TRUE)
  ]
)

cat("\nMissing values by variable:\n")
print(
  dataset_region_biome_year[
    ,
    .(
      n_missing_prec = sum(!is.finite(prec)),
      n_missing_evap = sum(!is.finite(evap))
    )
  ]
)

cat("\nSaved RDS:\n")
cat(OUTPUT_FILE, "\n")

cat("\nFinished region-biome annual aggregation.\n")