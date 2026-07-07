# ============================================================================
# Estimate dataset annual means by IPCC region and biome
#
# This script:
# 1. Merges precipitation and evaporation datasets with region and biome masks
# 2. Adds grid-cell area weights
# 3. Estimates area-weighted annual mean P and E per dataset x region x biome
# 4. Saves the aggregated object for Monte Carlo sampling
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

prec_evap <- readRDS(
  file.path(PATH_OUTPUT_DATA, "prec_evap.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Functions ===================================================================

get_cell_area_column <- function(dt) {
  
  possible_area_cols <- c(
    "cell_area",
    "area",
    "area_km2",
    "area_m2",
    "area_sum"
  )
  
  area_cols <- intersect(possible_area_cols, names(dt))
  
  if (length(area_cols) == 0L) {
    return(NA_character_)
  }
  
  area_cols[1]
}

prepare_grid_classes <- function(twc_grid_classes) {
  
  grid_dt <- as.data.table(copy(twc_grid_classes))
  
  required_cols <- c("lon", "lat", "region", "biome")
  missing_cols <- setdiff(required_cols, names(grid_dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "twc_grid_classes is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  area_col <- get_cell_area_column(grid_dt)
  
  if (is.na(area_col)) {
    
    message(
      "No explicit cell-area column found. ",
      "Using cos(latitude) as relative area weight."
    )
    
    grid_dt[
      ,
      cell_area := cos(lat * pi / 180)
    ]
    
  } else {
    
    message("Using existing area column: ", area_col)
    
    setnames(
      grid_dt,
      old = area_col,
      new = "cell_area",
      skip_absent = TRUE
    )
  }
  
  grid_dt <- grid_dt[
    is.finite(lon) &
      is.finite(lat) &
      !is.na(region) &
      !is.na(biome) &
      is.finite(cell_area) &
      cell_area > 0,
    .(lon, lat, region, biome, cell_area)
  ]
  
  unique(grid_dt, by = c("lon", "lat"))
}

safe_area_weighted_sum <- function(value, weight) {
  
  ok <- is.finite(value) & is.finite(weight) & weight > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(value[ok] * weight[ok])
}

safe_area_sum <- function(value, weight) {
  
  ok <- is.finite(value) & is.finite(weight) & weight > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(weight[ok])
}

# Analysis ====================================================================

required_prec_evap_cols <- c("lon", "lat", "dataset", "year", "prec", "evap")
missing_prec_evap_cols <- setdiff(required_prec_evap_cols, names(prec_evap))

if (length(missing_prec_evap_cols) > 0L) {
  stop(
    "prec_evap is missing required columns: ",
    paste(missing_prec_evap_cols, collapse = ", ")
  )
}

grid_classes <- prepare_grid_classes(twc_grid_classes)

prec_evap_cf <- merge(
  prec_evap,
  grid_classes,
  by = c("lon", "lat"),
  all = FALSE
)

dataset_region_biome_year <- prec_evap_cf[
  ,
  .(
    prec_area_sum = safe_area_weighted_sum(prec, cell_area),
    evap_area_sum = safe_area_weighted_sum(evap, cell_area),
    area_sum_prec = safe_area_sum(prec, cell_area),
    area_sum_evap = safe_area_sum(evap, cell_area),
    area_sum_both = sum(
      cell_area[
        is.finite(prec) &
          is.finite(evap) &
          is.finite(cell_area) &
          cell_area > 0
      ],
      na.rm = TRUE
    ),
    n_cells = uniqueN(paste(lon, lat)),
    n_valid_prec = sum(is.finite(prec)),
    n_valid_evap = sum(is.finite(evap)),
    n_valid_both = sum(is.finite(prec) & is.finite(evap))
  ),
  by = .(dataset, region, biome, year)
]

dataset_region_biome_year[
  ,
  `:=`(
    prec = prec_area_sum / area_sum_prec,
    evap = evap_area_sum / area_sum_evap,
    area_sum = area_sum_both
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
    "evap",
    "prec_area_sum",
    "evap_area_sum",
    "area_sum",
    "area_sum_prec",
    "area_sum_evap",
    "area_sum_both",
    "n_cells",
    "n_valid_prec",
    "n_valid_evap",
    "n_valid_both"
  )
)

setkey(
  dataset_region_biome_year,
  dataset,
  region,
  biome,
  year
)

# Outputs =====================================================================

# Keep only final lean output
to_save <- dataset_region_biome_year[
  ,
  .(
    dataset,
    region,
    biome,
    year,
    prec,
    evap
    )
]

saveRDS(to_save, 
        file.path(
          PATH_OUTPUT_DATA,
          "dataset_region_biome_year.Rds"
        )
)

# Validation ==================================================================

cat("\nOutput structure:\n")
print(str(dataset_region_biome_year))

cat("\nPreview:\n")
print(dataset_region_biome_year)

cat("\nNumber of datasets:\n")
print(dataset_region_biome_year[, uniqueN(dataset)])

cat("\nYear range:\n")
print(dataset_region_biome_year[, range(year, na.rm = TRUE)])

cat("\nMissing values by variable:\n")
print(
  dataset_region_biome_year[
    ,
    .(
      n_missing_prec = sum(!is.finite(prec)),
      n_missing_evap = sum(!is.finite(evap)))
  ]
)

cat("\nSmallest valid area sums:\n")
print(
  dataset_region_biome_year[
    order(area_sum)
  ][
    1:20,
    .(
      dataset,
      region,
      biome,
      year,
      area_sum,
      n_cells,
      n_valid_both,
      prec,
      evap
    )
  ]
)

cat("\nFinished aggregation.\n")

