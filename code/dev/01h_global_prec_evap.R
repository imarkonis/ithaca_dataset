# ============================================================================
# Estimate dataset global annual means
#
# This script:
# 1. Merges precipitation and evaporation datasets with the analysis grid
# 2. Adds grid cell area weights
# 3. Estimates area weighted annual global mean P and E per dataset
# 4. Adds annual global availability and flux
# 5. Saves the aggregated object for observational dataset plotting
# ============================================================================


# Inputs ======================================================================

source("source/twc_change.R")


prec_evap <- readRDS(
  file.path(PATH_OUTPUT_DATA, "prec_evap.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)


# Constants & Variables =======================================================

OUTPUT_FILE <- file.path(
  PATH_OUTPUT_DATA,
  "dataset_global_year.Rds"
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
  
  grid_dt <- copy(twc_grid_classes)
  
  required_cols <- c("lon", "lat")
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
      "No explicit cell area column found. ",
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
      is.finite(cell_area) &
      cell_area > 0,
    .(
      lon,
      lat,
      cell_area
    )
  ]
  
  unique(
    grid_dt,
    by = c("lon", "lat")
  )
}

safe_area_weighted_sum <- function(value, weight) {
  
  ok <- is.finite(value) &
    is.finite(weight) &
    weight > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(value[ok] * weight[ok])
}

safe_area_sum <- function(value, weight) {
  
  ok <- is.finite(value) &
    is.finite(weight) &
    weight > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(weight[ok])
}


# Analysis ====================================================================

required_prec_evap_cols <- c(
  "lon",
  "lat",
  "dataset",
  "year",
  "prec",
  "evap"
)

missing_prec_evap_cols <- setdiff(
  required_prec_evap_cols,
  names(prec_evap)
)

if (length(missing_prec_evap_cols) > 0L) {
  stop(
    "prec_evap is missing required columns: ",
    paste(missing_prec_evap_cols, collapse = ", ")
  )
}

grid_classes <- prepare_grid_classes(
  twc_grid_classes = twc_grid_classes
)

prec_evap_global <- merge(
  prec_evap,
  grid_classes,
  by = c("lon", "lat"),
  all = FALSE
)

dataset_global_year_full <- prec_evap_global[
  ,
  .(
    prec_area_sum = safe_area_weighted_sum(
      value = prec,
      weight = cell_area
    ),
    evap_area_sum = safe_area_weighted_sum(
      value = evap,
      weight = cell_area
    ),
    area_sum_prec = safe_area_sum(
      value = prec,
      weight = cell_area
    ),
    area_sum_evap = safe_area_sum(
      value = evap,
      weight = cell_area
    ),
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
  by = .(dataset, year)
]

dataset_global_year_full[
  ,
  `:=`(
    prec = prec_area_sum / area_sum_prec,
    evap = evap_area_sum / area_sum_evap,
    area_sum = area_sum_both
  )
]

dataset_global_year_full[
  ,
  `:=`(
    avail = prec - evap,
    flux = (prec + evap) / 2
  )
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
  dataset_global_year_full,
  dataset,
  year
)


# Outputs =====================================================================

dataset_global_year <- dataset_global_year_full[
  ,
  .(
    dataset,
    year,
    prec,
    evap,
    avail,
    flux
  )
]

saveRDS(
  dataset_global_year,
  OUTPUT_FILE
)


# Validation ==================================================================

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
      n_missing_evap = sum(!is.finite(evap)),
      n_missing_avail = sum(!is.finite(avail)),
      n_missing_flux = sum(!is.finite(flux))
    )
  ]
)

cat("\nSmallest valid area sums:\n")
print(
  dataset_global_year_full[
    order(area_sum)
  ][
    1:20,
    .(
      dataset,
      year,
      area_sum,
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