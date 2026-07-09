# ============================================================================
# Aggregate Monte Carlo dataset selections to annual P and E ensembles
#
# This script:
# 1. Reads Monte Carlo dataset selections for base and scenarios runs
# 2. Joins selections to existing area-weighted dataset-region-biome-year values
# 3. Aggregates selected values to IPCC region and global annual P and E
# 4. Saves compact outputs in PATH_OUTPUT_DATA using _base and _scenarios suffixes
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

dataset_region_biome_year <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_region_biome_year.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Constants & Variables =======================================================

RUN_IDS <- c(
  "base",
  "scenarios"
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

prepare_region_biome_area <- function(twc_grid_classes) {
  
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
    
    if (area_col != "cell_area") {
      setnames(
        grid_dt,
        old = area_col,
        new = "cell_area"
      )
    }
  }
  
  grid_dt[
    ,
    `:=`(
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  grid_dt <- grid_dt[
    is.finite(lon) &
      is.finite(lat) &
      !is.na(region) &
      !is.na(biome) &
      is.finite(cell_area) &
      cell_area > 0,
    .(lon, lat, region, biome, cell_area)
  ]
  
  grid_dt <- unique(
    grid_dt,
    by = c("lon", "lat")
  )
  
  region_biome_area <- grid_dt[
    ,
    .(
      area_weight = sum(cell_area, na.rm = TRUE)
    ),
    by = .(region, biome)
  ]
  
  setkey(
    region_biome_area,
    region,
    biome
  )
  
  region_biome_area
}

weighted_mean_safe <- function(value, weight) {
  
  ok <- is.finite(value) &
    is.finite(weight) &
    weight > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(value[ok] * weight[ok]) / sum(weight[ok])
}

prepare_dataset_region_biome_year <- function(dataset_region_biome_year) {
  
  dt <- as.data.table(copy(dataset_region_biome_year))
  
  required_cols <- c(
    "dataset",
    "region",
    "biome",
    "year",
    "prec",
    "evap"
  )
  
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "dataset_region_biome_year is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  dt[
    ,
    `:=`(
      dataset = as.character(dataset),
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  dt <- dt[
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
  
  setkey(
    dt,
    dataset,
    region,
    biome
  )
  
  dt
}

prepare_mc_selection <- function(mc_selection) {
  
  sel <- as.data.table(copy(mc_selection))
  
  required_cols <- c(
    "sim",
    "scenario",
    "region",
    "biome",
    "dataset"
  )
  
  missing_cols <- setdiff(required_cols, names(sel))
  
  if (length(missing_cols) > 0L) {
    stop(
      "mc_selection is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  sel[
    ,
    `:=`(
      scenario = as.character(scenario),
      region = as.character(region),
      biome = as.character(biome),
      dataset = as.character(dataset)
    )
  ]
  
  sel <- sel[
    ,
    .(
      sim,
      scenario,
      region,
      biome,
      dataset
    )
  ]
  
  setkey(
    sel,
    dataset,
    region,
    biome
  )
  
  sel
}

make_mc_region_biome_year <- function(dataset_region_biome_year, mc_selection) {
  
  dt <- prepare_dataset_region_biome_year(
    dataset_region_biome_year = dataset_region_biome_year
  )
  
  sel <- prepare_mc_selection(
    mc_selection = mc_selection
  )
  
  dataset_keys <- unique(
    dt[, .(dataset, region, biome)]
  )
  
  missing_selected_values <- sel[
    !dataset_keys,
    on = .(dataset, region, biome)
  ]
  
  if (nrow(missing_selected_values) > 0L) {
    print(missing_selected_values)
    stop(
      "Some selected dataset x region x biome combinations are missing from dataset_region_biome_year."
    )
  }
  
  out <- dt[
    sel,
    on = .(dataset, region, biome),
    allow.cartesian = TRUE,
    nomatch = 0L
  ][
    ,
    .(
      sim,
      scenario,
      region,
      biome,
      dataset,
      year,
      prec,
      evap
    )
  ]
  
  setkey(
    out,
    sim,
    scenario,
    region,
    biome,
    year
  )
  
  out
}

make_mc_region_year <- function(mc_region_biome_year, region_biome_area) {
  
  dt <- region_biome_area[
    copy(mc_region_biome_year),
    on = .(region, biome),
    nomatch = 0L
  ]
  
  missing_area <- mc_region_biome_year[
    !region_biome_area,
    on = .(region, biome)
  ]
  
  if (nrow(missing_area) > 0L) {
    print(unique(missing_area[, .(region, biome)]))
    stop("Some region x biome combinations are missing area weights.")
  }
  
  out <- dt[
    ,
    .(
      prec = weighted_mean_safe(prec, area_weight),
      evap = weighted_mean_safe(evap, area_weight)
    ),
    by = .(sim, scenario, region, year)
  ]
  
  setkey(
    out,
    sim,
    scenario,
    region,
    year
  )
  
  out
}

make_mc_global_year <- function(mc_region_biome_year, region_biome_area) {
  
  dt <- region_biome_area[
    copy(mc_region_biome_year),
    on = .(region, biome),
    nomatch = 0L
  ]
  
  missing_area <- mc_region_biome_year[
    !region_biome_area,
    on = .(region, biome)
  ]
  
  if (nrow(missing_area) > 0L) {
    print(unique(missing_area[, .(region, biome)]))
    stop("Some region x biome combinations are missing area weights.")
  }
  
  out <- dt[
    ,
    .(
      prec = weighted_mean_safe(prec, area_weight),
      evap = weighted_mean_safe(evap, area_weight)
    ),
    by = .(sim, scenario, year)
  ]
  
  setkey(
    out,
    sim,
    scenario,
    year
  )
  
  out
}

aggregate_one_run <- function(
    run_id,
    dataset_region_biome_year,
    region_biome_area
) {
  
  cat("\nAggregating MC run:", run_id, "\n")
  
  selection_file <- file.path(
    PATH_OUTPUT_DATA,
    paste0("mc_selection_", run_id, ".Rds")
  )
  
  if (!file.exists(selection_file)) {
    stop("Missing MC selection file: ", selection_file)
  }
  
  mc_selection <- readRDS(selection_file)
  
  mc_region_biome_year <- make_mc_region_biome_year(
    dataset_region_biome_year = dataset_region_biome_year,
    mc_selection = mc_selection
  )
  
  mc_region_year <- make_mc_region_year(
    mc_region_biome_year = mc_region_biome_year,
    region_biome_area = region_biome_area
  )
  
  mc_global_year <- make_mc_global_year(
    mc_region_biome_year = mc_region_biome_year,
    region_biome_area = region_biome_area
  )
  
  saveRDS(
    mc_region_biome_year,
    file.path(
      PATH_OUTPUT_DATA,
      paste0("mc_region_biome_year_", run_id, ".Rds")
    )
  )
  
  saveRDS(
    mc_region_year,
    file.path(
      PATH_OUTPUT_DATA,
      paste0("mc_region_year_", run_id, ".Rds")
    )
  )
  
  saveRDS(
    mc_global_year,
    file.path(
      PATH_OUTPUT_DATA,
      paste0("mc_global_year_", run_id, ".Rds")
    )
  )
  
  cat("\nSaved aggregated outputs for run:", run_id, "\n")
  
  cat("\nRegion x biome annual output structure:\n")
  str(mc_region_biome_year)
  
  cat("\nIPCC region annual output structure:\n")
  str(mc_region_year)
  
  cat("\nGlobal annual output structure:\n")
  str(mc_global_year)
  
  cat("\nGlobal annual output preview:\n")
  print(
    mc_global_year[
      order(sim, scenario, year)
    ][
      1:30
    ]
  )
  
  cat("\nMissing values in region x biome annual output:\n")
  print(
    mc_region_biome_year[
      ,
      .(
        n_missing_prec = sum(!is.finite(prec)),
        n_missing_evap = sum(!is.finite(evap))
      )
    ]
  )
  
  cat("\nMissing values in IPCC region annual output:\n")
  print(
    mc_region_year[
      ,
      .(
        n_missing_prec = sum(!is.finite(prec)),
        n_missing_evap = sum(!is.finite(evap))
      )
    ]
  )
  
  cat("\nMissing values in global annual output:\n")
  print(
    mc_global_year[
      ,
      .(
        n_missing_prec = sum(!is.finite(prec)),
        n_missing_evap = sum(!is.finite(evap))
      )
    ]
  )
  
  data.table(
    run_id = run_id,
    n_sims = mc_selection[, uniqueN(sim)],
    n_scenarios = mc_selection[, uniqueN(scenario)],
    n_region_biome_rows = nrow(mc_region_biome_year),
    n_region_rows = nrow(mc_region_year),
    n_global_rows = nrow(mc_global_year),
    n_missing_region_biome_prec = mc_region_biome_year[
      ,
      sum(!is.finite(prec))
    ],
    n_missing_region_biome_evap = mc_region_biome_year[
      ,
      sum(!is.finite(evap))
    ],
    n_missing_region_prec = mc_region_year[
      ,
      sum(!is.finite(prec))
    ],
    n_missing_region_evap = mc_region_year[
      ,
      sum(!is.finite(evap))
    ],
    n_missing_global_prec = mc_global_year[
      ,
      sum(!is.finite(prec))
    ],
    n_missing_global_evap = mc_global_year[
      ,
      sum(!is.finite(evap))
    ]
  )
}

# Analysis ====================================================================

region_biome_area <- prepare_region_biome_area(
  twc_grid_classes = twc_grid_classes
)

aggregation_summary <- rbindlist(
  lapply(RUN_IDS, function(run_id) {
    
    aggregate_one_run(
      run_id = run_id,
      dataset_region_biome_year = dataset_region_biome_year,
      region_biome_area = region_biome_area
    )
  })
)

# Outputs =====================================================================

saveRDS(
  aggregation_summary,
  file.path(
    PATH_OUTPUT_DATA,
    "mc_aggregation_summary.Rds"
  )
)

# Validation ==================================================================

cat("\nFinished all Monte Carlo aggregations.\n")

cat("\nAggregation summary:\n")
print(aggregation_summary)