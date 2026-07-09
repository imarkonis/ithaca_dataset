# ============================================================================
# Build scenario mean precipitation and evaporation from region-biome weights
#
# Outputs:
#   1. scenario_prec_evap_grid
#   2. scenario_prec_evap_region_biome
#   3. scenario_prec_evap_region
# ============================================================================

# Inputs ======================================================================

source("source/twc_change.R")

library(parallel)

prec_evap <- readRDS(
  file.path(PATH_OUTPUT_DATA, "prec_evap.Rds")
)

weights_rb <- readRDS(
  file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)


# Constants & Variables =======================================================

EXCLUDED_REGIONS <- character(0)

SCENARIOS <- sort(unique(weights_rb$scenario))

N_WORKERS <- min(length(SCENARIOS), detectCores())


# Functions ===================================================================

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(x[ok] * w[ok]) / sum(w[ok])
}

run_one_scenario <- function(scen) {
  
  message("Running scenario: ", scen)
  
  w_scen <- weights_rb[
    scenario == scen,
    .(region, biome, dataset, weight = w_region_biome)
  ]
  
  pe_scen <- merge(
    prec_evap_region,
    w_scen,
    by = c("region", "biome", "dataset"),
    allow.cartesian = TRUE
  )
  
  # 1. Grid-cell means ---------------------------------------------------------
  grid_dt <- pe_scen[
    ,
    .(
      prec = weighted_mean_safe(prec, weight),
      evap = weighted_mean_safe(evap, weight)
    ),
    by = .(lon, lat, year, region, biome, cell_area)
  ]
  
  grid_dt[, scenario := scen]
  
  setcolorder(
    grid_dt,
    c("scenario", "lon", "lat", "year", "region", "biome",
      "prec", "evap", "cell_area")
  )
  
  # 2. Region-biome means ------------------------------------------------------
  region_biome_dt <- grid_dt[
    ,
    .(
      prec = weighted_mean_safe(prec, cell_area),
      evap = weighted_mean_safe(evap, cell_area),
      area_sum = sum(cell_area[is.finite(prec) | is.finite(evap)], na.rm = TRUE),
      n_cells = uniqueN(paste(lon, lat))
    ),
    by = .(scenario, region, biome, year)
  ]
  
  # 3. Region means, aggregated from region-biome units ------------------------
  region_dt <- region_biome_dt[
    ,
    .(
      prec = weighted_mean_safe(prec, area_sum),
      evap = weighted_mean_safe(evap, area_sum),
      area_sum = sum(area_sum, na.rm = TRUE),
      n_region_biomes = uniqueN(biome),
      n_cells = sum(n_cells, na.rm = TRUE)
    ),
    by = .(scenario, region, year)
  ]
  
  list(
    grid = grid_dt,
    region_biome = region_biome_dt,
    region = region_dt
  )
}


# Analysis ====================================================================

setDT(prec_evap)
setDT(weights_rb)
setDT(twc_grid_classes)

# Add region, biome, and area to annual P/E data.
prec_evap_region <- merge(
  prec_evap,
  twc_grid_classes[, .(lon, lat, region, biome)],
  by = c("lon", "lat"),
  all.x = TRUE
)

prec_evap_region <- prec_evap_region[
  !is.na(region) &
    !is.na(biome) &
    !region %in% EXCLUDED_REGIONS
]

prec_evap_region[
  ,
  cell_area := cos(lat * pi / 180)
]

# Keep only valid region-biome weights.
weights_rb <- weights_rb[
  is.finite(w_region_biome) &
    w_region_biome >= 0 &
    !is.na(region) &
    !is.na(biome)
]

# Parallel execution, one scenario per forked worker.
N_WORKERS <- min(length(SCENARIOS), parallel::detectCores())

if (.Platform$OS.type == "windows") {
  N_WORKERS <- 1L
}

scenario_outputs <- parallel::mclapply(
  SCENARIOS,
  run_one_scenario,
  mc.cores = N_WORKERS,
  mc.preschedule = TRUE
)

# Merge scenario outputs into three data.tables.
scenario_prec_evap_grid <- rbindlist(
  lapply(scenario_outputs, `[[`, "grid"),
  use.names = TRUE
)

scenario_prec_evap_region_biome <- rbindlist(
  lapply(scenario_outputs, `[[`, "region_biome"),
  use.names = TRUE
)

scenario_prec_evap_region <- rbindlist(
  lapply(scenario_outputs, `[[`, "region"),
  use.names = TRUE
)

setkey(scenario_prec_evap_grid, scenario, lon, lat, year)
setkey(scenario_prec_evap_region_biome, scenario, region, biome, year)
setkey(scenario_prec_evap_region, scenario, region, year)

# Outputs =====================================================================

saveRDS(
  scenario_prec_evap_grid,
  file.path(PATH_OUTPUT_DATA, "scenario_prec_evap_grid.Rds")
)

saveRDS(
  scenario_prec_evap_region_biome,
  file.path(PATH_OUTPUT_DATA, "scenario_prec_evap_region_biome.Rds")
)

saveRDS(
  scenario_prec_evap_region,
  file.path(PATH_OUTPUT_DATA, "scenario_prec_evap_region.Rds")
)


# Validation ==================================================================

# 1. Region-biome weights sum to 1.
chk_weights <- weights_rb[
  ,
  .(s = sum(w_region_biome, na.rm = TRUE)),
  by = .(scenario, region, biome)
]

stopifnot(all(abs(chk_weights$s - 1) < 1e-6))

# 2. All scenarios produced all three scales.
stopifnot(identical(sort(unique(scenario_prec_evap_grid$scenario)), SCENARIOS))
stopifnot(identical(sort(unique(scenario_prec_evap_region_biome$scenario)), SCENARIOS))
stopifnot(identical(sort(unique(scenario_prec_evap_region$scenario)), SCENARIOS))

# 3. Coverage summary.
print(
  scenario_prec_evap_grid[
    ,
    .(
      n_cells = uniqueN(paste(lon, lat)),
      n_valid_prec = sum(is.finite(prec)),
      n_valid_evap = sum(is.finite(evap))
    ),
    by = .(scenario, year)
  ][
    ,
    .(
      min_cells = min(n_cells),
      max_cells = max(n_cells),
      min_valid_prec = min(n_valid_prec),
      min_valid_evap = min(n_valid_evap)
    ),
    by = scenario
  ][order(scenario)]
)

# 4. Spot check.
print(
  scenario_prec_evap_region[
    region == "MED"
  ][order(scenario, year)]
)
