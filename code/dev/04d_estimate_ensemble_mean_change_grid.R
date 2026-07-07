# ============================================================================
# Extrapolate region-biome Monte Carlo metric summaries to TWC grid cells
#
# This script:
# 1. Reads region-biome Monte Carlo metrics from the base ensemble
# 2. Reads twc_grid_classes.Rds as the target grid
# 3. Summarizes MC metrics per region x biome x variable
# 4. Inner-joins summaries to grid cells
# 5. Saves one grid-ready metric object
# ============================================================================

# Inputs ======================================================================

source("source/twc_change.R")

mc_region_biome_metrics_base <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_biome_metrics_base.Rds")
)

twc_grid <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Constants & Variables =======================================================

GRID_KEEP_COLS <- c(
  "lon",
  "lat",
  "region",
  "region_full",
  "continent",
  "circulation",
  "lat_zone",
  "biome",
  "climate_main",
  "hydrobelt",
  "hemisphere",
  "limited_1982_2001",
  "limited_2002_2021",
  "limited_change",
  "area",
  "area_weight"
)

# Functions ===================================================================

q_safe <- function(x, p) {
  
  x <- x[is.finite(x)]
  
  if (length(x) == 0L) {
    return(NA_real_)
  }
  
  unname(
    quantile(
      x,
      probs = p,
      na.rm = TRUE,
      type = 7
    )
  )
}

frac_safe <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0L) {
    return(NA_real_)
  }
  
  mean(x)
}

prepare_grid <- function(twc_grid, grid_keep_cols) {
  
  grid_dt <- as.data.table(copy(twc_grid))
  
  required_cols <- c(
    "lon",
    "lat",
    "region",
    "biome",
    "area",
    "area_weight"
  )
  
  missing_cols <- setdiff(required_cols, names(grid_dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "twc_grid is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  keep_cols <- intersect(
    grid_keep_cols,
    names(grid_dt)
  )
  
  grid_dt <- grid_dt[
    ,
    keep_cols,
    with = FALSE
  ]
  
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
      is.finite(area) &
      area > 0 &
      is.finite(area_weight) &
      area_weight > 0
  ]
  
  grid_dt <- unique(
    grid_dt,
    by = c("lon", "lat")
  )
  
  setkey(
    grid_dt,
    region,
    biome
  )
  
  grid_dt[]
}

prepare_metrics <- function(mc_region_biome_metrics_base) {
  
  metrics_dt <- as.data.table(copy(mc_region_biome_metrics_base))
  
  required_cols <- c(
    "sim",
    "scenario",
    "region",
    "biome",
    "variable",
    "mean_1982_2001",
    "mean_2002_2021",
    "diff_2002_2021_minus_1982_2001",
    "diff_percent",
    "diff_p_value",
    "diff_stat_sig",
    "slope_full",
    "slope_full_p_value",
    "slope_full_stat_sig"
  )
  
  missing_cols <- setdiff(required_cols, names(metrics_dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "mc_region_biome_metrics_base is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  metrics_dt[
    ,
    `:=`(
      scenario = as.character(scenario),
      region = as.character(region),
      biome = as.character(biome),
      variable = as.character(variable)
    )
  ]
  
  metrics_dt[]
}

summarise_metric_ensemble <- function(metrics_dt) {
  
  metric_summary <- metrics_dt[
    ,
    .(
      n_sims = uniqueN(sim),
      
      mean_1982_2001_q50 = q_safe(mean_1982_2001, 0.50),
      mean_2002_2021_q50 = q_safe(mean_2002_2021, 0.50),
      
      diff_q05 = q_safe(diff_2002_2021_minus_1982_2001, 0.05),
      diff_q25 = q_safe(diff_2002_2021_minus_1982_2001, 0.25),
      diff_q50 = q_safe(diff_2002_2021_minus_1982_2001, 0.50),
      diff_q75 = q_safe(diff_2002_2021_minus_1982_2001, 0.75),
      diff_q95 = q_safe(diff_2002_2021_minus_1982_2001, 0.95),
      diff_mean = mean(diff_2002_2021_minus_1982_2001, na.rm = TRUE),
      diff_sd = sd(diff_2002_2021_minus_1982_2001, na.rm = TRUE),
      diff_percent_q50 = q_safe(diff_percent, 0.50),
      diff_positive_frac = mean(
        diff_2002_2021_minus_1982_2001 > 0,
        na.rm = TRUE
      ),
      diff_negative_frac = mean(
        diff_2002_2021_minus_1982_2001 < 0,
        na.rm = TRUE
      ),
      diff_stat_sig_frac = frac_safe(diff_stat_sig),
      
      slope_q05 = q_safe(slope_full, 0.05),
      slope_q25 = q_safe(slope_full, 0.25),
      slope_q50 = q_safe(slope_full, 0.50),
      slope_q75 = q_safe(slope_full, 0.75),
      slope_q95 = q_safe(slope_full, 0.95),
      slope_mean = mean(slope_full, na.rm = TRUE),
      slope_sd = sd(slope_full, na.rm = TRUE),
      slope_positive_frac = mean(
        slope_full > 0,
        na.rm = TRUE
      ),
      slope_negative_frac = mean(
        slope_full < 0,
        na.rm = TRUE
      ),
      slope_stat_sig_frac = frac_safe(slope_full_stat_sig)
    ),
    by = .(scenario, region, biome, variable)
  ]
  
  if ("variable_label" %in% names(metrics_dt)) {
    
    labels_dt <- unique(
      metrics_dt[
        ,
        .(variable, variable_label)
      ]
    )
    
    metric_summary <- labels_dt[
      metric_summary,
      on = "variable"
    ]
  }
  
  setkey(
    metric_summary,
    region,
    biome
  )
  
  metric_summary[]
}

join_metrics_to_grid <- function(grid_dt, metric_summary) {
  
  grid_metric_summary <- merge(
    grid_dt,
    metric_summary,
    by = c("region", "biome"),
    all = FALSE,
    allow.cartesian = TRUE
  )
  
  setcolorder(
    grid_metric_summary,
    c(
      "lon",
      "lat",
      "region",
      "region_full",
      "continent",
      "circulation",
      "lat_zone",
      "biome",
      "climate_main",
      "hydrobelt",
      "hemisphere",
      "limited_1982_2001",
      "limited_2002_2021",
      "limited_change",
      "area",
      "area_weight",
      "scenario",
      "variable",
      "variable_label",
      setdiff(
        names(grid_metric_summary),
        c(
          "lon",
          "lat",
          "region",
          "region_full",
          "continent",
          "circulation",
          "lat_zone",
          "biome",
          "climate_main",
          "hydrobelt",
          "hemisphere",
          "limited_1982_2001",
          "limited_2002_2021",
          "limited_change",
          "area",
          "area_weight",
          "scenario",
          "variable",
          "variable_label"
        )
      )
    )
  )
  
  setkey(
    grid_metric_summary,
    lon,
    lat,
    scenario,
    variable
  )
  
  grid_metric_summary[]
}

# Analysis ====================================================================

grid_dt <- prepare_grid(
  twc_grid = twc_grid,
  grid_keep_cols = GRID_KEEP_COLS
)

metrics_dt <- prepare_metrics(
  mc_region_biome_metrics_base = mc_region_biome_metrics_base
)

metric_summary <- summarise_metric_ensemble(
  metrics_dt = metrics_dt
)

grid_metric_summary <- join_metrics_to_grid(
  grid_dt = grid_dt,
  metric_summary = metric_summary
)

# Outputs =====================================================================

saveRDS(
  grid_metric_summary,
  file.path(
    PATH_OUTPUT_DATA,
    "mc_grid_metric_summary_base.Rds"
  )
)

# Validation ==================================================================

cat("\nSaved grid-ready metric summary:\n")
cat(
  file.path(
    PATH_OUTPUT_DATA,
    "mc_grid_metric_summary_base.Rds"
  ),
  "\n"
)

cat("\nRows in original grid:\n")
print(nrow(twc_grid))

cat("\nRows in filtered grid before metric expansion:\n")
print(nrow(grid_dt))

cat("\nRows in grid metric summary:\n")
print(nrow(grid_metric_summary))

cat("\nRegion x biome combinations retained:\n")
print(
  unique(
    grid_metric_summary[
      ,
      .(region, biome)
    ]
  )[
    order(region, biome)
  ]
)

cat("\nGrid metric summary preview:\n")
print(
  grid_metric_summary[
    order(lon, lat, variable)
  ][
    1:40
  ]
)

cat("\nFinished creating simplified grid metric summary.\n")