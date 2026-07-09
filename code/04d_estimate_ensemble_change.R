# ============================================================================
# Estimate Monte Carlo period differences and full-period trends
#
# This script:
# 1. Reads Monte Carlo annual P and E ensembles
# 2. Adds water availability and water flux
# 3. Estimates mean differences between two equal periods
# 4. Estimates full-period linear slopes and p values
# 5. Saves metrics for global, IPCC region, and region x biome outputs
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

mc_global_year_base <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_global_year_base.Rds")
)

mc_region_year_base <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_year_base.Rds")
)

mc_region_biome_year_base <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_biome_year_base.Rds")
)

mc_global_year_scenarios <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_global_year_scenarios.Rds")
)

mc_region_year_scenarios <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_year_scenarios.Rds")
)

mc_region_biome_year_scenarios <- readRDS(
  file.path(PATH_OUTPUT_DATA, "mc_region_biome_year_scenarios.Rds")
)

# Constants & Variables =======================================================

PERIOD_1 <- 1982:2001
PERIOD_2 <- 2002:2021

ALPHA <- 0.05

VARIABLE_LEVELS <- c(
  "prec",
  "evap",
  "avail",
  "flux"
)

VARIABLE_LABELS <- c(
  prec = "Precipitation",
  evap = "Evaporation",
  avail = "Water availability",
  flux = "Water flux"
)

# Functions ===================================================================

prepare_metric_long <- function(dt, id_cols) {
  
  dt <- as.data.table(copy(dt))
  
  required_cols <- c(id_cols, "year", "prec", "evap")
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0L) {
    stop(
      "Input data is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  dt[
    ,
    `:=`(
      avail = prec - evap,
      flux = (prec + evap) / 2
    )
  ]
  
  long_dt <- melt(
    dt,
    id.vars = c(id_cols, "year"),
    measure.vars = VARIABLE_LEVELS,
    variable.name = "variable",
    value.name = "value"
  )
  
  long_dt[
    ,
    variable := as.character(variable)
  ]
  
  long_dt[]
}

safe_lm_slope <- function(year, value) {
  
  ok <- is.finite(year) & is.finite(value)
  
  if (sum(ok) < 3L) {
    return(
      data.table(
        slope = NA_real_,
        p_value = NA_real_,
        n_years = sum(ok)
      )
    )
  }
  
  year_ok <- year[ok]
  value_ok <- value[ok]
  
  if (length(unique(year_ok)) < 3L) {
    return(
      data.table(
        slope = NA_real_,
        p_value = NA_real_,
        n_years = length(year_ok)
      )
    )
  }
  
  fit <- tryCatch(
    lm(value_ok ~ year_ok),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(
      data.table(
        slope = NA_real_,
        p_value = NA_real_,
        n_years = length(year_ok)
      )
    )
  }
  
  coef_dt <- summary(fit)$coefficients
  
  if (!"year_ok" %in% rownames(coef_dt)) {
    return(
      data.table(
        slope = NA_real_,
        p_value = NA_real_,
        n_years = length(year_ok)
      )
    )
  }
  
  data.table(
    slope = unname(coef_dt["year_ok", "Estimate"]),
    p_value = unname(coef_dt["year_ok", "Pr(>|t|)"]),
    n_years = length(year_ok)
  )
}

safe_t_test_p <- function(value_1, value_2) {
  
  value_1 <- value_1[is.finite(value_1)]
  value_2 <- value_2[is.finite(value_2)]
  
  if (length(value_1) < 3L || length(value_2) < 3L) {
    return(NA_real_)
  }
  
  if (sd(value_1) == 0 && sd(value_2) == 0) {
    return(NA_real_)
  }
  
  tryCatch(
    t.test(value_2, value_1)$p.value,
    error = function(e) NA_real_
  )
}

estimate_one_metric <- function(year, value, period_1, period_2, alpha) {
  
  value_1 <- value[year %in% period_1]
  value_2 <- value[year %in% period_2]
  
  mean_1 <- mean(value_1, na.rm = TRUE)
  mean_2 <- mean(value_2, na.rm = TRUE)
  
  if (!is.finite(mean_1)) {
    mean_1 <- NA_real_
  }
  
  if (!is.finite(mean_2)) {
    mean_2 <- NA_real_
  }
  
  diff_abs <- mean_2 - mean_1
  
  diff_rel <- ifelse(
    is.finite(mean_1) && mean_1 != 0,
    100 * diff_abs / mean_1,
    NA_real_
  )
  
  diff_p_value <- safe_t_test_p(
    value_1 = value_1,
    value_2 = value_2
  )
  
  slope_full <- safe_lm_slope(
    year = year,
    value = value
  )
  
  data.table(
    period_1_start = min(period_1),
    period_1_end = max(period_1),
    period_2_start = min(period_2),
    period_2_end = max(period_2),
    mean_1982_2001 = mean_1,
    mean_2002_2021 = mean_2,
    diff_2002_2021_minus_1982_2001 = diff_abs,
    diff_percent = diff_rel,
    diff_p_value = diff_p_value,
    diff_stat_sig = is.finite(diff_p_value) & diff_p_value < alpha,
    slope_full = slope_full$slope,
    slope_full_p_value = slope_full$p_value,
    slope_full_stat_sig = is.finite(slope_full$p_value) & slope_full$p_value < alpha,
    n_years_full = slope_full$n_years
  )
}

estimate_mc_metrics <- function(dt, id_cols, period_1, period_2, alpha) {
  
  long_dt <- prepare_metric_long(
    dt = dt,
    id_cols = id_cols
  )
  
  metric_dt <- long_dt[
    ,
    estimate_one_metric(
      year = year,
      value = value,
      period_1 = period_1,
      period_2 = period_2,
      alpha = alpha
    ),
    by = c(id_cols, "variable")
  ]
  
  metric_dt[
    ,
    variable_label := VARIABLE_LABELS[variable]
  ]
  
  setcolorder(
    metric_dt,
    c(
      id_cols,
      "variable",
      "variable_label",
      setdiff(
        names(metric_dt),
        c(id_cols, "variable", "variable_label")
      )
    )
  )
  
  setkeyv(
    metric_dt,
    c(id_cols, "variable")
  )
  
  metric_dt[]
}

# Analysis ====================================================================

mc_global_metrics_base <- estimate_mc_metrics(
  dt = mc_global_year_base,
  id_cols = c("sim", "scenario"),
  period_1 = PERIOD_1,
  period_2 = PERIOD_2,
  alpha = ALPHA
)

mc_region_metrics_base <- estimate_mc_metrics(
  dt = mc_region_year_base,
  id_cols = c("sim", "scenario", "region"),
  period_1 = PERIOD_1,
  period_2 = PERIOD_2,
  alpha = ALPHA
)

mc_region_biome_metrics_base <- estimate_mc_metrics(
  dt = mc_region_biome_year_base,
  id_cols = c("sim", "scenario", "region", "biome"),
  period_1 = PERIOD_1,
  period_2 = PERIOD_2,
  alpha = ALPHA
)

mc_global_metrics_scenarios <- estimate_mc_metrics(
  dt = mc_global_year_scenarios,
  id_cols = c("sim", "scenario"),
  period_1 = PERIOD_1,
  period_2 = PERIOD_2,
  alpha = ALPHA
)

mc_region_metrics_scenarios <- estimate_mc_metrics(
  dt = mc_region_year_scenarios,
  id_cols = c("sim", "scenario", "region"),
  period_1 = PERIOD_1,
  period_2 = PERIOD_2,
  alpha = ALPHA
)

mc_region_biome_metrics_scenarios <- estimate_mc_metrics(
  dt = mc_region_biome_year_scenarios,
  id_cols = c("sim", "scenario", "region", "biome"),
  period_1 = PERIOD_1,
  period_2 = PERIOD_2,
  alpha = ALPHA
)

# Outputs =====================================================================

saveRDS(
  mc_global_metrics_base,
  file.path(PATH_OUTPUT_DATA, "mc_global_metrics_base.Rds")
)

saveRDS(
  mc_region_metrics_base,
  file.path(PATH_OUTPUT_DATA, "mc_region_metrics_base.Rds")
)

saveRDS(
  mc_region_biome_metrics_base,
  file.path(PATH_OUTPUT_DATA, "mc_region_biome_metrics_base.Rds")
)

saveRDS(
  mc_global_metrics_scenarios,
  file.path(PATH_OUTPUT_DATA, "mc_global_metrics_scenarios.Rds")
)

saveRDS(
  mc_region_metrics_scenarios,
  file.path(PATH_OUTPUT_DATA, "mc_region_metrics_scenarios.Rds")
)

saveRDS(
  mc_region_biome_metrics_scenarios,
  file.path(PATH_OUTPUT_DATA, "mc_region_biome_metrics_scenarios.Rds")
)

# Validation ==================================================================

cat("\nSaved Monte Carlo metric outputs to PATH_OUTPUT_DATA.\n")

cat("\nGlobal metrics base preview:\n")
print(
  mc_global_metrics_base[
    order(sim, scenario, variable)
  ][
    1:40
  ]
)

cat("\nRegion metrics base preview:\n")
print(
  mc_region_metrics_base[
    order(sim, scenario, region, variable)
  ][
    1:40
  ]
)

cat("\nRegion biome metrics base preview:\n")
print(
  mc_region_biome_metrics_base[
    order(sim, scenario, region, biome, variable)
  ][
    1:40
  ]
)

cat("\nGlobal metrics scenarios preview:\n")
print(
  mc_global_metrics_scenarios[
    order(sim, scenario, variable)
  ][
    1:40
  ]
)

cat("\nFinished Monte Carlo period difference and trend estimation.\n")