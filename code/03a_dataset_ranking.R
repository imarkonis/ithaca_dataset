# ============================================================================
# Rank candidate precipitation and evaporation datasets against candidate
# specific leave-out reference statistics
#
# This script:
#   1) reads candidate-reference tables for precipitation and evaporation
#   2) estimates relative bias for mean, SD, and Sen slope
#   3) evaluates slope significance agreement with the reference ensemble
#   4) ranks datasets within each grid cell
#   5) merges precipitation and evaporation ranks
#   6) builds best/worst dataset maps for four agreement properties
# ============================================================================


# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

prec_candidate_reference_values <- readRDS(
  file.path(PATH_OUTPUT_DATA, "prec_candidate_reference_values.Rds")
)

evap_candidate_reference_values <- readRDS(
  file.path(PATH_OUTPUT_DATA, "evap_candidate_reference_values.Rds")
)


# Constants & Variables =======================================================

EPSILON <- 1e-6

PROPERTY_BIAS <- c(
  prec_mean  = "prec_mean_bias",
  prec_trend = "prec_bias_slope",
  evap_mean  = "evap_mean_bias",
  evap_trend = "evap_bias_slope"
)


# Functions ===================================================================

relative_abs_bias <- function(value, reference) {
  abs(value - reference) / (abs(reference) + EPSILON)
}


check_required_columns <- function(dt, required_cols, object_name) {
  missing_cols <- setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        object_name,
        " is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  invisible(TRUE)
}


build_component_stats <- function(dt, prefix) {
  
  out <- dt[
    ,
    .(
      lon,
      lat,
      dataset = candidate_dataset,
      value_dataset = candidate_value_dataset,
      mean = candidate_mean,
      sd = candidate_sd,
      n_years_mean = candidate_n_years_mean,
      sen_slope = candidate_sen_slope,
      p_value = candidate_p_value,
      stat_sig = candidate_stat_sig,
      n_years_slope = candidate_n_years_slope,
      ref_mean_median,
      ref_sd_median,
      ref_slope_median
    )
  ]
  
  setnames(
    out,
    old = c(
      "value_dataset",
      "mean",
      "sd",
      "n_years_mean",
      "sen_slope",
      "p_value",
      "stat_sig",
      "n_years_slope",
      "ref_mean_median",
      "ref_sd_median",
      "ref_slope_median"
    ),
    new = paste0(
      prefix,
      c(
        "_value_dataset",
        "_mean",
        "_sd",
        "_n_years_mean",
        "_sen_slope",
        "_p_value",
        "_stat_sig",
        "_n_years_slope",
        "_ref_mean_median",
        "_ref_sd_median",
        "_ref_slope_median"
      )
    )
  )
  
  out
}


build_component_ranks <- function(dt, prefix) {
  
  out <- dt[
    ,
    .(
      lon,
      lat,
      dataset = candidate_dataset,
      value_dataset = candidate_value_dataset,
      candidate_mean,
      candidate_sd,
      candidate_sen_slope,
      candidate_p_value,
      candidate_stat_sig,
      ref_mean_median,
      ref_sd_median,
      ref_slope_median,
      majority_significant,
      majority_agrees
    )
  ]
  
  mean_bias_col <- paste0(prefix, "_mean_bias")
  sd_bias_col <- paste0(prefix, "_sd_bias")
  slope_bias_col <- paste0(prefix, "_bias_slope")
  slope_diff_col <- paste0(prefix, "_diff_slope")
  
  mean_rank_col <- paste0(prefix, "_mean_rank")
  sd_rank_col <- paste0(prefix, "_sd_rank")
  slope_rank_col <- paste0(prefix, "_rank_slope")
  
  check_sig_col <- paste0(prefix, "_check_significance")
  check_non_sig_col <- paste0(prefix, "_check_non_significance")
  
  out[
    ,
    (mean_bias_col) := relative_abs_bias(
      candidate_mean,
      ref_mean_median
    )
  ]
  
  out[
    ,
    (sd_bias_col) := relative_abs_bias(
      candidate_sd,
      ref_sd_median
    )
  ]
  
  out[
    ,
    (slope_diff_col) := abs(
      candidate_sen_slope - ref_slope_median
    )
  ]
  
  out[
    ,
    (slope_bias_col) := relative_abs_bias(
      candidate_sen_slope,
      ref_slope_median
    )
  ]
  
  out[
    ,
    (check_sig_col) := as.logical(NA)
  ]
  
  out[
    ,
    (check_non_sig_col) := as.logical(NA)
  ]
  
  out[
    majority_significant == TRUE &
      majority_agrees == TRUE &
      candidate_stat_sig == TRUE,
    (check_sig_col) := TRUE
  ]
  
  out[
    majority_significant == TRUE &
      majority_agrees == TRUE &
      candidate_stat_sig == FALSE,
    (check_sig_col) := FALSE
  ]
  
  out[
    majority_significant == FALSE &
      candidate_stat_sig == FALSE,
    (check_non_sig_col) := TRUE
  ]
  
  out[
    majority_significant == FALSE &
      candidate_stat_sig == TRUE,
    (check_non_sig_col) := FALSE
  ]
  
  out[
    ,
    (mean_rank_col) := frank(
      get(mean_bias_col),
      ties.method = "min",
      na.last = "keep"
    ),
    by = .(lon, lat)
  ]
  
  out[
    ,
    (sd_rank_col) := frank(
      get(sd_bias_col),
      ties.method = "min",
      na.last = "keep"
    ),
    by = .(lon, lat)
  ]
  
  # Keep old logic: slope rank is based on absolute slope difference.
  out[
    ,
    (slope_rank_col) := frank(
      get(slope_diff_col),
      ties.method = "min",
      na.last = "keep"
    ),
    by = .(lon, lat)
  ]
  
  keep_cols <- c(
    "lon",
    "lat",
    "dataset",
    "value_dataset",
    mean_bias_col,
    sd_bias_col,
    slope_bias_col,
    slope_diff_col,
    mean_rank_col,
    sd_rank_col,
    slope_rank_col,
    check_sig_col,
    check_non_sig_col
  )
  
  out <- out[, ..keep_cols]
  
  setnames(
    out,
    old = "value_dataset",
    new = paste0(prefix, "_value_dataset")
  )
  
  out
}


best_worst_at <- function(dt, bias_col) {
  dt[
    is.finite(get(bias_col)),
    {
      v <- get(bias_col)
      
      .(
        position = c("best", "worst"),
        dataset = c(dataset[which.min(v)], dataset[which.max(v)]),
        n_avail = .N
      )
    },
    by = .(lon, lat)
  ]
}


build_best_worst_maps <- function(dt, property_bias) {
  rbindlist(
    lapply(
      names(property_bias),
      function(prop) {
        best_worst_at(
          dt = dt,
          bias_col = property_bias[[prop]]
        )[
          ,
          property := prop
        ]
      }
    ),
    use.names = TRUE
  )
}


# Analysis ====================================================================

required_cols <- c(
  "candidate_dataset",
  "candidate_value_dataset",
  "lon",
  "lat",
  "candidate_mean",
  "candidate_sd",
  "candidate_n_years_mean",
  "candidate_sen_slope",
  "candidate_p_value",
  "candidate_stat_sig",
  "candidate_n_years_slope",
  "ref_mean_median",
  "ref_sd_median",
  "ref_slope_median",
  "majority_significant",
  "majority_agrees"
)

check_required_columns(
  dt = prec_candidate_reference_values,
  required_cols = required_cols,
  object_name = "prec_candidate_reference_values"
)

check_required_columns(
  dt = evap_candidate_reference_values,
  required_cols = required_cols,
  object_name = "evap_candidate_reference_values"
)

## Build component statistics

prec_stats <- build_component_stats(
  dt = prec_candidate_reference_values,
  prefix = "prec"
)

evap_stats <- build_component_stats(
  dt = evap_candidate_reference_values,
  prefix = "evap"
)

setkey(prec_stats, lon, lat, dataset)
setkey(evap_stats, lon, lat, dataset)

dataset_stats <- evap_stats[prec_stats, nomatch = 0]


## Build component ranks

prec_ranks <- build_component_ranks(
  dt = prec_candidate_reference_values,
  prefix = "prec"
)

evap_ranks <- build_component_ranks(
  dt = evap_candidate_reference_values,
  prefix = "evap"
)

setkey(prec_ranks, lon, lat, dataset)
setkey(evap_ranks, lon, lat, dataset)

dataset_ranks <- evap_ranks[prec_ranks, nomatch = 0]

dataset_stats[, dataset := factor(dataset)]
dataset_ranks[, dataset := factor(dataset)]


## Best and worst maps

bw_maps <- build_best_worst_maps(
  dt = dataset_ranks,
  property_bias = PROPERTY_BIAS
)

bw_maps[
  ,
  property := factor(
    property,
    levels = names(PROPERTY_BIAS)
  )
]

bw_maps[
  ,
  position := factor(
    position,
    levels = c("best", "worst")
  )
]


# Outputs =====================================================================

saveRDS(
  dataset_ranks,
  file.path(PATH_OUTPUT_DATA, "dataset_ranks.Rds")
)

saveRDS(
  dataset_stats,
  file.path(PATH_OUTPUT_DATA, "prec_evap_stats.Rds")
)

saveRDS(
  bw_maps,
  file.path(PATH_OUTPUT_DATA, "dataset_best_worst_maps.Rds")
)


# Plot ========================================================================

ggplot(bw_maps) +
  geom_tile(aes(x = lon, y = lat, fill = dataset)) +
  borders("world", colour = "grey20", linewidth = 0.15) +
  facet_grid(position ~ property) +
  coord_equal(expand = FALSE) +
  labs(
    title = "Best and worst dataset by grid cell",
    subtitle = "Ranked within cell by candidate-specific leave-out reference bias; lower bias = best",
    x = NULL,
    y = NULL,
    fill = "Dataset"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    panel.spacing = unit(0.4, "lines")
  )


# Validation ==================================================================

stopifnot(nrow(dataset_ranks) > 0)
stopifnot(nrow(dataset_stats) > 0)
stopifnot(nrow(bw_maps) > 0)

stopifnot(
  unique(
    dataset_stats[
      dataset == "GLEAM",
      prec_value_dataset
    ]
  ) == "MSWEP"
)

stopifnot(
  unique(
    dataset_stats[
      dataset == "GLEAM",
      evap_value_dataset
    ]
  ) == "GLEAM"
)

stopifnot(
  all(
    names(PROPERTY_BIAS) %in%
      unique(as.character(bw_maps$property))
  )
)

bw_maps[, .N, .(dataset, position)]