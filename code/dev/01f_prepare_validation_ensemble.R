# ============================================================================
# Estimate ensemble reference statistics for precipitation and evaporation
#
# This script:
#   1) uses the full 8 member precipitation and evaporation ensembles
#   2) estimates per dataset grid cell means and standard deviations
#   3) estimates Sen slopes and Mann Kendall p values per dataset and grid cell
#   4) computes full 8 member ensemble statistics
#   5) computes candidate specific reference statistics for the analysis datasets
#   6) removes the candidate dataset from the reference ensemble when present
#   7) keeps the full reference ensemble when the candidate is absent
#   8) saves raw ensemble datasets, slopes, full statistics, and candidate
#      reference statistics
# ============================================================================


# Inputs ======================================================================

source("source/twc_change.R")

library(parallel)
library(trend)

prec_evap_raw <- read_fst(
  file.path(PATH_OUTPUT_RAW, "prec_evap_raw.fst"),
  as.data.table = TRUE
)


# Constants & Variables =======================================================

P_VALUE_THRESHOLD <- 0.1
MIN_YEARS_FOR_TREND <- diff(FULL_PERIOD) - 4

N_WORKERS <- as.integer(
  Sys.getenv(
    "TWC_N_WORKERS",
    unset = max(1L, parallel::detectCores() - 2L)
  )
)

# Avoid oversubscription. Each forked worker should not also use many
# data.table threads.
data.table::setDTthreads(1L)


# Functions ===================================================================

safe_median <- function(x) {
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  median(x)
}


safe_iqr <- function(x) {
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  as.numeric(IQR(x))
}


get_reference_dataset_names <- function(ensemble_names, candidate_name) {
  if (candidate_name %in% ensemble_names) {
    return(setdiff(ensemble_names, candidate_name))
  }
  
  ensemble_names
}


validate_dataset_names <- function(dt, variable_name, dataset_names, label) {
  available_names <- unique(dt[variable == variable_name, dataset])
  missing_names <- setdiff(dataset_names, available_names)
  
  if (length(missing_names) > 0) {
    stop(
      paste0(
        "Missing ", label, " datasets for variable ", variable_name, ": ",
        paste(missing_names, collapse = ", ")
      )
    )
  }
  
  invisible(TRUE)
}


estimate_single_dataset_diagnostics <- function(dt, dataset_name) {
  data.table::setDTthreads(1L)
  
  dt_use <- copy(dt[dataset == dataset_name])
  
  if ("variable" %in% names(dt_use)) {
    dt_use[, variable := NULL]
  }
  
  setorder(dt_use, lon, lat, year)
  
  dataset_stats <- dt_use[
    ,
    .(
      dataset_mean = mean(value, na.rm = TRUE),
      dataset_sd = sd(value, na.rm = TRUE),
      n_years_mean = sum(is.finite(value))
    ),
    by = .(lon, lat)
  ]
  
  dataset_stats[
    ,
    dataset := dataset_name
  ]
  
  setcolorder(
    dataset_stats,
    c("lon", "lat", "dataset", setdiff(names(dataset_stats), c("lon", "lat", "dataset")))
  )
  
  dataset_slopes <- dt_use[
    ,
    {
      value_valid <- value[is.finite(value)]
      
      if (
        length(value_valid) >= MIN_YEARS_FOR_TREND &&
        length(unique(value_valid)) > 1
      ) {
        sen_result <- trend::sens.slope(x = value_valid)
        mk_result <- trend::mk.test(x = value_valid)
        
        list(
          sen_slope = as.numeric(sen_result$estimates),
          p_value = mk_result$p.value,
          n_years_slope = length(value_valid)
        )
      } else {
        list(
          sen_slope = NA_real_,
          p_value = NA_real_,
          n_years_slope = length(value_valid)
        )
      }
    },
    by = .(lon, lat)
  ]
  
  dataset_slopes[
    ,
    dataset := dataset_name
  ]
  
  setcolorder(
    dataset_slopes,
    c("lon", "lat", "dataset", setdiff(names(dataset_slopes), c("lon", "lat", "dataset")))
  )
  
  setorder(dt_use, dataset, lon, lat, year)
  setorder(dataset_stats, dataset, lon, lat)
  setorder(dataset_slopes, dataset, lon, lat)
  
  list(
    datasets = dt_use,
    dataset_stats = dataset_stats,
    dataset_slopes = dataset_slopes
  )
}


estimate_dataset_diagnostics <- function(dt, dataset_names, n_workers = N_WORKERS) {
  dt_use <- copy(dt[dataset %in% dataset_names])
  
  if ("variable" %in% names(dt_use)) {
    dt_use[, variable := NULL]
  }
  
  setorder(dt_use, dataset, lon, lat, year)
  
  message(
    "Estimating diagnostics for ",
    length(dataset_names),
    " datasets using ",
    n_workers,
    " workers."
  )
  
  diagnostic_list <- parallel::mclapply(
    dataset_names,
    function(dataset_name) {
      estimate_single_dataset_diagnostics(
        dt = dt,
        dataset_name = dataset_name
      )
    },
    mc.cores = min(n_workers, length(dataset_names)),
    mc.preschedule = TRUE
  )
  
  dataset_stats <- rbindlist(
    lapply(diagnostic_list, `[[`, "dataset_stats"),
    use.names = TRUE,
    fill = TRUE
  )
  
  dataset_slopes <- rbindlist(
    lapply(diagnostic_list, `[[`, "dataset_slopes"),
    use.names = TRUE,
    fill = TRUE
  )
  
  setorder(dataset_stats, dataset, lon, lat)
  setorder(dataset_slopes, dataset, lon, lat)
  
  list(
    datasets = dt_use,
    dataset_stats = dataset_stats,
    dataset_slopes = dataset_slopes
  )
}


estimate_ensemble_summary <- function(
    dataset_stats,
    dataset_slopes,
    dataset_names
) {
  
  ensemble_stats <- dataset_stats[
    dataset %in% dataset_names,
    .(
      n_datasets_mean = sum(is.finite(dataset_mean)),
      ens_median = safe_median(dataset_mean),
      ens_mean_iqr = safe_iqr(dataset_mean),
      ens_sd = safe_median(dataset_sd),
      ens_sd_iqr = safe_iqr(dataset_sd)
    ),
    by = .(lon, lat)
  ]
  
  ensemble_slopes <- dataset_slopes[
    dataset %in% dataset_names,
    .(
      n_datasets_slope = sum(is.finite(sen_slope)),
      ens_slope_median = safe_median(sen_slope),
      ens_slope_iqr = safe_iqr(sen_slope)
    ),
    by = .(lon, lat)
  ]
  
  slope_summary <- copy(dataset_slopes[dataset %in% dataset_names])
  
  slope_summary[
    ,
    significant := is.finite(p_value) & p_value < P_VALUE_THRESHOLD
  ]
  
  slope_summary[
    ,
    slope_sign := sign(sen_slope)
  ]
  
  agreement_summary <- slope_summary[
    ,
    .(
      n_total = .N,
      n_trend_available = sum(is.finite(sen_slope)),
      n_significant = sum(significant, na.rm = TRUE),
      n_pos = sum(significant == TRUE & slope_sign > 0, na.rm = TRUE),
      n_neg = sum(significant == TRUE & slope_sign < 0, na.rm = TRUE)
    ),
    by = .(lon, lat)
  ]
  
  agreement_summary[
    ,
    majority_significant := n_significant > floor(length(dataset_names) / 2)
  ]
  
  agreement_summary[
    ,
    majority_agrees := majority_significant == TRUE &
      pmax(n_pos, n_neg) > floor(n_significant / 2)
  ]
  
  agreement_summary[
    ,
    majority_sign := fifelse(
      n_pos > n_neg,
      1L,
      fifelse(
        n_neg > n_pos,
        -1L,
        0L
      )
    )
  ]
  
  ensemble_summary <- merge(
    ensemble_stats,
    ensemble_slopes,
    by = c("lon", "lat"),
    all = TRUE
  )
  
  ensemble_summary <- merge(
    ensemble_summary,
    agreement_summary,
    by = c("lon", "lat"),
    all = TRUE
  )
  
  setorder(ensemble_summary, lon, lat)
  
  ensemble_summary
}


estimate_candidate_reference_stats <- function(
    dataset_stats,
    dataset_slopes,
    ensemble_names,
    candidate_names,
    variable_name,
    n_workers = N_WORKERS
) {
  
  message(
    "Estimating candidate references for ",
    variable_name,
    " using ",
    min(n_workers, length(candidate_names)),
    " workers."
  )
  
  candidate_reference_stats <- rbindlist(
    parallel::mclapply(
      candidate_names,
      function(candidate_name) {
        data.table::setDTthreads(1L)
        
        reference_names <- get_reference_dataset_names(
          ensemble_names = ensemble_names,
          candidate_name = candidate_name
        )
        
        reference_stats <- estimate_ensemble_summary(
          dataset_stats = dataset_stats,
          dataset_slopes = dataset_slopes,
          dataset_names = reference_names
        )
        
        reference_stats[
          ,
          `:=`(
            variable = variable_name,
            candidate_dataset = candidate_name,
            candidate_present_in_variable = candidate_name %in% ensemble_names,
            candidate_removed_from_reference = candidate_name %in% ensemble_names,
            reference_n_datasets = length(reference_names),
            reference_names = paste(reference_names, collapse = ";")
          )
        ]
        
        reference_stats
      },
      mc.cores = min(n_workers, length(candidate_names)),
      mc.preschedule = TRUE
    ),
    fill = TRUE
  )
  
  setnames(
    candidate_reference_stats,
    old = c(
      "ens_median",
      "ens_mean_iqr",
      "ens_sd",
      "ens_sd_iqr",
      "ens_slope_median",
      "ens_slope_iqr"
    ),
    new = c(
      "ref_mean_median",
      "ref_mean_iqr",
      "ref_sd_median",
      "ref_sd_iqr",
      "ref_slope_median",
      "ref_slope_iqr"
    )
  )
  
  setcolorder(
    candidate_reference_stats,
    c(
      "variable",
      "candidate_dataset",
      "candidate_present_in_variable",
      "candidate_removed_from_reference",
      "reference_n_datasets",
      "reference_names",
      setdiff(
        names(candidate_reference_stats),
        c(
          "variable",
          "candidate_dataset",
          "candidate_present_in_variable",
          "candidate_removed_from_reference",
          "reference_n_datasets",
          "reference_names"
        )
      )
    )
  )
  
  setorder(candidate_reference_stats, candidate_dataset, lon, lat)
  
  candidate_reference_stats
}


estimate_ensemble_products <- function(
    dt,
    variable_name,
    ensemble_names,
    candidate_names,
    n_workers = N_WORKERS
) {
  
  diagnostics <- estimate_dataset_diagnostics(
    dt = dt,
    dataset_names = ensemble_names,
    n_workers = n_workers
  )
  
  full_ensemble_stats <- estimate_ensemble_summary(
    dataset_stats = diagnostics$dataset_stats,
    dataset_slopes = diagnostics$dataset_slopes,
    dataset_names = ensemble_names
  )
  
  full_ensemble_stats[
    ,
    `:=`(
      variable = variable_name,
      reference_type = "full_variable_ensemble",
      reference_n_datasets = length(ensemble_names),
      reference_names = paste(ensemble_names, collapse = ";")
    )
  ]
  
  candidate_reference_stats <- estimate_candidate_reference_stats(
    dataset_stats = diagnostics$dataset_stats,
    dataset_slopes = diagnostics$dataset_slopes,
    ensemble_names = ensemble_names,
    candidate_names = candidate_names,
    variable_name = variable_name,
    n_workers = n_workers
  )
  
  list(
    datasets = diagnostics$datasets,
    dataset_stats = diagnostics$dataset_stats,
    slopes = diagnostics$dataset_slopes,
    stats = full_ensemble_stats,
    candidate_reference_stats = candidate_reference_stats
  )
}

prepare_candidate_reference_values <- function(
    candidate_reference_stats,
    dataset_stats,
    raw_dt,
    variable_name,
    candidate_names,
    candidate_value_overrides = NULL
) {
  
  reference_use <- candidate_reference_stats[
    ,
    .(
      candidate_dataset,
      lon,
      lat,
      ref_mean_median,
      ref_mean_iqr,
      ref_sd_median,
      ref_sd_iqr,
      ref_slope_median,
      ref_slope_iqr,
      n_significant,
      n_pos,
      n_neg,
      majority_significant,
      majority_agrees,
      majority_sign
    )
  ]
  
  candidate_map <- data.table(
    candidate_dataset = candidate_names,
    candidate_value_dataset = candidate_names
  )
  
  if (!is.null(candidate_value_overrides)) {
    override_dt <- data.table(
      candidate_dataset = names(candidate_value_overrides),
      candidate_value_dataset = as.character(candidate_value_overrides)
    )
    
    candidate_map[
      override_dt,
      candidate_value_dataset := i.candidate_value_dataset,
      on = "candidate_dataset"
    ]
  }
  
  candidate_values_from_stats <- merge(
    candidate_map,
    dataset_stats[
      ,
      .(
        candidate_value_dataset = dataset,
        lon,
        lat,
        candidate_mean = dataset_mean,
        candidate_sd = dataset_sd,
        candidate_n_years = n_years_mean
      )
    ],
    by = "candidate_value_dataset",
    allow.cartesian = TRUE
  )
  
  missing_value_datasets <- setdiff(
    candidate_map$candidate_value_dataset,
    unique(dataset_stats$dataset)
  )
  
  if (length(missing_value_datasets) > 0) {
    
    candidate_values_from_raw <- merge(
      candidate_map[
        candidate_value_dataset %in% missing_value_datasets
      ],
      raw_dt[
        variable == variable_name &
          dataset %in% missing_value_datasets,
        .(
          candidate_value_dataset = dataset,
          lon,
          lat,
          candidate_mean = mean(value, na.rm = TRUE),
          candidate_sd = sd(value, na.rm = TRUE),
          candidate_n_years = sum(is.finite(value))
        ),
        by = .(dataset, lon, lat)
      ][
        ,
        dataset := NULL
      ],
      by = "candidate_value_dataset",
      allow.cartesian = TRUE
    )
    
    candidate_cell_means <- rbindlist(
      list(
        candidate_values_from_stats,
        candidate_values_from_raw
      ),
      use.names = TRUE,
      fill = TRUE
    )
    
  } else {
    
    candidate_cell_means <- candidate_values_from_stats
    
  }
  
  candidate_reference_values <- merge(
    candidate_cell_means,
    reference_use,
    by = c("candidate_dataset", "lon", "lat"),
    all.x = TRUE
  )
  
  setcolorder(
    candidate_reference_values,
    c(
      "candidate_dataset",
      "candidate_value_dataset",
      "lon",
      "lat",
      "candidate_mean",
      "candidate_sd",
      "candidate_n_years",
      "ref_mean_median",
      "ref_mean_iqr",
      "ref_sd_median",
      "ref_sd_iqr",
      "ref_slope_median",
      "ref_slope_iqr",
      "n_significant",
      "n_pos",
      "n_neg",
      "majority_significant",
      "majority_agrees",
      "majority_sign"
    )
  )
  
  setorder(candidate_reference_values, candidate_dataset, lon, lat)
  
  candidate_reference_values
}

prepare_candidate_reference_values <- function(
    candidate_reference_stats,
    dataset_stats,
    dataset_slopes,
    raw_dt,
    variable_name,
    candidate_names,
    candidate_value_overrides = NULL
) {
  
  reference_use <- candidate_reference_stats[
    ,
    .(
      candidate_dataset,
      lon,
      lat,
      ref_mean_median,
      ref_mean_iqr,
      ref_sd_median,
      ref_sd_iqr,
      ref_slope_median,
      ref_slope_iqr,
      n_significant,
      n_pos,
      n_neg,
      majority_significant,
      majority_agrees,
      majority_sign
    )
  ]
  
  candidate_map <- data.table(
    candidate_dataset = candidate_names,
    candidate_value_dataset = candidate_names
  )
  
  if (!is.null(candidate_value_overrides)) {
    override_dt <- data.table(
      candidate_dataset = names(candidate_value_overrides),
      candidate_value_dataset = as.character(candidate_value_overrides)
    )
    
    candidate_map[
      override_dt,
      candidate_value_dataset := i.candidate_value_dataset,
      on = "candidate_dataset"
    ]
  }
  
  candidate_values_from_stats <- merge(
    candidate_map,
    merge(
      dataset_stats[
        ,
        .(
          candidate_value_dataset = dataset,
          lon,
          lat,
          candidate_mean = dataset_mean,
          candidate_sd = dataset_sd,
          candidate_n_years_mean = n_years_mean
        )
      ],
      dataset_slopes[
        ,
        .(
          candidate_value_dataset = dataset,
          lon,
          lat,
          candidate_sen_slope = sen_slope,
          candidate_p_value = p_value,
          candidate_n_years_slope = n_years_slope
        )
      ],
      by = c("candidate_value_dataset", "lon", "lat"),
      all = TRUE
    ),
    by = "candidate_value_dataset",
    allow.cartesian = TRUE
  )
  
  candidate_values_from_stats[
    ,
    candidate_stat_sig := is.finite(candidate_p_value) &
      candidate_p_value < P_VALUE_THRESHOLD
  ]
  
  missing_value_datasets <- setdiff(
    candidate_map$candidate_value_dataset,
    unique(dataset_stats$dataset)
  )
  
  if (length(missing_value_datasets) > 0) {
    
    raw_cell_diagnostics <- raw_dt[
      variable == variable_name &
        dataset %in% missing_value_datasets,
      {
        value_ordered <- value[order(year)]
        value_valid <- value_ordered[is.finite(value_ordered)]
        
        if (
          length(value_valid) >= MIN_YEARS_FOR_TREND &&
          length(unique(value_valid)) > 1
        ) {
          sen_result <- sens.slope(x = value_valid)
          mk_result <- mk.test(x = value_valid)
          
          list(
            candidate_mean = mean(value, na.rm = TRUE),
            candidate_sd = sd(value, na.rm = TRUE),
            candidate_n_years_mean = sum(is.finite(value)),
            candidate_sen_slope = as.numeric(sen_result$estimates),
            candidate_p_value = mk_result$p.value,
            candidate_n_years_slope = length(value_valid)
          )
        } else {
          list(
            candidate_mean = mean(value, na.rm = TRUE),
            candidate_sd = sd(value, na.rm = TRUE),
            candidate_n_years_mean = sum(is.finite(value)),
            candidate_sen_slope = NA_real_,
            candidate_p_value = NA_real_,
            candidate_n_years_slope = length(value_valid)
          )
        }
      },
      by = .(
        candidate_value_dataset = dataset,
        lon,
        lat
      )
    ]
    
    raw_cell_diagnostics[
      ,
      candidate_stat_sig := is.finite(candidate_p_value) &
        candidate_p_value < P_VALUE_THRESHOLD
    ]
    
    candidate_values_from_raw <- merge(
      candidate_map[
        candidate_value_dataset %in% missing_value_datasets
      ],
      raw_cell_diagnostics,
      by = "candidate_value_dataset",
      allow.cartesian = TRUE
    )
    
    candidate_cell_values <- rbindlist(
      list(
        candidate_values_from_stats,
        candidate_values_from_raw
      ),
      use.names = TRUE,
      fill = TRUE
    )
    
  } else {
    
    candidate_cell_values <- candidate_values_from_stats
    
  }
  
  candidate_reference_values <- merge(
    candidate_cell_values,
    reference_use,
    by = c("candidate_dataset", "lon", "lat"),
    all.x = TRUE
  )
  
  setcolorder(
    candidate_reference_values,
    c(
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
      "ref_mean_iqr",
      "ref_sd_median",
      "ref_sd_iqr",
      "ref_slope_median",
      "ref_slope_iqr",
      "n_significant",
      "n_pos",
      "n_neg",
      "majority_significant",
      "majority_agrees",
      "majority_sign"
    )
  )
  
  setorder(candidate_reference_values, candidate_dataset, lon, lat)
  
  candidate_reference_values
}

# Analysis ====================================================================

validate_dataset_names(
  dt = prec_evap_raw,
  variable_name = "prec",
  dataset_names = PREC_ENSEMBLE_NAMES_SHORT,
  label = "precipitation ensemble"
)

validate_dataset_names(
  dt = prec_evap_raw,
  variable_name = "evap",
  dataset_names = EVAP_ENSEMBLE_NAMES_SHORT,
  label = "evaporation ensemble"
)

message(
  "Precipitation ensemble datasets: ",
  paste(PREC_ENSEMBLE_NAMES_SHORT, collapse = ", ")
)

message(
  "Evaporation ensemble datasets: ",
  paste(EVAP_ENSEMBLE_NAMES_SHORT, collapse = ", ")
)

message(
  "Analysis candidate datasets: ",
  paste(EVAP_NAMES_SHORT, collapse = ", ")
)

message(
  "Using workers: ",
  N_WORKERS
)

prec_ensemble <- estimate_ensemble_products(
  dt = prec_evap_raw[variable == "prec"],
  variable_name = "prec",
  ensemble_names = PREC_ENSEMBLE_NAMES_SHORT,
  candidate_names = EVAP_NAMES_SHORT,
  n_workers = N_WORKERS
)

evap_ensemble <- estimate_ensemble_products(
  dt = prec_evap_raw[variable == "evap"],
  variable_name = "evap",
  ensemble_names = EVAP_ENSEMBLE_NAMES_SHORT,
  candidate_names = EVAP_NAMES_SHORT,
  n_workers = N_WORKERS
)

prec_candidate_reference_values <- prepare_candidate_reference_values(
  candidate_reference_stats = prec_ensemble$candidate_reference_stats,
  dataset_stats = prec_ensemble$dataset_stats,
  dataset_slopes = prec_ensemble$slopes,
  raw_dt = prec_evap_raw,
  variable_name = "prec",
  candidate_names = EVAP_NAMES_SHORT,
  candidate_value_overrides = c(GLEAM = "MSWEP")
)

evap_candidate_reference_values <- prepare_candidate_reference_values(
  candidate_reference_stats = evap_ensemble$candidate_reference_stats,
  dataset_stats = evap_ensemble$dataset_stats,
  dataset_slopes = evap_ensemble$slopes,
  raw_dt = prec_evap_raw,
  variable_name = "evap",
  candidate_names = EVAP_NAMES_SHORT
)

# Outputs =====================================================================

saveRDS(
  prec_ensemble$stats,
  file.path(PATH_OUTPUT_DATA, "prec_ensemble_stats.Rds")
)

saveRDS(
  prec_ensemble$datasets,
  file.path(PATH_OUTPUT_DATA, "prec_ensemble.Rds")
)

saveRDS(
  prec_ensemble$dataset_stats,
  file.path(PATH_OUTPUT_DATA, "prec_dataset_stats.Rds")
)

saveRDS(
  prec_ensemble$slopes,
  file.path(PATH_OUTPUT_DATA, "prec_ensemble_slopes.Rds")
)

saveRDS(
  prec_ensemble$candidate_reference_stats,
  file.path(PATH_OUTPUT_DATA, "prec_candidate_reference_stats.Rds")
)

saveRDS(
  prec_candidate_reference_values,
  file.path(PATH_OUTPUT_DATA, "prec_candidate_reference_values.Rds")
)

saveRDS(
  evap_ensemble$stats,
  file.path(PATH_OUTPUT_DATA, "evap_ensemble_stats.Rds")
)

saveRDS(
  evap_ensemble$datasets,
  file.path(PATH_OUTPUT_DATA, "evap_ensemble.Rds")
)

saveRDS(
  evap_ensemble$dataset_stats,
  file.path(PATH_OUTPUT_DATA, "evap_dataset_stats.Rds")
)

saveRDS(
  evap_ensemble$slopes,
  file.path(PATH_OUTPUT_DATA, "evap_ensemble_slopes.Rds")
)

saveRDS(
  evap_ensemble$candidate_reference_stats,
  file.path(PATH_OUTPUT_DATA, "evap_candidate_reference_stats.Rds")
)

saveRDS(
  evap_candidate_reference_values,
  file.path(PATH_OUTPUT_DATA, "evap_candidate_reference_values.Rds")
)

# Validation ==================================================================

dummy <- prec_ensemble$candidate_reference_stats[lon == 36.875 & lat == 45.375]

stopifnot(nrow(prec_ensemble$stats) > 0)
stopifnot(nrow(evap_ensemble$stats) > 0)

stopifnot(nrow(prec_ensemble$candidate_reference_stats) > 0)
stopifnot(nrow(evap_ensemble$candidate_reference_stats) > 0)

stopifnot(
  all(PREC_ENSEMBLE_NAMES_SHORT %in% unique(prec_ensemble$datasets$dataset))
)

stopifnot(
  all(EVAP_ENSEMBLE_NAMES_SHORT %in% unique(evap_ensemble$datasets$dataset))
)

stopifnot(
  all(
    EVAP_NAMES_SHORT %in%
      unique(prec_ensemble$candidate_reference_stats$candidate_dataset)
  )
)

stopifnot(
  all(
    EVAP_NAMES_SHORT %in%
      unique(evap_ensemble$candidate_reference_stats$candidate_dataset)
  )
)

gleam_prec_reference <- unique(
  prec_ensemble$candidate_reference_stats[
    candidate_dataset == "GLEAM",
    .(
      candidate_present_in_variable,
      candidate_removed_from_reference,
      reference_n_datasets,
      reference_names
    )
  ]
)

gleam_evap_reference <- unique(
  evap_ensemble$candidate_reference_stats[
    candidate_dataset == "GLEAM",
    .(
      candidate_present_in_variable,
      candidate_removed_from_reference,
      reference_n_datasets,
      reference_names
    )
  ]
)

print(gleam_prec_reference)
print(gleam_evap_reference)

stopifnot(
  unique(
    prec_ensemble$candidate_reference_stats[
      candidate_dataset == "GLEAM",
      candidate_removed_from_reference
    ]
  ) == FALSE
)

stopifnot(
  unique(
    evap_ensemble$candidate_reference_stats[
      candidate_dataset == "GLEAM",
      candidate_removed_from_reference
    ]
  ) == TRUE
)
