# ============================================================================
# Generate Monte Carlo dataset selections
#
# This script:
# 1. Converts scenario weights to cumulative probability intervals
# 2. Creates shared random draws per simulation x IPCC region x biome
# 3. Generates two MC products:
#      a. 500-member base-scenario ensemble for main analysis
#      b. 100-member all-scenario ensemble for sensitivity analysis
# 4. Saves MC selection tables and selection matrices in PATH_OUTPUT_DATA
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

weights_dt <- readRDS(
  file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds")
)

# Constants & Variables =======================================================

RUN_SPECS <- data.table(
  run_id = c(
    "base",
    "scenarios"
  ),
  n_sims = c(
    500L,
    100L
  ),
  scenario_keep = c(
    "base",
    NA_character_
  ),
  seed = c(
    1979L,
    1979L
  )
)

# Functions ===================================================================

make_weight_cdf <- function(weights_dt, scenario_keep = NA_character_) {
  
  weights_region <- as.data.table(copy(weights_dt))
  
  required_cols <- c(
    "scenario",
    "region",
    "biome",
    "dataset",
    "w_region_biome"
  )
  
  missing_cols <- setdiff(required_cols, names(weights_region))
  
  if (length(missing_cols) > 0L) {
    stop(
      "weights_dt is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  weights_region[
    ,
    `:=`(
      scenario = as.character(scenario),
      region = as.character(region),
      biome = as.character(biome),
      dataset = as.character(dataset)
    )
  ]
  
  weights_region <- weights_region[
    is.finite(w_region_biome) &
      !is.na(w_region_biome) &
      w_region_biome > 0,
    .(scenario, region, biome, dataset, w_region_biome)
  ]
  
  if (!is.na(scenario_keep)) {
    weights_region <- weights_region[
      scenario == scenario_keep
    ]
  }
  
  if (nrow(weights_region) == 0L) {
    stop("No valid weights found for requested scenario selection.")
  }
  
  weights_region[
    ,
    w_region_biome := w_region_biome / sum(w_region_biome),
    by = .(scenario, region, biome)
  ]
  
  weight_check <- weights_region[
    ,
    .(
      w_sum = sum(w_region_biome),
      n_datasets = .N
    ),
    by = .(scenario, region, biome)
  ]
  
  bad_weights <- weight_check[
    abs(w_sum - 1) > 1e-10
  ]
  
  if (nrow(bad_weights) > 0L) {
    print(bad_weights)
    stop("Some scenario x region x biome weights do not sum to 1.")
  }
  
  setorder(
    weights_region,
    scenario,
    region,
    biome,
    dataset
  )
  
  weight_cdf <- weights_region[
    ,
    {
      p_high <- cumsum(w_region_biome)
      p_high[.N] <- 1
      
      p_low <- c(0, p_high[-.N])
      
      .(
        dataset = dataset,
        w_region_biome = w_region_biome,
        p_low = p_low,
        p_high = p_high
      )
    },
    by = .(scenario, region, biome)
  ]
  
  setkey(
    weight_cdf,
    scenario,
    region,
    biome,
    dataset
  )
  
  weight_cdf
}

make_mc_random <- function(weight_cdf, n_sims, seed) {
  
  set.seed(seed)
  
  region_biome_draws <- unique(
    weight_cdf[, .(region, biome)]
  )
  
  setorder(
    region_biome_draws,
    region,
    biome
  )
  
  mc_random <- region_biome_draws[
    ,
    .(sim = seq_len(n_sims)),
    by = .(region, biome)
  ]
  
  mc_random[
    ,
    u := runif(.N)
  ]
  
  setkey(
    mc_random,
    region,
    biome,
    sim
  )
  
  mc_random
}

make_mc_selection <- function(weight_cdf, mc_random) {
  
  weight_cdf <- as.data.table(copy(weight_cdf))
  mc_random <- as.data.table(copy(mc_random))
  
  weight_cdf[
    ,
    `:=`(
      scenario = as.character(scenario),
      region = as.character(region),
      biome = as.character(biome),
      dataset = as.character(dataset)
    )
  ]
  
  mc_random[
    ,
    `:=`(
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  mc_selection <- weight_cdf[
    mc_random,
    on = .(region, biome),
    allow.cartesian = TRUE
  ][
    u >= p_low & u < p_high,
    .(
      sim,
      scenario,
      region,
      biome,
      dataset,
      u
    )
  ]
  
  scenario_region_biome <- unique(
    weight_cdf[, .(scenario, region, biome)]
  )
  
  sim_values <- unique(mc_random$sim)
  
  expected_selection <- scenario_region_biome[
    ,
    .(sim = sim_values),
    by = .(scenario, region, biome)
  ][
    ,
    .(sim, scenario, region, biome)
  ]
  
  selection_count <- mc_selection[
    ,
    .N,
    by = .(sim, scenario, region, biome)
  ]
  
  selection_check <- merge(
    expected_selection,
    selection_count,
    by = c("sim", "scenario", "region", "biome"),
    all.x = TRUE
  )
  
  selection_check[
    is.na(N),
    N := 0L
  ]
  
  bad_selection <- selection_check[
    N != 1L
  ]
  
  if (nrow(bad_selection) > 0L) {
    print(bad_selection)
    stop(
      "Some sim x scenario x region x biome combinations do not have exactly one selected dataset."
    )
  }
  
  setkey(
    mc_selection,
    sim,
    scenario,
    region,
    biome
  )
  
  mc_selection
}

run_mc_selection <- function(
    run_id,
    n_sims,
    scenario_keep,
    seed,
    weights_dt
) {
  
  cat("\nRunning MC selection:", run_id, "\n")
  
  weight_cdf <- make_weight_cdf(
    weights_dt = weights_dt,
    scenario_keep = scenario_keep
  )
  
  mc_random <- make_mc_random(
    weight_cdf = weight_cdf,
    n_sims = n_sims,
    seed = seed
  )
  
  mc_selection <- make_mc_selection(
    weight_cdf = weight_cdf,
    mc_random = mc_random
  )
  
  selection_matrix <- dcast(
    mc_selection,
    sim + region + biome + u ~ scenario,
    value.var = "dataset"
  )
  
  saveRDS(
    weight_cdf,
    file.path(
      PATH_OUTPUT_DATA,
      paste0("weight_cdf_", run_id, ".Rds")
    )
  )
  
  saveRDS(
    mc_random,
    file.path(
      PATH_OUTPUT_DATA,
      paste0("mc_random_region_biome_", run_id, ".Rds")
    )
  )
  
  saveRDS(
    mc_selection,
    file.path(
      PATH_OUTPUT_DATA,
      paste0("mc_selection_", run_id, ".Rds")
    )
  )
  
  saveRDS(
    selection_matrix,
    file.path(
      PATH_OUTPUT_DATA,
      paste0("mc_selection_matrix_", run_id, ".Rds")
    )
  )
  
  weight_cdf_problem <- weight_cdf[
    ,
    .(
      first_low = min(p_low),
      last_high = max(p_high),
      n_datasets = .N
    ),
    by = .(scenario, region, biome)
  ][
    abs(first_low - 0) > 1e-12 |
      abs(last_high - 1) > 1e-12
  ]
  
  selection_problem <- mc_selection[
    ,
    .N,
    by = .(sim, scenario, region, biome)
  ][
    N != 1L
  ]
  
  cat("\nSaved MC selection outputs with suffix: _", run_id, "\n", sep = "")
  
  cat("\nNumber of simulations:\n")
  print(mc_selection[, uniqueN(sim)])
  
  cat("\nNumber of scenarios:\n")
  print(mc_selection[, uniqueN(scenario)])
  
  cat("\nWeight CDF check:\n")
  print(weight_cdf_problem)
  
  cat("\nSelection count check:\n")
  print(selection_problem)
  
  cat("\nSelection preview:\n")
  print(
    mc_selection[
      order(sim, region, biome, scenario)
    ][
      1:30
    ]
  )
  
  data.table(
    run_id = run_id,
    n_sims = n_sims,
    n_scenarios = mc_selection[, uniqueN(scenario)],
    n_selection_rows = nrow(mc_selection),
    n_weight_cdf_problem = nrow(weight_cdf_problem),
    n_selection_problem = nrow(selection_problem)
  )
}

# Analysis ====================================================================

run_summary <- rbindlist(
  lapply(seq_len(nrow(RUN_SPECS)), function(i) {
    
    run_mc_selection(
      run_id = RUN_SPECS$run_id[i],
      n_sims = RUN_SPECS$n_sims[i],
      scenario_keep = RUN_SPECS$scenario_keep[i],
      seed = RUN_SPECS$seed[i],
      weights_dt = weights_dt
    )
  })
)

# Validation ==================================================================

cat("\nFinished all MC selection runs.\n")

cat("\nRun summary:\n")
print(run_summary)