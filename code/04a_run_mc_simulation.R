# ============================================================================
# Generate Monte Carlo dataset selections.
#
# TWO MC PRODUCTS ARE CREATED:
#
# 1. SENSITIVITY ANALYSIS — current twc_change paper
#
#    100 simulations for EVERY weighting scenario.
#
#    For each simulation x region x biome, the SAME uniform random draw u is
#    passed through every scenario's dataset probability distribution.
#
#    Therefore scenario differences in selected datasets arise only from
#    differences in the scenario weights, not from independent MC noise.
#
#
# 2. CHANGE ENSEMBLE — main_dataset for twc change analysis
#
#    1000 simulations using ONLY the "change" weighting scenario.
#
#    This is the production MC dataset-selection ensemble for the other paper.
#    Only mc_selection_change.Rds is saved for this run.
#
#
# NESTED RANDOM DESIGN
# --------------------
#
# A single 1000-member random stream is generated first.
#
#   simulations 1:100
#       -> used for the all-scenario sensitivity analysis
#
#   simulations 1:1000
#       -> used for the change-only production ensemble
#
# Consequently, the "change" selections in the 100-member sensitivity ensemble
# are exactly simulations 1:100 of mc_selection_change.
#
#
# Example paired sensitivity draw:
#
#   u = 0.18
#
#   base   -> ERA5
#   change -> TERRA
#
# The shared-u design allows later sensitivity analysis to identify changes in
# dataset provenance caused specifically by the weighting scenario.
#
#
# Outputs for sensitivity analysis:
#
#   weight_cdf_scenarios.Rds
#   mc_random_region_biome_scenarios.Rds
#   mc_selection_scenarios.Rds
#   mc_selection_matrix_scenarios.Rds
#
# Output for main_dataset / other paper:
#
#   mc_selection_change.Rds
#
# No separate CDF, random-number table, or selection matrix is saved for the
# 1000-member change run because mc_selection_change already contains the
# selected dataset and its corresponding random draw u.
# ============================================================================


# Libraries ==================================================================

source("code/_source.R")


# Inputs ======================================================================

weights_dt <- readRDS(
  file.path(
    PATH_OUTPUT_OUTPUT,
    "weights_region_biome.Rds"
  )
)


# Constants ===================================================================

N_SIMS_SENSITIVITY <- 100L
N_SIMS_CHANGE      <- 1000L

MC_SEED <- 1979L

CHANGE_SCENARIO <- "change"


# Functions ===================================================================

make_weight_cdf <- function(weights_dt) {
  
  weights_region <- as.data.table(
    copy(weights_dt)
  )
  
  
  # Required columns ----------------------------------------------------------
  
  required_cols <- c(
    "scenario",
    "region",
    "biome",
    "dataset",
    "w_region_biome"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(weights_region)
  )
  
  if (length(missing_cols) > 0L) {
    
    stop(
      "weights_dt is missing required columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  
  # Standardise grouping variables -------------------------------------------
  
  weights_region[
    ,
    `:=`(
      scenario = as.character(scenario),
      region   = as.character(region),
      biome    = as.character(biome),
      dataset  = as.character(dataset)
    )
  ]
  
  
  # Validate probabilities ----------------------------------------------------
  
  if (
    any(
      !is.finite(weights_region$w_region_biome) |
      weights_region$w_region_biome < 0
    )
  ) {
    
    stop(
      "Region-biome weights contain non-finite or negative values."
    )
  }
  
  
  # There must be exactly one probability for each
  # scenario x region x biome x dataset.
  
  duplicates <- weights_region[
    ,
    .N,
    by = .(
      scenario,
      region,
      biome,
      dataset
    )
  ][
    N != 1L
  ]
  
  if (nrow(duplicates) > 0L) {
    
    print(duplicates)
    
    stop(
      "Duplicate scenario x region x biome x dataset weights found."
    )
  }
  
  
  # Region-biome probabilities already sum to one upstream.
  #
  # Do NOT renormalise here: failure should reveal an upstream problem rather
  # than silently modifying the intended probabilities.
  
  weight_check <- weights_region[
    ,
    .(
      w_sum = sum(w_region_biome)
    ),
    by = .(
      scenario,
      region,
      biome
    )
  ]
  
  bad_weights <- weight_check[
    abs(w_sum - 1) > 1e-6
  ]
  
  if (nrow(bad_weights) > 0L) {
    
    print(bad_weights)
    
    stop(
      "Some scenario x region x biome weights do not sum to 1."
    )
  }
  
  
  # Check region-biome support across scenarios -------------------------------
  
  n_scenarios_total <- uniqueN(
    weights_region$scenario
  )
  
  support_check <- unique(
    weights_region[
      ,
      .(
        scenario,
        region,
        biome
      )
    ]
  )[
    ,
    .(
      n_scenarios = uniqueN(scenario)
    ),
    by = .(
      region,
      biome
    )
  ][
    n_scenarios != n_scenarios_total
  ]
  
  if (nrow(support_check) > 0L) {
    
    print(support_check)
    
    stop(
      "Some region-biome units are missing from one or more scenarios."
    )
  }
  
  
  # Fix dataset ordering globally --------------------------------------------
  #
  # Common random numbers are most interpretable when every scenario constructs
  # its categorical CDF using the same deterministic dataset ordering.
  
  dataset_levels <- sort(
    unique(
      weights_region$dataset
    )
  )
  
  weights_region[
    ,
    dataset_order := match(
      dataset,
      dataset_levels
    )
  ]
  
  setorder(
    weights_region,
    scenario,
    region,
    biome,
    dataset_order
  )
  
  
  # Construct cumulative probability intervals -------------------------------
  
  weight_cdf <- weights_region[
    ,
    {
      
      p_high <- cumsum(
        w_region_biome
      )
      
      # Force the final interval boundary to exactly one to remove numerical
      # floating-point drift.
      
      p_high[.N] <- 1
      
      p_low <- c(
        0,
        p_high[-.N]
      )
      
      .(
        dataset = dataset,
        w_region_biome = w_region_biome,
        p_low = p_low,
        p_high = p_high
      )
    },
    by = .(
      scenario,
      region,
      biome
    )
  ]
  
  
  # Validate CDF boundaries ---------------------------------------------------
  
  cdf_check <- weight_cdf[
    ,
    .(
      first_low = first(p_low),
      last_high = last(p_high)
    ),
    by = .(
      scenario,
      region,
      biome
    )
  ][
    abs(first_low) > 1e-12 |
      abs(last_high - 1) > 1e-12
  ]
  
  if (nrow(cdf_check) > 0L) {
    
    print(cdf_check)
    
    stop(
      "Invalid cumulative probability intervals."
    )
  }
  
  
  setkey(
    weight_cdf,
    scenario,
    region,
    biome,
    dataset
  )
  
  weight_cdf
}


make_mc_random <- function(
    weight_cdf,
    n_sims,
    seed
) {
  
  set.seed(seed)
  
  
  # There is deliberately NO scenario dimension in this table.
  #
  # One random draw u is generated for every:
  #
  #   simulation x region x biome
  #
  # The same u can therefore be passed through every scenario CDF.
  
  region_biome_draws <- unique(
    weight_cdf[
      ,
      .(
        region,
        biome
      )
    ]
  )
  
  setorder(
    region_biome_draws,
    region,
    biome
  )
  
  
  mc_random <- region_biome_draws[
    ,
    .(
      sim = seq_len(n_sims)
    ),
    by = .(
      region,
      biome
    )
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


make_mc_selection <- function(
    weight_cdf,
    mc_random
) {
  
  weight_cdf <- as.data.table(
    copy(weight_cdf)
  )
  
  mc_random <- as.data.table(
    copy(mc_random)
  )
  
  
  # Apply each shared random draw to every relevant scenario CDF.
  
  mc_selection <- weight_cdf[
    mc_random,
    on = .(
      region,
      biome
    ),
    allow.cartesian = TRUE
  ][
    u >= p_low &
      u < p_high,
    .(
      sim,
      scenario,
      region,
      biome,
      dataset,
      u
    )
  ]
  
  
  # Validate exactly one selected dataset -------------------------------------
  
  expected_selection <- merge(
    mc_random[
      ,
      .(
        sim,
        region,
        biome
      )
    ],
    unique(
      weight_cdf[
        ,
        .(
          scenario,
          region,
          biome
        )
      ]
    ),
    by = c(
      "region",
      "biome"
    ),
    allow.cartesian = TRUE
  )[
    ,
    .(
      sim,
      scenario,
      region,
      biome
    )
  ]
  
  
  selection_count <- mc_selection[
    ,
    .N,
    by = .(
      sim,
      scenario,
      region,
      biome
    )
  ]
  
  
  selection_check <- merge(
    expected_selection,
    selection_count,
    by = c(
      "sim",
      "scenario",
      "region",
      "biome"
    ),
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
      paste0(
        "Some sim x scenario x region x biome combinations ",
        "do not have exactly one selected dataset."
      )
    )
  }
  
  
  # Validate common random numbers --------------------------------------------
  #
  # When multiple scenarios are present, every scenario must carry exactly the
  # same u for the same simulation x region x biome.
  
  shared_u_check <- mc_selection[
    ,
    .(
      n_u = uniqueN(u)
    ),
    by = .(
      sim,
      region,
      biome
    )
  ][
    n_u != 1L
  ]
  
  if (nrow(shared_u_check) > 0L) {
    
    print(shared_u_check)
    
    stop(
      "Scenarios do not share identical random draws."
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


# Weight CDFs =================================================================

# Construct the scenario CDFs once.

weight_cdf <- make_weight_cdf(
  weights_dt
)


# Confirm that the change scenario required by the other paper exists.

if (!(CHANGE_SCENARIO %in% weight_cdf$scenario)) {
  
  stop(
    paste0(
      "Required change scenario '",
      CHANGE_SCENARIO,
      "' is not present."
    )
  )
}


# Isolate the change CDF.
#
# This is used to define the spatial support of the 1000-member master random
# stream and later to generate the production ensemble for the other paper.

weight_cdf_change <- weight_cdf[
  scenario == CHANGE_SCENARIO
]


# Master random stream =========================================================
#
# Generate 1000 random realizations ONCE.
#
# This stream underlies BOTH analyses:
#
#   sims 1:100    -> all-scenario sensitivity analysis
#   sims 1:1000   -> change-only ensemble for main_dataset
#
# This nesting guarantees that the first 100 change realizations are identical
# between the two products.

mc_random_master <- make_mc_random(
  weight_cdf = weight_cdf_change,
  n_sims = N_SIMS_CHANGE,
  seed = MC_SEED
)


# ============================================================================
# PRODUCT 1 — SENSITIVITY ANALYSIS FOR CURRENT twc_change PAPER
# ============================================================================


# Use only the first 100 members of the master random stream.

mc_random_scenarios <- mc_random_master[
  sim <= N_SIMS_SENSITIVITY
]


# Apply those same draws to every weighting scenario.

mc_selection_scenarios <- make_mc_selection(
  weight_cdf = weight_cdf,
  mc_random = mc_random_scenarios
)


# Wide paired-selection matrix.
#
# One row:
#
#   simulation x region x biome x common u
#
# One column per weighting scenario.
#
# This is particularly useful for diagnosing when alternative weighting
# assumptions cause the selected source dataset to change.

mc_selection_matrix_scenarios <- dcast(
  mc_selection_scenarios,
  sim + region + biome + u ~ scenario,
  value.var = "dataset"
)

setorder(
  mc_selection_matrix_scenarios,
  sim,
  region,
  biome
)


# Outputs =====================================================================

# Save sensitivity-analysis products.

saveRDS(
  weight_cdf,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "weight_cdf_scenarios.Rds"
  )
)

saveRDS(
  mc_random_scenarios,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_random_region_biome_scenarios.Rds"
  )
)

saveRDS(
  mc_selection_scenarios,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_selection_scenarios.Rds"
  )
)

saveRDS(
  mc_selection_matrix_scenarios,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_selection_matrix_scenarios.Rds"
  )
)


# ============================================================================
# PRODUCT 2 — CHANGE ENSEMBLE FOR main_dataset / OTHER PAPER
# ============================================================================
#
# This is NOT an additional sensitivity scenario for the present paper.
#
# It is the 1000-member dataset-selection ensemble required by the separate
# main_dataset analysis, whose preferred weighting is the "change" scenario.
#
# Only mc_selection_change is saved.
#
# No weight-CDF file, random-number file, or wide selection matrix is required
# for that paper. The selected dataset and underlying u are both retained in
# mc_selection_change itself.
# ============================================================================


mc_selection_change <- make_mc_selection(
  weight_cdf = weight_cdf_change,
  mc_random = mc_random_master
)


# Remove the scenario column because every row is necessarily "change".
#
# The file itself is explicitly named mc_selection_change, so retaining a
# constant scenario column adds no information.

mc_selection_change[
  ,
  scenario := NULL
]


# Save ONLY this product for the other paper.

saveRDS(
  mc_selection_change,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_selection_change.Rds"
  )
)


# Validation ==================================================================

# 1. Sensitivity ensemble dimensions ------------------------------------------

stopifnot(
  uniqueN(
    mc_selection_scenarios$sim
  ) == N_SIMS_SENSITIVITY
)


stopifnot(
  uniqueN(
    mc_selection_scenarios$scenario
  ) == uniqueN(
    weight_cdf$scenario
  )
)


# 2. Change ensemble contains exactly 1000 simulations ------------------------

stopifnot(
  uniqueN(
    mc_selection_change$sim
  ) == N_SIMS_CHANGE
)


# 3. Verify nesting explicitly -------------------------------------------------
#
# The first 100 change realizations in the 1000-member production ensemble
# must exactly reproduce the change selections in the sensitivity ensemble.

change_sensitivity <- mc_selection_scenarios[
  scenario == CHANGE_SCENARIO,
  .(
    sim,
    region,
    biome,
    dataset,
    u
  )
]

change_first_1000 <- mc_selection_change[
  sim <= N_SIMS_SENSITIVITY,
  .(
    sim,
    region,
    biome,
    dataset,
    u
  )
]

setorder(
  change_sensitivity,
  sim,
  region,
  biome
)

setorder(
  change_first_1000,
  sim,
  region,
  biome
)

# Check dimensions first.
stopifnot(
  nrow(change_sensitivity) ==
    nrow(change_first_1000)
)

# Check identifiers.
stopifnot(
  all(
    change_sensitivity$sim ==
      change_first_1000$sim
  ),
  all(
    change_sensitivity$region ==
      change_first_1000$region
  ),
  all(
    change_sensitivity$biome ==
      change_first_1000$biome
  )
)

# Check selected datasets.
stopifnot(
  all(
    change_sensitivity$dataset ==
      change_first_1000$dataset
  )
)

# Check that the underlying common random numbers are exactly the same.
stopifnot(
  all(
    change_sensitivity$u ==
      change_first_1000$u
  )
)

message(
  paste0(
    "Nested-design check passed: change simulations 1:",
    N_SIMS_SENSITIVITY,
    " are identical in both MC products."
  )
)

# 4. Every scenario has the same number of sensitivity selections ------------

selection_counts <- mc_selection_scenarios[
  ,
  .(
    n_selections = .N
  ),
  by = scenario
]

stopifnot(
  uniqueN(
    selection_counts$n_selections
  ) == 1L
)


# Summary =====================================================================

cat(
  "\nFinished Monte Carlo dataset selection.\n"
)

cat(
  "\nSensitivity analysis:\n",
  "  simulations: ",
  N_SIMS_SENSITIVITY,
  "\n  scenarios: ",
  uniqueN(mc_selection_scenarios$scenario),
  "\n",
  sep = ""
)

cat(
  "\nmain_dataset / other paper:\n",
  "  scenario: ",
  CHANGE_SCENARIO,
  "\n  simulations: ",
  N_SIMS_CHANGE,
  "\n",
  sep = ""
)

cat(
  "\nNested-design check passed:\n",
  "  change simulations 1:",
  N_SIMS_SENSITIVITY,
  " are identical in both products.\n",
  sep = ""
)

cat(
  "\nSaved sensitivity outputs:\n",
  "  weight_cdf_scenarios.Rds\n",
  "  mc_random_region_biome_scenarios.Rds\n",
  "  mc_selection_scenarios.Rds\n",
  "  mc_selection_matrix_scenarios.Rds\n",
  sep = ""
)

cat(
  "\nSaved main_dataset output ONLY:\n",
  "  mc_selection_change.Rds\n",
  sep = ""
)
