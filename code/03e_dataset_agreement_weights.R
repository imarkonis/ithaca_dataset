# ============================================================================
# Dataset agreement weights for twc_change under TEN weighting scenarios.
#
# The scenarios explore TWO conceptually different dimensions:
#
#   AXIS 1 — WHAT evidence is emphasised within the hierarchical weighting:
#
#     base
#       Balanced reference case:
#       climatology : change = 50 : 50
#       mean : SD within climatology = 50 : 50
#       significance : slope within change = 50 : 50
#       precipitation : evaporation = 50 : 50
#
#     change
#       Change-dominant setup used for the main_dataset analysis:
#       climatology : change = 25 : 75
#       significance : slope within change = 75 : 25
#       mean : SD within climatology remains 50 : 50
#       precipitation : evaporation remains 50 : 50
#
#     trend_dominant
#       Pure-change sensitivity endpoint:
#       climatology : change = 0 : 100
#       significance : slope within change = 75 : 25
#
#     clim_dominant
#       Pure-climatology sensitivity endpoint:
#       climatology : change = 100 : 0
#       mean : SD within climatology = 50 : 50
#
#     evap_dominant
#       Uses only evaporation agreement in the final P/E combination.
#
#     prec_dominant
#       Uses only precipitation agreement in the final P/E combination.
#
#   AXIS 2 — HOW dataset performance is converted to sampling probability:
#
#     inverse_loss
#       Better agreement (smaller loss) -> larger probability.
#
#     rank_linear / rank_exp
#       Use only PRE-COMPUTED mean and slope ranks, with equal mean/slope
#       importance. SD and significance are deliberately omitted.
#
#     inverted
#       Adversarial mirror of the base case: poorer agreement receives more
#       weight. Used as a stress test rather than a plausible weighting.
#
#     neutral
#       Exact 1/n probability over all physics-passing datasets.
#
# Overall scenarios:
#   6 inverse-loss share scenarios:
#     base, change, trend_dominant, clim_dominant,
#     evap_dominant, prec_dominant
#
#   3 alternative weighting-method scenarios:
#     rank_linear, rank_exp, inverted
#
#   1 neutral scenario:
#     neutral
#
# Pipeline:
#   1) apply the physics filter
#   2) transform individual agreement metrics to per-cell probabilities
#   3) combine probabilities through the hierarchy, or use a direct rank/uniform path
#   4) evaluate all ten scenarios
#   5) save tidy weights, detailed base weights, and diagnostics
#
# All six bias metrics are non-negative loss magnitudes. Smaller values mean
# closer agreement with the leave-out reference statistics.
# ============================================================================


# Libraries ==================================================================

source("code/_source.R")


# Inputs ======================================================================

dataset_ranks <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "dataset_ranks.Rds")
)


# Constants ===================================================================

# --- Hierarchical blend shares ------------------------------------------------
#
# Every *_SHARE parameter is the share assigned to the FIRST component in the
# corresponding pair in COMBINE_SPEC. The second component therefore receives
# 1 - share.

SHARES_BASE <- list(

  # relative agreement in the climatological mean vs relative agreement in SD.
  PREC_CLIM_MEAN_SHARE = 0.5,

  # relative agreement in the climatological mean vs relative agreement in SD.
  EVAP_CLIM_MEAN_SHARE = 0.5,

  # agreement on trend significance vs agreement on Sen-slope magnitude.
  PREC_TREND_SIG_SHARE = 0.5,

  # agreement on trend significance vs agreement on Sen-slope magnitude.
  EVAP_TREND_SIG_SHARE = 0.5,

  # climatological behaviour vs change/trend behaviour.
  PREC_CLIM_SHARE = 0.5,

  # climatological behaviour vs change/trend behaviour.
  EVAP_CLIM_SHARE = 0.5,

  # precipitation evidence vs evaporation evidence.
  FINAL_PREC_SHARE = 0.5
)

with_shares <- function(...) {
  modifyList(SHARES_BASE, list(...))
}


# --- Change-dominant scientific setup ----------------------------------------
#

CHANGE_TREND_SHARE <- 0.75
CHANGE_SIG_SHARE   <- 0.75


# --- Rank-scenario constants --------------------------------------------------
#
# rank_linear / rank_exp intentionally form a simpler methodological foil.
#
# They use only:
#   precipitation mean rank
#   evaporation mean rank
#   precipitation slope rank
#   evaporation slope rank
#
# P and E are first averaged within mean and slope. The resulting mean and
# slope ranks are then combined using RANK_SLOPE_SHARE.
#
# SD ranks and significance agreement are deliberately excluded. Therefore
# these scenarios test the effect of rank-based weighting rather than trying
# to reproduce the full hierarchical weighting.

RANK_SLOPE_SHARE <- 0.5
# 0.5 = equal contribution of slope/change rank and climatological-mean rank.

RANK_EXP_BASE <- 0.5
# Exponential rank weighting:
# each one-rank deterioration multiplies the score by 0.5.
# Smaller values approach winner-take-all weighting.

RANK_N <- uniqueN(dataset_ranks[["dataset"]])
# Fixed rank ladder across the five core candidate datasets.

# Rank column names as stored in dataset_ranks.
# Note the upstream naming convention *_rank_slope rather than *_slope_rank.

RANK_COLS <- c(
  prec_mean  = "prec_mean_rank",
  prec_slope = "prec_rank_slope",
  evap_mean  = "evap_mean_rank",
  evap_slope = "evap_rank_slope"
)

# Weight scenarios ============================================================

WEIGHT_SCENARIOS <- list(
  
  # Balanced primary scenario for twc_change.
  base = list(
    transform = "inverse_loss",
    shares = SHARES_BASE
  ),
  
  # Change-dominant scenario:
  #
  #   climatology : change = 25 : 75
  #   significance : slope = 75 : 25 within change
  #
  # Effective contributions within P or E when all components are available:
  #
  #   climatological mean = 0.25 * 0.50 = 12.50%
  #   climatological SD   = 0.25 * 0.50 = 12.50%
  #   trend significance  = 0.75 * 0.75 = 56.25%
  #   trend slope         = 0.75 * 0.25 = 18.75%
  #
  change = list(
    transform = "inverse_loss",
    shares = with_shares(
      PREC_CLIM_SHARE      = 1 - CHANGE_TREND_SHARE,
      EVAP_CLIM_SHARE      = 1 - CHANGE_TREND_SHARE,
      PREC_TREND_SIG_SHARE = CHANGE_SIG_SHARE,
      EVAP_TREND_SIG_SHARE = CHANGE_SIG_SHARE
    )
  ),
  
  # Pure-change endpoint.
  #
  # Climatology contributes nothing, while the definition of "change" remains
  # identical to the change scenario: 75% significance + 25% slope.
  trend_dominant = list(
    transform = "inverse_loss",
    shares = with_shares(
      PREC_CLIM_SHARE      = 0,
      EVAP_CLIM_SHARE      = 0,
      PREC_TREND_SIG_SHARE = CHANGE_SIG_SHARE,
      EVAP_TREND_SIG_SHARE = CHANGE_SIG_SHARE
    )
  ),
  
  # Pure-climatology endpoint.
  #
  # Change contributes nothing. Mean and SD remain equally weighted.
  clim_dominant = list(
    transform = "inverse_loss",
    shares = with_shares(
      PREC_CLIM_SHARE = 1,
      EVAP_CLIM_SHARE = 1
    )
  ),
  
  # Evaporation-only endpoint of the final P/E combination.
  #
  # The internal evaporation hierarchy remains the balanced base hierarchy.
  evap_dominant = list(
    transform = "inverse_loss",
    shares = with_shares(
      FINAL_PREC_SHARE = 0
    )
  ),
  
  # Precipitation-only endpoint of the final P/E combination.
  #
  # The internal precipitation hierarchy remains the balanced base hierarchy.
  prec_dominant = list(
    transform = "inverse_loss",
    shares = with_shares(
      FINAL_PREC_SHARE = 1
    )
  ),
  
  # Rank-based methodological sensitivity:
  # fixed linear mapping from combined pre-computed rank to score.
  rank_linear = list(
    transform = "rank_linear"
  ),
  
  # Rank-based methodological sensitivity:
  # exponentially decreasing score with worsening combined rank.
  rank_exp = list(
    transform = "rank_exp"
  ),
  
  # Adversarial sensitivity:
  # reverses the base performance preference while retaining balanced shares.
  inverted = list(
    transform = "inverted",
    shares = SHARES_BASE
  ),
  
  # No performance-based preference:
  # all physics-passing datasets receive exactly 1/n.
  neutral = list(
    transform = "uniform"
  )
)

# Weight definitions ==========================================================

LOSS_WEIGHT_MAP <- c(
  weight_prec_mean  = "prec_mean_bias",
  weight_prec_sd    = "prec_sd_bias",
  weight_evap_mean  = "evap_mean_bias",
  weight_evap_sd    = "evap_sd_bias",
  weight_prec_slope = "prec_bias_slope",
  weight_evap_slope = "evap_bias_slope"
)


# Hierarchical combination specification =====================================

COMBINE_SPEC <- list(
  
  # Precipitation climatology:
  # mean agreement vs SD agreement.
  list(
    out   = "weight_prec_clim",
    x     = "weight_prec_mean",
    y     = "weight_prec_sd",
    share = "PREC_CLIM_MEAN_SHARE"
  ),
  
  # Evaporation climatology:
  # mean agreement vs SD agreement.
  list(
    out   = "weight_evap_clim",
    x     = "weight_evap_mean",
    y     = "weight_evap_sd",
    share = "EVAP_CLIM_MEAN_SHARE"
  ),
  
  # Precipitation change:
  # significance agreement vs Sen-slope agreement.
  list(
    out   = "weight_prec_trend",
    x     = "weight_prec_sig",
    y     = "weight_prec_slope",
    share = "PREC_TREND_SIG_SHARE"
  ),
  
  # Evaporation change:
  # significance agreement vs Sen-slope agreement.
  list(
    out   = "weight_evap_trend",
    x     = "weight_evap_sig",
    y     = "weight_evap_slope",
    share = "EVAP_TREND_SIG_SHARE"
  ),
  
  # Overall precipitation:
  # climatological agreement vs change agreement.
  list(
    out   = "weight_prec",
    x     = "weight_prec_clim",
    y     = "weight_prec_trend",
    share = "PREC_CLIM_SHARE"
  ),
  
  # Overall evaporation:
  # climatological agreement vs change agreement.
  list(
    out   = "weight_evap",
    x     = "weight_evap_clim",
    y     = "weight_evap_trend",
    share = "EVAP_CLIM_SHARE"
  ),
  
  # Final dataset probability:
  # precipitation agreement vs evaporation agreement.
  list(
    out   = "weight",
    x     = "weight_prec",
    y     = "weight_evap",
    share = "FINAL_PREC_SHARE"
  )
)

# Required / output columns ===================================================

REQUIRED_COLS <- c(
  "lon",
  "lat",
  "dataset",
  "pe_ratio_check",
  "n_below_pet",
  unname(LOSS_WEIGHT_MAP),
  unname(RANK_COLS),
  "prec_check_significance",
  "prec_check_non_significance",
  "evap_check_significance",
  "evap_check_non_significance"
)

BASE_OUTPUT_COLS <- c(
  "lon",
  "lat",
  "dataset",
  names(LOSS_WEIGHT_MAP),
  "weight_prec_sig",
  "weight_evap_sig",
  "weight_prec_clim",
  "weight_evap_clim",
  "weight_prec_trend",
  "weight_evap_trend",
  "weight_prec",
  "weight_evap",
  "weight"
)


# Functions ===================================================================

validate_input <- function(dt) {
  
  missing <- setdiff(REQUIRED_COLS, names(dt))
  
  if (length(missing)) {
    stop(
      "Missing required columns: ",
      toString(missing)
    )
  }
  
  # Fail loudly if any supposed loss metric contains negative values.
  #
  # All six metrics are expected to be non-negative magnitudes. A signed slope
  # bias here would invalidate inverse-loss weighting and previously caused a
  # silent weighting error.
  
  neg <- names(LOSS_WEIGHT_MAP)[
    vapply(
      unname(LOSS_WEIGHT_MAP),
      \(col) any(dt[[col]] < 0, na.rm = TRUE),
      logical(1)
    )
  ]
  
  if (length(neg)) {
    stop(
      "Negative values in loss metric(s): ",
      toString(LOSS_WEIGHT_MAP[neg])
    )
  }
  
  invisible(TRUE)
}


normalize_prob <- function(x) {
  
  out <- rep(NA_real_, length(x))
  
  ok <- is.finite(x) & x > 0
  
  if (!any(ok)) {
    return(out)
  }
  
  s <- sum(x[ok])
  
  out[ok] <- if (is.finite(s) && s > 0) {
    x[ok] / s
  } else {
    1 / sum(ok)
  }
  
  out
}


# Convex combination:
#
#   a * x + (1 - a) * y
#
# NA-aware:
# if one side is unavailable, its nominal share is removed and the available
# side is renormalised to the full contribution.
#
# At exact endpoints (a = 0 or a = 1), the zero-weight component cannot rescue
# a missing selected component. Thus clim_dominant and trend_dominant remain
# genuine 100/0 sensitivity endpoints.

weighted_pair_mean <- function(x, y, a) {
  
  ox <- is.finite(x)
  oy <- is.finite(y)
  
  num <- fifelse(ox, a * x, 0) +
    fifelse(oy, (1 - a) * y, 0)
  
  den <- fifelse(ox, a, 0) +
    fifelse(oy, 1 - a, 0)
  
  fifelse(
    den > 0,
    num / den,
    NA_real_
  )
}


# --- Hierarchical performance transforms -------------------------------------

# Convert a non-negative loss into a sampling probability.
#
# Smaller loss -> larger score -> larger probability.
#
# This is scale-sensitive: when losses are nearly equal, weights approach
# uniformity; a near-zero loss can become strongly dominant.

loss_to_prob <- function(loss, eps = 1e-6) {
  
  q <- rep(NA_real_, length(loss))
  ok <- is.finite(loss)
  
  q[ok] <- 1 / (eps + loss[ok])
  
  normalize_prob(q)
}


# Adversarial mirror of inverse loss.
#
# Larger loss -> larger probability.
#
# Used only as a stress test. If all datasets perform similarly, the result
# naturally approaches uniform weighting.

inverted_loss_to_prob <- function(loss, eps = 1e-6) {
  
  q <- rep(NA_real_, length(loss))
  ok <- is.finite(loss)
  
  q[ok] <- eps + loss[ok]
  
  normalize_prob(q)
}


# Convert significance/non-significance agreement to probability.
#
# Agreement receives raw score 1.
# Disagreement receives raw score "fail" (default 0.10).
#
# The resulting scores are normalised across datasets within each grid cell.

significance_to_prob <- function(
    sig,
    non_sig,
    fail = 0.10
) {
  
  raw <- fcoalesce(
    fifelse(
      !is.na(sig),
      fifelse(sig, 1, fail),
      NA_real_
    ),
    fifelse(
      !is.na(non_sig),
      fifelse(non_sig, 1, fail),
      NA_real_
    )
  )
  
  normalize_prob(raw)
}


# Adversarial significance transform.
#
# Agreement receives the low score "fail"; disagreement receives 1.

inverted_significance_to_prob <- function(
    sig,
    non_sig,
    fail = 0.10
) {
  
  raw <- fcoalesce(
    fifelse(
      !is.na(sig),
      fifelse(sig, fail, 1),
      NA_real_
    ),
    fifelse(
      !is.na(non_sig),
      fifelse(non_sig, fail, 1),
      NA_real_
    )
  )
  
  normalize_prob(raw)
}


# Transform dispatch used by hierarchy-based scenarios only.

TRANSFORMS <- list(
  inverse_loss = list(
    loss = loss_to_prob,
    sig  = significance_to_prob
  ),
  inverted = list(
    loss = inverted_loss_to_prob,
    sig  = inverted_significance_to_prob
  )
)


# Combine two already-transformed per-cell weight components and re-normalise
# the resulting dataset weights to sum to one within each grid cell.

combine_pair_prob <- function(
    dt,
    out,
    x,
    y,
    a
) {
  
  dt[, (out) := weighted_pair_mean(
    get(x),
    get(y),
    a
  )]
  
  dt[, (out) := normalize_prob(get(out)),
     by = .(lon, lat)]
  
  invisible(dt)
}


# Apply only the scenario-independent physical admissibility filter.
#
# Missing significance information is NOT propagated blindly through the
# hierarchy. weighted_pair_mean() handles it NA-aware: if significance is
# missing but slope is available, the change component falls back to slope.
#
# Rank scenarios are unaffected because they never use significance.

prep_raw <- function(dt) {
  
  validate_input(dt)
  
  # n_below_pet > 6 retains cells where AET < PET for a strict majority:
  # at least 7 of the 12 PET combinations.
  
  copy(dt)[
    pe_ratio_check == TRUE &
      n_below_pet > 6
  ][]
}


# Transform raw losses and significance agreement into primitive per-cell
# probabilities.
#
# This depends only on the transform, not on scenario shares, so each distinct
# transform can be computed once and reused.

apply_transform <- function(
    raw,
    tr
) {
  
  dt <- copy(raw)
  
  for (out_col in names(LOSS_WEIGHT_MAP)) {
    
    dt[, (out_col) := tr$loss(
      get(LOSS_WEIGHT_MAP[[out_col]])
    ),
    by = .(lon, lat)]
  }
  
  dt[, weight_prec_sig := tr$sig(
    prec_check_significance,
    prec_check_non_significance
  ),
  by = .(lon, lat)]
  
  dt[, weight_evap_sig := tr$sig(
    evap_check_significance,
    evap_check_non_significance
  ),
  by = .(lon, lat)]
  
  dt[]
}


# Combine primitive probabilities through the hierarchy using one scenario's
# scientific blend shares.

compute_weights <- function(
    components,
    shares
) {
  
  dt <- copy(components)
  
  for (s in COMBINE_SPEC) {
    
    combine_pair_prob(
      dt,
      s$out,
      s$x,
      s$y,
      shares[[s$share]]
    )
  }
  
  dt[]
}


# DIRECT PATH 1 — rank scenarios ==============================================
#
# Use only the four PRE-COMPUTED ranks.
#
# Step 1:
#   average precipitation and evaporation ranks within climatological mean.
#
# Step 2:
#   average precipitation and evaporation ranks within slope/change.
#
# Step 3:
#   blend slope rank vs mean rank using RANK_SLOPE_SHARE.
#
# Step 4:
#   convert the combined rank R to either a linear or exponential positive score.
#
# Step 5:
#   normalise scores across datasets within each grid cell.
#
# Lower ranks are better.
#
# No re-ranking occurs here, so any near-tie spatial pattern originates in the
# pre-computed ranks rather than being introduced by this script.
#
# SD and significance are intentionally omitted.

rank_weights <- function(
    raw,
    kind = c("linear", "exp"),
    w_slope = RANK_SLOPE_SHARE,
    rank_n = RANK_N,
    exp_base = RANK_EXP_BASE
) {
  
  kind <- match.arg(kind)
  
  dt <- copy(raw)
  
  dt[, rank_mean := weighted_pair_mean(
    get(RANK_COLS[["prec_mean"]]),
    get(RANK_COLS[["evap_mean"]]),
    0.5
  )]
  
  dt[, rank_slope := weighted_pair_mean(
    get(RANK_COLS[["prec_slope"]]),
    get(RANK_COLS[["evap_slope"]]),
    0.5
  )]
  
  dt[, R := weighted_pair_mean(
    rank_slope,
    rank_mean,
    w_slope
  )]
  
  dt[, score := if (kind == "linear") {
    rank_n + 1 - R
  } else {
    exp_base ^ (R - 1)
  }]
  
  dt[, weight := normalize_prob(score),
     by = .(lon, lat)]
  
  dt[
    is.finite(weight),
    .(lon, lat, dataset, weight)
  ][]
}


# DIRECT PATH 2 — neutral ======================================================
#
# Exact 1/n weighting over physics-passing datasets in each grid cell.
#
# This bypasses the hierarchy completely so that neutral remains provably flat
# and can serve as the unbiased substrate for provenance sensitivity analyses.

uniform_weights <- function(raw) {
  
  out <- copy(raw)[
    ,
    .(lon, lat, dataset)
  ]
  
  out[, weight := 1 / .N,
      by = .(lon, lat)]
  
  out[]
}


# Run all weighting scenarios ==================================================

run_weight_scenarios <- function(
    raw,
    scenarios,
    transforms = TRANSFORMS
) {
  
  # Cache transformed hierarchy components because base/change/clim/trend/P/E
  # scenarios all use the same inverse-loss primitives and differ only in how
  # those primitives are combined.
  
  comp_cache <- list()
  
  rbindlist(
    lapply(
      names(scenarios),
      function(nm) {
        
        sc <- scenarios[[nm]]
        
        message(
          "Running scenario: ",
          nm,
          "  (transform = ",
          sc$transform,
          ")"
        )
        
        out <- switch(
          sc$transform,
          
          uniform = uniform_weights(raw),
          
          rank_linear = rank_weights(
            raw,
            "linear"
          ),
          
          rank_exp = rank_weights(
            raw,
            "exp"
          ),
          
          {
            # Hierarchy-based paths:
            # inverse_loss and inverted.
            
            if (is.null(comp_cache[[sc$transform]])) {
              
              comp_cache[[sc$transform]] <<- apply_transform(
                raw,
                transforms[[sc$transform]]
              )
            }
            
            compute_weights(
              comp_cache[[sc$transform]],
              sc$shares
            )
          }
        )
        
        out[, scenario := nm][]
        
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
}


# Diagnostics =================================================================
#
# eff_n is the inverse-Simpson effective number of datasets:
#
#   eff_n = 1 / sum(weight^2)
#
# It measures concentration/diversity of the sampling distribution, NOT
# confidence in the datasets.
#
# For neutral weighting:
#   eff_n == n_candidates exactly.

make_diagnostics <- function(dt) {
  
  dt[
    is.finite(weight),
    {
      
      top <- which.max(weight)
      
      .(
        n_candidates = .N,
        dataset_top = dataset[top],
        weight_top = weight[top],
        eff_n = 1 / sum(weight^2)
      )
    },
    by = .(scenario, lon, lat)
  ]
}


# Analysis ====================================================================

weights_all <- run_weight_scenarios(
  prep_raw(dataset_ranks),
  WEIGHT_SCENARIOS
)

weights_tidy <- weights_all[
  ,
  .(
    lon,
    lat,
    dataset,
    weight,
    scenario
  )
]

weights_base <- weights_all[
  scenario == "base",
  ..BASE_OUTPUT_COLS
]

weight_diag <- make_diagnostics(
  weights_all
)


# Outputs =====================================================================

saveRDS(
  weights_tidy,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_weights.Rds"
  )
)

saveRDS(
  weights_base,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_weights_base_detailed.Rds"
  )
)

saveRDS(
  weight_diag,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_weight_diagnostics.Rds"
  )
)


# Validation ==================================================================

# 1. Every scenario must define a proper per-cell probability distribution.

sums <- weights_tidy[
  is.finite(weight),
  .(
    s = sum(weight)
  ),
  by = .(
    scenario,
    lon,
    lat
  )
]

stopifnot(
  all(
    abs(sums$s - 1) < 1e-6
  )
)


# 2. Neutral must be exactly uniform.
#
# Therefore its effective number of datasets must equal the actual number of
# candidate datasets in every cell.

stopifnot(
  weight_diag[
    scenario == "neutral",
    all(
      abs(eff_n - n_candidates) < 1e-9
    )
  ]
)


# 3. Concentration summary.
#
# Expected qualitative behaviour:
#
#   neutral     -> least concentrated
#   rank_linear -> relatively gentle rank discrimination
#   rank_exp    -> stronger rank discrimination
#
# inverse-loss and inverted concentration depends on how strongly candidate
# datasets differ within individual cells.

print(
  weight_diag[
    ,
    .(
      mean_eff_n = round(
        mean(eff_n),
        3
      ),
      mean_n = round(
        mean(n_candidates),
        3
      )
    ),
    by = scenario
  ][
    order(mean_eff_n)
  ]
)


# 4. Adversarial check.
#
# base and inverted reverse the preference supplied by the agreement metrics.
# Their top datasets should therefore agree relatively rarely except where only
# one candidate survives. Some additional agreement in multi-candidate cells is
# possible because of ties and the hierarchical combinations.

chk <- merge(
  weight_diag[
    scenario == "base",
    .(
      lon,
      lat,
      top_base = dataset_top
    )
  ],
  weight_diag[
    scenario == "inverted",
    .(
      lon,
      lat,
      top_inv = dataset_top
    )
  ],
  by = c(
    "lon",
    "lat"
  )
)

message(
  sprintf(
    paste0(
      "base vs inverted top-dataset agreement: %.3f ",
      "(single-candidate fraction = %.3f)"
    ),
    chk[, mean(top_base == top_inv)],
    weight_diag[
      scenario == "base",
      mean(n_candidates == 1)
    ]
  )
)


# 5. Mean sampling weight assigned to every dataset under each scenario.
#
# This is the average provenance budget from which each Monte Carlo world draws.

print(
  weights_tidy[
    ,
    .(
      mean_weight = mean(
        weight,
        na.rm = TRUE
      )
    ),
    by = .(
      scenario,
      dataset
    )
  ][
    order(
      scenario,
      -mean_weight
    )
  ]
)


# Optional diagnostic map ======================================================

if (interactive()) {
  
  library(ggplot2)
  library(maps)
  library(grid)
  
  show_scn <- "base"
  
  print(
    ggplot(
      weight_diag[
        scenario == show_scn
      ]
    ) +
      geom_tile(
        aes(
          lon,
          lat,
          fill = dataset_top
        )
      ) +
      borders(
        "world",
        colour = "grey20",
        linewidth = 0.2
      ) +
      coord_equal(
        expand = FALSE
      ) +
      labs(
        title = "Dominant dataset by grid cell",
        subtitle = paste(
          show_scn,
          "scenario"
        ),
        x = NULL,
        y = NULL,
        fill = "Dataset"
      ) +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(
          1.4,
          "cm"
        )
      )
  )
}


# Spot checks =================================================================
#
# Inspect two example cells across all ten scenarios.

lon_test_a <- 9.875
lon_test_b <- 69.875
lat_test   <- 35.125

weights_base[
  lon == lon_test_a &
    lat == lat_test
]

weights_base[
  lon == lon_test_b &
    lat == lat_test
]

dcast(
  weights_tidy[
    lon == lon_test_a &
      lat == lat_test
  ],
  lon + lat + dataset ~ scenario,
  value.var = "weight"
)

dcast(
  weights_tidy[
    lon == lon_test_b &
      lat == lat_test
  ],
  lon + lat + dataset ~ scenario,
  value.var = "weight"
)


# Candidate-count distribution ===============================================
#
# Number of datasets surviving the AET < PET majority criterion before the full
# pe_ratio_check filter is applied.

aa <- dataset_ranks[
  ,
  .(
    N = sum(
      n_below_pet > 6,
      na.rm = TRUE
    )
  ),
  by = .(
    lon,
    lat
  )
]

table(aa$N)