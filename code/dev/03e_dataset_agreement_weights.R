# ============================================================================
# Dataset agreement weights for twc_change under EIGHT weighting scenarios.
#
# Scenarios decompose on TWO orthogonal axes (cf. our scenario-design notes):
#   AXIS 1  what is weighted ......... the COMBINE_SPEC blend shares
#   AXIS 2  how performance maps ...... the loss/sig/rank -> probability TRANSFORM
#
#     inverse_loss x {base, clim, evap, prec shares}  -> 4 scenarios  (axis 1)
#     {rank_linear, rank_exp, inverted}               -> 3 scenarios  (axis 2)
#     uniform (neutral, the flat substrate)           -> 1 scenario
#
# AXIS-2 transforms (how a cell's performance becomes a sampling probability):
#   inverse_loss / inverted .... 1/(eps+loss) and its mirror, blended through
#                                COMBINE_SPEC over six bias components + significance.
#   rank_linear / rank_exp ..... PRE-COMPUTED ranks only (prec_mean, prec_slope,
#                                evap_mean, evap_slope; slopes at RANK_SLOPE_SHARE),
#                                averaged to one mean rank R, mapped linearly / exp.
#                                No re-ranking, no hierarchy, no sd, no significance.
#   uniform (neutral) .......... exact 1/n over surviving datasets.
#
# The four inverse_loss scenarios share the transform and differ only in shares;
# the three axis-2 scenarios are single-knob foils for the transform itself.
# trend_dominant dropped on purpose: clim<->trend is already spanned by
# base <-> clim_dominant, and discriminating power lives on the prec<->evap axis.
#
# Pipeline: 1 physics filter  2 per-cell transform  3 (hierarchy or direct)
#           4 eight scenarios  5 tidy weights + base detail + diagnostics
# Slope biases are non-negative magnitudes (fixed upstream); all six bias metrics
# are therefore treated identically as relative-bias losses.
# ============================================================================

source("source/twc_change.R")

dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, "dataset_ranks.Rds"))


# Constants ===================================================================

# --- AXIS 1: blend shares (inverse_loss + inverted) --------------------------
# base shares are the fixed point. clim/evap/prec are single-share deltas off it.
#
# !! FLAG: PREC/EVAP_CLIM_SHARE = 0.3 here (climate 0.3 / trend 0.7), matching the
#    methods table AND the deliberate clim_share = 0.3 choice. The script you sent
#    had 0.5 in `base` ONLY (evap/prec_dominant already used 0.3). 0.5 also
#    contradicts base's stated interpretation ("stronger emphasis on trend than
#    climatology"). Set to 0.3; confirm if 0.5 was intentional -- every base-scenario
#    number downstream moves with this.
SHARES_BASE <- list(
  PREC_CLIM_MEAN_SHARE = 0.5, EVAP_CLIM_MEAN_SHARE = 0.5,
  PREC_TREND_SIG_SHARE = 0.7, EVAP_TREND_SIG_SHARE = 0.7,
  PREC_CLIM_SHARE      = 0.3, EVAP_CLIM_SHARE      = 0.3,
  FINAL_PREC_SHARE     = 0.5
)
with_shares <- function(...) modifyList(SHARES_BASE, list(...))

# --- AXIS 2: rank scenarios use the PRE-COMPUTED ranks in dataset_ranks -------
# rank_linear / rank_exp take four existing rank columns, average P & E within mean
# and within slope, blend slope vs mean at RANK_SLOPE_SHARE, and map the combined
# mean rank R (lower = better) to a weight. sd ranks and significance are NOT used,
# so the rank scenarios also sidestep the missing-precip-significance NA issue.
RANK_SLOPE_SHARE <- 0.7   # slope (trend) weight vs mean (climatology) in the combined rank
RANK_EXP_BASE    <- 0.5   # rank_exp: weight of rank r+1 vs r; -> 0 = winner-take-all (best mosaic)
RANK_N           <- uniqueN(dataset_ranks[["dataset"]])  # fixed 1..N rank ladder (5 core datasets)

# rank column names AS THEY APPEAR in dataset_ranks (note *_rank_slope, not *_slope_rank)
RANK_COLS <- c(prec_mean = "prec_mean_rank",  prec_slope = "prec_rank_slope",
               evap_mean = "evap_mean_rank",  evap_slope = "evap_rank_slope")

WEIGHT_SCENARIOS <- list(
  base          = list(transform = "inverse_loss", shares = SHARES_BASE),
  clim_dominant = list(transform = "inverse_loss", shares = with_shares(PREC_CLIM_SHARE = 0.9, EVAP_CLIM_SHARE = 0.9)),
  evap_dominant = list(transform = "inverse_loss", shares = with_shares(FINAL_PREC_SHARE = 0.10)),
  prec_dominant = list(transform = "inverse_loss", shares = with_shares(FINAL_PREC_SHARE = 0.90)),
  rank_linear   = list(transform = "rank_linear"),   # combined pre-computed rank; see rank_weights()
  rank_exp      = list(transform = "rank_exp"),      # combined pre-computed rank; see rank_weights()
  inverted      = list(transform = "inverted",     shares = SHARES_BASE),
  neutral       = list(transform = "uniform")        # exact 1/n; see uniform_weights()
)

# loss-weight column  <-  source bias column (all treated as non-negative loss)
LOSS_WEIGHT_MAP <- c(
  weight_prec_mean  = "prec_mean_bias",  weight_prec_sd    = "prec_sd_bias",
  weight_evap_mean  = "evap_mean_bias",  weight_evap_sd    = "evap_sd_bias",
  weight_prec_slope = "prec_bias_slope", weight_evap_slope = "evap_bias_slope"
)

# ordered hierarchy (inverse_loss / inverted only): later rows depend on earlier
COMBINE_SPEC <- list(
  list(out = "weight_prec_clim",  x = "weight_prec_mean", y = "weight_prec_sd",    share = "PREC_CLIM_MEAN_SHARE"),
  list(out = "weight_evap_clim",  x = "weight_evap_mean", y = "weight_evap_sd",    share = "EVAP_CLIM_MEAN_SHARE"),
  list(out = "weight_prec_trend", x = "weight_prec_sig",  y = "weight_prec_slope", share = "PREC_TREND_SIG_SHARE"),
  list(out = "weight_evap_trend", x = "weight_evap_sig",  y = "weight_evap_slope", share = "EVAP_TREND_SIG_SHARE"),
  list(out = "weight_prec",       x = "weight_prec_clim", y = "weight_prec_trend", share = "PREC_CLIM_SHARE"),
  list(out = "weight_evap",       x = "weight_evap_clim", y = "weight_evap_trend", share = "EVAP_CLIM_SHARE"),
  list(out = "weight",            x = "weight_prec",      y = "weight_evap",       share = "FINAL_PREC_SHARE")
)

REQUIRED_COLS <- c(
  "lon", "lat", "dataset", "pe_ratio_check", "n_below_pet",
  unname(LOSS_WEIGHT_MAP), unname(RANK_COLS),
  "prec_check_significance", "prec_check_non_significance",
  "evap_check_significance", "evap_check_non_significance"
)

BASE_OUTPUT_COLS <- c(
  "lon", "lat", "dataset", names(LOSS_WEIGHT_MAP), "weight_prec_sig", "weight_evap_sig",
  "weight_prec_clim", "weight_evap_clim", "weight_prec_trend", "weight_evap_trend",
  "weight_prec", "weight_evap", "weight"
)


# Functions ===================================================================

validate_input <- function(dt) {
  missing <- setdiff(REQUIRED_COLS, names(dt))
  if (length(missing)) stop("Missing required columns: ", toString(missing))
  
  # Fail loud if any bias metric is signed: a silent drop here was the original bug.
  neg <- names(LOSS_WEIGHT_MAP)[vapply(
    unname(LOSS_WEIGHT_MAP), \(c) any(dt[[c]] < 0, na.rm = TRUE), logical(1))]
  if (length(neg)) stop("Negative values in loss metric(s): ", toString(LOSS_WEIGHT_MAP[neg]))
  invisible(TRUE)
}

normalize_prob <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & x > 0
  if (!any(ok)) return(out)
  s <- sum(x[ok])
  out[ok] <- if (is.finite(s) && s > 0) x[ok] / s else 1 / sum(ok)
  out
}

# convex combo a*x + (1-a)*y, NA-aware (drops the missing side and renormalizes a).
# Reused for both probabilities (hierarchy) and ranks (rank scenarios).
weighted_pair_mean <- function(x, y, a) {
  ox <- is.finite(x); oy <- is.finite(y)
  num <- fifelse(ox, a * x, 0) + fifelse(oy, (1 - a) * y, 0)
  den <- fifelse(ox, a, 0)     + fifelse(oy, 1 - a, 0)
  fifelse(den > 0, num / den, NA_real_)
}

# --- AXIS-2 transforms: hierarchy components (inverse_loss / inverted) --------
loss_to_prob <- function(loss, eps = 1e-6) {            # scale-SENSITIVE; ~uniform on near-ties
  q <- rep(NA_real_, length(loss)); ok <- is.finite(loss)
  q[ok] <- 1 / (eps + loss[ok]); normalize_prob(q)
}
inverted_loss_to_prob <- function(loss, eps = 1e-6) {   # adversarial mirror: worst loss -> most weight
  q <- rep(NA_real_, length(loss)); ok <- is.finite(loss)
  q[ok] <- eps + loss[ok]; normalize_prob(q)            # self-limiting: -> uniform where all agree
}
significance_to_prob <- function(sig, non_sig, fail = 0.10) {
  raw <- fcoalesce(
    fifelse(!is.na(sig),     fifelse(sig, 1, fail),     NA_real_),
    fifelse(!is.na(non_sig), fifelse(non_sig, 1, fail), NA_real_))
  normalize_prob(raw)
}
inverted_significance_to_prob <- function(sig, non_sig, fail = 0.10) {  # agree penalized, disagree boosted
  raw <- fcoalesce(
    fifelse(!is.na(sig),     fifelse(sig, fail, 1),     NA_real_),
    fifelse(!is.na(non_sig), fifelse(non_sig, fail, 1), NA_real_))
  normalize_prob(raw)
}

# axis-2 dispatch for the HIERARCHY-based transforms only
TRANSFORMS <- list(
  inverse_loss = list(loss = loss_to_prob,          sig = significance_to_prob),
  inverted     = list(loss = inverted_loss_to_prob, sig = inverted_significance_to_prob)
)

combine_pair_prob <- function(dt, out, x, y, a) {
  dt[, (out) := weighted_pair_mean(get(x), get(y), a)]
  dt[, (out) := normalize_prob(get(out)), by = .(lon, lat)]
  invisible(dt)
}

# Truly scenario-independent: physics filter only. Computed once.
# (NA propagation from the ~13% missing precip-significance rows is unchanged by
#  this rebuild; it affects only the hierarchy transforms, not the rank scenarios.)
prep_raw <- function(dt) {
  validate_input(dt)
  # n_below_pet > 6 keeps cells where AET < PET for a STRICT majority (>= 7) of the 12
  # PET combos. 
  copy(dt)[pe_ratio_check == TRUE & n_below_pet > 6][]
}

# HIERARCHY path (inverse_loss / inverted): raw losses+sig -> component probs.
# Depends only on the transform, so cached once per distinct transform.
apply_transform <- function(raw, tr) {
  dt <- copy(raw)
  for (out_col in names(LOSS_WEIGHT_MAP))
    dt[, (out_col) := tr$loss(get(LOSS_WEIGHT_MAP[[out_col]])), by = .(lon, lat)]
  dt[, weight_prec_sig := tr$sig(prec_check_significance, prec_check_non_significance), by = .(lon, lat)]
  dt[, weight_evap_sig := tr$sig(evap_check_significance, evap_check_non_significance), by = .(lon, lat)]
  dt[]
}
compute_weights <- function(components, shares) {
  dt <- copy(components)
  for (s in COMBINE_SPEC) combine_pair_prob(dt, s$out, s$x, s$y, shares[[s$share]])
  dt[]
}

# DIRECT path 1 -- rank scenarios. Use ONLY the four pre-computed ranks. Average P
# and E within mean and within slope (NA-aware), blend slope (w_slope) vs mean, then
# map the combined mean rank R to a positive score on the fixed 1..rank_n ladder and
# normalize per cell. No re-ranking -> any near-tie speckle is inherited from
# dataset_ranks, not introduced here; equal combined ranks get equal weight.
rank_weights <- function(raw, kind = c("linear", "exp"),
                         w_slope = RANK_SLOPE_SHARE, rank_n = RANK_N, exp_base = RANK_EXP_BASE) {
  kind <- match.arg(kind)
  dt <- copy(raw)
  dt[, rank_mean  := weighted_pair_mean(get(RANK_COLS[["prec_mean"]]),  get(RANK_COLS[["evap_mean"]]),  0.5)]
  dt[, rank_slope := weighted_pair_mean(get(RANK_COLS[["prec_slope"]]), get(RANK_COLS[["evap_slope"]]), 0.5)]
  dt[, R          := weighted_pair_mean(rank_slope, rank_mean, w_slope)]   # lower = better
  dt[, score      := if (kind == "linear") rank_n + 1 - R else exp_base ^ (R - 1)]
  dt[, weight     := normalize_prob(score), by = .(lon, lat)]
  dt[is.finite(weight), .(lon, lat, dataset, weight)][]
}

# DIRECT path 2 -- neutral. EXACT 1/n over physics-passing datasets per cell. Built
# directly (not routed through the NA-aware blend) so it stays provably flat -- its
# role is the unbiased substrate for the inverse provenance analysis.
uniform_weights <- function(raw) {
  out <- copy(raw)[, .(lon, lat, dataset)]
  out[, weight := 1 / .N, by = .(lon, lat)]
  out[]
}

run_weight_scenarios <- function(raw, scenarios, transforms = TRANSFORMS) {
  comp_cache <- list()
  rbindlist(lapply(names(scenarios), function(nm) {
    sc <- scenarios[[nm]]
    message("Running scenario: ", nm, "  (transform = ", sc$transform, ")")
    out <- switch(sc$transform,
                  uniform     = uniform_weights(raw),
                  rank_linear = rank_weights(raw, "linear"),
                  rank_exp    = rank_weights(raw, "exp"),
                  {  # default: hierarchy-based (inverse_loss, inverted)
                    if (is.null(comp_cache[[sc$transform]]))
                      comp_cache[[sc$transform]] <<- apply_transform(raw, transforms[[sc$transform]])
                    compute_weights(comp_cache[[sc$transform]], sc$shares)
                  })
    out[, scenario := nm][]
  }), use.names = TRUE, fill = TRUE)
}

# Per-cell sampling concentration. eff_n = inverse-Simpson effective n of datasets:
# a DIVERSITY measure, not confidence (neutral reads eff_n == n_candidates exactly).
make_diagnostics <- function(dt) {
  dt[is.finite(weight),
     {
       top <- which.max(weight)
       .(n_candidates = .N, dataset_top = dataset[top],
         weight_top = weight[top], eff_n = 1 / sum(weight^2))
     },
     by = .(scenario, lon, lat)]
}


# Analysis ====================================================================

weights_all  <- run_weight_scenarios(prep_raw(dataset_ranks), WEIGHT_SCENARIOS)
weights_tidy <- weights_all[, .(lon, lat, dataset, weight, scenario)]
weights_base <- weights_all[scenario == "base", ..BASE_OUTPUT_COLS]
weight_diag  <- make_diagnostics(weights_all)


# Outputs =====================================================================

saveRDS(weights_tidy, file.path(PATH_OUTPUT_DATA, "dataset_weights.Rds"))
saveRDS(weights_base, file.path(PATH_OUTPUT_DATA, "dataset_weights_base_detailed.Rds"))
saveRDS(weight_diag,  file.path(PATH_OUTPUT_DATA, "dataset_weight_diagnostics.Rds"))


# Validation ==================================================================

# 1. every scenario's weights form a per-cell distribution
sums <- weights_tidy[is.finite(weight), .(s = sum(weight)), by = .(scenario, lon, lat)]
stopifnot(all(abs(sums$s - 1) < 1e-6))

# 2. neutral is exactly uniform: eff_n must equal n_candidates everywhere
stopifnot(weight_diag[scenario == "neutral", all(abs(eff_n - n_candidates) < 1e-9)])

# 3. concentration summary. Expect neutral highest mean eff_n; rank_exp lowest
#    (steep ladder); rank_linear gentler; inverse_loss/inverted in between with their
#    spread depending on how separated the cell-level losses are.
print(weight_diag[, .(mean_eff_n = round(mean(eff_n), 3),
                      mean_n     = round(mean(n_candidates), 3)),
                  by = scenario][order(mean_eff_n)])

# 4. adversarial check: base-best and inverted-"best" coincide only in single-candidate
#    cells, so their top-dataset agreement should be ~ the single-candidate fraction
chk <- merge(weight_diag[scenario == "base",     .(lon, lat, top_base = dataset_top)],
             weight_diag[scenario == "inverted", .(lon, lat, top_inv  = dataset_top)],
             by = c("lon", "lat"))
message(sprintf("base vs inverted top-dataset agreement: %.3f (expected ~ single-candidate frac = %.3f)",
                chk[, mean(top_base == top_inv)],
                weight_diag[scenario == "base", mean(n_candidates == 1)]))

# 5. mean weight per dataset per scenario -- provenance budget each MC world samples from
print(weights_tidy[, .(mean_weight = mean(weight, na.rm = TRUE)),
                   by = .(scenario, dataset)][order(scenario, -mean_weight)])

if (interactive()) {
  library(ggplot2); library(maps); library(grid)
  show_scn <- "base"
  print(
    ggplot(weight_diag[scenario == show_scn]) +
      geom_tile(aes(lon, lat, fill = dataset_top)) +
      borders("world", colour = "grey20", linewidth = 0.2) +
      coord_equal(expand = FALSE) +
      labs(title = "Dominant dataset by grid cell", subtitle = paste(show_scn, "scenario"),
           x = NULL, y = NULL, fill = "Dataset") +
      theme(legend.position = "bottom", legend.key.width = unit(1.4, "cm"))
  )
}

# spot-check two cells across all eight scenarios
lon_test_a <- 9.875; lon_test_b <- 69.875; lat_test <- 35.125
weights_base[lon == lon_test_a & lat == lat_test]
weights_base[lon == lon_test_b & lat == lat_test]
dcast(weights_tidy[lon == lon_test_a & lat == lat_test], lon + lat + dataset ~ scenario, value.var = "weight")
dcast(weights_tidy[lon == lon_test_b & lat == lat_test], lon + lat + dataset ~ scenario, value.var = "weight")

# candidate-count distribution after the PET filter (how many cells lose datasets / drop out)
aa <- dataset_ranks[, .(N = sum(n_below_pet > 6, na.rm = TRUE)), by = .(lon, lat)]
table(aa$N)
