# ============================================================================
# Aggregate dataset agreement weights to region-biome level.
#
# 1. Join grid-cell weights with region and biome masks
# 2. Filter invalid cells and excluded regions
# 3. Region-biome sampling probability per scenario x dataset (ALL-CELL mean)
# 4. Biome fractions within each scenario-region-dataset
# 5. Coverage check: region-biome units emptied by the PET filter
# ============================================================================

# Libraries ===================================================================
source("source/twc_change.R")

# Inputs ======================================================================
weights_dt       <- readRDS(file.path(PATH_OUTPUT_DATA, "dataset_weights.Rds"))
twc_grid_classes <- readRDS(file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds"))

# Helpers =====================================================================
normalize_prob <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & (x > 0)
  if (!any(ok)) return(out)
  s <- sum(x[ok])
  out[ok] <- if (is.finite(s) && s > 0) x[ok] / s else 1 / sum(ok)
  out
}

# Prepare grid-level region data ==============================================
weights_region <- merge(
  weights_dt,
  twc_grid_classes[, .(lon, lat, region, biome)],
  by = c("lon", "lat"), all.x = TRUE
)
# NOTE: cells with no region/biome (outside the masks) are dropped here silently.
weights_region <- weights_region[
  is.finite(weight) & weight >= 0 & !is.na(region) & !is.na(biome)
]

# Region-biome sampling probabilities ========================================
# Denominator = ALL cells in the unit (absent dataset-cells count as 0), NOT the
# per-dataset present-cell count. With the PET filter dropping datasets unevenly,
# the conditional mean (mean over present cells) over-credits patchy datasets: a
# product that survives only a few harsh, low-survivor cells inherits the high
# per-cell weight there and is REWARDED for its absence elsewhere. The all-cell
# mean penalizes absence correctly and -- since per-cell weights already sum to 1
# over survivors -- sums to 1 across datasets by construction.
#   To revert to the methods-text conditional version:
#     weights_region_biome <- weights_region[, .(w_raw = mean(weight), n_cells = .N), by = ...]
n_unit <- unique(weights_region[, .(scenario, region, biome, lon, lat)])[
  , .(n_unit = .N), by = .(scenario, region, biome)
]

weights_region_biome <- weights_region[
  , .(weight_sum = sum(weight), n_cells = .N),
  by = .(scenario, region, biome, dataset)
]
weights_region_biome <- merge(weights_region_biome, n_unit,
                              by = c("scenario", "region", "biome"))
weights_region_biome[, w_raw := weight_sum / n_unit]                   # all-cell mean
weights_region_biome[, w_region_biome := normalize_prob(w_raw),        # no-op safeguard
                     by = .(scenario, region, biome)]

# Biome fractions: of dataset d's surviving footprint in region r, the share in
# each biome (dataset-specific, hence sensitive to differential filtering -- confirm
# this is the intended meaning vs a dataset-independent geographic biome share).
weights_region_biome[, biome_fraction := n_cells / sum(n_cells),
                     by = .(scenario, region, dataset)]

weights_region_biome[, c("weight_sum", "n_unit", "w_raw") := NULL]
setcolorder(
  weights_region_biome,
  c("scenario", "region", "biome", "dataset",
    "w_region_biome", "biome_fraction", "n_cells")
)

# Outputs =====================================================================
saveRDS(weights_region_biome, file.path(PATH_OUTPUT_DATA, "weights_region_biome.Rds"))

# Validation ==================================================================
# 1. all-cell means sum to 1 across datasets per unit BEFORE normalization
#    (the property that makes this a proper sampling distribution)
raw_sums <- weights_region[, .(weight_sum = sum(weight)), by = .(scenario, region, biome, dataset)
][n_unit, on = .(scenario, region, biome)
][, .(s = sum(weight_sum) / first(n_unit)), by = .(scenario, region, biome)]
stopifnot(all(abs(raw_sums$s - 1) < 1e-6))

# 2. normalized weights sum to 1 per unit (always true, but guards the safeguard)
post <- weights_region_biome[, .(s = sum(w_region_biome)), by = .(scenario, region, biome)]
stopifnot(all(abs(post$s - 1) < 1e-6))

# 3. COVERAGE: region-biome units the PET filter emptied entirely -- the MC cannot
#    sample a dataset for these, so they become holes in every scenario's grid.
#    Expected to concentrate at high latitudes (the n_below_pet > 6 threshold).
all_units     <- unique(twc_grid_classes[!is.na(region) & !is.na(biome), .(region, biome)])
present_units <- unique(weights_region[, .(region, biome)])
missing_units <- fsetdiff(all_units, present_units)
message(sprintf("region-biome units: %d total, %d retained, %d EMPTIED by PET filter",
                nrow(all_units), nrow(present_units), nrow(missing_units)))
if (nrow(missing_units)) print(missing_units[order(region, biome)])

# 4. per-unit cell counts (thin units give noisy region-biome priors)
unit_cov <- unique(weights_region[, .(region, biome, lon, lat)])[, .(n_cells = .N), by = .(region, biome)]
message(sprintf("retained units with < 5 cells: %d (of %d)", unit_cov[n_cells < 5, .N], nrow(unit_cov)))

# 5. spot-check: rows must sum to 1 per biome, across a hierarchy / direct / rank scenario
make_prob_wide <- function(dt, reg = "MED", scen = "base", value_col = "w_region_biome") {
  dt <- copy(dt)
  if ("scenario" %in% names(dt)) dt <- dt[scenario == scen]
  if ("region"   %in% names(dt)) dt <- dt[region == reg]
  dt <- dt[, .(biome, dataset, value = get(value_col))][is.finite(value)]
  dt <- dt[, .(value = mean(value)), by = .(biome, dataset)]
  dcast(dt, biome ~ dataset, value.var = "value")
}
print(rowSums(make_prob_wide(weights_region_biome, scen = "base")[, -1]))
print(rowSums(make_prob_wide(weights_region_biome, scen = "neutral")[, -1]))
print(rowSums(make_prob_wide(weights_region_biome, scen = "rank_exp")[, -1]))
