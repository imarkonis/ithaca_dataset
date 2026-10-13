# ============================================================================
# Aggregate grid-cell agreement weights to region-biome sampling probabilities.
#
# Physical admissibility is assessed independently of the weighting scenario:
#
#   P / E >= 0.9 AND E < PET in at least 7 of 12 comparisons
#
# Dataset eligibility requires passing both gates in at least 80% of ALL
# classified grid cells within an IPCC region x biome (equal-cell coverage).
# Missing dataset-cell combinations count as failures.
#
# Water biome units are removed entirely. Every other region-biome is retained:
# if no dataset is eligible, write a single NA dataset / NA probability record
# per scenario. These records are not valid CDF intervals; 04a propagates them
# into NA dataset selections.
#
# Eligible datasets retain their equal-cell regional agreement aggregation:
#   sum(grid-cell weight) / number of cells with weights in the unit.
# Locally absent datasets contribute zero to this average. Once ineligible
# datasets are excluded, probabilities are renormalised among eligible ones.
# Neutral assigns 1/n among regionally eligible datasets.
#
# Physical coverage diagnostics are saved separately for reproducibility.
# ============================================================================


# Libraries ==================================================================

source("code/_source.R")


# Settings ===================================================================

REGIONAL_COVERAGE_THRESHOLD <- 0.80


# Inputs ======================================================================

weights_dt <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_OUTPUT, "dataset_weights.Rds"))
)

dataset_ranks <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_OUTPUT, "dataset_ranks.Rds"))
)

grid_classes <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds"))
)


# Sampling grid ===============================================================

# All classified cells enter the physical-coverage denominator. "Water" is
# excluded from both the eligibility diagnostic and the sampling output.

grid <- unique(
  grid_classes[
    !is.na(region) &
      !is.na(biome) &
      as.character(biome) != "Water",
    .(
      lon,
      lat,
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
)

stopifnot(
  nrow(grid) > 0L,
  uniqueN(grid, by = c("lon", "lat")) == nrow(grid)
)

unit_size <- grid[
  ,
  .(n_total = .N),
  by = .(region, biome)
]

scenarios <- sort(unique(as.character(weights_dt$scenario)))
datasets <- sort(unique(as.character(dataset_ranks$dataset)))

stopifnot(length(scenarios) > 0L, length(datasets) > 0L)


# Regional physical admissibility ============================================

physical_gate <- dataset_ranks[
  ,
  .(
    lon,
    lat,
    dataset = as.character(dataset),
    pass = pe_ratio_check %in% TRUE &
      !is.na(n_below_pet) &
      n_below_pet > 6
  )
]

stopifnot(
  uniqueN(physical_gate, by = c("lon", "lat", "dataset")) ==
    nrow(physical_gate)
)

physical_gate <- merge(
  physical_gate,
  grid,
  by = c("lon", "lat")
)

pass_counts <- physical_gate[
  ,
  .(n_passing = sum(pass)),
  by = .(region, biome, dataset)
]

# Include every dataset in every non-Water region-biome. Missing rows imply
# zero physically admissible cells, not a smaller coverage denominator.

regional_coverage <- unit_size[
  ,
  .(dataset = datasets),
  by = .(region, biome, n_total)
]

regional_coverage <- merge(
  regional_coverage,
  pass_counts,
  by = c("region", "biome", "dataset"),
  all.x = TRUE
)

regional_coverage[
  is.na(n_passing),
  n_passing := 0L
]

regional_coverage[
  ,
  `:=`(
    coverage = n_passing / n_total,
    eligible = n_passing / n_total >= REGIONAL_COVERAGE_THRESHOLD
  )
]

setorder(regional_coverage, region, biome, dataset)

unit_eligibility <- regional_coverage[
  ,
  .(
    n_total = first(n_total),
    n_eligible = sum(eligible)
  ),
  by = .(region, biome)
]

missing_units <- unit_eligibility[n_eligible == 0L, .(region, biome)]


# Equal-cell regional agreement weights ======================================

weights_region <- merge(
  weights_dt[
    is.finite(weight) &
      weight >= 0,
    .(
      scenario = as.character(scenario),
      lon,
      lat,
      dataset = as.character(dataset),
      weight
    )
  ],
  grid,
  by = c("lon", "lat")
)

stopifnot(
  uniqueN(
    weights_region,
    by = c("scenario", "lon", "lat", "dataset")
  ) == nrow(weights_region)
)

# Denominator contains all grid cells retained by the upstream scenario,
# whether or not each individual dataset survives at that grid cell.

unit_cells <- unique(
  weights_region[
    ,
    .(scenario, region, biome, lon, lat)
  ]
)[
  ,
  .(n_cells = .N),
  by = .(scenario, region, biome)
]

weight_sums <- weights_region[
  ,
  .(weight_sum = sum(weight)),
  by = .(scenario, region, biome, dataset)
]

regional_raw <- merge(
  weight_sums,
  unit_cells,
  by = c("scenario", "region", "biome")
)

regional_raw[
  ,
  w_region_biome := weight_sum / n_cells
]


# Apply the common regional eligibility gate =================================

# One record for every eligible dataset under every weighting scenario.

eligible_scenarios <- regional_coverage[
  eligible == TRUE,
  .(region, biome, dataset)
][
  ,
  .(scenario = scenarios),
  by = .(region, biome, dataset)
]

weights_region_biome <- merge(
  eligible_scenarios,
  regional_raw[
    ,
    .(scenario, region, biome, dataset, w_region_biome)
  ],
  by = c("scenario", "region", "biome", "dataset"),
  all.x = TRUE
)

# Locally missing weight records have zero contribution, not an inflated
# footprint-conditional average.

weights_region_biome[
  is.na(w_region_biome),
  w_region_biome := 0
]

# If some eligible unit has no nonzero agreement probabilities for a
# non-neutral scenario, this is an upstream problem, not a PET-gate failure.

bad_weight_units <- weights_region_biome[
  scenario != "neutral",
  .(weight_total = sum(w_region_biome)),
  by = .(scenario, region, biome)
][
  !is.finite(weight_total) |
    weight_total <= 0
]

if (nrow(bad_weight_units)) {
  print(bad_weight_units)
  stop("Some physically eligible units have no usable dataset weights.")
}

# For neutral, remove all performance and footprint preferences.
# For other scenarios, renormalise only after removing ineligible datasets.

weights_region_biome[
  ,
  w_region_biome := if (scenario[1] == "neutral") {
    rep(1 / .N, .N)
  } else {
    w_region_biome / sum(w_region_biome)
  },
  by = .(scenario, region, biome)
]


# Explicit NA rows for non-Water units with no eligible dataset ===============

missing_scenarios <- missing_units[
  ,
  .(scenario = scenarios),
  by = .(region, biome)
][
  ,
  .(
    scenario,
    region,
    biome,
    dataset = NA_character_,
    w_region_biome = NA_real_
  )
]

weights_region_biome <- rbindlist(
  list(
    weights_region_biome[
      ,
      .(scenario, region, biome, dataset, w_region_biome)
    ],
    missing_scenarios
  ),
  use.names = TRUE
)

setorder(
  weights_region_biome,
  scenario,
  region,
  biome,
  dataset
)


# Validation =================================================================

stopifnot(
  !any(weights_region_biome$biome == "Water"),
  uniqueN(
    weights_region_biome,
    by = c("scenario", "region", "biome", "dataset")
  ) == nrow(weights_region_biome)
)

valid_weights <- weights_region_biome[!is.na(dataset)]

stopifnot(
  all(is.finite(valid_weights$w_region_biome)),
  all(valid_weights$w_region_biome >= 0)
)

prob_check <- valid_weights[
  ,
  .(total = sum(w_region_biome)),
  by = .(scenario, region, biome)
]

stopifnot(all(abs(prob_check$total - 1) < 1e-6))

# Every non-Water region-biome must be represented once per scenario, either
# by eligible dataset probabilities or by an explicit NA record.

support <- unique(
  weights_region_biome[
    ,
    .(scenario, region, biome)
  ]
)

stopifnot(
  nrow(support) == nrow(unit_size) * length(scenarios)
)

na_check <- weights_region_biome[
  is.na(dataset),
  .N,
  by = .(scenario, region, biome)
]

stopifnot(
  all(na_check$N == 1L),
  nrow(na_check) == nrow(missing_units) * length(scenarios),
  all(is.na(weights_region_biome[is.na(dataset)]$w_region_biome))
)

# Confirm neutral is exactly uniform among regionally eligible datasets.

neutral_check <- valid_weights[
  scenario == "neutral",
  .(spread = max(w_region_biome) - min(w_region_biome)),
  by = .(region, biome)
]

stopifnot(all(neutral_check$spread < 1e-12))


# Outputs ====================================================================

saveRDS(
  weights_region_biome,
  file.path(PATH_OUTPUT_OUTPUT, "weights_region_biome.Rds")
)

saveRDS(
  regional_coverage,
  file.path(PATH_OUTPUT_OUTPUT, "regional_physical_coverage.Rds")
)

message(
  sprintf(
    "Regional coverage gate %.0f%%: %d non-Water units, %d without eligible datasets.",
    100 * REGIONAL_COVERAGE_THRESHOLD,
    nrow(unit_size),
    nrow(missing_units)
  )
)

print(
  unit_eligibility[
    ,
    .(n_units = .N, n_cells = sum(n_total)),
    by = n_eligible
  ][
    order(n_eligible)
  ]
)

if (nrow(missing_units)) {
  print(unit_eligibility[n_eligible == 0L][order(region, biome)])
}

message(
  sprintf(
    "Non-Water region-biome units with < 5 cells: %d (of %d)",
    unit_size[n_total < 5L, .N],
    nrow(unit_size)
  )
)
