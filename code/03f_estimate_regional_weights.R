# ============================================================================
# Aggregate grid-cell dataset agreement weights to region-biome probabilities.
#
# For each scenario x region x biome x dataset:
#
#   w_region_biome =
#     sum(cell_weight * grid-cell dataset weight) /
#     sum(cell_weight over all retained cells in the region-biome unit)
#
# The denominator contains ALL retained grid cells in the region-biome unit,
# irrespective of whether a particular dataset survives the PET filter there.
# Therefore dataset absence is treated as an implicit zero contribution rather
# than averaging only over the dataset's surviving footprint.
#
# Because grid-cell dataset weights already sum to 1 within each retained cell,
# the resulting region-biome dataset weights also sum to 1 by construction.
#
# No additional normalization is required.
# ============================================================================


# Libraries ==================================================================

source("code/_source.R")

# Inputs ======================================================================

weights_dt <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "dataset_weights.Rds")
)

grid_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
)


# Join weights with spatial classes ===========================================

# Keep only cells assigned to a valid region and biome.
#
# cell_weight is the latitude-dependent grid-cell area weight used to ensure
# that high-latitude cells do not receive the same spatial influence as larger
# low-latitude cells.

weights_region <- merge(
  weights_dt,
  grid_classes[
    !is.na(region) &
      !is.na(biome),
    .(
      lon,
      lat,
      region,
      biome,
      cell_weight
    )
  ],
  by = c("lon", "lat")
)


# Retain only valid sampling probabilities and valid positive cell weights.

weights_region <- weights_region[
  is.finite(weight) &
    weight >= 0 &
    is.finite(cell_weight) &
    cell_weight > 0
]


# Region-biome area denominator ===============================================

# Total represented area of each retained scenario x region x biome unit.
#
# unique() is required because weights_region contains one row per dataset,
# whereas each grid cell must contribute its area only once to the denominator.

unit_area <- unique(
  weights_region[
    ,
    .(
      scenario,
      region,
      biome,
      lon,
      lat,
      cell_weight
    )
  ]
)[
  ,
  .(
    unit_weight = sum(cell_weight)
  ),
  by = .(
    scenario,
    region,
    biome
  )
]


# Region-biome dataset probabilities ==========================================

# For every dataset, sum its area-weighted grid-cell sampling probabilities.
#
# If a dataset is absent from some cells because of the PET filter, those cells
# contribute zero to the numerator but remain represented in unit_weight.
#
# This prevents datasets with patchy surviving coverage from being rewarded by
# averaging only over the cells in which they remain available.

weights_region_biome <- weights_region[
  ,
  .(
    weighted_sum = sum(
      weight * cell_weight
    )
  ),
  by = .(
    scenario,
    region,
    biome,
    dataset
  )
]


# Attach the common area denominator for each region-biome unit.

weights_region_biome <- unit_area[
  weights_region_biome,
  on = .(
    scenario,
    region,
    biome
  )
]


# Final region-biome probability assigned to each dataset.

weights_region_biome[
  ,
  w_region_biome := weighted_sum / unit_weight
]


# Keep only quantities needed downstream.

weights_region_biome <- weights_region_biome[
  ,
  .(
    scenario,
    region,
    biome,
    dataset,
    w_region_biome
  )
]

setorder(
  weights_region_biome,
  scenario,
  region,
  biome,
  dataset
)

# Output ======================================================================

saveRDS(
  weights_region_biome,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "weights_region_biome.Rds"
  )
)

# Validation ==================================================================

# 1. Region-biome probabilities must sum to 1 across datasets.
#
# This should hold by construction because:
#
#   sum(dataset weights) = 1
#
# within each grid cell and all datasets share the same regional area
# denominator.

prob_check <- weights_region_biome[
  ,
  .(
    s = sum(w_region_biome)
  ),
  by = .(
    scenario,
    region,
    biome
  )
]

stopifnot(
  all(
    abs(prob_check$s - 1) < 1e-6
  )
)


# 2. Ensure all resulting probabilities are finite and non-negative.

stopifnot(
  all(
    is.finite(weights_region_biome$w_region_biome)
  )
)

stopifnot(
  all(
    weights_region_biome$w_region_biome >= 0
  )
)


# Coverage ====================================================================

# Identify region-biome units present in the spatial classification but with no
# surviving grid cells after the upstream filtering.
#
# These units cannot receive dataset sampling probabilities downstream.

all_units <- unique(
  grid_classes[
    !is.na(region) &
      !is.na(biome),
    .(
      region,
      biome
    )
  ]
)

present_units <- unique(
  weights_region[
    ,
    .(
      region,
      biome
    )
  ]
)

missing_units <- fsetdiff(
  all_units,
  present_units
)

message(
  sprintf(
    paste0(
      "Region-biome units: %d total, ",
      "%d retained, %d without surviving cells"
    ),
    nrow(all_units),
    nrow(present_units),
    nrow(missing_units)
  )
)

if (nrow(missing_units)) {
  
  print(
    missing_units[
      order(
        region,
        biome
      )
    ]
  )
}


# 3. Report very small retained region-biome units.
#
# These are valid but may have spatial probabilities based on little area / few
# grid cells and are useful to flag during diagnostics.

unit_coverage <- unique(
  weights_region[
    ,
    .(
      region,
      biome,
      lon,
      lat,
      cell_weight
    )
  ]
)[
  ,
  .(
    n_cells = .N,
    unit_weight = sum(cell_weight)
  ),
  by = .(
    region,
    biome
  )
]

message(
  sprintf(
    "Retained region-biome units with < 5 cells: %d (of %d)",
    unit_coverage[n_cells < 5, .N],
    nrow(unit_coverage)
  )
)



