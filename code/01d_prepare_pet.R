# ============================================================================
# Prepare potential evapotranspiration change products for twc_change
#
# This script:
# 1. Loads annual PET products
# 2. Restricts PET data to the analysis period and TWC grid
# 3. Converts the PET products to long format
# 4. Computes full-period and period-specific PET summaries
# 5. Calculates PET ensemble ranges and changes
# ============================================================================

# Libraries ===================================================================

source("code/_source.R")

# Input datasets ==============================================================

pet_raw <- read_fst(
  file.path(
    PATH_OUTPUT_RAW,
    "other/merra2_mswx_pet_mm_1980_2024_yearly.fst"
  ),
  as.data.table = TRUE
)

twc_grid <- readRDS(
  file.path(
    PATH_OUTPUT_OUTPUT,
    "twc_complete_grid.Rds"
  )
)

# Helpers =====================================================================

get_period_label <- function(year_vec, split_year) {
  factor(
    ifelse(
      year_vec < split_year,
      "1982_2001",
      "2002_2021"
    ),
    levels = c("1982_2001", "2002_2021"),
    ordered = TRUE
  )
}

# Prepare PET data ============================================================

pet <- copy(pet_raw)

pet[, year := year(date)]
pet[, date := NULL]

pet <- pet[
  year >= FULL_PERIOD[1] &
    year <= FULL_PERIOD[2]
]

pet[
  ,
  period := get_period_label(
    year,
    END_PERIOD_1
  )
]

setnames(
  pet,
  old = c("x", "y", "source"),
  new = c("lon", "lat", "dataset")
)

# Use the TWC grid only as a spatial mask.
# This avoids including grid metadata as PET methods during melt().

twc_coordinates <- unique(
  twc_grid[
    ,
    .(lon, lat)
  ]
)

pet <- merge(
  pet,
  twc_coordinates,
  by = c("lon", "lat")
)

# Identify PET value columns explicitly =======================================

pet_id_columns <- c(
  "lon",
  "lat",
  "year",
  "dataset",
  "period"
)

pet_value_columns <- setdiff(
  names(pet),
  pet_id_columns
)

# Convert PET products to long format =========================================

pet <- melt(
  pet,
  id.vars = pet_id_columns,
  measure.vars = pet_value_columns,
  variable.name = "method",
  value.name = "value"
)

pet[, method := as.character(method)]

setcolorder(
  pet,
  c(
    "lon",
    "lat",
    "year",
    "period",
    "dataset",
    "method",
    "value"
  )
)

# Full-period PET means ========================================================

pet_mean <- pet[
  ,
  .(
    value = mean(value, na.rm = TRUE)
  ),
  by = .(
    lon,
    lat,
    dataset,
    method
  )
]

# Dataset and method period means =============================================

pet_periods <- pet[
  ,
  .(
    pet = mean(value, na.rm = TRUE)
  ),
  by = .(
    lon,
    lat,
    period,
    dataset,
    method
  )
]

# PET ensemble statistics =====================================================

pet_stats <- pet[
  ,
  .(
    pet_mean = round(
      mean(value, na.rm = TRUE),
      1
    ),
    pet_min = min(
      value,
      na.rm = TRUE
    ),
    pet_max = max(
      value,
      na.rm = TRUE
    ),
    n = sum(!is.na(value))
  ),
  by = .(
    lon,
    lat,
    period
  )
]

# Retain grid cells with adequate PET coverage.

pet_stats <- pet_stats[n > 200]

# Convert period statistics to wide format ====================================

pet_mean_wide <- dcast(
  pet_stats,
  lon + lat ~ period,
  value.var = "pet_mean"
)

pet_min_wide <- dcast(
  pet_stats,
  lon + lat ~ period,
  value.var = "pet_min"
)

pet_max_wide <- dcast(
  pet_stats,
  lon + lat ~ period,
  value.var = "pet_max"
)

setnames(
  pet_mean_wide,
  old = c(
    "1982_2001",
    "2002_2021"
  ),
  new = c(
    "pet_mean_1982_2001",
    "pet_mean_2002_2021"
  )
)

setnames(
  pet_min_wide,
  old = c(
    "1982_2001",
    "2002_2021"
  ),
  new = c(
    "pet_min_1982_2001",
    "pet_min_2002_2021"
  )
)

setnames(
  pet_max_wide,
  old = c(
    "1982_2001",
    "2002_2021"
  ),
  new = c(
    "pet_max_1982_2001",
    "pet_max_2002_2021"
  )
)

# Combine PET summaries =======================================================

pet_final <- Reduce(
  f = function(x, y) {
    merge(
      x,
      y,
      by = c("lon", "lat"),
      all = TRUE
    )
  },
  x = list(
    pet_mean_wide,
    pet_min_wide,
    pet_max_wide
  )
)

pet_final[
  ,
  pet_change :=
    pet_mean_2002_2021 -
    pet_mean_1982_2001
]

setcolorder(
  pet_final,
  c(
    "lon",
    "lat",
    "pet_min_1982_2001",
    "pet_mean_1982_2001",
    "pet_max_1982_2001",
    "pet_min_2002_2021",
    "pet_mean_2002_2021",
    "pet_max_2002_2021",
    "pet_change"
  )
)

# Outputs =====================================================================

write_fst(
  pet,
  file.path(
    PATH_OUTPUT_DATA,
    "pet.fst"
  )
)

saveRDS(
  pet_mean,
  file = file.path(
    PATH_OUTPUT_DATA,
    "pet_mean.Rds"
  )
)

saveRDS(
  pet_periods,
  file = file.path(
    PATH_OUTPUT_DATA,
    "pet_ensemble_periods.Rds"
  )
)

saveRDS(
  pet_final,
  file = file.path(
    PATH_OUTPUT_DATA,
    "pet_change.Rds"
  )
)

# Validation ==================================================================

pet_validation <- copy(pet_final)

pet_validation[
  ,
  rel_range := round(
    (
      pet_max_1982_2001 -
        pet_min_1982_2001
    ) /
      pet_mean_2002_2021,
    2
  )
]

ggplot(
  pet_validation,
  aes(
    x = lon,
    y = lat,
    colour = rel_range
  )
) +
  geom_point(
    size = 0.25,
    alpha = 0.7
  ) +
  coord_equal() +
  labs(
    x = NULL,
    y = NULL,
    colour = "Relative\nPET range"
  ) +
  theme_void()