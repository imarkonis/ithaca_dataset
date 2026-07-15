# ============================================================================
# Prepare potential evapotranspiration (PET) products for twc_change
#
# This script:
# 1. Loads the annual PET products
# 2. Restricts PET to the analysis period and the TWC grid
# 3. Converts the PET products to long format
# 4. Computes full-period PET means per dataset and method
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(lubridate)

# Inputs =====================================================================

pet_raw <- read_fst(
  file.path(PATH_OUTPUT_RAW, "other/merra2_mswx_pet_mm_1980_2024_yearly.fst"),
  as.data.table = TRUE
)

twc_grid <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "twc_complete_grid.Rds")
)

# Constants & Variables ======================================================

twc_coordinates <- unique(twc_grid[, .(lon, lat)])

# Analysis ===================================================================

pet <- copy(pet_raw)

pet[, year := year(date)]
pet[, date := NULL]

pet <- pet[year >= FULL_PERIOD[1] & year <= FULL_PERIOD[2]]

setnames(
  pet,
  old = c("x", "y", "source"),
  new = c("lon", "lat", "dataset")
)

pet <- merge(pet, twc_coordinates, by = c("lon", "lat"))

# Convert PET products to long format ----------------------------------------

pet_id_columns    <- c("lon", "lat", "year", "dataset")
pet_value_columns <- setdiff(names(pet), pet_id_columns)

pet <- melt(
  pet,
  id.vars       = pet_id_columns,
  measure.vars  = pet_value_columns,
  variable.name = "method",
  value.name    = "value"
)

pet[, method := as.character(method)]

setcolorder(pet, c("lon", "lat", "year", "dataset", "method", "value"))

# Full-period PET means ------------------------------------------------------

pet_mean <- pet[
  ,
  .(value = mean(value, na.rm = TRUE)),
  by = .(lon, lat, dataset, method)
]

# Outputs ====================================================================

write_fst(
  pet,
  file.path(PATH_OUTPUT_DATA, "pet.fst")
)

write_fst(
  pet_mean,
  file.path(PATH_OUTPUT_DATA, "pet_mean.fst")
)

# Validation =================================================================

pet_validation <- pet_mean[
  ,
  .(pet_mean = mean(value, na.rm = TRUE)),
  by = .(lon, lat)
]

ggplot(
  pet_validation,
  aes(x = lon, y = lat, colour = pet_mean)
) +
  geom_point(size = 0.25, alpha = 0.7) +
  coord_equal() +
  labs(x = NULL, y = NULL, colour = "Mean PET") +
  theme_void()
