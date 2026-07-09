# ============================================================================
# Prepare precipitation, evaporation, and PET change products for twc_change
#
# This script:
# 1. Builds annual precipitation and evaporation pairs for core datasets
# 2. Computes period means for 1982 to 2001 and 2002 to 2021
# 3. Derives precipitation and evaporation change classes
# 4. Processes PET products and exports period summaries and change products
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Input datasets ===============================================================

prec_evap <- readRDS(file.path(PATH_OUTPUT_DATA, "prec_evap.Rds"))

pet_raw <- read_fst(
  file.path(PATH_OUTPUT_RAW, "other/merra2_mswx_pet_mm_1980_2024_yearly.fst"),
  as.data.table = TRUE
)

twc_grid <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_complete_grid.Rds")
)

# Helpers =====================================================================

get_period_label <- function(year_vec, split_year) {
  factor(
    ifelse(year_vec < split_year, "1982_2001", "2002_2021"),
    levels = c("1982_2001", "2002_2021"),
    ordered = TRUE
  )
}

# Constants & Variables =======================================================

prec_evap[, period := get_period_label(year, END_PERIOD_1)]

setcolorder(
  prec_evap,
  c("lon", "lat", "year", "period", "dataset", "prec", "evap")
)

# Analysis ====================================================================

prec_evap_means <- prec_evap[
  ,
  .(
    prec = mean(prec, na.rm = TRUE),
    evap = mean(evap, na.rm = TRUE)
  ),
  by = .(lon, lat, dataset, period)
]

prec_evap_change <- dcast(
  prec_evap_means,
  lon + lat + dataset ~ period,
  value.var = c("prec", "evap")
)

prec_evap_change[
  ,
  `:=`(
    prec_change = prec_2002_2021 - prec_1982_2001,
    evap_change = evap_2002_2021 - evap_1982_2001 
  )
]

# PET ========================================================================

pet <- copy(pet_raw)

pet[, year := year(date)]
pet[, date := NULL]
pet <- pet[year >= FULL_PERIOD[1] & year <= FULL_PERIOD[2]]

pet[, period := get_period_label(year, period_split_year)]

setnames(
  pet,
  old = c("x", "y", "source"),
  new = c("lon", "lat", "dataset")
)

pet <- merge(
  pet,
  twc_grid,
  by = c("lon", "lat")
)

pet <- melt(
  pet,
  id.vars = c("lon", "lat", "year", "dataset", "period"),
  variable.name = "method",
  value.name = "value"
)

pet_mean <- pet[
  ,
  .(value = mean(value, na.rm = TRUE)),
  by = .(lon, lat, dataset, method)
]

pet_periods <- pet[
  ,
  .(pet = mean(value, na.rm = TRUE)),
  by = .(lon, lat, period, dataset, method)
]

pet_stats <- pet[
  ,
  .(
    pet_mean = round(mean(value, na.rm = TRUE), 1),
    pet_min = min(value, na.rm = TRUE),
    pet_max = max(value, na.rm = TRUE),
    n = sum(!is.na(value))
  ),
  by = .(lon, lat, period)
]

pet_stats <- pet_stats[n > 200]

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
  old = c( "1982_2001", "2002_2021"),
  new = c("pet_mean_1982_2001", "pet_mean_2002_2021")
)

setnames(
  pet_min_wide,
  old = c( "1982_2001", "2002_2021"),
  new = c("pet_min_1982_2001", "pet_min_2002_2021")
)

setnames(
  pet_max_wide,
  old = c( "1982_2001", "2002_2021"),
  new = c("pet_max_1982_2001", "pet_max_2002_2021")
)

pet_final <- Reduce(
  f = function(x, y) merge(x, y, by = c("lon", "lat"), all = TRUE),
  x = list(pet_mean_wide, pet_min_wide, pet_max_wide)
)

pet_final[
  ,
  pet_change := pet_mean_2002_2021 - pet_mean_1982_2001
]

setcolorder(
  pet_final,
  c(
    "lon", "lat",
    "pet_min_1982_2001", "pet_mean_1982_2001", "pet_max_1982_2001",
    "pet_min_2002_2021", "pet_mean_2002_2021", "pet_max_2002_2021",
    "pet_change"
  )
)

# Outputs =====================================================================

write_fst(
  prec_evap, 
  file.path(PATH_OUTPUT_DATA, "prec_evap_periods.fst")
)

saveRDS(
  prec_evap_change,
  file = file.path(PATH_OUTPUT_DATA, "prec_evap_change.Rds")
)

write_fst(
  pet,
  file.path(PATH_OUTPUT_DATA, "pet.fst")
)

saveRDS(
  pet_mean,
  file = file.path(PATH_OUTPUT_DATA, "pet_mean.Rds")
)

saveRDS(
  pet_periods,
  file = file.path(PATH_OUTPUT_DATA, "pet_ensemble_periods.Rds")
)

saveRDS(
  pet_final,
  file = file.path(PATH_OUTPUT_DATA, "pet_change.Rds")
)

# Validate ====================================================================

pet_final[
  ,
  rel_range := round(
    (pet_max_1982_2001 - pet_min_1982_2001) / pet_mean_2002_2021,
    2
  )
]

ggplot(pet_final) +
  geom_point(aes(x = lon, y = lat, colour = rel_range))

pet_final[, rel_range := NULL]
