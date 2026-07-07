# ============================================================================
# Validate PET versus AET consistency and derive water versus energy
# limitation classes and transitions for twc_change
#
# This script:
# 1. Validates PET against AET for mean and minimum PET products
# 2. Classifies each grid cell and dataset as water or energy limited
#    for 1982 to 2001 and 2002 to 2021
# 3. Derives limitation regime transitions
# 4. Appends the dataset level limitation class information to
#    twc_grid_classes
# 5. Derives mean grid level limitation classes across datasets and
#    appends them to region_classes
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

prec_evap_change <- readRDS(
  file.path(PATH_OUTPUT_DATA, "prec_evap_change.Rds")
)

pet_change <- readRDS(
  file.path(PATH_OUTPUT_DATA, "pet_change.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Helpers =====================================================================

check_pet_aet <- function(dt_pet, dt_aet, pet_prefix) {
  merge(
    dt_pet,
    dt_aet[, .(
      lon, lat, dataset,
      evap_1982_2001,
      evap_2002_2021
    )],
    by = c("lon", "lat")
  )[
    , .(
      lon,
      lat,
      dataset,
      diff_1982_2001 = get(paste0(pet_prefix, "_1982_2001")) - evap_1982_2001,
      diff_2002_2021 = get(paste0(pet_prefix, "_2002_2021")) - evap_2002_2021
    )
  ]
}

report_pet_aet_check <- function(dt_check, label) {
  message("\n", label)
  print(table(dt_check[diff_1982_2001 < 0, dataset]))
  print(table(dt_check[diff_2002_2021 < 0, dataset]))
}

classify_limitation <- function(pet, prec) {
  factor(
    fifelse(pet > prec, "water", "energy"),
    levels = c("energy", "water")
  )
}

classify_limitation_change <- function(limited_1982_2001, limited_2002_2021) {
  factor(
    fifelse(
      limited_1982_2001 == "water" & limited_2002_2021 == "water", "w-w",
      fifelse(
        limited_1982_2001 == "water" & limited_2002_2021 == "energy", "w-e",
        fifelse(
          limited_1982_2001 == "energy" & limited_2002_2021 == "water",
          "e-w",
          "e-e"
        )
      )
    ),
    levels = c("w-w", "w-e", "e-w", "e-e")
  )
}

# Analysis ====================================================================

## Combined analysis table
twc <- merge(
  pet_change,
  prec_evap_change,
  by = c("lon", "lat")
)

twc[, limited_1982_2001 := classify_limitation(
  pet_mean_1982_2001,
  prec_1982_2001
)]

twc[, limited_2002_2021 := classify_limitation(
  pet_mean_2002_2021,
  prec_2002_2021
)]

twc[, limited_change := classify_limitation_change(
  limited_1982_2001,
  limited_2002_2021
)]

## Mean grid level classification

limited_mean <- twc[
  , .(
    prec_1982_2001 = mean(prec_1982_2001, na.rm = TRUE),
    prec_2002_2021 = mean(prec_2002_2021, na.rm = TRUE),
    pet_mean_1982_2001 = mean(pet_mean_1982_2001, na.rm = TRUE),
    pet_mean_2002_2021 = mean(pet_mean_2002_2021, na.rm = TRUE)
  ),
  by = .(lon, lat)
]

limited_mean[, limited_1982_2001 := classify_limitation(
  pet_mean_1982_2001,
  prec_1982_2001
)]

limited_mean[, limited_2002_2021 := classify_limitation(
  pet_mean_2002_2021,
  prec_2002_2021
)]

limited_mean[, limited_change := classify_limitation_change(
  limited_1982_2001,
  limited_2002_2021
)]

limited_mean <- limited_mean[, .(
  lon,
  lat,
  limited_1982_2001,
  limited_2002_2021,
  limited_change
)]

## Append to class tables 

limited_classes <- twc[, .(
  lon,
  lat,
  dataset,
  limited_1982_2001,
  limited_2002_2021,
  limited_change
)]

# Very few PET grid cells are missing
twc_grid_classes <- merge(
  twc_grid_classes,
  limited_mean,
  by = c("lon", "lat"),
  all.x = TRUE
)

# Output ======================================================================

saveRDS(
  twc_grid_classes,
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

saveRDS(
  twc[, .(lon, lat, dataset, prec_change, evap_change, pet_change,
          limited_1982_2001, limited_2002_2021, limited_change)],
  file.path(PATH_OUTPUT_DATA, "limited_change_dataset.Rds")
)

# Validation ==================================================================

## PET versus AET consistency 
pet_mean_aet_check <- check_pet_aet(
  dt_pet = pet_change,
  dt_aet = prec_evap_change,
  pet_prefix = "pet_mean"
)

pet_min_aet_check <- check_pet_aet(
  dt_pet = pet_change,
  dt_aet = prec_evap_change,
  pet_prefix = "pet_min"
)

report_pet_aet_check(pet_mean_aet_check, "PET mean versus AET validation")
report_pet_aet_check(pet_min_aet_check, "PET minimum versus AET validation")

print(table(twc$limited_1982_2001))
print(table(twc$limited_2002_2021))
print(table(twc[, limited_change, .(dataset)]))
print(table(twc[limited_1982_2001 == "water", limited_2002_2021]))
print(table(twc[limited_1982_2001 == "energy", limited_2002_2021]))

print(table(limited_mean$limited_mean_1982_2001))
print(table(limited_mean$limited_mean_2002_2021))
print(table(limited_mean$limited_mean_change))


