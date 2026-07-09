# ============================================================================
# Summarize grid level precipitation, evaporation, water availability,
# water flux, and limitation regime changes by IPCC region for twc_change
#
# This script:
# 1. Merges grid level change products with IPCC land regions
# 2. Computes regional mean changes in precipitation, evaporation,
#    water availability, and water flux
# 3. Derives mean based regional storyline classes
# 4. Derives mode based regional spatial classes
# 5. Quantifies fractions of grid cells shifting toward energy
#    or water limitation
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

prec_evap_change <- readRDS(
  file.path(PATH_OUTPUT_DATA, "prec_evap_change.Rds")
)

avail_flux_change <- readRDS(
  file.path(PATH_OUTPUT_DATA, "avail_flux_change.Rds")
)

twc_grid_classes <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
)

# Helpers =====================================================================

build_change_sign <- function(x, pos_label, neg_label) {
  fifelse(
    x > 0, pos_label,
    fifelse(x < 0, neg_label, NA_character_)
  )
}

build_prec_evap_class <- function(prec_class, evap_class) {
  fcase(
    prec_class == "pos" & evap_class == "pos", "prec_pos-evap_pos",
    prec_class == "pos" & evap_class == "neg", "prec_pos-evap_neg",
    prec_class == "neg" & evap_class == "pos", "prec_neg-evap_pos",
    prec_class == "neg" & evap_class == "neg", "prec_neg-evap_neg",
    default = NA_character_
  )
}

build_flux_avail_class <- function(flux_class, avail_class) {
  fcase(
    flux_class == "accelerated" & avail_class == "wetter", "wetter-accelerated",
    flux_class == "accelerated" & avail_class == "drier", "drier-accelerated",
    flux_class == "decelerated" & avail_class == "wetter", "wetter-decelerated",
    flux_class == "decelerated" & avail_class == "drier", "drier-decelerated",
    default = NA_character_
  )
}

get_mode_by_group <- function(dt, group_cols, class_col) {
  tmp <- dt[, .N, by = c(group_cols, class_col)]
  
  setorderv(
    tmp,
    cols = c(group_cols, "N"),
    order = c(rep(1, length(group_cols)), -1)
  )
  
  tmp[
    ,
    .SD[1],
    by = group_cols
  ][
    ,
    c(group_cols, class_col),
    with = FALSE
  ]
}

# Analysis ====================================================================

## Grid table with IPCC regions 
ipcc_change_grid <- merge(
  prec_evap_change,
  twc_grid_classes,
  by = c("lon", "lat")
)

ipcc_change_grid <- merge(
  ipcc_change_grid,
  avail_flux_change[, .(
    lon,
    lat,
    dataset,
    avail_change,
    flux_change,
    avail_class,
    flux_class,
    flux_avail
  )],
  by = c("lon", "lat", "dataset")
)

ipcc_change_grid[
  ,
  total_grids := .N,
  by = .(dataset, region)
]

## Mode based regional classes 
class_cols <- c(
  "flux_class",
  "avail_class",
  "flux_avail",
  "limited_change"
)

get_mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ipcc_change_class_modes <- ipcc_change_grid[
  ,
  lapply(.SD, get_mode_value),
  by = .(dataset, region),
  .SDcols = class_cols
]

## Mean based regional changes 
ipcc_change_mean <- ipcc_change_grid[
  ,
  .(
    prec_change = mean(prec_change, na.rm = TRUE),
    evap_change = mean(evap_change, na.rm = TRUE),
    avail_change = mean(avail_change, na.rm = TRUE),
    flux_change = mean(flux_change, na.rm = TRUE),
    total_grids = mean(total_grids)
  ),
  by = .(dataset, region)
]

ipcc_change_mean <- merge(
  ipcc_change_mean,
  ipcc_change_class_modes,
  by = c("dataset", "region"),
  all.x = TRUE
)

# Outputs =====================================================================

saveRDS(
  ipcc_change_grid,
  file.path(PATH_OUTPUT_DATA, "ipcc_change_grid.Rds")
)

saveRDS(
  ipcc_change_mean,
  file.path(PATH_OUTPUT_DATA, "ipcc_change_mean.Rds")
)

saveRDS(
  ipcc_change_class_modes,
  file.path(PATH_OUTPUT_DATA, "ipcc_change_class_modes.Rds")
)


# Validation ===================================================================

sigcols <- c(
  "dataset",
  "region",
  "flux_class",
  "avail_class",
  "flux_avail"
)

matches <- merge(
  unique(ipcc_change_mean[, ..sigcols]),
  unique(ipcc_change_class_modes[, ..sigcols]),
  by = sigcols
)

message("Matched regional classifications: ", nrow(matches))
message("Unique retained regions: ", length(unique(ipcc_change_mean$region)))

print(table(matches$dataset))
