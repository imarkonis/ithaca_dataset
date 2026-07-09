# ============================================================================
# Prepare water availability and water flux change products for twc_change
#
# This script:
# 1. Derives yearly water availability and water flux from precipitation and
#    evaporation
# 2. Computes period means for 1982 to 2001 and 2002 to 2021
# 3. Computes changes between periods
# 4. Classifies grid cells into water availability and flux change storylines
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")

# Inputs ======================================================================

prec_evap <- read_fst(
  file.path(PATH_OUTPUT_DATA, "prec_evap_periods.fst"),
  as.data.table = TRUE
)

# Helpers =====================================================================

build_change_sign <- function(x, pos_label, neg_label) {
  fifelse(
    x > 0, pos_label,
    fifelse(x < 0, neg_label, NA_character_)
  )
}

# Analysis ====================================================================

prec_evap[
  ,
  `:=`(
    avail = prec - evap,
    flux = (prec + evap) / 2
  )
]

## Yearly estimates
avail_flux_yearly <- prec_evap[
  ,
  .(lon, lat, year, period, dataset, avail, flux)
]

## Period means 
avail_flux_periods <- avail_flux_yearly[
  ,
  .(
    avail = mean(avail, na.rm = TRUE),
    flux = mean(flux, na.rm = TRUE)
  ),
  by = .(lon, lat, period, dataset)
]

## Period change ==============================================================

avail_flux_change <- dcast(
  avail_flux_periods,
  lon + lat + dataset ~ period,
  value.var = c("avail", "flux")
)

avail_flux_change[
  ,
  `:=`(
    avail_change = avail_2002_2021 - avail_1982_2001,
    flux_change = flux_2002_2021 - flux_1982_2001,
    avail_change_rel = (avail_2002_2021 - avail_1982_2001) / avail_1982_2001,
    flux_change_rel = (flux_2002_2021 - flux_1982_2001) / flux_1982_2001
  )
]

avail_flux_change[
  ,
  `:=`(
    avail_class = build_change_sign(avail_change, "wetter", "drier"),
    flux_class = build_change_sign(flux_change, "accelerated", "decelerated")
  )
]

avail_flux_change[
  ,
  flux_avail := fcase(
    flux_class == "accelerated" & avail_class == "wetter", "wetter-accelerated",
    flux_class == "accelerated" & avail_class == "drier", "drier-accelerated",
    flux_class == "decelerated" & avail_class == "wetter", "wetter-decelerated",
    flux_class == "decelerated" & avail_class == "drier", "drier-decelerated",
    default = NA_character_
  )
]

avail_flux_change[
  ,
  `:=`(
    avail_class = factor(avail_class, levels = c("drier", "wetter")),
    flux_class = factor(flux_class, levels = c("decelerated", "accelerated")),
    flux_avail = factor(
      flux_avail,
      levels = c(
        "wetter-accelerated",
        "wetter-decelerated",
        "drier-accelerated",
        "drier-decelerated"
      )
    )
  )
]

avail_flux_change <- avail_flux_change[complete.cases(avail_flux_change)]
avail_flux_change <- avail_flux_change[, .(lon, lat, dataset, 
                      avail_change, flux_change, 
                      avail_change_rel, flux_change_rel, 
                      avail_class, flux_class, flux_avail)]
# Outputs =====================================================================

write_fst(
  avail_flux_yearly, 
  file.path(PATH_OUTPUT_DATA, "avail_flux_year.fst")
)

saveRDS(
  avail_flux_periods,
  file = file.path(PATH_OUTPUT_DATA, "avail_flux_periods.Rds")
)

saveRDS(
  avail_flux_change,
  file = file.path(PATH_OUTPUT_DATA, "avail_flux_change.Rds")
)
