# ============================================================================
# Build spatial classes for the complete TWC grid
#
# This script:
#   1. assigns IPCC region and descriptive classes to each grid cell
#   2. derives hemisphere and latitude zone from grid-cell latitude
#   3. computes latitude-based grid-cell area weights
#   4. assigns biome, Köppen-Geiger climate, and land-cover classes
#   5. derives the area-weighted dominant property for each IPCC region
#
# Grid-level classes:
#   - IPCC region
#   - continent
#   - hemisphere
#   - latitude zone
#   - biome
#   - Köppen-Geiger level-1 climate class
#   - Köppen-Geiger climate class
#   - land-cover class
#
# Region-level classes are defined as the area-weighted dominant grid-cell
# class within each IPCC region.
#
# Outputs:
#   - region_classes.Rds
#   - grid_classes.Rds
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

library(pRecipe)

# Inputs =====================================================================

masks <- pRecipe_masks()

twc_grid <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "twc_complete_grid.Rds")
)

# Constants ==================================================================

EXCLUDED_REGIONS <- c("BOB", "ARS", "GIC")

HEMISPHERE_LEVELS <- c(
  "north",
  "south"
)

LAT_ZONE_LEVELS <- c(
  "highlatitude",
  "midlatitude",
  "subtropical",
  "tropical"
)

LAT_TROPICS <- 23.5
LAT_MIDLATITUDE <- 35
LAT_HIGHLATITUDE <- 60

# Helper functions ============================================================

set_factor_levels <- function(dt, column, levels_vec) {
  
  dt[
    ,
    (column) := factor(
      get(column),
      levels = levels_vec
    )
  ]
  
  dt
}


# Return the area-weighted dominant class.
area_mode <- function(x, weight) {
  
  z <- data.table(
    class = as.character(x),
    weight = weight
  )
  
  z <- z[
    !is.na(class) &
      class != "" &
      !is.na(weight)
  ]
  
  if (nrow(z) == 0L) {
    return(NA_character_)
  }
  
  z <- z[
    ,
    .(weight = sum(weight)),
    by = class
  ]
  
  z[
    order(-weight, class),
    class[1]
  ]
}


# Return the fraction of classified area occupied by the dominant class.
area_mode_share <- function(x, weight) {
  
  z <- data.table(
    class = as.character(x),
    weight = weight
  )
  
  z <- z[
    !is.na(class) &
      class != "" &
      !is.na(weight)
  ]
  
  if (nrow(z) == 0L) {
    return(NA_real_)
  }
  
  z <- z[
    ,
    .(weight = sum(weight)),
    by = class
  ]
  
  max(z$weight) / sum(z$weight)
}

# Grid-level classes ==========================================================

mask_classes <- masks[
  land_mask == "land",
  .(
    lon,
    lat,
    region = as.character(ipcc_short_region),
    region_full = as.character(ipcc_region),
    continent = as.character(ipcc_continent),
    biome = as.character(biome_short_class),
    kg_class = as.character(KG_class),
    kg_class_1 = as.character(KG_class_1),
    land_cover = as.character(land_cover_class)
  )
]

twc_grid_classes <- merge(
  twc_grid,
  mask_classes,
  by = c("lon", "lat"),
  all.x = TRUE
)

# Remove ocean IPCC regions and excluded regions.
twc_grid_classes <- twc_grid_classes[
  !region %like% "O$" &
    !region %in% EXCLUDED_REGIONS
]

# Area weights ================================================================

# For a regular longitude-latitude grid, grid-cell area is proportional to
# cos(latitude). Absolute area is not required because only relative weights
# are needed for aggregation.

twc_grid_classes[
  ,
  cell_weight := cos(lat * pi / 180)
]

twc_grid_classes[
  ,
  area_weight := cell_weight / sum(cell_weight, na.rm = TRUE)
]

# Hemisphere ==================================================================

twc_grid_classes[
  ,
  hemisphere := fifelse(
    lat >= 0,
    "north",
    "south"
  )
]

twc_grid_classes <- set_factor_levels(
  twc_grid_classes,
  "hemisphere",
  HEMISPHERE_LEVELS
)

# Latitude zone ===============================================================

twc_grid_classes[
  ,
  lat_zone := fcase(
    abs(lat) >= LAT_HIGHLATITUDE, "highlatitude",
    abs(lat) >= LAT_MIDLATITUDE,  "midlatitude",
    abs(lat) >= LAT_TROPICS,      "subtropical",
    default =                     "tropical"
  )
]

twc_grid_classes <- set_factor_levels(
  twc_grid_classes,
  "lat_zone",
  LAT_ZONE_LEVELS
)

# Region-level classes ========================================================

region_classes <- twc_grid_classes[
  ,
  .(
    region_full = area_mode(region_full, cell_weight),
    continent = area_mode(continent, cell_weight),
    hemisphere = area_mode(hemisphere, cell_weight),
    lat_zone = area_mode(lat_zone, cell_weight),
    biome = area_mode(biome, cell_weight),
    kg_class = area_mode(kg_class, cell_weight),
    kg_class_1 = area_mode(kg_class_1, cell_weight),
    land_cover = area_mode(land_cover, cell_weight),
    
    hemisphere_share = area_mode_share(hemisphere, cell_weight),
    lat_zone_share = area_mode_share(lat_zone, cell_weight),
    biome_share = area_mode_share(biome, cell_weight),
    kg_class_share = area_mode_share(kg_class, cell_weight),
    kg_class_1_share = area_mode_share(kg_class_1, cell_weight),
    land_cover_share = area_mode_share(land_cover, cell_weight)
  ),
  by = region
]

# Apply factor kg_class_1# Apply factor levels =========================================================

region_classes <- set_factor_levels(
  region_classes,
  "hemisphere",
  HEMISPHERE_LEVELS
)

region_classes <- set_factor_levels(
  region_classes,
  "lat_zone",
  LAT_ZONE_LEVELS
)

# Order ======================================================================

setorder(
  region_classes,
  hemisphere,
  lat_zone,
  continent,
  region
)

setcolorder(
  region_classes,
  c(
    "region",
    "region_full",
    "continent",
    "hemisphere",
    "lat_zone",
    "biome",
    "kg_class_1",
    "kg_class",
    "land_cover",
    "hemisphere_share",
    "lat_zone_share",
    "biome_share",
    "kg_class_share",
    "kg_class_1_share",
    "land_cover_share"
  )
)

# Outputs =====================================================================

saveRDS(
  region_classes,
  file.path(PATH_OUTPUT_OUTPUT, "region_classes.Rds")
)

saveRDS(
  twc_grid_classes,
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
)

# Validate ====================================================================

ggplot(
  twc_grid_classes,
  aes(x = lon, y = lat, fill = region)
) +
  geom_raster() +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "IPCC region"
  )


ggplot(
  twc_grid_classes,
  aes(x = lon, y = lat, fill = hemisphere)
) +
  geom_raster() +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Hemisphere"
  )


ggplot(
  twc_grid_classes,
  aes(x = lon, y = lat, fill = lat_zone)
) +
  geom_raster() +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Latitude zone"
  )


ggplot(
  twc_grid_classes,
  aes(x = lon, y = lat, fill = biome)
) +
  geom_raster() +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Biome"
  )


ggplot(
  twc_grid_classes,
  aes(x = lon, y = lat, fill = kg_class)
) +
  geom_raster() +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Köppen-Geiger class"
  )


ggplot(
  twc_grid_classes,
  aes(x = lon, y = lat, fill = kg_class_1)
) +
  geom_raster() +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Köppen-Geiger level 3"
  )


ggplot(
  twc_grid_classes,
  aes(x = lon, y = lat, fill = land_cover)
) +
  geom_raster() +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Land cover"
  )