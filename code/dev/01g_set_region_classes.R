# ============================================================================
# Build spatial classes for the complete TWC grid
#
# This script:
#   1. defines IPCC region level descriptive classes
#   2. derives hemisphere automatically from region extent
#   3. computes grid cell area weights
#   4. assigns IPCC region and biome to each grid cell
#   5. merges region level classes back to the grid
#
# Outputs:
#   - region_classes.Rds
#   - twc_complete_grid_classes.Rds
# ============================================================================

# Libraries ===================================================================

source("source/twc_change.R")
source("source/geo_functions.R")

# Input datasets ==============================================================

masks <- pRecipe::pRecipe_masks()

twc_grid <- readRDS(
  file.path(PATH_OUTPUT_DATA, "twc_complete_grid.Rds")
)

# Constants ===================================================================

EXCLUDED_REGIONS <- c("BOB", "ARS", "GIC")
CIRCULATION_LEVELS <- c(
  "polar",
  "stormtrack",
  "continental",
  "monsoon",
  "itcz_margin",
  "dry_subsidence"
)
LAT_ZONE_LEVELS <- c(
  "highlatitude",
  "midlatitude",
  "subtropical",
  "tropical"
)
CLIMATE_MAIN_LEVELS <- c(
  "cold",
  "temperate",
  "tropical",
  "monsoonal",
  "arid"
)
HYDROBELT_LEVELS <- c(
  "cold",
  "humid",
  "subhumid",
  "dry_subhumid",
  "monsoonal",
  "arid"
)
HEMISPHERE_LEVELS <- c("north", "tropics", "south")

# Helper functions ============================================================

set_factor_levels <- function(dt, column, levels_vec) {
  dt[, (column) := factor(get(column), levels = levels_vec)]
  dt
}

# Region level descriptors ====================================================

REGION_CLASS <- data.table(
  region = c(
    "ARP","CAF","CAR","CAU","CNA","EAS","EAU","ECA","EEU","ENA",
    "ESAF","ESB","GIC","MDG","MED","NAU","NCA","NEAF","NEN","NES",
    "NEU","NSA","NWN","NWS","NZ","RAR","RFE","SAH","SAM","SAS",
    "SAU","SCA","SEA","SEAF","SES","SSA","SWS","TIB","WAF","WCA",
    "WCE","WNA","WSAF","WSB"
  ),
  
  region_full = c(
    "Arabian Peninsula",
    "Central Africa",
    "Caribbean",
    "Central Australia",
    "Central N. America",
    "E. Asia",
    "E. Australia",
    "E. Central Asia",
    "E. Europe",
    "E. N. America",
    "E. Southern Africa",
    "E. Siberia",
    "Greenland/Iceland",
    "Madagascar",
    "Mediterranean",
    "N. Australia",
    "Central America",
    "NE. Africa",
    "NE. N. America",
    "NE. S. America",
    "N. Europe",
    "N. S. America",
    "NW. N. America",
    "NW. S. America",
    "New Zealand",
    "Russian Arctic",
    "Russian Far East",
    "Sahara",
    "S. American Monsoon",
    "S. Asia",
    "S. Australia",
    "S. Central America",
    "SE. Asia",
    "SE. Africa",
    "SE. S. America",
    "S. S. America",
    "SW. S. America",
    "Tibetan Plateau",
    "W. Africa",
    "W. Central Asia",
    "W. & Central Europe",
    "W. N. America",
    "W. Southern Africa",
    "W. Siberia"
  ),
  
  continent = c(
    "Asia","Africa","Central America","Australasia","N. America","Asia","Australasia","Asia","Europe","N. America",
    "Africa","Asia","Europe","Africa","Europe","Australasia","Central America","Africa","N. America","S. America",
    "Europe","S. America","N. America","S. America","Australasia","Asia","Asia","Africa","S. America","Asia",
    "Australasia","Central America","Asia","Africa","S. America","S. America","S. America","Asia","Africa","Asia",
    "Europe","N. America","Africa","Asia"
  ),
  
  circulation = c(
    "dry_subsidence","monsoon","itcz_margin","dry_subsidence","continental","monsoon",
    "itcz_margin","continental","continental","stormtrack","itcz_margin","continental",
    "stormtrack","itcz_margin","stormtrack","itcz_margin","itcz_margin","dry_subsidence",
    "stormtrack","itcz_margin","stormtrack","itcz_margin","stormtrack","itcz_margin",
    "stormtrack","continental","stormtrack","dry_subsidence","monsoon","monsoon",
    "stormtrack","itcz_margin","monsoon","itcz_margin","dry_subsidence","stormtrack",
    "dry_subsidence","continental","monsoon","continental","stormtrack","stormtrack",
    "dry_subsidence","continental"
  ),
  
  lat_zone = c(
    "subtropical","tropical","tropical","subtropical","midlatitude","midlatitude",
    "subtropical","midlatitude","midlatitude","midlatitude","tropical","highlatitude",
    "highlatitude","tropical","subtropical","tropical","tropical","subtropical",
    "highlatitude","tropical","highlatitude","tropical","highlatitude","tropical",
    "midlatitude","highlatitude","highlatitude","subtropical","tropical","subtropical",
    "midlatitude","tropical","tropical","tropical","subtropical","midlatitude",
    "subtropical","midlatitude","tropical","midlatitude","midlatitude","midlatitude",
    "subtropical","highlatitude"
  ),
  
  climate_main = c(
    "arid","monsoonal","tropical","arid","temperate","monsoonal",
    "temperate","arid","cold","temperate","tropical","cold",
    "cold","tropical","temperate","tropical","tropical","arid",
    "cold","tropical","cold","tropical","cold","tropical",
    "temperate","cold","cold","arid","monsoonal","monsoonal",
    "temperate","tropical","monsoonal","tropical","temperate","temperate",
    "arid","cold","monsoonal","arid","temperate","temperate",
    "arid","cold"
  ),
  
  hydrobelt = c(
    "arid","humid","humid","arid","humid","humid",
    "humid","arid","humid","humid","subhumid","cold",
    "cold","humid","dry_subhumid","subhumid","humid","arid",
    "cold","humid","humid","humid","cold","humid",
    "humid","cold","cold","arid","humid","monsoonal",
    "subhumid","humid","monsoonal","subhumid","subhumid","humid",
    "arid","cold","subhumid","arid","humid","humid",
    "arid","cold"
  )
)

# Derive hemisphere from actual IPCC region extent ============================

region_hemisphere <- masks[
  land_mask == "land" &
    !ipcc_short_region %like% "O$" &
    !ipcc_short_region %in% EXCLUDED_REGIONS,
  .(
    lat_min = min(lat, na.rm = TRUE),
    lat_max = max(lat, na.rm = TRUE)
  ),
  by = .(region = ipcc_short_region)
]

region_hemisphere[
  ,
  hemisphere := fifelse(
    lat_min < 0 & lat_max > 0,
    "tropics",
    fifelse(lat_max <= 0, "south", "north")
  )
]

REGION_CLASS <- merge(
  REGION_CLASS,
  region_hemisphere[, .(region, hemisphere)],
  by = "region",
  all.x = TRUE
)

# Apply factor levels =========================================================

REGION_CLASS <- set_factor_levels(REGION_CLASS, "hemisphere", HEMISPHERE_LEVELS)
REGION_CLASS <- set_factor_levels(REGION_CLASS, "circulation", CIRCULATION_LEVELS)
REGION_CLASS <- set_factor_levels(REGION_CLASS, "lat_zone", LAT_ZONE_LEVELS)
REGION_CLASS <- set_factor_levels(REGION_CLASS, "climate_main", CLIMATE_MAIN_LEVELS)
REGION_CLASS <- set_factor_levels(REGION_CLASS, "hydrobelt", HYDROBELT_LEVELS)

# Analysis ====================================================================

twc_grid <- grid_area(twc_grid)
twc_grid[, area_weight := area / sum(area, na.rm = TRUE)]

mask_classes <- masks[
  land_mask == "land",
  .(
    lon,
    lat,
    region = as.character(ipcc_short_region),
    biome = factor(biome_short_class)
  )
]

twc_grid_classes <- merge(
  twc_grid,
  mask_classes,
  by = c("lon", "lat"),
  all.x = TRUE
)

twc_grid_classes <- twc_grid_classes[
  !region %like% "O$" &
    !region %in% EXCLUDED_REGIONS
]

twc_grid_classes <- merge(
  twc_grid_classes,
  REGION_CLASS,
  by = "region",
  all.x = TRUE
)

region_class <- REGION_CLASS[region %in% unique(twc_grid_classes$region)]

setorder(
  region_class,
  hemisphere, lat_zone, climate_main, hydrobelt, circulation, continent
)

setcolorder(
  region_class,
  c(
    "region",
    "region_full",
    "hemisphere",
    "lat_zone",
    "climate_main",
    "hydrobelt",
    "circulation",
    "continent"
  )
)

# Outputs =====================================================================

saveRDS(
  REGION_CLASS,
  file.path(PATH_OUTPUT_DATA, "region_classes.Rds")
)

saveRDS(
  twc_grid_classes,
  file.path(PATH_OUTPUT_DATA, "twc_grid_classes.Rds")
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
  aes(x = lon, y = lat, fill = hydrobelt)
) +
  geom_raster() +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Hydrobelt"
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
  aes(x = lon, y = lat, fill = circulation)
) +
  geom_raster() +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Circulation patterns"
  )
