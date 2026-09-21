# ============================================================================
# Aggregate the 100 x 10 scenario Monte Carlo selections to annual P and E
# ensembles.
#
# 1. Join selected datasets to dataset-region-biome-year P and E
# 2. Aggregate region-biome values to IPCC regions using area weights
# 3. Aggregate directly from region-biome values to global annual P and E
#
# The separate 1000-member change ensemble for the other paper is not used here.
# ============================================================================


# Libraries ==================================================================

source("code/_source.R")


# Inputs ======================================================================

dataset_region_biome_year <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "dataset_region_biome_year.Rds")
)

grid_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
)

mc_selection <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "mc_selection_scenarios.Rds")
)


# Functions ===================================================================

prepare_region_biome_area <- function(grid_classes) {
  
  grid_dt <- as.data.table(copy(grid_classes))
  
  # Use existing cell-area information where available; otherwise use the
  # latitude-dependent relative cell area.
  if ("cell_weight" %in% names(grid_dt)) {
    
    grid_dt[, cell_area_weight := cell_weight]
    
  } else {
    
    grid_dt[, cell_area_weight := cos(lat * pi / 180)]
  }
  
  unique(
    grid_dt[
      !is.na(region) &
        !is.na(biome) &
        is.finite(cell_area_weight) &
        cell_area_weight > 0,
      .(
        lon,
        lat,
        region = as.character(region),
        biome = as.character(biome),
        cell_area_weight
      )
    ],
    by = c("lon", "lat")
  )[
    ,
    .(
      area_weight = sum(cell_area_weight)
    ),
    by = .(
      region,
      biome
    )
  ]
}


weighted_mean_safe <- function(value, weight) {
  
  ok <- is.finite(value) &
    is.finite(weight) &
    weight > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(value[ok] * weight[ok]) / sum(weight[ok])
}


make_mc_region_biome_year <- function(
    dataset_region_biome_year,
    mc_selection
) {
  
  dt <- as.data.table(copy(dataset_region_biome_year))
  sel <- as.data.table(copy(mc_selection))
  
  dt[
    ,
    `:=`(
      dataset = as.character(dataset),
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  sel[
    ,
    `:=`(
      scenario = as.character(scenario),
      region = as.character(region),
      biome = as.character(biome),
      dataset = as.character(dataset)
    )
  ]
  
  dt[
    sel,
    on = .(
      dataset,
      region,
      biome
    ),
    allow.cartesian = TRUE,
    nomatch = 0L
  ][
    ,
    .(
      sim,
      scenario,
      region,
      biome,
      dataset,
      year,
      prec,
      evap
    )
  ]
}


make_mc_region_year <- function(
    mc_region_biome_year,
    region_biome_area
) {
  
  region_biome_area[
    mc_region_biome_year,
    on = .(
      region,
      biome
    )
  ][
    ,
    .(
      prec = weighted_mean_safe(
        prec,
        area_weight
      ),
      evap = weighted_mean_safe(
        evap,
        area_weight
      )
    ),
    by = .(
      sim,
      scenario,
      region,
      year
    )
  ]
}


make_mc_global_year <- function(
    mc_region_biome_year,
    region_biome_area
) {
  
  # Aggregate directly from region-biome values so regional averages are not
  # averaged a second time.
  
  region_biome_area[
    mc_region_biome_year,
    on = .(
      region,
      biome
    )
  ][
    ,
    .(
      prec = weighted_mean_safe(
        prec,
        area_weight
      ),
      evap = weighted_mean_safe(
        evap,
        area_weight
      )
    ),
    by = .(
      sim,
      scenario,
      year
    )
  ]
}


# Analysis ====================================================================

region_biome_area <- prepare_region_biome_area(
  grid_classes
)

mc_region_biome_year <- make_mc_region_biome_year(
  dataset_region_biome_year,
  mc_selection
)

mc_region_year <- make_mc_region_year(
  mc_region_biome_year,
  region_biome_area
)

mc_global_year <- make_mc_global_year(
  mc_region_biome_year,
  region_biome_area
)


# Outputs =====================================================================

write_fst(
  mc_region_biome_year,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_region_biome_year_scenarios.fst"
  )
)

saveRDS(
  mc_region_year,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_region_year_scenarios.Rds"
  )
)

saveRDS(
  mc_global_year,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_global_year_scenarios.Rds"
  )
)


# Summary =====================================================================

cat(
  "\nFinished 100 x 10 Monte Carlo scenario aggregation.\n"
)

cat(
  "Simulations: ",
  uniqueN(mc_selection$sim),
  "\nScenarios: ",
  uniqueN(mc_selection$scenario),
  "\n",
  sep = ""
)

print(
  data.table(
    output = c(
      "region_biome_year",
      "region_year",
      "global_year"
    ),
    n_rows = c(
      nrow(mc_region_biome_year),
      nrow(mc_region_year),
      nrow(mc_global_year)
    )
  )
)
