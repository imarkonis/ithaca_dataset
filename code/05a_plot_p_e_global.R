# ============================================================================
# Compare original datasets and Monte Carlo scenarios in P-E and ΔP-ΔE space
# for Global, Northern Hemisphere, and Southern Hemisphere.
#
# 1. Aggregate dataset and Monte Carlo region-biome estimates to:
#      - Global
#      - Northern Hemisphere
#      - Southern Hemisphere
# 2. Estimate full-period mean P and E
# 3. Split the record into two equal periods and estimate:
#      ΔP = late mean P - early mean P
#      ΔE = late mean E - early mean E
# 4. Plot:
#      - Monte Carlo members as transparent grey clouds
#      - scenario centroids as colored, scenario-specific symbols
#      - original datasets as labelled open circles
#      - dataset centroid as a bold black X
# ============================================================================


# Libraries ==================================================================

source("code/_source.R")

library(data.table)
library(fst)
library(ggplot2)
library(ggrepel)


# Inputs ======================================================================

dataset_region_biome_year <- readRDS(
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_region_biome_year.Rds"
  )
)

grid_classes <- readRDS(
  file.path(
    PATH_OUTPUT_OUTPUT,
    "grid_classes.Rds"
  )
)

mc_region_biome_year <- read_fst(
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_region_biome_year_scenarios.fst"
  )
)


# Functions ===================================================================

weighted_mean_safe <- function(value, weight) {
  
  ok <- is.finite(value) &
    is.finite(weight) &
    weight > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(value[ok] * weight[ok]) / sum(weight[ok])
}


prepare_region_biome_scope_area <- function(grid_classes) {
  
  grid_dt <- as.data.table(copy(grid_classes))
  
  # Use existing cell-area information where available; otherwise use
  # latitude-dependent relative cell area.
  if ("cell_weight" %in% names(grid_dt)) {
    
    grid_dt[, cell_area_weight := cell_weight]
    
  } else {
    
    grid_dt[, cell_area_weight := cos(lat * pi / 180)]
  }
  
  grid_dt <- unique(
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
    by = c(
      "lon",
      "lat"
    )
  )
  
  
  # Hemisphere areas ----------------------------------------------------------
  
  hemisphere_area <- grid_dt[
    ,
    .(
      area_weight = sum(cell_area_weight)
    ),
    by = .(
      scope = fifelse(
        lat >= 0,
        "Northern Hemisphere",
        "Southern Hemisphere"
      ),
      region,
      biome
    )
  ]
  
  
  # Global areas --------------------------------------------------------------
  
  global_area <- grid_dt[
    ,
    .(
      area_weight = sum(cell_area_weight)
    ),
    by = .(
      region,
      biome
    )
  ][
    ,
    scope := "Global"
  ]
  
  
  rbindlist(
    list(
      global_area,
      hemisphere_area
    ),
    use.names = TRUE
  )[
    ,
    .(
      scope,
      region,
      biome,
      area_weight
    )
  ]
}


aggregate_dataset_scope_year <- function(
    dataset_region_biome_year,
    region_biome_scope_area
) {
  
  dt <- as.data.table(
    copy(dataset_region_biome_year)
  )
  
  dt[
    ,
    `:=`(
      dataset = as.character(dataset),
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  region_biome_scope_area[
    dt,
    on = .(
      region,
      biome
    ),
    allow.cartesian = TRUE
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
      dataset,
      scope,
      year
    )
  ]
}


aggregate_mc_scope_year <- function(
    mc_region_biome_year,
    region_biome_scope_area
) {
  
  dt <- as.data.table(
    copy(mc_region_biome_year)
  )
  
  dt[
    ,
    `:=`(
      scenario = as.character(scenario),
      region = as.character(region),
      biome = as.character(biome)
    )
  ]
  
  region_biome_scope_area[
    dt,
    on = .(
      region,
      biome
    ),
    allow.cartesian = TRUE
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
      scope,
      year
    )
  ]
}


make_equal_period_map <- function(years) {
  
  years <- sort(
    unique(
      years[
        is.finite(years)
      ]
    )
  )
  
  n_half <- floor(
    length(years) / 2
  )
  
  if (n_half < 1) {
    stop("Not enough years to split into two periods.")
  }
  
  early_years <- head(
    years,
    n_half
  )
  
  late_years <- tail(
    years,
    n_half
  )
  
  dropped_years <- setdiff(
    years,
    c(
      early_years,
      late_years
    )
  )
  
  cat(
    "\nEarly period: ",
    min(early_years),
    "-",
    max(early_years),
    " (",
    length(early_years),
    " years)\n",
    sep = ""
  )
  
  cat(
    "Late period: ",
    min(late_years),
    "-",
    max(late_years),
    " (",
    length(late_years),
    " years)\n",
    sep = ""
  )
  
  if (length(dropped_years) > 0) {
    
    cat(
      "Dropped middle year(s): ",
      paste(
        dropped_years,
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
  }
  
  rbind(
    data.table(
      year = early_years,
      period = "early"
    ),
    data.table(
      year = late_years,
      period = "late"
    )
  )
}


estimate_period_change <- function(
    dt,
    group_cols,
    period_map
) {
  
  dt <- as.data.table(
    copy(dt)
  )
  
  # Restrict to years used in the two equal periods.
  dt <- period_map[
    dt,
    on = "year",
    nomatch = 0L
  ]
  
  # Mean P and E within each period.
  period_mean <- dt[
    ,
    .(
      prec = mean(
        prec,
        na.rm = TRUE
      ),
      evap = mean(
        evap,
        na.rm = TRUE
      )
    ),
    by = c(
      group_cols,
      "period"
    )
  ]
  
  # Reshape early and late means.
  wide <- dcast(
    period_mean,
    formula = as.formula(
      paste(
        paste(
          group_cols,
          collapse = " + "
        ),
        "~ period"
      )
    ),
    value.var = c(
      "prec",
      "evap"
    )
  )
  
  # Late minus early.
  wide[
    ,
    `:=`(
      dprec = prec_late - prec_early,
      devap = evap_late - evap_early
    )
  ]
  
  wide[]
}


# Analysis ====================================================================


# Area weights ----------------------------------------------------------------

region_biome_scope_area <- prepare_region_biome_scope_area(
  grid_classes
)


# Annual P and E --------------------------------------------------------------

dataset_scope_year <- aggregate_dataset_scope_year(
  dataset_region_biome_year,
  region_biome_scope_area
)

mc_scope_year <- aggregate_mc_scope_year(
  mc_region_biome_year,
  region_biome_scope_area
)


# Full-period mean P and E =====================================================


# Individual datasets ---------------------------------------------------------

dataset_scope_mean <- dataset_scope_year[
  ,
  .(
    prec = mean(
      prec,
      na.rm = TRUE
    ),
    evap = mean(
      evap,
      na.rm = TRUE
    )
  ),
  by = .(
    dataset,
    scope
  )
]


# Monte Carlo members ---------------------------------------------------------

mc_scope_mean <- mc_scope_year[
  ,
  .(
    prec = mean(
      prec,
      na.rm = TRUE
    ),
    evap = mean(
      evap,
      na.rm = TRUE
    )
  ),
  by = .(
    sim,
    scenario,
    scope
  )
]


# Scenario centroids ----------------------------------------------------------

mc_scope_mean_centroid <- mc_scope_mean[
  ,
  .(
    prec = mean(
      prec,
      na.rm = TRUE
    ),
    evap = mean(
      evap,
      na.rm = TRUE
    )
  ),
  by = .(
    scenario,
    scope
  )
]


# Dataset centroid ------------------------------------------------------------

dataset_scope_mean_centroid <- dataset_scope_mean[
  ,
  .(
    prec = mean(
      prec,
      na.rm = TRUE
    ),
    evap = mean(
      evap,
      na.rm = TRUE
    )
  ),
  by = .(
    scope
  )
]


# Two-period ΔP and ΔE =========================================================

common_years <- intersect(
  unique(dataset_scope_year$year),
  unique(mc_scope_year$year)
)

period_map <- make_equal_period_map(
  common_years
)


# Individual datasets ---------------------------------------------------------

dataset_scope_change <- estimate_period_change(
  dt = dataset_scope_year,
  group_cols = c(
    "dataset",
    "scope"
  ),
  period_map = period_map
)


# Monte Carlo members ---------------------------------------------------------

mc_scope_change <- estimate_period_change(
  dt = mc_scope_year,
  group_cols = c(
    "sim",
    "scenario",
    "scope"
  ),
  period_map = period_map
)


# Scenario centroids ----------------------------------------------------------

mc_scope_change_centroid <- mc_scope_change[
  ,
  .(
    dprec = mean(
      dprec,
      na.rm = TRUE
    ),
    devap = mean(
      devap,
      na.rm = TRUE
    )
  ),
  by = .(
    scenario,
    scope
  )
]


# Dataset centroid ------------------------------------------------------------

dataset_scope_change_centroid <- dataset_scope_change[
  ,
  .(
    dprec = mean(
      dprec,
      na.rm = TRUE
    ),
    devap = mean(
      devap,
      na.rm = TRUE
    )
  ),
  by = .(
    scope
  )
]


# Plot settings ===============================================================

scope_levels <- c(
  "Global",
  "Northern Hemisphere",
  "Southern Hemisphere"
)

scenario_levels <- sort(
  unique(
    mc_scope_mean$scenario
  )
)


# Apply consistent factor ordering --------------------------------------------

dataset_scope_mean[
  ,
  scope := factor(
    scope,
    levels = scope_levels
  )
]

dataset_scope_mean_centroid[
  ,
  scope := factor(
    scope,
    levels = scope_levels
  )
]

mc_scope_mean[
  ,
  `:=`(
    scope = factor(
      scope,
      levels = scope_levels
    ),
    scenario = factor(
      scenario,
      levels = scenario_levels
    )
  )
]

mc_scope_mean_centroid[
  ,
  `:=`(
    scope = factor(
      scope,
      levels = scope_levels
    ),
    scenario = factor(
      scenario,
      levels = scenario_levels
    )
  )
]

dataset_scope_change[
  ,
  scope := factor(
    scope,
    levels = scope_levels
  )
]

dataset_scope_change_centroid[
  ,
  scope := factor(
    scope,
    levels = scope_levels
  )
]

mc_scope_change[
  ,
  `:=`(
    scope = factor(
      scope,
      levels = scope_levels
    ),
    scenario = factor(
      scenario,
      levels = scenario_levels
    )
  )
]

mc_scope_change_centroid[
  ,
  `:=`(
    scope = factor(
      scope,
      levels = scope_levels
    ),
    scenario = factor(
      scenario,
      levels = scenario_levels
    )
  )
]


# Scenario symbols ------------------------------------------------------------
#
# Explicit values avoid ggplot's default six-shape limit.

available_shapes <- c(
  16,  # circle
  17,  # triangle
  15,  # square
  18,  # diamond
  3,   # plus
  4,   # cross
  7,
  8,   # star
  9,
  10,
  11,
  12,
  13,
  14
)

if (length(scenario_levels) > length(available_shapes)) {
  stop("More scenarios than available manual shapes.")
}

scenario_shapes <- setNames(
  available_shapes[
    seq_along(scenario_levels)
  ],
  scenario_levels
)


# P-E plot ====================================================================

p_pe <- ggplot() +
  
  # Monte Carlo cloud
  geom_point(
    data = mc_scope_mean,
    aes(
      x = prec,
      y = evap
    ),
    colour = "grey40",
    alpha = 0.15,
    size = 1.5
  ) +
  
  # Scenario centroids
  geom_point(
    data = mc_scope_mean_centroid,
    aes(
      x = prec,
      y = evap,
      colour = scenario,
      shape = scenario
    ),
    size = 4,
    stroke = 1.1
  ) +
  
  # Individual datasets
  geom_point(
    data = dataset_scope_mean,
    aes(
      x = prec,
      y = evap
    ),
    shape = 21,
    fill = "white",
    colour = "black",
    size = 3,
    stroke = 0.8
  ) +
  
  # Dataset labels
  geom_text_repel(
    data = dataset_scope_mean,
    aes(
      x = prec,
      y = evap,
      label = dataset
    ),
    size = 3,
    colour = "black",
    box.padding = 0.25,
    point.padding = 0.2,
    segment.alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Dataset centroid
  geom_point(
    data = dataset_scope_mean_centroid,
    aes(
      x = prec,
      y = evap
    ),
    shape = 4,
    colour = "black",
    size = 6,
    stroke = 2
  ) +
  
  facet_wrap(
    ~ scope,
    nrow = 1,
    scales = "free"
  ) +
  
  scale_shape_manual(
    values = scenario_shapes
  ) +
  
  labs(
    x = "Precipitation (P)",
    y = "Evaporation (E)",
    colour = "Scenario",
    shape = "Scenario",
    title = "Dataset estimates and Monte Carlo scenarios in P-E space"
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(
        size = 3.5,
        alpha = 1
      )
    ),
    shape = guide_legend(
      override.aes = list(
        size = 3.5,
        alpha = 1
      )
    )
  ) +
  
  theme_bw() +
  
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(
      fill = "grey95"
    )
  )


# ΔP-ΔE plot ==================================================================

p_delta_pe <- ggplot() +
  
  # Zero-change reference lines
  geom_hline(
    yintercept = 0,
    colour = "grey60",
    linewidth = 0.4
  ) +
  
  geom_vline(
    xintercept = 0,
    colour = "grey60",
    linewidth = 0.4
  ) +
  
  # Monte Carlo cloud
  geom_point(
    data = mc_scope_change,
    aes(
      x = dprec,
      y = devap
    ),
    colour = "grey40",
    alpha = 0.15,
    size = 1.5
  ) +
  
  # Scenario centroids
  geom_point(
    data = mc_scope_change_centroid,
    aes(
      x = dprec,
      y = devap,
      colour = scenario,
      shape = scenario
    ),
    size = 4,
    stroke = 1.1
  ) +
  
  # Individual datasets
  geom_point(
    data = dataset_scope_change,
    aes(
      x = dprec,
      y = devap
    ),
    shape = 21,
    fill = "white",
    colour = "black",
    size = 3,
    stroke = 0.8
  ) +
  
  # Dataset labels
  geom_text_repel(
    data = dataset_scope_change,
    aes(
      x = dprec,
      y = devap,
      label = dataset
    ),
    size = 3,
    colour = "black",
    box.padding = 0.25,
    point.padding = 0.2,
    segment.alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Dataset centroid
  geom_point(
    data = dataset_scope_change_centroid,
    aes(
      x = dprec,
      y = devap
    ),
    shape = 4,
    colour = "black",
    size = 6,
    stroke = 2
  ) +
  
  facet_wrap(
    ~ scope,
    nrow = 1,
    scales = "free"
  ) +
  
  scale_shape_manual(
    values = scenario_shapes
  ) +
  
  labs(
    x = expression(Delta * "P"),
    y = expression(Delta * "E"),
    colour = "Scenario",
    shape = "Scenario",
    title = "Dataset estimates and Monte Carlo scenarios in \u0394P-\u0394E space",
    subtitle = "Change = late-period mean - early-period mean"
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(
        size = 3.5,
        alpha = 1
      )
    ),
    shape = guide_legend(
      override.aes = list(
        size = 3.5,
        alpha = 1
      )
    )
  ) +
  
  theme_bw() +
  
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(
      fill = "grey95"
    )
  )


# Outputs =====================================================================

ggsave(
  filename = file.path(
    PATH_OUTPUT_OUTPUT,
    "pe_dataset_vs_scenarios.png"
  ),
  plot = p_pe,
  width = 13,
  height = 4.8,
  dpi = 300
)

ggsave(
  filename = file.path(
    PATH_OUTPUT_OUTPUT,
    "delta_pe_dataset_vs_scenarios.png"
  ),
  plot = p_delta_pe,
  width = 13,
  height = 4.8,
  dpi = 300
)


# Save derived tables ---------------------------------------------------------

saveRDS(
  dataset_scope_mean,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_scope_mean.Rds"
  )
)

saveRDS(
  mc_scope_mean,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_scope_mean_scenarios.Rds"
  )
)

saveRDS(
  dataset_scope_change,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_scope_change.Rds"
  )
)

saveRDS(
  mc_scope_change,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_scope_change_scenarios.Rds"
  )
)

saveRDS(
  mc_scope_mean_centroid,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_scope_mean_centroids.Rds"
  )
)

saveRDS(
  mc_scope_change_centroid,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_scope_change_centroids.Rds"
  )
)


# Validation ==================================================================

stopifnot(nrow(dataset_scope_mean) > 0)
stopifnot(nrow(mc_scope_mean) > 0)
stopifnot(nrow(dataset_scope_change) > 0)
stopifnot(nrow(mc_scope_change) > 0)

# Summary =====================================================================

cat(
  "\nFinished P-E and ΔP-ΔE scenario comparison.\n"
)

cat(
  "\nDataset centroids in P-E space:\n"
)

print(
  dataset_scope_mean_centroid
)

cat(
  "\nScenario centroids in P-E space:\n"
)

print(
  mc_scope_mean_centroid[
    order(
      scope,
      scenario
    )
  ]
)

cat(
  "\nDataset centroids in ΔP-ΔE space:\n"
)

print(
  dataset_scope_change_centroid
)

cat(
  "\nScenario centroids in ΔP-ΔE space:\n"
)

print(
  mc_scope_change_centroid[
    order(
      scope,
      scenario
    )
  ]
)