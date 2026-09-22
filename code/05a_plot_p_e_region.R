# ============================================================================
# Compare original datasets and Monte Carlo scenarios in P-E and ΔP-ΔE space
# for each IPCC region.
#
# 1. Aggregate region-biome annual P and E to IPCC regions using area weights
# 2. Estimate full-period mean P and E
# 3. Split the record into two equal periods and estimate:
#      ΔP = late mean P - early mean P
#      ΔE = late mean E - early mean E
# 4. Plot each IPCC region as a facet:
#      - Monte Carlo members as transparent grey clouds
#      - scenario centroids as colored, scenario-specific symbols
#      - original datasets as labelled open circles
#      - dataset centroid as a bold black X
# ============================================================================


# Libraries ==================================================================

source("code/_source.R")

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


prepare_region_biome_area <- function(grid_classes) {
  
  grid_dt <- as.data.table(
    copy(grid_classes)
  )
  
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
    by = c(
      "lon",
      "lat"
    )
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


aggregate_dataset_region_year <- function(
    dataset_region_biome_year,
    region_biome_area
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
  
  region_biome_area[
    dt,
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
      dataset,
      region,
      year
    )
  ]
}


aggregate_mc_region_year <- function(
    mc_region_biome_year,
    region_biome_area
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
  
  region_biome_area[
    dt,
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
  
  dt <- period_map[
    dt,
    on = "year",
    nomatch = 0L
  ]
  
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

region_biome_area <- prepare_region_biome_area(
  grid_classes
)


# Annual IPCC-region estimates ------------------------------------------------

dataset_region_year <- aggregate_dataset_region_year(
  dataset_region_biome_year,
  region_biome_area
)

mc_region_year <- aggregate_mc_region_year(
  mc_region_biome_year,
  region_biome_area
)


# P-E space ===================================================================


# Individual datasets ---------------------------------------------------------

dataset_region_mean <- dataset_region_year[
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
    region
  )
]


# Monte Carlo members ---------------------------------------------------------

mc_region_mean <- mc_region_year[
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
    region
  )
]


# Scenario centroids ----------------------------------------------------------

mc_region_mean_centroid <- mc_region_mean[
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
    region
  )
]


# Dataset centroid ------------------------------------------------------------

dataset_region_mean_centroid <- dataset_region_mean[
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
    region
  )
]


# ΔP-ΔE space =================================================================

common_years <- intersect(
  unique(dataset_region_year$year),
  unique(mc_region_year$year)
)

period_map <- make_equal_period_map(
  common_years
)


# Individual datasets ---------------------------------------------------------

dataset_region_change <- estimate_period_change(
  dt = dataset_region_year,
  group_cols = c(
    "dataset",
    "region"
  ),
  period_map = period_map
)


# Monte Carlo members ---------------------------------------------------------

mc_region_change <- estimate_period_change(
  dt = mc_region_year,
  group_cols = c(
    "sim",
    "scenario",
    "region"
  ),
  period_map = period_map
)


# Scenario centroids ----------------------------------------------------------

mc_region_change_centroid <- mc_region_change[
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
    region
  )
]


# Dataset centroid ------------------------------------------------------------

dataset_region_change_centroid <- dataset_region_change[
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
    region
  )
]


# Plot settings ===============================================================

region_levels <- sort(
  unique(
    dataset_region_mean$region
  )
)

scenario_levels <- sort(
  unique(
    mc_region_mean$scenario
  )
)


# Factors ---------------------------------------------------------------------

dataset_region_mean[
  ,
  region := factor(
    region,
    levels = region_levels
  )
]

dataset_region_mean_centroid[
  ,
  region := factor(
    region,
    levels = region_levels
  )
]

mc_region_mean[
  ,
  `:=`(
    region = factor(
      region,
      levels = region_levels
    ),
    scenario = factor(
      scenario,
      levels = scenario_levels
    )
  )
]

mc_region_mean_centroid[
  ,
  `:=`(
    region = factor(
      region,
      levels = region_levels
    ),
    scenario = factor(
      scenario,
      levels = scenario_levels
    )
  )
]

dataset_region_change[
  ,
  region := factor(
    region,
    levels = region_levels
  )
]

dataset_region_change_centroid[
  ,
  region := factor(
    region,
    levels = region_levels
  )
]

mc_region_change[
  ,
  `:=`(
    region = factor(
      region,
      levels = region_levels
    ),
    scenario = factor(
      scenario,
      levels = scenario_levels
    )
  )
]

mc_region_change_centroid[
  ,
  `:=`(
    region = factor(
      region,
      levels = region_levels
    ),
    scenario = factor(
      scenario,
      levels = scenario_levels
    )
  )
]


# Scenario symbols ------------------------------------------------------------

available_shapes <- c(
  16,
  17,
  15,
  18,
  3,
  4,
  7,
  8,
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

p_pe_region <- ggplot() +
  
  # Monte Carlo cloud
  geom_point(
    data = mc_region_mean,
    aes(
      x = prec,
      y = evap
    ),
    colour = "grey40",
    alpha = 0.12,
    size = 0.8
  ) +
  
  # Scenario centroids
  geom_point(
    data = mc_region_mean_centroid,
    aes(
      x = prec,
      y = evap,
      colour = scenario,
      shape = scenario
    ),
    size = 3,
    stroke = 1
  ) +
  
  # Individual datasets
  geom_point(
    data = dataset_region_mean,
    aes(
      x = prec,
      y = evap
    ),
    shape = 21,
    fill = "white",
    colour = "black",
    size = 2.2,
    stroke = 0.7
  ) +
  
  # Dataset labels
  geom_text_repel(
    data = dataset_region_mean,
    aes(
      x = prec,
      y = evap,
      label = dataset
    ),
    size = 2.2,
    colour = "black",
    box.padding = 0.15,
    point.padding = 0.1,
    segment.alpha = 0.4,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  # Dataset centroid
  geom_point(
    data = dataset_region_mean_centroid,
    aes(
      x = prec,
      y = evap
    ),
    shape = 4,
    colour = "black",
    size = 4.5,
    stroke = 1.6
  ) +
  
  facet_wrap(
    ~ region,
    ncol = 5,
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
    title = "Dataset estimates and Monte Carlo scenarios in P-E space",
    subtitle = "IPCC regions"
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(
        size = 3,
        alpha = 1
      )
    ),
    shape = guide_legend(
      override.aes = list(
        size = 3,
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
    ),
    strip.text = element_text(
      size = 8
    )
  )


# ΔP-ΔE plot ==================================================================

p_delta_pe_region <- ggplot() +
  
  # Zero-change lines
  geom_hline(
    yintercept = 0,
    colour = "grey60",
    linewidth = 0.3
  ) +
  
  geom_vline(
    xintercept = 0,
    colour = "grey60",
    linewidth = 0.3
  ) +
  
  # Monte Carlo cloud
  geom_point(
    data = mc_region_change,
    aes(
      x = dprec,
      y = devap
    ),
    colour = "grey40",
    alpha = 0.12,
    size = 0.8
  ) +
  
  # Scenario centroids
  geom_point(
    data = mc_region_change_centroid,
    aes(
      x = dprec,
      y = devap,
      colour = scenario,
      shape = scenario
    ),
    size = 3,
    stroke = 1
  ) +
  
  # Individual datasets
  geom_point(
    data = dataset_region_change,
    aes(
      x = dprec,
      y = devap
    ),
    shape = 21,
    fill = "white",
    colour = "black",
    size = 2.2,
    stroke = 0.7
  ) +
  
  # Dataset labels
  geom_text_repel(
    data = dataset_region_change,
    aes(
      x = dprec,
      y = devap,
      label = dataset
    ),
    size = 2.2,
    colour = "black",
    box.padding = 0.15,
    point.padding = 0.1,
    segment.alpha = 0.4,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  # Dataset centroid
  geom_point(
    data = dataset_region_change_centroid,
    aes(
      x = dprec,
      y = devap
    ),
    shape = 4,
    colour = "black",
    size = 4.5,
    stroke = 1.6
  ) +
  
  facet_wrap(
    ~ region,
    ncol = 5,
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
    title = "Dataset estimates and Monte Carlo scenarios in ΔP-ΔE space",
    subtitle = "IPCC regions; change = late-period mean - early-period mean"
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(
        size = 3,
        alpha = 1
      )
    ),
    shape = guide_legend(
      override.aes = list(
        size = 3,
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
    ),
    strip.text = element_text(
      size = 8
    )
  )


# Outputs =====================================================================

ggsave(
  filename = file.path(
    PATH_OUTPUT_OUTPUT,
    "pe_dataset_vs_scenarios_ipcc_regions.png"
  ),
  plot = p_pe_region,
  width = 16,
  height = 13,
  dpi = 300
)

ggsave(
  filename = file.path(
    PATH_OUTPUT_OUTPUT,
    "delta_pe_dataset_vs_scenarios_ipcc_regions.png"
  ),
  plot = p_delta_pe_region,
  width = 16,
  height = 13,
  dpi = 300
)


# Save derived tables ---------------------------------------------------------

saveRDS(
  dataset_region_mean,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_region_mean.Rds"
  )
)

saveRDS(
  mc_region_mean,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_region_mean_scenarios.Rds"
  )
)

saveRDS(
  dataset_region_change,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_region_change.Rds"
  )
)

saveRDS(
  mc_region_change,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_region_change_scenarios.Rds"
  )
)


# Validation ==================================================================

stopifnot(nrow(dataset_region_mean) > 0)
stopifnot(nrow(mc_region_mean) > 0)
stopifnot(nrow(dataset_region_change) > 0)
stopifnot(nrow(mc_region_change) > 0)

# Summary =====================================================================

cat(
  "\nFinished IPCC-region P-E and ΔP-ΔE comparison.\n"
)

cat(
  "\nNumber of IPCC regions: ",
  uniqueN(dataset_region_mean$region),
  "\n",
  sep = ""
)