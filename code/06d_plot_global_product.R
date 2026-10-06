# ============================================================================
# Figure 5: global product. What does the resulting P-E ensemble look like?
#
# (a) global annual precipitation 1982-2021, (b) global annual evaporation, both
# built the same way, and (c) the long-term mean P-E level space for global,
# northern-hemisphere and southern-hemisphere land.
#
# Layers of (a) and (b), back to front:
#   - range over the eight weighting scenarios of the DETERMINISTIC scenario
#     means (the expectation of each scenario's Monte Carlo draw: probability-
#     weighted sum of the five dataset series), light band;
#   - base Monte Carlo: 5th-95th percentile envelope over the 100 members, dark
#     band, and its median;
#   - the five coherent candidate series, thin lines;
#   - the arithmetic mean of the five candidates, dashed.
# Weighting-scenario uncertainty and Monte Carlo dataset-choice uncertainty are
# never pooled: they are separate layers.
#
# Panel (c) shows, per geography, the five coherent (P, E) candidate pairs, their
# arithmetic mean, the base Monte Carlo cloud with its median and a 5th-95th
# percentile cross.
#
# No new statistics are estimated. Re-used files:
#   dataset_region_biome_year.Rds (03g), weights_region_biome.Rds (03f),
#   grid_classes.Rds (01g), mc_global_year_scenarios.Rds (04b) and, for the
#   hemisphere Monte Carlo means, mc_scope_mean_scenarios.Rds (05a_global).
# If that 05a file does not exist yet, the same quantity is aggregated from
# mc_region_biome_year_scenarios.fst (04b); this is slower and needs more memory.
#
# All dataset-based values use the same land mask as the Monte Carlo product: the
# region x biome units that have weights (units emptied by the physics gate are
# left out, 0.01 % of land area in the current run).
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")

library(ggrepel)

# Inputs =====================================================================

dataset_region_biome_year <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "dataset_region_biome_year.Rds")
)

weights_region_biome <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "weights_region_biome.Rds")
)

grid_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
)

mc_global_year <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "mc_global_year_scenarios.Rds")
)

# Constants & Variables ======================================================

BASE_SCENARIO <- "base"

# Scenarios whose deterministic means form the weighting-scenario band.
ENVELOPE_SCENARIOS <- SCENARIO_ORDER

MC_PROBS <- c(0.05, 0.5, 0.95)

SCOPES <- c("Global", "Northern Hemisphere", "Southern Hemisphere")

# Series names used for the legend.
NAME_MEAN   <- "Arithmetic mean"
NAME_MEDIAN <- "ITHACA base, median"
NAME_BAND_MC <- "Base Monte Carlo, 5-95 %"
NAME_BAND_SCENARIOS <- "Weighting scenarios, range"

COL_ITHACA <- "#08306b"
COL_SCENARIO_BAND <- "grey70"

# Functions ==================================================================

# Area-weighted mean over region x biome units, by the columns in `by_cols`,
# scope and year. `scope_area` holds one area per unit and scope.
aggregate_scopes <- function(values, scope_area, by_cols) {
  merged <- merge(
    values,
    scope_area,
    by = c("region", "biome"),
    allow.cartesian = TRUE
  )

  merged[
    ,
    .(
      prec = weighted_mean_safe(prec, area),
      evap = weighted_mean_safe(evap, area)
    ),
    by = c(by_cols, "scope", "year")
  ]
}

# Probability-weighted dataset series: the expectation of the Monte Carlo draw.
scenario_expectation <- function(weights, dataset_values) {
  merge(
    weights,
    dataset_values,
    by = c("region", "biome", "dataset"),
    allow.cartesian = TRUE
  )[
    ,
    .(
      prec = sum(w_region_biome * prec),
      evap = sum(w_region_biome * evap)
    ),
    by = .(scenario, region, biome, year)
  ]
}

# Long-term (all years) mean of a yearly series, by the columns in `by_cols`.
long_term_mean <- function(series, by_cols) {
  series[
    ,
    .(prec = mean(prec), evap = mean(evap)),
    by = by_cols
  ]
}

# Base Monte Carlo long-term mean per member and scope.
mc_scope_means <- function(scope_area) {
  scope_file <- file.path(PATH_OUTPUT_OUTPUT, "mc_scope_mean_scenarios.Rds")

  if (file.exists(scope_file)) {
    message("Hemisphere Monte Carlo means: ", scope_file)

    out <- as.data.table(readRDS(scope_file))
    out <- out[scenario == BASE_SCENARIO]
    out[, scope := as.character(scope)]

    return(out[, .(sim, scope, prec, evap)])
  }

  message(
    "mc_scope_mean_scenarios.Rds not found (run 05a_plot_p_e_global.R to create it): ",
    "aggregating mc_region_biome_year_scenarios.fst instead."
  )

  members <- read_fst(
    file.path(PATH_OUTPUT_OUTPUT, "mc_region_biome_year_scenarios.fst"),
    columns = c("sim", "scenario", "region", "biome", "year", "prec", "evap"),
    as.data.table = TRUE
  )

  members <- members[scenario == BASE_SCENARIO]

  long_term_mean(
    aggregate_scopes(members, scope_area, by_cols = "sim"),
    by_cols = c("sim", "scope")
  )
}

# Percentiles of one column by the grouping columns.
percentile_table <- function(dt, column, by_cols) {
  dt[
    ,
    {
      q <- quantile(get(column), MC_PROBS, names = FALSE)
      .(q05 = q[1], q50 = q[2], q95 = q[3])
    },
    by = by_cols
  ]
}

# Time-series panel for one variable ("prec" or "evap").
series_panel <- function(variable, y_label, title) {
  candidates <- dataset_year[
    scope == "Global",
    .(year, series = dataset, value = get(variable))
  ]

  mean_line <- mean_year[
    scope == "Global",
    .(year, series = NAME_MEAN, value = get(variable))
  ]

  mc_band <- mc_year_percentiles[[variable]]

  mc_median <- mc_band[, .(year, series = NAME_MEDIAN, value = q50)]

  scenario_band <- scenario_range[[variable]]

  lines_dt <- rbindlist(list(candidates, mean_line, mc_median))
  lines_dt[, series := factor(series, levels = c(CANDIDATES, NAME_MEAN, NAME_MEDIAN))]

  ggplot() +
    geom_ribbon(
      data = scenario_band,
      aes(x = year, ymin = lo, ymax = hi, fill = NAME_BAND_SCENARIOS)
    ) +
    geom_ribbon(
      data = mc_band,
      aes(x = year, ymin = q05, ymax = q95, fill = NAME_BAND_MC)
    ) +
    geom_line(
      data = lines_dt,
      aes(x = year, y = value, colour = series, linetype = series, linewidth = series)
    ) +
    scale_colour_manual(name = NULL, values = series_colours, breaks = names(series_colours)) +
    scale_linetype_manual(name = NULL, values = series_linetypes, breaks = names(series_colours)) +
    scale_linewidth_manual(name = NULL, values = series_linewidths, breaks = names(series_colours)) +
    scale_fill_manual(
      name = NULL,
      values = setNames(
        c(scales::alpha(COL_SCENARIO_BAND, 0.55), scales::alpha(COL_ITHACA, 0.30)),
        c(NAME_BAND_SCENARIOS, NAME_BAND_MC)
      ),
      breaks = c(NAME_BAND_MC, NAME_BAND_SCENARIOS)
    ) +
    scale_x_continuous(breaks = seq(1985, 2015, by = 10), limits = year_limits, expand = c(0.01, 0)) +
    labs(x = "Year", y = y_label, title = title) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 9),
      legend.position = "none"
    )
}

# Level-space panel for one geography.
level_panel <- function(scope_name) {
  candidates <- dataset_levels[scope == scope_name]
  mean_point <- mean_levels[scope == scope_name]
  cloud <- mc_members[scope == scope_name]
  central <- mc_levels[scope == scope_name]

  ggplot() +
    geom_point(data = cloud, aes(x = prec, y = evap), colour = COL_ITHACA, alpha = 0.18, size = 0.7) +
    geom_segment(
      data = central,
      aes(x = prec_q05, xend = prec_q95, y = evap_q50, yend = evap_q50),
      colour = COL_ITHACA, linewidth = 0.5
    ) +
    geom_segment(
      data = central,
      aes(x = prec_q50, xend = prec_q50, y = evap_q05, yend = evap_q95),
      colour = COL_ITHACA, linewidth = 0.5
    ) +
    geom_point(
      data = central, aes(x = prec_q50, y = evap_q50, shape = NAME_MEDIAN),
      fill = COL_ITHACA, colour = "white", size = 3.2, stroke = 0.6
    ) +
    geom_point(
      data = candidates, aes(x = prec, y = evap, colour = dataset),
      shape = 16, size = 2.4
    ) +
    geom_text_repel(
      data = candidates, aes(x = prec, y = evap, label = dataset, colour = dataset),
      size = 2.3, fontface = "bold", min.segment.length = 0.2, box.padding = 0.25,
      max.overlaps = Inf, seed = 1
    ) +
    geom_point(
      data = mean_point, aes(x = prec, y = evap, shape = NAME_MEAN),
      colour = "black", size = 3, stroke = 0.9
    ) +
    scale_colour_manual(values = DATASET_COLS, guide = "none") +
    scale_shape_manual(
      name = NULL,
      values = setNames(c(23, 4), c(NAME_MEDIAN, NAME_MEAN)),
      breaks = c(NAME_MEDIAN, NAME_MEAN)
    ) +
    labs(
      x = expression(Precipitation~(mm~yr^{-1})),
      y = expression(Evaporation~(mm~yr^{-1})),
      title = paste(scope_name, "land")
    ) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 9),
      legend.position = "none"
    )
}

# Analysis ===================================================================

weights <- weights_region_biome[scenario %in% SCENARIO_ORDER]
weights[, dataset := as.character(dataset)]

dataset_values <- as.data.table(dataset_region_biome_year)
dataset_values[, dataset := as.character(dataset)]

# One land mask: the units that have weights.
weighted_units <- unique(weights[, .(region, biome)])

unit_areas <- merge(unit_area(grid_classes), weighted_units, by = c("region", "biome"))

hemisphere_areas <- merge(unit_area_hemisphere(grid_classes), weighted_units, by = c("region", "biome"))

scope_area <- rbindlist(list(
  unit_areas[, .(region, biome, scope = "Global", area)],
  hemisphere_areas[
    ,
    .(
      region, biome,
      scope = fifelse(hemisphere == "north", "Northern Hemisphere", "Southern Hemisphere"),
      area
    )
  ]
))

dataset_values <- merge(dataset_values, weighted_units, by = c("region", "biome"))

# Candidate series and their arithmetic mean --------------------------------------------

dataset_year <- aggregate_scopes(dataset_values, scope_area, by_cols = "dataset")

mean_year <- dataset_year[
  ,
  .(prec = mean(prec), evap = mean(evap)),
  by = .(scope, year)
]

# Deterministic scenario means (expectations), global ------------------------------------

expectation <- scenario_expectation(weights, dataset_values)

scenario_year <- aggregate_scopes(
  expectation[scenario %in% ENVELOPE_SCENARIOS],
  scope_area[scope == "Global"],
  by_cols = "scenario"
)

scenario_range <- lapply(
  c(prec = "prec", evap = "evap"),
  function(variable) {
    scenario_year[
      ,
      .(lo = min(get(variable)), hi = max(get(variable))),
      by = year
    ]
  }
)

# Base Monte Carlo, global annual percentiles --------------------------------------------

mc_base_year <- as.data.table(mc_global_year)[scenario == BASE_SCENARIO]

mc_year_percentiles <- lapply(
  c(prec = "prec", evap = "evap"),
  function(variable) percentile_table(mc_base_year, variable, by_cols = "year")
)

# Long-term level space ---------------------------------------------------------------------

dataset_levels <- long_term_mean(dataset_year, by_cols = c("dataset", "scope"))
mean_levels <- long_term_mean(mean_year, by_cols = "scope")

mc_members <- mc_scope_means(scope_area)

mc_levels <- merge(
  percentile_table(mc_members, "prec", by_cols = "scope")[
    , .(scope, prec_q05 = q05, prec_q50 = q50, prec_q95 = q95)
  ],
  percentile_table(mc_members, "evap", by_cols = "scope")[
    , .(scope, evap_q05 = q05, evap_q50 = q50, evap_q95 = q95)
  ],
  by = "scope"
)

# Figure ----------------------------------------------------------------------------------------

year_limits <- range(dataset_year$year)

series_colours <- c(
  DATASET_COLS,
  setNames(c("grey15", COL_ITHACA), c(NAME_MEAN, NAME_MEDIAN))
)
series_linetypes <- c(setNames(rep("solid", length(CANDIDATES)), CANDIDATES), setNames(c("22", "solid"), c(NAME_MEAN, NAME_MEDIAN)))
series_linewidths <- c(setNames(rep(0.35, length(CANDIDATES)), CANDIDATES), setNames(c(0.7, 1.1), c(NAME_MEAN, NAME_MEDIAN)))

p_prec <- series_panel("prec", expression(Precipitation~(mm~yr^{-1})), "a  Global precipitation")
p_evap <- series_panel("evap", expression(Evaporation~(mm~yr^{-1})), "b  Global evaporation")

p_global <- level_panel("Global") + labs(title = "c  Global land")
p_north <- level_panel("Northern Hemisphere") + labs(title = "c  Northern Hemisphere land")
p_south <- level_panel("Southern Hemisphere") + labs(title = "c  Southern Hemisphere land")

# Legends: series and bands of (a) and (b), markers of (c).
series_legend <- legend_element(
  p_prec + theme(legend.box = "vertical", legend.text = element_text(size = 7)) +
    guides(
      colour = guide_legend(nrow = 1, order = 1),
      linetype = guide_legend(nrow = 1, order = 1),
      linewidth = guide_legend(nrow = 1, order = 1),
      fill = guide_legend(nrow = 1, order = 2)
    )
)

marker_legend <- legend_element(
  p_global + theme(legend.text = element_text(size = 7)) +
    guides(shape = guide_legend(nrow = 2))
)

figure_5 <- wrap_plots(
  p_prec, p_evap, p_global, p_north, p_south, series_legend, marker_legend,
  design = "AAAC\nAAAC\nAAAD\nBBBD\nBBBE\nBBBE\nFFFG",
  heights = c(1, 1, 1, 1, 1, 1, 0.75)
) +
  plot_annotation(
    caption = paste0(
      "Dark band: spread across the 100 base Monte Carlo members (dataset-choice uncertainty). ",
      "Light band: range of the deterministic means of the eight weighting scenarios (weighting uncertainty).\n",
      "All values on one land mask; candidate pairs keep their coherent P-E pairing."
    ),
    theme = theme(plot.caption = element_text(size = 6.5, colour = "grey30", hjust = 0))
  )

# Outputs ====================================================================

save_figure(figure_5, "fig05_global_product", width = 11, height = 8.4)

# Validation =================================================================

# Annual weights of each scenario expectation sum to one, so every expectation
# stays within the range of the five dataset values.
stopifnot(
  expectation[, all(prec >= min(dataset_values$prec) - 1e-6 & prec <= max(dataset_values$prec) + 1e-6)]
)

# The base Monte Carlo median should sit on the deterministic base expectation.
base_expectation <- scenario_year[scenario == BASE_SCENARIO]

check <- merge(
  mc_year_percentiles$prec[, .(year, mc_prec = q50)],
  mc_year_percentiles$evap[, .(year, mc_evap = q50)],
  by = "year"
)
check <- merge(check, base_expectation[, .(year, det_prec = prec, det_evap = evap)], by = "year")

cat(
  "\nBase: Monte Carlo median minus deterministic expectation (mm yr-1), max |difference| over years:",
  "\n  precipitation ", round(check[, max(abs(mc_prec - det_prec))], 2),
  "\n  evaporation   ", round(check[, max(abs(mc_evap - det_evap))], 2), "\n", sep = ""
)

# The deterministic scenarios and the five datasets, long-term global means.
cat("\nGlobal long-term means (mm yr-1), P / E:\n")
print(
  rbindlist(list(
    dataset_levels[scope == "Global", .(series = dataset, prec = round(prec, 1), evap = round(evap, 1))],
    mean_levels[scope == "Global", .(series = NAME_MEAN, prec = round(prec, 1), evap = round(evap, 1))],
    mc_levels[scope == "Global", .(series = NAME_MEDIAN, prec = round(prec_q50, 1), evap = round(evap_q50, 1))]
  ))
)

cat("\nBand width in the last year, global P / E (mm yr-1):\n")
print(data.table(
  band = c(NAME_BAND_MC, NAME_BAND_SCENARIOS),
  prec = round(c(
    mc_year_percentiles$prec[year == max(year), q95 - q05],
    scenario_range$prec[year == max(year), hi - lo]
  ), 1),
  evap = round(c(
    mc_year_percentiles$evap[year == max(year), q95 - q05],
    scenario_range$evap[year == max(year), hi - lo]
  ), 1)
))

# Global level in (c) equals the long-term mean of the annual global series.
stopifnot(
  isTRUE(all.equal(
    dataset_levels[scope == "Global" & dataset == CANDIDATES[1], prec],
    dataset_year[scope == "Global" & dataset == CANDIDATES[1], mean(prec)]
  ))
)
