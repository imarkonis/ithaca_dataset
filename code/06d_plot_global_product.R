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
#
# Output  : fig05_global_product.pdf / .png / _caption.txt in PATH_OUTPUT_FIGURES
# Note    : converted to the shared figure standard (docs/SCIENTIFIC_FIGURE_*.md,
#           code/_figs.R) with AI assistance (Claude Code, 2026-10-06); to be
#           checked by the authors
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")
source("code/_figs.R") # current publication design system; restores modern export helpers

library(ggrepel) # direct labels of the candidate pairs in panel (c)

# Inputs =====================================================================

dataset_region_biome_year <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "dataset_region_biome_year.Rds")
))

weights_region_biome <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "weights_region_biome.Rds")
))

grid_classes <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
))

mc_global_year <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "mc_global_year_scenarios.Rds")
))

stopifnot(
  all(c("dataset", "region", "biome", "year", "prec", "evap") %in% names(dataset_region_biome_year)),
  all(c("scenario", "region", "biome", "dataset", "w_region_biome") %in% names(weights_region_biome)),
  all(c("lon", "lat", "region", "biome", "cell_weight", "hemisphere") %in% names(grid_classes)),
  all(c("sim", "scenario", "year", "prec", "evap") %in% names(mc_global_year)),
  !anyDuplicated(dataset_region_biome_year[, .(dataset, region, biome, year)]),
  all(CANDIDATES %in% as.character(dataset_region_biome_year$dataset))
)

# Scientific parameters ======================================================

BASE_SCENARIO <- "base"

# Scenarios whose deterministic means form the weighting-scenario band: all eight
# of the sensitivity analysis, including the adversarial `inverted`, so the band
# is the full methodological range (restrict it here for a narrower reading).
ENVELOPE_SCENARIOS <- SCENARIO_ORDER

# Monte Carlo band: 5th-95th percentile over the 100 base members, and the median.
MC_PROBS <- c(0.05, 0.5, 0.95)

# Final ESSD/Copernicus figure height. Width comes from the active journal profile.
FIGURE_HEIGHT_MM <- 125

# Series keys and names used in the legend.
SERIES_MEAN   <- "mean"
SERIES_MEDIAN <- "median"
SERIES_LEVELS <- c(CANDIDATES, SERIES_MEAN, SERIES_MEDIAN)
SERIES_LABELS <- c(
  DATASET_LABELS,
  setNames(c("Arithmetic mean", "ITHACA base, median"), c(SERIES_MEAN, SERIES_MEDIAN))
)
BAND_MC        <- "Base Monte Carlo, 5\u201395 %"
BAND_SCENARIOS <- "Weighting scenarios, range"

# Colours and line styles per series: the five pairs keep their dataset colours
# (thin, subdued), the benchmark mean is dark and dashed, ITHACA is the focal
# element (COL_HIGHLIGHT) and the heaviest line.
series_colours <- c(PAL_DATASETS, setNames(c(COL_TEXT, COL_HIGHLIGHT), c(SERIES_MEAN, SERIES_MEDIAN)))
series_linetypes <- c(
  setNames(rep(FIG_LINETYPES[1], length(CANDIDATES)), CANDIDATES),
  setNames(c(FIG_LINETYPES[2], FIG_LINETYPES[1]), c(SERIES_MEAN, SERIES_MEDIAN))
)
series_linewidths <- c(
  setNames(rep(FIG_LINEWIDTH, length(CANDIDATES)), CANDIDATES),
  setNames(c(FIG_LINEWIDTH_EMPH, FIG_LINEWIDTH_EMPH), c(SERIES_MEAN, SERIES_MEDIAN))
)
series_shapes <- c(
  setNames(rep(FIG_SHAPES[1], length(CANDIDATES)), CANDIDATES),
  setNames(c(4, FIG_SHAPES[4]), c(SERIES_MEAN, SERIES_MEDIAN))
)

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

# Scales shared by the plots and by the legend plots. Only the aesthetics a plot
# maps are added: a manual scale for an unmapped aesthetic has no data to match
# and warns.
series_scales <- function(aesthetics = c("colour", "fill", "linetype", "linewidth", "shape")) {
  scale_for <- list(
    colour = scale_colour_manual(
      name = NULL, values = series_colours, breaks = SERIES_LEVELS, labels = SERIES_LABELS
    ),
    fill = scale_fill_manual(
      name = NULL, values = series_colours, breaks = SERIES_LEVELS, labels = SERIES_LABELS
    ),
    linetype = scale_linetype_manual(
      name = NULL, values = series_linetypes, breaks = SERIES_LEVELS, labels = SERIES_LABELS
    ),
    linewidth = scale_linewidth_manual(
      name = NULL, values = series_linewidths, breaks = SERIES_LEVELS, labels = SERIES_LABELS
    ),
    shape = scale_shape_manual(
      name = NULL, values = series_shapes, breaks = SERIES_LEVELS, labels = SERIES_LABELS
    )
  )

  unname(scale_for[match.arg(aesthetics, names(scale_for), several.ok = TRUE)])
}

# Time-series panel for one variable ("prec" or "evap").
series_panel <- function(variable, y_label, show_x_axis) {
  candidates <- dataset_year[
    scope == "Global",
    .(year, series = factor(dataset, levels = SERIES_LEVELS), value = get(variable))
  ]

  mean_line <- mean_year[
    scope == "Global",
    .(year, series = factor(SERIES_MEAN, levels = SERIES_LEVELS), value = get(variable))
  ]

  mc_band <- mc_year_percentiles[[variable]]
  mc_median <- mc_band[, .(year, series = factor(SERIES_MEDIAN, levels = SERIES_LEVELS), value = q50)]

  scenario_band <- scenario_range[[variable]]

  p <- ggplot() +
    geom_ribbon(
      data = scenario_band,
      aes(x = year, ymin = lo, ymax = hi),
      fill = COL_CONTEXT, alpha = FIG_ALPHA_CONTEXT
    ) +
    # The Monte Carlo band is the dominant uncertainty layer: twice the ribbon alpha.
    geom_ribbon(
      data = mc_band,
      aes(x = year, ymin = q05, ymax = q95),
      fill = COL_HIGHLIGHT, alpha = 2 * FIG_ALPHA_RIBBON
    ) +
    geom_line(
      data = candidates,
      aes(x = year, y = value, colour = series, linetype = series, linewidth = series),
      alpha = FIG_ALPHA_CONTEXT
    ) +
    geom_line(
      data = mean_line,
      aes(x = year, y = value, colour = series, linetype = series, linewidth = series)
    ) +
    geom_line(
      data = mc_median,
      aes(x = year, y = value, colour = series, linetype = series, linewidth = series)
    ) +
    series_scales(c("colour", "linetype", "linewidth")) +
    scale_x_continuous(
      breaks = seq(1985, 2015, by = 10),
      limits = year_limits,
      labels = label_minus,
      expand = expansion(mult = 0.01)
    ) +
    scale_y_continuous(labels = label_minus) +
    labs(x = if (show_x_axis) "Year" else NULL, y = y_label) +
    theme_pub(legend_position = "none")

  if (!show_x_axis) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }

  p
}

# Level-space panel: one facet per geography, free scales.
level_panel <- function() {
  scope_factor <- function(x) factor(x, levels = SCOPES, labels = paste(SCOPES, "land"))

  candidates <- copy(dataset_levels)[, `:=`(series = factor(dataset, levels = SERIES_LEVELS),
                                            scope = scope_factor(scope))]
  mean_point <- copy(mean_levels)[, `:=`(series = factor(SERIES_MEAN, levels = SERIES_LEVELS),
                                         scope = scope_factor(scope))]
  cloud <- copy(mc_members)[, scope := scope_factor(scope)]
  central <- copy(mc_levels)[, `:=`(series = factor(SERIES_MEDIAN, levels = SERIES_LEVELS),
                                    scope = scope_factor(scope))]

  ggplot() +
    geom_point(
      data = cloud, aes(x = prec, y = evap),
      colour = COL_HIGHLIGHT, size = FIG_POINT_SIZE_SMALL, alpha = FIG_ALPHA_CONTEXT, stroke = 0
    ) +
    geom_segment(
      data = central,
      aes(x = prec_q05, xend = prec_q95, y = evap_q50, yend = evap_q50),
      colour = COL_HIGHLIGHT, linewidth = FIG_LINEWIDTH
    ) +
    geom_segment(
      data = central,
      aes(x = prec_q50, xend = prec_q50, y = evap_q05, yend = evap_q95),
      colour = COL_HIGHLIGHT, linewidth = FIG_LINEWIDTH
    ) +
    geom_point(
      data = central, aes(x = prec_q50, y = evap_q50, colour = series, fill = series, shape = series),
      size = FIG_POINT_SIZE * 2, stroke = FIG_POINT_STROKE
    ) +
    geom_point(
      data = candidates, aes(x = prec, y = evap, colour = series, fill = series, shape = series),
      size = FIG_POINT_SIZE * 1.6, stroke = FIG_POINT_STROKE
    ) +
    # Direct labels in the global panel only: the hemisphere panels are too small
    # for six labels, and the colours are explained by the legend.
    geom_text_repel(
      data = candidates[scope == levels(scope)[1]],
      aes(x = prec, y = evap, label = DATASET_LABELS[as.character(dataset)]),
      colour = COL_TEXT, family = FIG_FONT, size = FIG_GEOM_TEXT_SIZE,
      box.padding = FIG_LABEL_PADDING, force = FIG_LABEL_FORCE,
      min.segment.length = 0.2, segment.size = FIG_LINEWIDTH_REF,
      max.overlaps = Inf, seed = 1
    ) +
    geom_point(
      data = mean_point, aes(x = prec, y = evap, colour = series, fill = series, shape = series),
      size = FIG_POINT_SIZE * 2, stroke = FIG_LINEWIDTH_EMPH / 0.4
    ) +
    series_scales(c("colour", "fill", "shape")) +
    facet_wrap(vars(scope), ncol = 1, scales = "free") +
    scale_x_continuous(labels = label_minus) +
    scale_y_continuous(labels = label_minus) +
    labs(
      x = expression("Precipitation (mm yr"^{-1} * ")"),
      y = expression("Evaporation (mm yr"^{-1} * ")")
    ) +
    theme_pub(legend_position = "none") +
    theme(strip.text = element_text(hjust = 0, size = FIG_STRIP_SIZE))
}

# Legend plots: one legend for all series (line style and marker), one for the bands.
series_legend_plot <- function() {
  # Two points per series so that each key can draw its line.
  key_data <- CJ(series = factor(SERIES_LEVELS, levels = SERIES_LEVELS), x = 1:2)
  key_data[, y := as.integer(series)]

  ggplot(key_data, aes(x = x, y = y)) +
    geom_line(aes(colour = series, linetype = series, linewidth = series, group = series)) +
    geom_point(aes(colour = series, fill = series, shape = series), size = FIG_POINT_SIZE * 1.6,
               stroke = FIG_POINT_STROKE) +
    series_scales() +
    guides(
      colour = guide_legend(nrow = 2, byrow = TRUE),
      fill = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE),
      linewidth = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    theme_pub(legend_position = "bottom")
}

band_legend_plot <- function() {
  key_data <- data.table(
    band = factor(c(BAND_MC, BAND_SCENARIOS), levels = c(BAND_MC, BAND_SCENARIOS)),
    x = 1:2, y = 1:2
  )

  ggplot(key_data, aes(x = x, ymin = y - 1, ymax = y)) +
    geom_ribbon(aes(fill = band, group = band)) +
    scale_fill_manual(
      name = NULL,
      values = setNames(
        c(scales::alpha(COL_HIGHLIGHT, 2 * FIG_ALPHA_RIBBON), scales::alpha(COL_CONTEXT, FIG_ALPHA_CONTEXT)),
        c(BAND_MC, BAND_SCENARIOS)
      )
    ) +
    guides(fill = guide_legend(ncol = 1)) +
    theme_pub(legend_position = "bottom")
}

# Analysis ===================================================================

weights <- weights_region_biome[scenario %in% SCENARIO_ORDER]
weights[, dataset := as.character(dataset)]

dataset_values <- copy(dataset_region_biome_year)
dataset_values[, dataset := as.character(dataset)]

# One land mask: the units that have weights.
weighted_units <- unique(weights[, .(region, biome)])

unit_areas <- merge(unit_area(grid_classes), weighted_units, by = c("region", "biome"))

hemisphere_areas <- merge(unit_area_hemisphere(grid_classes), weighted_units, by = c("region", "biome"))

SCOPES <- c("Global", "Northern Hemisphere", "Southern Hemisphere")

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

mc_base_year <- mc_global_year[scenario == BASE_SCENARIO]

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

year_limits <- range(dataset_year$year)

# Validation before rendering ------------------------------------------------------------------

# Annual weights of each scenario expectation sum to one, so every expectation
# stays within the range of the five dataset values.
stopifnot(
  expectation[, all(prec >= min(dataset_values$prec) - 1e-6 & prec <= max(dataset_values$prec) + 1e-6)],
  expectation[, all(evap >= min(dataset_values$evap) - 1e-6 & evap <= max(dataset_values$evap) + 1e-6)]
)

# Complete yearly sequence for every plotted series: no gap is bridged by a line.
expected_years <- seq(year_limits[1], year_limits[2])
stopifnot(
  dataset_year[scope == "Global", all(sort(year) == expected_years), by = dataset][, all(V1)],
  setequal(mc_base_year$year, expected_years),
  uniqueN(mc_base_year$sim) == 100L
)

# Panels ----------------------------------------------------------------------------------------------

p_prec <- series_panel("prec", expression("Precipitation (mm yr"^{-1} * ")"), show_x_axis = FALSE)
p_evap <- series_panel("evap", expression("Evaporation (mm yr"^{-1} * ")"), show_x_axis = TRUE)
p_level <- level_panel()

# One flat layout: three data panels and two legend elements. Only the data panels
# get a tag, in the profile format (the legend cells get empty tags).
panel_tags <- c(paste0(FIG_SPEC$tag_prefix, c("a", "b", "c"), FIG_SPEC$tag_suffix), "", "")

figure_5 <- wrap_plots(
  p_prec, p_evap, p_level,
  legend_element(series_legend_plot()), legend_element(band_legend_plot()),
  design = "AAAAAACC\nAAAAAACC\nBBBBBBCC\nBBBBBBCC\nDDDDDDEE",
  heights = c(1, 1, 1, 1, 0.45)
) +
  plot_annotation(tag_levels = list(panel_tags)) &
  theme(plot.tag = element_text(family = FIG_FONT, size = FIG_TAG_SIZE, face = "bold", colour = COL_TEXT))

# Outputs ====================================================================

figure_stem <- file.path(PATH_OUTPUT_FIGURES, "fig05_global_product")
save_figure(
  figure_5,
  file_stem = figure_stem,
  width_mm = FIG_WIDTH_DOUBLE,
  height_mm = FIGURE_HEIGHT_MM
)
check_fonts(paste0(figure_stem, ".pdf"))

caption <- paste0(
  "Global annual precipitation and evaporation of the ITHACA product, 1982\u20132021. ",
  "(a) Precipitation and (b) evaporation over global land: the five coherent P\u2013E candidate ",
  "pairs (thin lines), their arithmetic mean (dashed line), the base Monte Carlo ",
  "median (heavy line) with its 5th\u201395th percentile band over the 100 members ",
  "(dataset-choice uncertainty), and the range of the deterministic means of the eight ",
  "weighting scenarios (light band, weighting uncertainty); the two uncertainty sources are ",
  "not pooled. (c) Long-term (1982\u20132021) mean precipitation and evaporation for global, ",
  "northern-hemisphere and southern-hemisphere land: candidate pairs (circles), their ",
  "arithmetic mean (cross), the base Monte Carlo members (small points) and median ",
  "(diamond) with 5th\u201395th percentile bars. All values use one land mask (region \u00D7 biome ",
  "units with weights) and cell-area weights; scales differ between panels. ",
  "Data: dataset_region_biome_year.Rds, weights_region_biome.Rds, mc_global_year_scenarios.Rds."
)
writeLines(caption, paste0(figure_stem, "_caption.txt"))

# Data QA ====================================================================

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

cat("\nGlobal long-term means (mm yr-1), P / E:\n")
print(
  rbindlist(list(
    dataset_levels[scope == "Global", .(series = dataset, prec = round(prec, 1), evap = round(evap, 1))],
    mean_levels[scope == "Global", .(series = SERIES_LABELS[[SERIES_MEAN]], prec = round(prec, 1), evap = round(evap, 1))],
    mc_levels[scope == "Global", .(series = SERIES_LABELS[[SERIES_MEDIAN]], prec = round(prec_q50, 1), evap = round(evap_q50, 1))]
  ))
)

cat("\nBand width in the last year, global P / E (mm yr-1):\n")
print(data.table(
  band = c(BAND_MC, BAND_SCENARIOS),
  prec = round(c(
    mc_year_percentiles$prec[year == max(year), q95 - q05],
    scenario_range$prec[year == max(year), hi - lo]
  ), 1),
  evap = round(c(
    mc_year_percentiles$evap[year == max(year), q95 - q05],
    scenario_range$evap[year == max(year), hi - lo]
  ), 1)
))

# Year-over-year change of each candidate's global mean (percent): a jump flags a
# data problem in the input, not a result (see the MERRA-2 2021 issue).
cat("\nLargest year-over-year change of the global mean, by candidate (%):\n")
print(dataset_year[
  scope == "Global",
  {
    change <- 100 * (prec / data.table::shift(prec) - 1)
    i <- which.max(abs(change))
    .(year = year[i], precipitation = round(change[i], 1))
  },
  by = dataset
])

# Global level in (c) equals the long-term mean of the annual global series.
stopifnot(
  isTRUE(all.equal(
    dataset_levels[scope == "Global" & dataset == CANDIDATES[1], prec],
    dataset_year[scope == "Global" & dataset == CANDIDATES[1], mean(prec)]
  ))
)
