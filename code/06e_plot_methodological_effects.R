# ============================================================================
# Figure 6: methodological effects. How different is the ITHACA product from a
# simple average of the five candidates, and which part of the framework causes
# the difference?
#
# This is not a validation: it shows differences, shifts and sensitivities, not
# accuracy.
#
# Methods compared (long-term mean 1982-2021, mm yr-1), all as probability-
# weighted means of the five coherent P-E candidate pairs:
#   Naive     equal weight 1/5 on all five candidates (no gate, no performance);
#   Neutral   scenario "neutral": equal weight among the candidates that survive
#             the physics gate (gate only);
#   Base      scenario "base": gate and performance weighting;
#   Top-1     all probability on the unit's highest base-weight candidate
#             (winner-take-all foil, not a preferred product);
#   Inverted  scenario "inverted" (adversarial sensitivity bound);
#   Base MC   median and 5th-95th percentile of the base Monte Carlo members.
# Effects: gate = Neutral - Naive; weighting = Base - Neutral; total = Base - Naive.
#
# Definitions fixed here:
#   - One footprint for every method: grid cells of grid_classes.Rds in region x
#     biome units that have weights, where ALL FIVE candidates are present with
#     the full 40 years in 03a's prec_evap_stats.Rds. The naive mean is therefore
#     the mean of exactly five candidates everywhere. (This leaves out 1.3 % of
#     land area where a candidate is missing.) The Monte Carlo product uses the
#     units' own means over all available cells, so the base deterministic value
#     and the Monte Carlo interval differ slightly in mask (checked below).
#   - Cell values are the candidates' long-term cell means saved by 03a
#     (prec_mean, evap_mean, MSWEP precipitation for GLEAM).
#   - Weights are the operational region x biome weights of 03f
#     (weights_region_biome.Rds), applied to every cell of the unit. This is how
#     04c builds the gridded product; the expectation of the Monte Carlo draw.
#   - Regional and global means are cell-area weighted (cell_weight).
#
# Nothing is re-estimated: only existing files are combined.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")

# Inputs =====================================================================

prec_evap_stats <- as.data.table(
  readRDS(file.path(PATH_OUTPUT_OUTPUT, "prec_evap_stats.Rds"))
)[
  ,
  .(lon, lat, dataset, prec_mean, evap_mean, prec_n_years_mean, evap_n_years_mean)
]

grid_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
)

weights_region_biome <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "weights_region_biome.Rds")
)

region_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "region_classes.Rds")
)

mc_global_year <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "mc_global_year_scenarios.Rds")
)

weight_diagnostics <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "dataset_weight_diagnostics.Rds")
)[
  scenario %in% c("base", "neutral", "inverted"),
  .(scenario, lon, lat, n_candidates, eff_n)
]

# Constants & Variables ======================================================

N_CANDIDATES <- length(CANDIDATES)
FULL_YEARS <- diff(FULL_PERIOD) + 1

# Display labels and symbols (not the dataset colours: methods, not datasets).
COL_NAIVE    <- "grey45"
COL_NEUTRAL  <- "grey20"
COL_BASE     <- "#08306b"
COL_INVERTED <- "#b2182b"
COL_TOP1     <- "#8c6d31"

METHOD_LEVELS <- c("Naive mean", "Neutral", "Base", "Base Monte Carlo", "Top-1", "Inverted")
METHOD_GROUPS <- c(
  "Naive mean" = "Sequence", "Neutral" = "Sequence", "Base" = "Sequence",
  "Base Monte Carlo" = "MC", "Top-1" = "Foils", "Inverted" = "Foils"
)

# Köppen-Geiger main groups (level 1), ordering of the regional panel.
CLIMATE_CLASSES <- c(
  A = "A  Tropical", B = "B  Arid", C = "C  Temperate",
  D = "D  Continental", E = "E  Polar"
)

# Maps: symmetric colour limits are taken from this area-weighted quantile of
# |Base - Naive|, shared by P and E if the two are within this ratio.
MAP_LIMIT_QUANTILE <- 0.98
SHARED_LIMIT_RATIO <- 1.5

# Functions ==================================================================

# Unit weights of one scenario as a matrix-ready table (0 for absent candidates).
unit_weight_table <- function(weights, scenario_name) {
  wide <- dcast(
    weights[scenario == scenario_name],
    region + biome ~ dataset,
    value.var = "w_region_biome",
    fill = 0
  )

  for (candidate in setdiff(CANDIDATES, names(wide))) {
    wide[, (candidate) := 0]
  }

  setcolorder(wide, c("region", "biome", CANDIDATES))
  wide[]
}

# Winner-take-all weights: all probability on the maximum (split on exact ties).
top_one_table <- function(base_table) {
  top <- copy(base_table)
  probabilities <- as.matrix(top[, ..CANDIDATES])
  is_max <- probabilities == apply(probabilities, 1, max)
  top[, (CANDIDATES) := as.data.table(is_max / rowSums(is_max))]
  top[]
}

# Probability-weighted cell values for one weight table (rows follow `cells`).
weighted_cell_values <- function(cells, weight_table, values) {
  unit_weights <- weight_table[cells[, .(region, biome)], on = .(region, biome)]
  weights <- as.matrix(unit_weights[, ..CANDIDATES])

  stopifnot(!anyNA(weights))

  rowSums(values * weights)
}

# Area-weighted quantiles.
weighted_quantile <- function(x, w, probs) {
  ordered <- order(x)
  cumulative <- cumsum(w[ordered]) / sum(w)

  vapply(probs, function(p) x[ordered][which(cumulative >= p)[1]], numeric(1))
}

# Analysis ===================================================================

# Footprint: all five candidates, full period, unit with weights ---------------------

wide_stats <- dcast(
  prec_evap_stats,
  lon + lat ~ dataset,
  value.var = c("prec_mean", "evap_mean", "prec_n_years_mean", "evap_n_years_mean")
)

complete_columns <- unlist(lapply(
  c("prec_mean", "evap_mean", "prec_n_years_mean", "evap_n_years_mean"),
  function(prefix) paste(prefix, CANDIDATES, sep = "_")
))

wide_stats <- wide_stats[complete.cases(wide_stats[, ..complete_columns])]

full_period <- wide_stats[
  ,
  Reduce(`&`, lapply(
    c(paste0("prec_n_years_mean_", CANDIDATES), paste0("evap_n_years_mean_", CANDIDATES)),
    function(column) get(column) == FULL_YEARS
  ))
]

weights <- weights_region_biome[scenario %in% c("base", "neutral", "inverted")]
weights[, dataset := as.character(dataset)]

weighted_units <- unique(weights[, .(region, biome)])

cells <- merge(
  grid_classes[!is.na(region) & !is.na(biome), .(lon, lat, region, biome, cell_weight)],
  weighted_units,
  by = c("region", "biome")
)

footprint <- wide_stats[full_period]
cells <- merge(cells, footprint, by = c("lon", "lat"))
setorder(cells, lon, lat)

prec_values <- as.matrix(cells[, paste0("prec_mean_", CANDIDATES), with = FALSE])
evap_values <- as.matrix(cells[, paste0("evap_mean_", CANDIDATES), with = FALSE])

# Weight tables of every method -----------------------------------------------------------

base_table <- unit_weight_table(weights, "base")

weight_tables <- list(
  naive = NULL,
  neutral = unit_weight_table(weights, "neutral"),
  base = base_table,
  top1 = top_one_table(base_table),
  inverted = unit_weight_table(weights, "inverted")
)

method_values <- function(weight_table, values) {
  if (is.null(weight_table)) {
    return(rowMeans(values))
  }

  weighted_cell_values(cells, weight_table, values)
}

for (method_name in names(weight_tables)) {
  cells[, (paste0("P_", method_name)) := method_values(weight_tables[[method_name]], prec_values)]
  cells[, (paste0("E_", method_name)) := method_values(weight_tables[[method_name]], evap_values)]
}

value_columns <- as.vector(outer(c("P_", "E_"), names(weight_tables), paste0))

# Global and regional means, and the effects ---------------------------------------------------

effects_from_means <- function(means) {
  out <- copy(means)

  for (variable in c("P", "E")) {
    out[
      ,
      (paste0(variable, "_gate")) := get(paste0(variable, "_neutral")) - get(paste0(variable, "_naive"))
    ]
    out[
      ,
      (paste0(variable, "_weight")) := get(paste0(variable, "_base")) - get(paste0(variable, "_neutral"))
    ]
    out[
      ,
      (paste0(variable, "_total")) := get(paste0(variable, "_base")) - get(paste0(variable, "_naive"))
    ]
    out[
      ,
      (paste0(variable, "_inverted_shift")) := get(paste0(variable, "_inverted")) - get(paste0(variable, "_naive"))
    ]
    out[
      ,
      (paste0(variable, "_top1_shift")) := get(paste0(variable, "_top1")) - get(paste0(variable, "_naive"))
    ]
  }

  out[]
}

global_means <- effects_from_means(
  cells[, lapply(.SD, weighted_mean_safe, weight = cell_weight), .SDcols = value_columns]
)

region_means <- effects_from_means(
  cells[, lapply(.SD, weighted_mean_safe, weight = cell_weight), by = region, .SDcols = value_columns]
)

# Base Monte Carlo, long-term global mean per member ---------------------------------------------

mc_members <- as.data.table(mc_global_year)[
  scenario == "base",
  .(prec = mean(prec), evap = mean(evap)),
  by = sim
]

mc_summary <- list(
  P = quantile(mc_members$prec, c(0.05, 0.5, 0.95), names = FALSE),
  E = quantile(mc_members$evap, c(0.05, 0.5, 0.95), names = FALSE)
)

# Panel (a): ladder data ----------------------------------------------------------------------------

ladder_for <- function(variable) {
  v <- function(method_name) global_means[[paste0(variable, "_", method_name)]]

  out <- data.table(
    method = METHOD_LEVELS,
    value = c(v("naive"), v("neutral"), v("base"), mc_summary[[variable]][2], v("top1"), v("inverted")),
    lo = c(NA, NA, NA, mc_summary[[variable]][1], NA, NA),
    hi = c(NA, NA, NA, mc_summary[[variable]][3], NA, NA)
  )

  out[
    ,
    `:=`(
      group = factor(METHOD_GROUPS[method], levels = c("Sequence", "MC", "Foils")),
      method = factor(method, levels = rev(METHOD_LEVELS))
    )
  ]

  out[]
}

# Panel (c): regional ordering --------------------------------------------------------------------------

region_order <- merge(
  region_classes[, .(region, kg_class_1 = as.character(kg_class_1))],
  grid_classes[!is.na(region), .(latitude = weighted_mean_safe(lat, cell_weight)), by = region],
  by = "region"
)

region_order[
  ,
  climate := fifelse(kg_class_1 %in% names(CLIMATE_CLASSES), CLIMATE_CLASSES[kg_class_1], "Other")
]

region_order[, climate := factor(climate, levels = c(CLIMATE_CLASSES, "Other"))]
setorder(region_order, climate, -latitude)

region_effects <- merge(region_means, region_order[, .(region, climate)], by = "region")
region_effects[, region := factor(region, levels = rev(region_order$region))]

# Plots ===================================================================================================

point_scales <- list(
  scale_shape_manual(
    name = NULL,
    values = c("Naive mean" = 21, "Neutral" = 21, "Base" = 21, "Base Monte Carlo" = 23, "Top-1" = 24, "Inverted" = 22),
    guide = "none"
  ),
  scale_fill_manual(
    name = NULL,
    values = c("Naive mean" = "white", "Neutral" = COL_NEUTRAL, "Base" = COL_BASE,
               "Base Monte Carlo" = COL_BASE, "Top-1" = COL_TOP1, "Inverted" = "white"),
    guide = "none"
  ),
  scale_colour_manual(
    name = NULL,
    values = c("Naive mean" = COL_NAIVE, "Neutral" = COL_NEUTRAL, "Base" = COL_BASE,
               "Base Monte Carlo" = COL_BASE, "Top-1" = COL_TOP1, "Inverted" = COL_INVERTED),
    guide = "none"
  )
)

ladder_panel <- function(variable, title, x_label) {
  ladder <- ladder_for(variable)

  steps <- ladder[method %in% c("Naive mean", "Neutral", "Base")]
  steps <- steps[order(match(as.character(method), METHOD_LEVELS))]
  arrows <- data.table(
    x = steps$value[-3], xend = steps$value[-1],
    y = steps$method[-3], yend = steps$method[-1],
    group = steps$group[-3]
  )

  effect_text <- sprintf(
    "Δgate %+.1f   Δweight %+.1f   Δtotal %+.1f",
    global_means[[paste0(variable, "_gate")]],
    global_means[[paste0(variable, "_weight")]],
    global_means[[paste0(variable, "_total")]]
  )

  ggplot(ladder, aes(x = value, y = method)) +
    geom_segment(
      data = arrows, aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE, colour = "grey55", linewidth = 0.35,
      arrow = arrow(length = unit(1.4, "mm"), type = "closed")
    ) +
    geom_linerange(
      data = ladder[!is.na(lo)], aes(xmin = lo, xmax = hi, y = method),
      inherit.aes = FALSE, colour = COL_BASE, linewidth = 0.9
    ) +
    geom_point(aes(shape = method, fill = method, colour = method), size = 2.8, stroke = 0.8) +
    geom_text(aes(label = sprintf("%.1f", value)), vjust = -1.2, size = 2.3) +
    point_scales +
    facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_x_continuous(expand = expansion(mult = 0.12)) +
    labs(x = x_label, y = NULL, title = title, subtitle = paste0(effect_text, " mm yr⁻¹")) +
    theme_bw(base_size = 8) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 90, size = 6.5, colour = "grey30"),
      plot.title = element_text(face = "bold", size = 9),
      plot.subtitle = element_text(size = 6.8, colour = "grey25")
    )
}

# Maps: Base - Naive per cell.
map_limit <- function(difference) {
  weighted_quantile(abs(difference), cells$cell_weight, MAP_LIMIT_QUANTILE)
}

limit_p <- map_limit(cells$P_base - cells$P_naive)
limit_e <- map_limit(cells$E_base - cells$E_naive)

if (max(limit_p, limit_e) / min(limit_p, limit_e) <= SHARED_LIMIT_RATIO) {
  limit_p <- limit_e <- max(limit_p, limit_e)
}

difference_map <- function(variable, limit, title) {
  map_dt <- cells[
    ,
    .(lon, lat, difference = get(paste0(variable, "_base")) - get(paste0(variable, "_naive")))
  ]

  quantiles <- weighted_quantile(map_dt$difference, cells$cell_weight, c(0.05, 0.5, 0.95))
  median_abs <- weighted_quantile(abs(map_dt$difference), cells$cell_weight, 0.5)

  cell_map(
    map_dt, "difference",
    scale_fill_gradient2(
      name = expression(Delta~(mm~yr^{-1})),
      low = "#b35806", mid = "white", high = "#2166ac", midpoint = 0,
      limits = c(-limit, limit), oob = scales::squish,
      guide = guide_colourbar(barwidth = unit(4, "cm"), barheight = unit(0.28, "cm"))
    ),
    title = title
  ) +
    map_label(
      sprintf("median |Δ| %.1f (5-95 %%: %.0f to %.0f)", median_abs, quantiles[1], quantiles[3]),
      size = 1.9
    )
}

# Regional decomposition.
region_panel <- function(variable, title, x_label) {
  d <- copy(region_effects)

  d[, `:=`(
    gate = get(paste0(variable, "_gate")),
    weight = get(paste0(variable, "_weight")),
    total = get(paste0(variable, "_total")),
    inverted = get(paste0(variable, "_inverted_shift"))
  )]

  ggplot(d, aes(y = region)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_segment(
      aes(x = 0, xend = gate, yend = region, colour = "Gate (Neutral - Naive)"),
      linewidth = 2.4, lineend = "butt"
    ) +
    geom_segment(
      aes(x = gate, xend = gate + weight, yend = region, colour = "Weighting (Base - Neutral)"),
      linewidth = 2.4, lineend = "butt"
    ) +
    geom_point(aes(x = total, shape = "Total (Base - Naive)"), size = 1.6, colour = "black") +
    geom_point(
      aes(x = inverted, shape = "Inverted - Naive"),
      size = 1.5, colour = COL_INVERTED, fill = "white", stroke = 0.5
    ) +
    scale_colour_manual(
      name = NULL,
      values = c("Gate (Neutral - Naive)" = COL_NEUTRAL, "Weighting (Base - Neutral)" = COL_BASE)
    ) +
    scale_shape_manual(
      name = NULL,
      values = c("Total (Base - Naive)" = 16, "Inverted - Naive" = 22)
    ) +
    facet_grid(climate ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(x = x_label, y = NULL, title = title) +
    theme_bw(base_size = 8) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey94"),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1, size = 6.8, face = "bold"),
      axis.text.y = element_text(size = 6.2),
      plot.title = element_text(face = "bold", size = 9),
      legend.position = "none"
    )
}

p_ladder_p <- ladder_panel("P", "a  Precipitation, global mean", expression(Mean~P~(mm~yr^{-1})))
p_ladder_e <- ladder_panel("E", "a  Evaporation, global mean", expression(Mean~E~(mm~yr^{-1})))

p_map_p <- difference_map("P", limit_p, "b  Base - Naive, precipitation")
p_map_e <- difference_map("E", limit_e, "b  Base - Naive, evaporation")

p_region_p <- region_panel("P", "c  Precipitation: effect on the regional mean", expression(Difference~from~naive~mean~(mm~yr^{-1})))
p_region_e <- region_panel("E", "c  Evaporation: effect on the regional mean", expression(Difference~from~naive~mean~(mm~yr^{-1})))

region_legend <- legend_element(
  region_panel("P", "", "") +
    theme(legend.text = element_text(size = 7)) +
    guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2))
)

# Reading order: global magnitude (a), spatial pattern (b), regional attribution (c).
figure_6 <- wrap_plots(
  p_ladder_p, p_ladder_e,
  p_map_p + theme(legend.position = "none"),
  p_map_e + theme(legend.position = "none"),
  legend_element(p_map_p), legend_element(p_map_e),
  p_region_p, p_region_e, region_legend,
  design = "AAAAAABBBBBB\nCCCCCCDDDDDD\nEEEEEEFFFFFF\nGGGGGGHHHHHH\nIIIIIIIIIIII",
  heights = c(1.9, 1.9, 0.3, 6.2, 0.3)
) +
  plot_annotation(
    caption = paste0(
      "Naive: mean of the five candidates. Neutral: equal weight among candidates passing the physics gate. ",
      "Base: gate and performance weighting. Top-1 and Inverted are methodological bounds, not alternative products. ",
      "Monte Carlo: base scenario only, median and 5-95 %. Differences, not accuracy."
    ),
    theme = theme(plot.caption = element_text(size = 6.5, colour = "grey30", hjust = 0))
  )

# Outputs ====================================================================

save_figure(figure_6, "fig06_methodological_effects", width = 11, height = 12)

# Validation =================================================================

tolerance <- 1e-6

# 1. gate + weighting = total, globally and by region. The total is also computed
#    independently, from the per-cell Base - Naive differences.
for (variable in c("P", "E")) {
  cell_difference <- cells[[paste0(variable, "_base")]] - cells[[paste0(variable, "_naive")]]

  global_independent <- weighted_mean_safe(cell_difference, cells$cell_weight)
  stopifnot(abs(global_means[[paste0(variable, "_gate")]] + global_means[[paste0(variable, "_weight")]] - global_independent) < tolerance)

  region_independent <- cells[
    ,
    .(independent = weighted_mean_safe(get(paste0(variable, "_base")) - get(paste0(variable, "_naive")), cell_weight)),
    by = region
  ]

  check <- merge(region_means, region_independent, by = "region")
  stopifnot(check[, all(abs(get(paste0(variable, "_gate")) + get(paste0(variable, "_weight")) - independent) < tolerance)])
}

# 2. one mask and one period: every method is computed from the same cell table,
#    and every cell has the full period for all five candidates.
stopifnot(nrow(cells) == uniqueN(cells[, .(lon, lat)]))
stopifnot(cells[, all(get(paste0("prec_n_years_mean_", CANDIDATES[1])) == FULL_YEARS)])

# 3. the deterministic Base lies within the Monte Carlo interval.
cat("\nGlobal long-term means (mm yr-1): deterministic Base vs base Monte Carlo 5 / 50 / 95 %\n")

for (variable in c("P", "E")) {
  base_value <- global_means[[paste0(variable, "_base")]]
  interval <- mc_summary[[variable]]

  cat(
    variable, ": Base ", round(base_value, 1), "   MC ", paste(round(interval, 1), collapse = " / "),
    if (base_value >= interval[1] && base_value <= interval[3]) "   (inside)" else "   (OUTSIDE the interval)",
    "\n", sep = ""
  )
}

# 4. neutral is exactly uniform among the surviving candidates in every cell.
neutral_check <- weight_diagnostics[scenario == "neutral", max(abs(eff_n - n_candidates))]
stopifnot(neutral_check < 1e-8)

# 5. Top-1 selects the maximum base probability (equal split only on exact ties).
top_matrix <- as.matrix(weight_tables$top1[, ..CANDIDATES])
base_matrix <- as.matrix(weight_tables$base[, ..CANDIDATES])
stopifnot(all(abs(rowSums(top_matrix) - 1) < tolerance))
stopifnot(all(base_matrix[top_matrix > 0] >= apply(base_matrix, 1, max)[row(top_matrix)[top_matrix > 0]] - tolerance))

# 6. Base, Neutral and Inverted give probability to exactly the same candidates.
support <- function(table) {
  table[, paste(region, biome, apply(as.matrix(.SD) > 0, 1, paste, collapse = "")), .SDcols = CANDIDATES]
}

stopifnot(identical(support(weight_tables$neutral), support(weight_tables$base)))
stopifnot(identical(support(weight_tables$inverted), support(weight_tables$base)))

cat("\nFootprint: ", nrow(cells), " cells, ",
    round(100 * cells[, sum(cell_weight)] / grid_classes[, sum(cell_weight)], 2),
    " % of land area; ", uniqueN(cells[, .(region, biome)]), " units\n", sep = "")

cat("\nGlobal effects (mm yr-1):\n")
print(
  data.table(
    variable = c("P", "E"),
    naive = c(global_means$P_naive, global_means$E_naive),
    gate = c(global_means$P_gate, global_means$E_gate),
    weighting = c(global_means$P_weight, global_means$E_weight),
    total = c(global_means$P_total, global_means$E_total),
    top1 = c(global_means$P_top1_shift, global_means$E_top1_shift),
    inverted = c(global_means$P_inverted_shift, global_means$E_inverted_shift)
  )[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)]
)

cat("\nMap colour limits (mm yr-1): P +/-", round(limit_p, 1), ", E +/-", round(limit_e, 1), "\n", sep = "")
