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
# Panels: (a) global ladder of the methods; (b) Base - Naive per grid cell for P
# and E; (c) regional decomposition of the effect into gate and weighting.
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
#
# Input   : prec_evap_stats.Rds (03a), grid_classes.Rds (01g),
#           weights_region_biome.Rds (03f), region_classes.Rds,
#           mc_global_year_scenarios.Rds, dataset_weight_diagnostics.Rds
# Output  : fig06_methodological_effects.pdf / .png / _caption.txt in
#           PATH_OUTPUT_FIGURES
# Note    : converted to the shared figure standard (docs/SCIENTIFIC_FIGURE_*.md,
#           code/_figs.R) with AI assistance (Claude Code, 2026-10-06); to be
#           checked by the authors
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")
source("code/_figs.R") # current publication design system; restores modern export helpers

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

# Scientific parameters ======================================================

N_CANDIDATES <- length(CANDIDATES)
FULL_YEARS <- diff(FULL_PERIOD) + 1

# Interval of the base Monte Carlo members, the same as in Figure 5.
MC_PROBS <- c(0.05, 0.5, 0.95)

# Map colour limits are symmetric around zero (no change) and taken from this
# area-weighted quantile of |Base - Naive|, so a few extreme cells do not wash
# out the pattern; cells beyond the limit are drawn in the end colours and their
# area share is stated in the caption. One limit serves P and E, because the two
# maps are read against each other; a warning is raised if the two variables
# need limits that differ by more than this ratio.
MAP_LIMIT_QUANTILE <- 0.98
SHARED_LIMIT_RATIO <- 1.5

# Köppen-Geiger main groups (level 1): ordering and labels of the regional panel.
# Regions are ordered by group, then from north to south (area-weighted latitude).
CLIMATE_CLASSES <- c(
  A = "tropical", B = "arid", C = "temperate", D = "continental", E = "polar"
)
CLIMATE_OTHER <- "Other"

# Numerical tolerance of the consistency checks.
CHECK_TOLERANCE <- 1e-6

# Display encodings ==========================================================

# Methods are not datasets, so they use none of the dataset colours. The focal
# product (Base, as in Figure 5) is the highlight colour; the gate step and the
# methods that only use the gate are grey; the two foils are dark, told apart by
# marker shape. Every row is also labelled directly on the axis.
METHOD_LEVELS <- c("Naive mean", "Neutral", "Base", "Base Monte Carlo", "Top-1", "Inverted")

METHOD_COLOUR <- c(
  "Naive mean" = COL_REFERENCE, "Neutral" = COL_REFERENCE,
  "Base" = COL_HIGHLIGHT, "Base Monte Carlo" = COL_HIGHLIGHT,
  "Top-1" = COL_TEXT, "Inverted" = COL_TEXT
)
METHOD_FILL <- c(
  "Naive mean" = COL_WHITE, "Neutral" = COL_REFERENCE,
  "Base" = COL_HIGHLIGHT, "Base Monte Carlo" = COL_HIGHLIGHT,
  "Top-1" = COL_TEXT, "Inverted" = COL_WHITE
)
METHOD_SHAPE <- setNames(FIG_SHAPES[c(1, 1, 1, 4, 3, 2)], METHOD_LEVELS)

# Effects of the regional decomposition, in legend order.
EFFECT_LABELS <- c(
  gate = "Gate (Neutral − Naive)",
  weight = "Weighting (Base − Neutral)",
  total = "Total (Base − Naive)",
  inverted = "Inverted − Naive"
)
EFFECT_COLOUR <- c(gate = COL_REFERENCE, weight = COL_HIGHLIGHT, total = COL_TEXT, inverted = COL_TEXT)
EFFECT_FILL <- c(gate = COL_REFERENCE, weight = COL_HIGHLIGHT, total = COL_TEXT, inverted = COL_WHITE)
EFFECT_SHAPE <- c(gate = FIG_SHAPES[2], weight = FIG_SHAPES[2], total = FIG_SHAPES[6], inverted = FIG_SHAPES[2])

PANEL_LABELS <- c(P = "Precipitation", E = "Evaporation")

# Ladder value labels go to the right of their marker, or to the left when the
# marker lies in the right part of its panel's range (above this fraction), so
# the label stays inside the panel.
LABEL_LEFT_FRACTION <- 0.8

# Final ESSD/Copernicus figure height (the routine maximum). Width comes from the
# active journal profile.
FIGURE_HEIGHT_MM <- 170

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

# Signed number with a typographic minus, for the caption.
signed <- function(x, digits = 1) {
  sub("-", "−", formatC(x, format = "f", digits = digits, flag = "+"), fixed = TRUE)
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
  P = quantile(mc_members$prec, MC_PROBS, names = FALSE),
  E = quantile(mc_members$evap, MC_PROBS, names = FALSE)
)

# Panel (a): ladder data ----------------------------------------------------------------------------

ladder_for <- function(variable) {
  v <- function(method_name) global_means[[paste0(variable, "_", method_name)]]

  data.table(
    panel = PANEL_LABELS[[variable]],
    method = METHOD_LEVELS,
    value = c(v("naive"), v("neutral"), v("base"), mc_summary[[variable]][2], v("top1"), v("inverted")),
    lo = c(NA, NA, NA, mc_summary[[variable]][1], NA, NA),
    hi = c(NA, NA, NA, mc_summary[[variable]][3], NA, NA)
  )
}

ladder <- rbindlist(lapply(c("P", "E"), ladder_for))
ladder[, `:=`(
  panel = factor(panel, levels = PANEL_LABELS),
  method = factor(method, levels = rev(METHOD_LEVELS))
)]

ladder[
  ,
  label_left := (value - min(value, lo, na.rm = TRUE)) /
    (max(value, hi, na.rm = TRUE) - min(value, lo, na.rm = TRUE)) > LABEL_LEFT_FRACTION,
  by = panel
]

# The two steps of the sequence Naive -> Neutral (gate) -> Base (weighting).
ladder_steps <- rbindlist(lapply(c("P", "E"), function(variable) {
  v <- function(method_name) global_means[[paste0(variable, "_", method_name)]]

  data.table(
    panel = factor(PANEL_LABELS[[variable]], levels = PANEL_LABELS),
    step = c("gate", "weight"),
    x = c(v("naive"), v("neutral")),
    xend = c(v("neutral"), v("base")),
    y = factor(c("Naive mean", "Neutral"), levels = levels(ladder$method)),
    yend = factor(c("Neutral", "Base"), levels = levels(ladder$method))
  )
}))

# Panel (c): regional ordering --------------------------------------------------------------------------

region_order <- merge(
  region_classes[, .(region, kg_class_1 = as.character(kg_class_1))],
  grid_classes[!is.na(region), .(latitude = weighted_mean_safe(lat, cell_weight)), by = region],
  by = "region"
)

region_order[
  ,
  climate := fifelse(kg_class_1 %in% names(CLIMATE_CLASSES), kg_class_1, CLIMATE_OTHER)
]

region_order[, climate := factor(climate, levels = c(names(CLIMATE_CLASSES), CLIMATE_OTHER))]
setorder(region_order, climate, -latitude)

region_effects <- merge(region_means, region_order[, .(region, climate)], by = "region")
region_effects[, region := factor(region, levels = rev(region_order$region))]

# Panel (b): Base - Naive per cell ------------------------------------------------------------------------

cell_differences <- rbindlist(lapply(c("P", "E"), function(variable) {
  cells[
    ,
    .(
      lon, lat, cell_weight,
      panel = factor(PANEL_LABELS[[variable]], levels = PANEL_LABELS),
      difference = get(paste0(variable, "_base")) - get(paste0(variable, "_naive"))
    )
  ]
}))

limits_by_variable <- cell_differences[
  ,
  .(limit = weighted_quantile(abs(difference), cell_weight, MAP_LIMIT_QUANTILE)),
  by = panel
]

MAP_LIMIT <- max(limits_by_variable$limit)

if (MAP_LIMIT / min(limits_by_variable$limit) > SHARED_LIMIT_RATIO) {
  warning(
    "The map limits of P and E differ by more than a factor ", SHARED_LIMIT_RATIO,
    "; the shared colour scale hides the pattern of the variable with the smaller limit.",
    call. = FALSE
  )
}

check_limits(cell_differences$difference, c(-MAP_LIMIT, MAP_LIMIT), what = "Base - Naive difference")

# Plots ===================================================================================================

strip_left_aligned <- theme(strip.text = element_text(hjust = 0, size = FIG_STRIP_SIZE))

# (a) Ladder: one row per method, one column per variable (free x scale).
ladder_panel <- function() {
  ggplot(ladder, aes(x = value, y = method)) +
    geom_segment(
      data = ladder_steps[step == "gate"], aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE, colour = EFFECT_COLOUR[["gate"]], linewidth = FIG_LINEWIDTH,
      arrow = arrow(length = unit(FIG_POINT_SIZE, "mm"), type = "closed")
    ) +
    geom_segment(
      data = ladder_steps[step == "weight"], aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE, colour = EFFECT_COLOUR[["weight"]], linewidth = FIG_LINEWIDTH,
      arrow = arrow(length = unit(FIG_POINT_SIZE, "mm"), type = "closed")
    ) +
    geom_linerange(
      data = ladder[!is.na(lo)], aes(xmin = lo, xmax = hi, y = method),
      inherit.aes = FALSE, colour = METHOD_COLOUR[["Base Monte Carlo"]], linewidth = FIG_LINEWIDTH_EMPH
    ) +
    geom_point(
      aes(shape = method, fill = method, colour = method),
      size = FIG_POINT_SIZE * 2, stroke = FIG_POINT_STROKE * 2
    ) +
    # Values are printed beside their marker, clear of the near-vertical arrows;
    # the Monte Carlo value sits above its interval line.
    geom_text(
      data = ladder[method != "Base Monte Carlo" & !label_left], aes(label = sprintf("%.1f", value)),
      hjust = -0.35, size = FIG_GEOM_TEXT_SIZE, colour = COL_TEXT, family = FIG_FONT
    ) +
    geom_text(
      data = ladder[method != "Base Monte Carlo" & label_left], aes(label = sprintf("%.1f", value)),
      hjust = 1.35, size = FIG_GEOM_TEXT_SIZE, colour = COL_TEXT, family = FIG_FONT
    ) +
    geom_text(
      data = ladder[method == "Base Monte Carlo"], aes(label = sprintf("%.1f", value)),
      vjust = -1.1, size = FIG_GEOM_TEXT_SIZE, colour = COL_TEXT, family = FIG_FONT
    ) +
    scale_shape_manual(values = METHOD_SHAPE, guide = "none") +
    scale_fill_manual(values = METHOD_FILL, guide = "none") +
    scale_colour_manual(values = METHOD_COLOUR, guide = "none") +
    facet_grid(. ~ panel, scales = "free_x") +
    # More room on the right, where the value labels are.
    scale_x_continuous(breaks = scales::breaks_pretty(n = 4), labels = label_minus,
                       expand = expansion(mult = c(0.12, 0.3))) +
    labs(x = expression("Global mean (mm yr"^{-1} * ")"), y = NULL) +
    theme_pub(grid = TRUE, legend_position = "none") +
    theme(panel.grid.major.x = element_blank()) +
    strip_left_aligned
}

# (b) Base - Naive maps, one shared colour scale.
difference_maps <- function() {
  publication_map(
    cell_differences, "difference",
    scale_fill_div(
      name = expression(Delta ~ "Base − Naive (mm yr"^{-1} * ")"),
      midpoint = 0, type = "change",
      limits = c(-MAP_LIMIT, MAP_LIMIT), labels = label_minus
    ),
    legend_position = "bottom", coord_labels = FALSE
  ) +
    facet_wrap(vars(panel), ncol = 1) +
    strip_left_aligned +
    # Left margin that holds the panel tag, so it does not sit on the strip text.
    theme(plot.margin = margin(
      FIG_MARGIN_PT[["top"]], FIG_MARGIN_PT[["right"]], FIG_MARGIN_PT[["bottom"]],
      FIG_MARGIN_PT[["left"]] + 2 * FIG_TAG_SIZE, unit = "pt"
    ))
}

# (c) Regional decomposition: the gate and weighting effects are stacked by sign
# from zero (positive effects to the right, negative to the left, gate first), so
# each bar keeps its own length even when the two effects have opposite signs;
# the total (dot) is their sum, and the inverted shift is the open square. The
# two variables are separate plots with the same rows; the left one carries the
# row labels.
region_panel <- function(variable, show_labels) {
  d <- region_effects[
    ,
    .(
      region, climate,
      panel = factor(PANEL_LABELS[[variable]], levels = PANEL_LABELS),
      gate = get(paste0(variable, "_gate")),
      weight = get(paste0(variable, "_weight")),
      total = get(paste0(variable, "_total")),
      inverted = get(paste0(variable, "_inverted_shift"))
    )
  ]

  # Row position inside each climate facet (a free discrete scale numbers only
  # the rows that are present), for the bar rectangles.
  d[, row := frank(as.integer(region), ties.method = "first"), by = climate]

  d[, `:=`(
    gate_lo = pmin(gate, 0),
    gate_hi = pmax(gate, 0),
    weight_start = fifelse(weight >= 0, pmax(gate, 0), pmin(gate, 0))
  )]
  d[, `:=`(
    weight_lo = pmin(weight_start, weight_start + weight),
    weight_hi = pmax(weight_start, weight_start + weight)
  )]

  p <- ggplot(d, aes(y = region)) +
    geom_vline_ref(0) +
    geom_rect(
      aes(xmin = gate_lo, xmax = gate_hi, ymin = row - FIG_BAR_HALF_HEIGHT, ymax = row + FIG_BAR_HALF_HEIGHT),
      fill = EFFECT_FILL[["gate"]]
    ) +
    geom_rect(
      aes(xmin = weight_lo, xmax = weight_hi, ymin = row - FIG_BAR_HALF_HEIGHT, ymax = row + FIG_BAR_HALF_HEIGHT),
      fill = EFFECT_FILL[["weight"]]
    ) +
    geom_point(
      aes(x = total), shape = EFFECT_SHAPE[["total"]], size = FIG_POINT_SIZE,
      colour = EFFECT_COLOUR[["total"]]
    ) +
    geom_point(
      aes(x = inverted), shape = EFFECT_SHAPE[["inverted"]], size = FIG_POINT_SIZE,
      colour = EFFECT_COLOUR[["inverted"]], fill = EFFECT_FILL[["inverted"]], stroke = FIG_POINT_STROKE * 2
    ) +
    facet_grid(climate ~ panel, scales = "free_y", space = "free_y", switch = "y") +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3), labels = label_minus,
                       expand = expansion(mult = 0.05)) +
    labs(x = expression(Delta ~ "(mm yr"^{-1} * ")"), y = NULL) +
    theme_pub(legend_position = "none") +
    theme(strip.placement = "outside", strip.text.x = element_text(hjust = 0, size = FIG_STRIP_SIZE))

  if (show_labels) {
    p + theme(strip.text.y.left = element_text(angle = 0, size = FIG_STRIP_SIZE))
  } else {
    p + theme(
      strip.text.y.left = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
}

# Legend of the regional decomposition (squares are bars, dot and open square
# are the markers).
effect_legend_plot <- function() {
  key_data <- data.table(
    effect = factor(EFFECT_LABELS, levels = EFFECT_LABELS),
    x = seq_along(EFFECT_LABELS), y = 1
  )

  ggplot(key_data, aes(x = x, y = y)) +
    geom_point(
      aes(colour = effect, fill = effect, shape = effect),
      size = FIG_POINT_SIZE * 1.6, stroke = FIG_POINT_STROKE * 2
    ) +
    scale_colour_manual(name = NULL, values = setNames(EFFECT_COLOUR, EFFECT_LABELS)) +
    scale_fill_manual(name = NULL, values = setNames(EFFECT_FILL, EFFECT_LABELS)) +
    scale_shape_manual(name = NULL, values = setNames(EFFECT_SHAPE, EFFECT_LABELS)) +
    guides(
      colour = guide_legend(nrow = 2, byrow = TRUE),
      fill = guide_legend(nrow = 2, byrow = TRUE),
      shape = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    theme_pub(legend_position = "bottom")
}

p_ladder <- ladder_panel()
p_maps <- difference_maps()
p_region_p <- region_panel("P", show_labels = TRUE)
p_region_e <- region_panel("E", show_labels = FALSE)

# One flat layout: the ladder and the maps on the left, the regional decomposition
# (two plots) on the right, and the legend of the decomposition under the maps.
# Reading order: global magnitude (a), spatial pattern (b), regional attribution
# (c). Only (a), (b) and (c) are tagged, in the profile format.
panel_tags <- c(paste0(FIG_SPEC$tag_prefix, c("a", "b", "c"), FIG_SPEC$tag_suffix), "", "")

# The ladder and the maps are whole-plot elements: they are not panel-aligned
# with each other, so the maps use the full width of their column instead of
# inheriting the margin of the ladder's method labels. Widths and heights are
# relative (about mm); the regional plots share their rows by construction.
figure_6 <- wrap_plots(
  plot_element(p_ladder), plot_element(p_maps), p_region_p, p_region_e,
  legend_element(effect_legend_plot()),
  design = "ACD\nBCD\nECD",
  widths = c(88, 54, 41),
  heights = c(48, 108, 11)
) +
  plot_annotation(tag_levels = list(panel_tags)) &
  theme(plot.tag = element_text(family = FIG_FONT, size = FIG_TAG_SIZE, face = "bold", colour = COL_TEXT))

# Outputs ====================================================================

figure_stem <- file.path(PATH_OUTPUT_FIGURES, "fig06_methodological_effects")
quiet_raster_gaps(save_figure(
  figure_6,
  file_stem = figure_stem,
  width_mm = FIG_WIDTH_DOUBLE,
  height_mm = FIGURE_HEIGHT_MM
))
check_fonts(paste0(figure_stem, ".pdf"))

footprint_share <- 100 * cells[, sum(cell_weight)] / grid_classes[, sum(cell_weight)]

beyond_share <- cell_differences[
  ,
  .(share = 100 * sum(cell_weight[abs(difference) > MAP_LIMIT]) / sum(cell_weight)),
  by = panel
]

# "gate, weighting and total" effects of one variable, e.g. "+2.3, −1.7 and +0.7".
effect_values <- function(variable) {
  paste0(
    signed(global_means[[paste0(variable, "_gate")]]), ", ",
    signed(global_means[[paste0(variable, "_weight")]]), " and ",
    signed(global_means[[paste0(variable, "_total")]])
  )
}

caption <- paste0(
  "Methodological effects on the long-term (1982–2021) mean precipitation and evaporation of the ",
  "ITHACA product. (a) Global land mean for the naive mean of the five coherent P–E candidate pairs ",
  "(equal weights), Neutral (equal weights among the candidates that pass the physics gate), Base (gate ",
  "and performance weights), the base Monte Carlo (median, with the 5th–95th percentile range of the ",
  "100 members), and two methodological bounds that are not alternative products: Top-1 (all weight on the ",
  "unit’s highest-weight candidate) and Inverted (adversarial weights). Arrows show the gate step ",
  "(grey) and the weighting step (purple). The gate, weighting and total effects on the global mean are ",
  effect_values("P"), " mm yr⁻¹ for precipitation and ", effect_values("E"),
  " mm yr⁻¹ for evaporation. ",
  "(b) Base minus naive mean per 0.25° grid cell (180° W–180° E, 58° S–84° N) on one ",
  "symmetric colour scale limited to ±", sprintf("%.0f", MAP_LIMIT), " mm yr⁻¹ (the ",
  sprintf("%.0f", 100 * MAP_LIMIT_QUANTILE), "th area-weighted percentile of the absolute differences); ",
  "larger differences, ", sprintf("%.1f", beyond_share[panel == PANEL_LABELS[["P"]], share]), " % of the ",
  "footprint area for precipitation and ", sprintf("%.1f", beyond_share[panel == PANEL_LABELS[["E"]], share]),
  " % for evaporation, are drawn in the end colours. (c) Difference from the naive mean (Δ) of the ",
  "regional means, split into the gate effect and the weighting effect (bars stacked by sign from zero), ",
  "with the total (dots) and the Inverted shift (open squares); regions are grouped by ",
  "Köppen–Geiger main group (",
  paste(names(CLIMATE_CLASSES), CLIMATE_CLASSES, collapse = ", "), ") and ordered from north to south. ",
  "All values use one footprint (cells with all five candidates for the full 40 years, ",
  sprintf("%.1f", footprint_share), " % of land area), the region × biome weights of the ",
  "operational product and cell-area weights; axis ranges differ between panels. The figure shows ",
  "differences between methods, not accuracy. ",
  "Data: prec_evap_stats.Rds, weights_region_biome.Rds, mc_global_year_scenarios.Rds."
)
writeLines(caption, paste0(figure_stem, "_caption.txt"))

# Validation =================================================================

# 1. gate + weighting = total, globally and by region. The total is also computed
#    independently, from the per-cell Base - Naive differences.
for (variable in c("P", "E")) {
  cell_difference <- cells[[paste0(variable, "_base")]] - cells[[paste0(variable, "_naive")]]

  global_independent <- weighted_mean_safe(cell_difference, cells$cell_weight)
  stopifnot(abs(global_means[[paste0(variable, "_gate")]] + global_means[[paste0(variable, "_weight")]] - global_independent) < CHECK_TOLERANCE)

  region_independent <- cells[
    ,
    .(independent = weighted_mean_safe(get(paste0(variable, "_base")) - get(paste0(variable, "_naive")), cell_weight)),
    by = region
  ]

  check <- merge(region_means, region_independent, by = "region")
  stopifnot(check[, all(abs(get(paste0(variable, "_gate")) + get(paste0(variable, "_weight")) - independent) < CHECK_TOLERANCE)])
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
stopifnot(all(abs(rowSums(top_matrix) - 1) < CHECK_TOLERANCE))
stopifnot(all(base_matrix[top_matrix > 0] >= apply(base_matrix, 1, max)[row(top_matrix)[top_matrix > 0]] - CHECK_TOLERANCE))

# 6. Base, Neutral and Inverted give probability to exactly the same candidates.
support <- function(table) {
  table[, paste(region, biome, apply(as.matrix(.SD) > 0, 1, paste, collapse = "")), .SDcols = CANDIDATES]
}

stopifnot(identical(support(weight_tables$neutral), support(weight_tables$base)))
stopifnot(identical(support(weight_tables$inverted), support(weight_tables$base)))

# 7. every region of the decomposition has a climate group and a row.
stopifnot(
  !anyNA(region_effects$climate),
  nlevels(droplevels(region_effects$region)) == uniqueN(region_effects$region)
)

cat("\nFootprint: ", nrow(cells), " cells, ", round(footprint_share, 2),
    " % of land area; ", uniqueN(cells[, .(region, biome)]), " units; ",
    uniqueN(region_effects$region), " regions\n", sep = "")

cat("\nRegions by Koeppen-Geiger main group:\n")
print(region_effects[, .N, by = climate][order(climate)])

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

cat("\nBase - Naive per cell (area-weighted, mm yr-1):\n")
print(
  cell_differences[
    ,
    {
      q <- weighted_quantile(difference, cell_weight, c(0.05, 0.5, 0.95))
      .(
        q05 = q[1], median = q[2], q95 = q[3],
        median_abs = weighted_quantile(abs(difference), cell_weight, 0.5),
        limit = weighted_quantile(abs(difference), cell_weight, MAP_LIMIT_QUANTILE)
      )
    },
    by = panel
  ][, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)]
)

cat("\nMap colour limit (mm yr-1): +/-", round(MAP_LIMIT, 1),
    "; area share beyond it (%): P ", round(beyond_share[panel == PANEL_LABELS[["P"]], share], 2),
    ", E ", round(beyond_share[panel == PANEL_LABELS[["E"]], share], 2), "\n", sep = "")
