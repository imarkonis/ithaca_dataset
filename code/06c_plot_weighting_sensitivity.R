# ============================================================================
# Figure 4: weighting sensitivity. How much does the weighting scenario change
# which dataset is most representative?
#
# Four panels, 2 x 2:
#
# (a) Number of datasets that pass the per-cell physics filter of 03e in each
#     grid cell (0-5).
# (b) Most representative dataset across the eight scenarios (the mode of the
#     per-unit winners), shaded by the number of scenarios that agree (1-8).
# (c) Change in dataset weight relative to the base scenario, for every dataset
#     and every other scenario (percentage points of area-weighted mean weight).
# (d) Dataset flipping: where the most representative dataset of the base
#     scenario is X, the share of that area in which another scenario selects Y,
#     pooled over the seven non-base scenarios.
#
# The eight scenarios are SCENARIO_ORDER_MAIN: the ten scenarios of 03e without
# neutral (no weighting preference) and inverted (labelled "Disagreement", an
# adversarial stress test).
#
# (b)-(d) are at the operational resolution: region x biome units, painted onto
# the grid cells of each unit. The most representative dataset of a unit in a
# scenario is the dataset with the highest sampling probability. Units without
# an eligible dataset (explicit NA records from 03f) are shown as no data and
# left out of every percentage and average.
#
# Input   : weights_region_biome.Rds (03f), dataset_weights.Rds (03e),
#           grid_classes.Rds (01g)
# Output  : fig04_weighting_sensitivity.pdf / .png / _caption.txt in
#           PATH_OUTPUT_FIGURES
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")
source("code/_figs.R") # current publication design system; restores modern export helpers

# Inputs =====================================================================

weights_region_biome <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "weights_region_biome.Rds")
))

grid_classes <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
))

# Only the neutral scenario is needed: it gives exactly 1/n to every dataset
# that passes the per-cell physics filter, which does not depend on the scenario.
GATE_SCENARIO <- "neutral"

dataset_weights <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "dataset_weights.Rds")
))[scenario == GATE_SCENARIO & weight > 0, .(lon, lat, dataset)]

stopifnot(
  all(c("scenario", "region", "biome", "dataset", "w_region_biome") %in% names(weights_region_biome)),
  all(c("lon", "lat", "region", "biome", "cell_weight") %in% names(grid_classes)),
  !anyDuplicated(grid_classes[, .(lon, lat)]),
  all(SCENARIO_ORDER_MAIN %in% weights_region_biome$scenario),
  nrow(dataset_weights) > 0
)

# Constants & Variables ======================================================

N_CANDIDATES <- length(CANDIDATES)
N_SCENARIOS <- length(SCENARIO_ORDER_MAIN)
SCENARIOS_OTHER <- setdiff(SCENARIO_ORDER_MAIN, "base")
N_OTHER <- length(SCENARIOS_OTHER)

LEVEL_NO_DATA <- "No data"

# Final ESSD/Copernicus figure height. Width comes from the active journal profile.
FIGURE_HEIGHT_MM <- 140

# The map row is taller than the heatmap row: the maps are wide, and their
# legends sit below them.
ROW_HEIGHTS <- c(1, 1.15)

# Extra top margin keeps the panel tags clear of the panel titles.
PANEL_MARGIN <- ggplot2::margin(10, FIG_MARGIN_PT[["right"]], FIG_MARGIN_PT[["bottom"]], FIG_MARGIN_PT[["left"]], unit = "pt")

# Functions ==================================================================

# Paint unit values onto the grid cells of the mask.
paint_units <- function(cells, unit_values) {
  merge(cells, unit_values, by = c("region", "biome"), all.x = TRUE)
}

# Signed percentage-point label with a true minus sign. The sign follows the
# rounded value, so a change that rounds to zero is shown as 0.0, never -0.0.
label_signed <- function(x, digits = 1) {
  x <- round(x, digits)

  paste0(
    ifelse(x > 0, "+", ifelse(x < 0, "−", "")),
    formatC(abs(x), format = "f", digits = digits)
  )
}

# Heatmap in the publication style: tiles with a value label, no axis lines.
# `text_colour` holds one label colour per row of `dt`, chosen from the tile
# fill so that every label is readable.
heatmap_panel <- function(dt, x, y, fill, label, fill_scale, text_colour) {
  ggplot(dt, aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]])) +
    geom_tile(colour = COL_WHITE, linewidth = FIG_AXIS_LINEWIDTH) +
    geom_text(
      aes(label = .data[[label]]),
      colour = text_colour,
      size = FIG_GEOM_TEXT_SIZE,
      family = FIG_FONT
    ) +
    fill_scale +
    scale_x_discrete(expand = expansion(mult = 0)) +
    scale_y_discrete(expand = expansion(mult = 0)) +
    labs(x = NULL, y = NULL) +
    facet_wrap(vars(panel)) +
    theme_pub(legend_position = "bottom") +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      strip.text = element_text(hjust = 0, size = FIG_STRIP_SIZE),
      plot.margin = PANEL_MARGIN
    )
}

# Analysis ===================================================================

weights <- weights_region_biome[scenario %in% SCENARIO_ORDER_MAIN]
weights[, dataset := as.character(dataset)]

# Units with no eligible dataset carry one explicit NA record per scenario.
no_selection_units <- unique(weights[is.na(dataset), .(region, biome)])
weights <- weights[!is.na(dataset)]

stopifnot(
  all(weights$dataset %in% CANDIDATES),
  all(is.finite(weights$w_region_biome) & weights$w_region_biome >= 0),
  !anyDuplicated(weights[, .(scenario, region, biome, dataset)])
)

weighted_units <- unique(weights[, .(region, biome)])

# The mask: grid cells of every region x biome unit.
cells <- grid_classes[
  !is.na(region) & !is.na(biome),
  .(lon, lat, region, biome)
]

area <- unit_area(grid_classes)
total_area <- area[weighted_units, on = .(region, biome), sum(area)]

# (a) Datasets passing the per-cell gate -------------------------------------------

gate_cells <- dataset_weights[, .(n_datasets = .N), by = .(lon, lat)]

gate_dt <- merge(cells, gate_cells, by = c("lon", "lat"), all.x = TRUE)
gate_dt[is.na(n_datasets), n_datasets := 0L]
gate_dt[, n_datasets_f := factor(n_datasets, levels = 0:N_CANDIDATES)]
gate_dt[, panel := factor("Datasets passing the gate")]

gate_scale <- scale_fill_cat(
  name = "Number of datasets",
  values = setNames(
    c(COL_MISSING, fig_colours("seq_default", N_CANDIDATES)),
    0:N_CANDIDATES
  ),
  drop = FALSE,
  guide = guide_legend(nrow = 2)
)

# Compact legends under the maps: keys as small as the legend text allows.
compact_legend <- theme(
  legend.key.width = grid::unit(FIG_LEGEND_KEY_MM, "mm"),
  legend.box = "horizontal",
  legend.box.just = "top",
  strip.text = element_text(hjust = 0, size = FIG_STRIP_SIZE),
  plot.margin = PANEL_MARGIN
)

p_gate <- publication_map(gate_dt, "n_datasets_f", gate_scale, "bottom", coord_labels = FALSE) +
  facet_wrap(vars(panel)) +
  compact_legend

# (b) Agreement on the most representative dataset ---------------------------------------

# Highest sampling probability wins in each unit and scenario.
setorder(weights, scenario, region, biome, -w_region_biome, dataset)

winners <- weights[
  ,
  .(winner = dataset[1]),
  by = .(scenario, region, biome)
]

stopifnot(winners[, .N, by = .(region, biome)][, all(N == N_SCENARIOS)])

agreement <- winners[, .N, by = .(region, biome, winner)][
  order(region, biome, -N, winner),
  .(modal = winner[1], n_agree = N[1]),
  by = .(region, biome)
]

agreement_dt <- paint_units(cells, agreement)
agreement_dt[is.na(modal), `:=`(modal = LEVEL_NO_DATA, n_agree = N_SCENARIOS)]
agreement_dt[, modal := factor(modal, levels = c(CANDIDATES, LEVEL_NO_DATA))]
agreement_dt[, panel := factor("Scenario agreement")]

modal_scale <- scale_fill_cat(
  name = "Most representative dataset",
  values = c(PAL_DATASETS[CANDIDATES], setNames(COL_MISSING, LEVEL_NO_DATA)),
  limits = c(CANDIDATES, LEVEL_NO_DATA),
  drop = FALSE,
  guide = guide_legend(nrow = 2, order = 1)
)

# Agreement is the shade: the more scenarios select the modal dataset, the
# fuller the colour.
agree_scale <- scale_alpha_continuous(
  name = "Scenarios agreeing",
  range = c(0.3, 1),
  limits = c(1, N_SCENARIOS),
  breaks = c(2, 4, 6, 8),
  guide = guide_legend(nrow = 2, order = 2, override.aes = list(fill = COL_REFERENCE))
)

p_agree <- publication_map(agreement_dt, "modal", modal_scale, "bottom", coord_labels = FALSE) +
  aes(alpha = n_agree) +
  agree_scale +
  facet_wrap(vars(panel)) +
  compact_legend

# (c) Weight change relative to the base scenario ---------------------------------------

# Area-weighted mean sampling probability (%) of every dataset in every scenario.
shares <- merge(
  weights,
  area,
  by = c("region", "biome")
)[
  ,
  .(share = 100 * sum(area * w_region_biome) / total_area),
  by = .(scenario, dataset)
]

shares <- merge(
  CJ(scenario = SCENARIO_ORDER_MAIN, dataset = CANDIDATES),
  shares,
  by = c("scenario", "dataset"),
  all.x = TRUE
)
shares[is.na(share), share := 0]

change <- merge(
  shares[scenario %in% SCENARIOS_OTHER],
  shares[scenario == "base", .(dataset, base_share = share)],
  by = "dataset"
)
change[, change := share - base_share]

CHANGE_LIMIT <- ceiling(max(abs(change$change)))
check_limits(change$change, c(-CHANGE_LIMIT, CHANGE_LIMIT), "Weight change relative to base")

change[, `:=`(
  scenario_f = factor(scenario, levels = rev(SCENARIOS_OTHER), labels = rev(SCENARIO_LABELS[SCENARIOS_OTHER])),
  dataset_f = factor(dataset, levels = CANDIDATES),
  label = label_signed(change),
  panel = factor("Weight change relative to base")
)]

change_scale <- scale_fill_div(
  name = "Change in weight (percentage points)",
  midpoint = 0,
  type = "change",
  limits = c(-CHANGE_LIMIT, CHANGE_LIMIT),
  labels = label_minus
)

change_fill <- scales::pal_gradient_n(fig_colours("div_change"))(
  scales::rescale(change$change, to = c(0, 1), from = c(-CHANGE_LIMIT, CHANGE_LIMIT))
)

p_change <- heatmap_panel(
  change, "dataset_f", "scenario_f", "change", "label",
  change_scale, label_colour(change_fill)
)

# (d) Dataset flipping -----------------------------------------------------------------------------

base_winner <- winners[scenario == "base", .(region, biome, from = winner)]
other_winner <- winners[scenario %in% SCENARIOS_OTHER, .(region, biome, to = winner)]

flips <- merge(other_winner, base_winner, by = c("region", "biome"))
flips <- merge(flips, area, by = c("region", "biome"))

flip_area <- flips[, .(flip_area = sum(area)), by = .(from, to)]

# Base area of each dataset, counted once per other scenario.
base_area <- merge(base_winner, area, by = c("region", "biome"))[
  ,
  .(base_area = sum(area) * N_OTHER),
  by = from
]

flip <- merge(
  CJ(from = CANDIDATES, to = CANDIDATES),
  flip_area,
  by = c("from", "to"),
  all.x = TRUE
)
flip <- merge(flip, base_area, by = "from", all.x = TRUE)
flip[is.na(flip_area) & !is.na(base_area), flip_area := 0]
flip[, flip_pct := 100 * flip_area / base_area]

# The diagonal is no change: it is left grey and unlabelled.
flip[from == to, flip_pct := NA_real_]

FLIP_LIMIT <- ceiling(max(flip$flip_pct, na.rm = TRUE))

flip[, `:=`(
  from_f = factor(from, levels = rev(CANDIDATES)),
  to_f = factor(to, levels = CANDIDATES),
  label = fifelse(is.na(flip_pct), "", formatC(flip_pct, format = "f", digits = 1)),
  panel = factor("Dataset flipping")
)]

flip_scale <- scale_fill_seq(
  name = "Area changing dataset (%)",
  palette = "seq_default",
  limits = c(0, FLIP_LIMIT),
  labels = label_minus
)

flip_fill <- scales::pal_gradient_n(fig_colours("seq_default"))(
  scales::rescale(flip$flip_pct, to = c(0, 1), from = c(0, FLIP_LIMIT))
)
flip_fill[is.na(flip$flip_pct)] <- COL_MISSING

p_flip <- heatmap_panel(
  flip, "to_f", "from_f", "flip_pct", "label",
  flip_scale, label_colour(flip_fill)
) +
  labs(x = "Other scenarios", y = "Base")

# Assembly ---------------------------------------------------------------------------------------

# Each panel is a layout element of its own: the maps (fixed aspect ratio) are
# not pushed sideways to line up with the row labels of the heatmaps below.
figure_4 <- add_panel_tags(
  ((plot_element(p_gate) | plot_element(p_agree)) /
     (plot_element(p_change) | plot_element(p_flip))) +
    plot_layout(heights = ROW_HEIGHTS)
)

# Outputs ====================================================================

figure_stem <- file.path(PATH_OUTPUT_FIGURES, "fig04_weighting_sensitivity")
quiet_raster_gaps(
  save_figure(
    figure_4,
    file_stem = figure_stem,
    width_mm = FIG_WIDTH_DOUBLE,
    height_mm = FIGURE_HEIGHT_MM
  )
)
check_fonts(paste0(figure_stem, ".pdf"))

caption <- paste0(
  "Sensitivity of the most representative dataset to the weighting scenario. ",
  "Maps show the 0.25° land grid (180° W–180° E, 58° S–84° N). ",
  "(a) Number of the five datasets that pass the per-cell physical-consistency filter in each grid cell. ",
  "(b) Most representative dataset, that is the one with the highest sampling probability in a ",
  "region × biome unit, selected by most of the ", N_SCENARIOS, " scenarios: ",
  paste(SCENARIO_LABELS[SCENARIO_ORDER_MAIN], collapse = ", "), ". ",
  "The neutral and the disagreement scenario are not included. Colour shows the dataset and ",
  "shading the number of scenarios that agree; units without an eligible dataset are shown as no data. ",
  "(c) Change in the area-weighted mean sampling probability of each dataset in each scenario ",
  "compared with the base scenario, in percentage points. ",
  "(d) Share of the area, in percent, where the most representative dataset of the base scenario ",
  "(rows) is replaced by another dataset (columns) in the other ", N_OTHER, " scenarios; ",
  "each scenario counts equally, and the diagonal (no change) is left grey. ",
  "Data: dataset_weights.Rds (a), weights_region_biome.Rds (b–d), 1982–2021."
)
writeLines(caption, paste0(figure_stem, "_caption.txt"))

# Validation =================================================================

# Probabilities sum to 1 in every unit and scenario.
stopifnot(weights[, .(s = sum(w_region_biome)), by = .(scenario, region, biome)][, all(abs(s - 1) < 1e-6)])

# Dataset shares add up to 100 % in every scenario.
stopifnot(shares[, .(s = sum(share)), by = scenario][, all(abs(s - 100) < 1e-6)])

# Changes relative to base add up to zero in every scenario.
stopifnot(change[, .(s = sum(change)), by = scenario][, all(abs(s) < 1e-6)])

# Every scenario agrees at least ceiling(N_SCENARIOS / N_CANDIDATES) times on the mode.
stopifnot(agreement[, all(n_agree >= ceiling(N_SCENARIOS / N_CANDIDATES) & n_agree <= N_SCENARIOS)])

# Flips plus the unchanged share make 100 % of each dataset's base area.
stay <- merge(
  flips[from == to, .(stay_area = sum(area)), by = from],
  base_area,
  by = "from"
)
stopifnot(
  merge(flip[from != to, .(off = sum(flip_pct)), by = from], stay, by = "from")[
    , all(abs(off + 100 * stay_area / base_area - 100) < 1e-6)
  ]
)

cat("\nUnits: ", nrow(unique(cells[, .(region, biome)])), " in the grid, ", nrow(weighted_units),
    " with weights, ", nrow(no_selection_units), " with no eligible dataset (NA records)\n", sep = "")

cat("\nGrid cells by number of datasets passing the gate:\n")
print(gate_dt[, .(cells = .N), by = n_datasets][order(n_datasets)])

cat("\nScenarios agreeing on the most representative dataset (of ", N_SCENARIOS, "), by unit:\n", sep = "")
print(agreement[, .(units = .N), by = n_agree][order(n_agree)])

cat("\nArea-weighted mean weight (%) by scenario and dataset:\n")
print(dcast(shares, scenario ~ dataset, value.var = "share")[match(SCENARIO_ORDER_MAIN, scenario)])

cat("\nArea (%) where the base-scenario dataset (row) flips to another (column), all scenarios:\n")
print(dcast(flip, from ~ to, value.var = "flip_pct")[match(CANDIDATES, from)])
