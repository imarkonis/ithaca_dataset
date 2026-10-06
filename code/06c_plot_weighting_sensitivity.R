# ============================================================================
# Figure 4: weighting sensitivity. How does ITHACA respond to the methodological
# assumptions?
#
# All maps are at the operational resolution: region x biome units, painted onto
# the grid cells of each unit. Everything is read from weights_region_biome.Rds
# (03f); nothing is re-estimated.
#
# (a) dominant dataset (highest sampling probability) in each of the eight
#     weighting scenarios;
# (b) effective number of datasets, N_eff = 1 / sum(w^2), in the same eight
#     scenarios, on one common scale (a concentration / diversity measure, not a
#     confidence measure);
# (c) modal dominant dataset across scenarios, and (d) the number of scenarios
#     that agree on it;
# (e) physics-gate coverage: how many of the five candidates are still available
#     in each unit before any weighting (the same for all scenarios).
#
# Ties: a unit whose two highest probabilities differ by less than TIE_TOLERANCE
# has no dominant dataset and is shown as a tie (this includes many neutral
# units). Ties do not vote in (c, d). The neutral scenario never votes either:
# it is the no-preference reference, so its dominant dataset only reflects which
# candidates survive the gate, not a weighting choice. The vote is therefore
# over the remaining seven scenarios.
#
# Input   : weights_region_biome.Rds (03f), grid_classes.Rds (01g)
# Output  : fig04_weighting_sensitivity.pdf / .png / _caption.txt in
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

weights_region_biome <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "weights_region_biome.Rds")
))

grid_classes <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
))

stopifnot(
  all(c("scenario", "region", "biome", "dataset", "w_region_biome") %in% names(weights_region_biome)),
  all(c("lon", "lat", "region", "biome", "cell_weight") %in% names(grid_classes)),
  !anyDuplicated(grid_classes[, .(lon, lat)]),
  !anyDuplicated(weights_region_biome[, .(scenario, region, biome, dataset)]),
  all(is.finite(weights_region_biome$w_region_biome) & weights_region_biome$w_region_biome >= 0),
  all(SCENARIO_ORDER %in% weights_region_biome$scenario)
)

# Scientific parameters ======================================================

N_CANDIDATES <- length(CANDIDATES)

# Two top probabilities closer than this are an exact tie (probabilities are sums
# of cell-area products, so only exact ties fall below it).
TIE_TOLERANCE <- 1e-6

# The neutral scenario has no weighting preference, so it does not vote in the
# robustness count. Its positive probabilities define which candidates are
# available in a unit (the physics gate), the same in every scenario.
ROBUSTNESS_EXCLUDED <- "neutral"
SUPPORT_SCENARIO <- "neutral"
N_VOTERS <- length(setdiff(SCENARIO_ORDER, ROBUSTNESS_EXCLUDED))

# N_eff is bounded by 1 (one dataset owns the unit) and the number of candidates
# (equal weights); the full natural range is shown so scenarios read on one scale.
NEFF_LIMITS <- c(1, N_CANDIDATES)

LEVEL_TIE     <- "Tie"
LEVEL_NO_DATA <- "No candidate"
LEVEL_EMPTIED <- "Unit emptied by the gate"

# Final ESSD/Copernicus figure height. Width comes from the active journal profile.
FIGURE_HEIGHT_MM <- 165

# Functions ==================================================================

# Highest probability, second highest, winner, N_eff and number of available
# candidates for every scenario x region x biome.
unit_summary <- function(weights) {
  weights[
    ,
    {
      ordered <- order(-w_region_biome)
      probability <- w_region_biome[ordered]

      .(
        winner = dataset[ordered][1],
        top_1 = probability[1],
        top_2 = if (.N > 1L) probability[2] else 0,
        n_effective = 1 / sum(w_region_biome^2),
        n_available = sum(w_region_biome > 0)
      )
    },
    by = .(scenario, region, biome)
  ]
}

# Paint unit values onto the grid cells of the mask.
paint_units <- function(cells, unit_values) {
  merge(cells, unit_values, by = c("region", "biome"), all.x = TRUE)
}

# Small multiples: one frame, one scale, strips carry the panel names.
facet_map <- function(dt, fill, fill_scale, ncol = 4, legend_position = "right") {
  publication_map(dt, fill, fill_scale, legend_position, coord_labels = FALSE) +
    facet_wrap(vars(panel), ncol = ncol) +
    theme(strip.text = element_text(hjust = 0, size = FIG_STRIP_SIZE))
}

# Analysis ===================================================================

weights <- weights_region_biome[scenario %in% SCENARIO_ORDER]
weights[, dataset := as.character(dataset)]

stopifnot(all(unique(weights$dataset) %in% CANDIDATES))

units <- unit_summary(weights)

units[
  ,
  `:=`(
    tie = (top_1 - top_2) < TIE_TOLERANCE,
    dominant = fifelse((top_1 - top_2) < TIE_TOLERANCE, LEVEL_TIE, winner)
  )
]

# The mask: grid cells of every region x biome unit.
cells <- grid_classes[
  !is.na(region) & !is.na(biome),
  .(lon, lat, region, biome)
]

# Units present in the grid but without any surviving candidate.
all_units <- unique(cells[, .(region, biome)])
weighted_units <- unique(weights[, .(region, biome)])
emptied_units <- fsetdiff(all_units, weighted_units)

scenario_panel <- function(scenario_name) {
  factor(
    SCENARIO_LABELS[[scenario_name]],
    levels = SCENARIO_LABELS[SCENARIO_ORDER]
  )
}

# (a) Dominant dataset per scenario -------------------------------------------------

dominant_levels <- c(CANDIDATES, LEVEL_TIE, LEVEL_NO_DATA)

dominant_long <- rbindlist(lapply(SCENARIO_ORDER, function(scenario_name) {
  map_dt <- paint_units(
    cells,
    units[scenario == scenario_name, .(region, biome, dominant)]
  )

  map_dt[is.na(dominant), dominant := LEVEL_NO_DATA]
  map_dt[, `:=`(
    dominant = factor(dominant, levels = dominant_levels),
    panel = scenario_panel(scenario_name)
  )]
  map_dt[, .(lon, lat, dominant, panel)]
}))

dominant_scale <- scale_fill_cat(
  name = NULL,
  values = c(
    PAL_DATASETS,
    setNames(c(COL_CONTEXT, COL_MISSING), c(LEVEL_TIE, LEVEL_NO_DATA))
  ),
  limits = dominant_levels,
  labels = c(DATASET_LABELS, setNames(c(LEVEL_TIE, LEVEL_NO_DATA), c(LEVEL_TIE, LEVEL_NO_DATA))),
  drop = FALSE,
  guide = guide_legend(ncol = 1)
)

p_dominant <- facet_map(dominant_long, "dominant", dominant_scale)

# (b) Effective number of datasets per scenario ----------------------------------------

neff_long <- rbindlist(lapply(SCENARIO_ORDER, function(scenario_name) {
  map_dt <- paint_units(
    cells,
    units[scenario == scenario_name, .(region, biome, n_effective)]
  )

  map_dt[, panel := scenario_panel(scenario_name)]
  map_dt[, .(lon, lat, n_effective, panel)]
}))

# 1e-9 tolerance: N_eff reaches its natural bounds up to floating-point noise.
check_limits(units$n_effective, NEFF_LIMITS + c(-1e-9, 1e-9), "Effective number of datasets (units)")

neff_scale <- scale_fill_seq(
  name = expression(N[eff]),
  palette = "seq_default",
  limits = NEFF_LIMITS,
  breaks = seq(NEFF_LIMITS[1], NEFF_LIMITS[2]),
  labels = label_minus
)

p_neff <- facet_map(neff_long, "n_effective", neff_scale)

# (c, d) Cross-scenario robustness ---------------------------------------------------------

votes <- units[!(scenario %in% ROBUSTNESS_EXCLUDED) & !tie, .N, by = .(region, biome, winner)]

robustness <- votes[
  order(region, biome, -N),
  .(
    modal = winner[1],
    n_agree = N[1],
    split = .N > 1L && N[2] == N[1],
    n_decisive = sum(N)
  ),
  by = .(region, biome)
]

robustness[, modal_label := fifelse(split, LEVEL_TIE, modal)]

# Units where no scenario has a dominant dataset.
undecided <- fsetdiff(weighted_units, robustness[, .(region, biome)])

if (nrow(undecided) > 0) {
  robustness <- rbindlist(
    list(
      robustness,
      undecided[, `:=`(modal = NA_character_, n_agree = 0L, split = FALSE,
                       n_decisive = 0L, modal_label = LEVEL_TIE)]
    ),
    use.names = TRUE
  )
}

modal_dt <- paint_units(cells, robustness[, .(region, biome, modal_label, n_agree)])
modal_dt[is.na(modal_label), modal_label := LEVEL_NO_DATA]
modal_dt[, modal_label := factor(modal_label, levels = dominant_levels)]
modal_dt[, n_agree_f := factor(n_agree, levels = seq_len(N_VOTERS))]
modal_dt[n_agree == 0L, n_agree_f := NA]
modal_dt[, panel := factor("Modal dataset")]

# Same colours as (a), so the modal map needs no legend of its own.
p_modal <- publication_map(modal_dt, "modal_label", dominant_scale, "none", coord_labels = FALSE) +
  facet_wrap(vars(panel)) +
  theme(strip.text = element_text(hjust = 0, size = FIG_STRIP_SIZE))

# Only the levels that occur are listed in the legends of (d) and (e); colours stay
# tied to the value, so they match the full scale.
agree_scale <- scale_fill_cat(
  name = paste0("Scenarios agreeing (of ", N_VOTERS, ")"),
  values = setNames(fig_colours("seq_default", N_VOTERS), seq_len(N_VOTERS)),
  na.translate = FALSE,
  guide = guide_legend(nrow = 2, byrow = TRUE)
)

# Units without a vote (none in practice) are left blank rather than drawn as NA.
agree_dt <- modal_dt[!is.na(n_agree_f)][, panel := factor("Scenario agreement")]
p_agree <- publication_map(agree_dt, "n_agree_f", agree_scale, "bottom", coord_labels = FALSE) +
  facet_wrap(vars(panel)) +
  theme(strip.text = element_text(hjust = 0, size = FIG_STRIP_SIZE))

# (e) Physics-gate coverage ---------------------------------------------------------------------

gate <- units[scenario == SUPPORT_SCENARIO, .(region, biome, n_available)]

gate_dt <- paint_units(cells, gate)
gate_dt[, n_available_f := factor(n_available, levels = seq_len(N_CANDIDATES))]
gate_dt[, panel := factor("Gate coverage")]

# Cells of emptied units, marked with crosses (thinned if the units are large).
emptied_cells <- merge(cells, emptied_units, by = c("region", "biome"))

if (nrow(emptied_cells) > 2000L) {
  emptied_cells <- emptied_cells[
    (round(lon / 0.25) %% 3 == 0) & (round(lat / 0.25) %% 3 == 0)
  ]
}

emptied_cells[, `:=`(marker = LEVEL_EMPTIED, panel = factor("Gate coverage"))]

gate_scale <- scale_fill_cat(
  name = paste0("Candidates surviving the gate (of ", N_CANDIDATES, ")"),
  values = setNames(fig_colours("seq_default", N_CANDIDATES), seq_len(N_CANDIDATES)),
  na.translate = FALSE,
  guide = guide_legend(nrow = 2, byrow = TRUE, order = 1)
)

# Emptied units have no value; they are left blank and marked with crosses.
p_gate <- publication_map(
  gate_dt[!is.na(n_available_f)], "n_available_f", gate_scale, "bottom", coord_labels = FALSE
) +
  facet_wrap(vars(panel)) +
  geom_point(
    data = emptied_cells,
    aes(x = lon, y = lat, shape = marker),
    inherit.aes = FALSE,
    size = FIG_POINT_SIZE,
    stroke = FIG_POINT_STROKE,
    colour = COL_TEXT
  ) +
  scale_shape_manual(
    name = NULL,
    values = setNames(4, LEVEL_EMPTIED),
    guide = guide_legend(order = 2)
  ) +
  theme(strip.text = element_text(hjust = 0, size = FIG_STRIP_SIZE))

# Assembly ---------------------------------------------------------------------------------------

figure_4 <- add_panel_tags(
  p_dominant / p_neff / (p_modal | p_agree | p_gate) +
    plot_layout(heights = c(2.2, 2.2, 1.5))
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
  "Sensitivity of the dataset probabilities to the weighting scenario. ",
  "All maps show region × biome sampling units on the 0.25° land grid ",
  "(180° W–180° E, 58° S–84° N). ",
  "(a) Dominant dataset, the one with the highest sampling probability, in each of the ",
  "eight weighting scenarios; units whose two highest probabilities are equal are shown as a tie. ",
  "(b) Effective number of datasets, N_eff = 1 / Σ w², on one common scale from 1 (one dataset ",
  "owns the unit) to ", N_CANDIDATES, " (equal weights); N_eff measures how concentrated the ",
  "probability is, not confidence. (c) Modal dominant dataset across the ", N_VOTERS,
  " scenarios other than neutral (neutral has no weighting preference; ties do not vote), ",
  "with the same colours as (a). (d) Number of those scenarios that select the modal dataset. ",
  "(e) Number of the five candidates still available in each unit after the physical-consistency gate, ",
  "which is the same in all scenarios; crosses mark units emptied by the gate. ",
  "Data: weights_region_biome.Rds, 1982–2021."
)
writeLines(caption, paste0(figure_stem, "_caption.txt"))

# Validation =================================================================

# Every scenario covers the same units, and probabilities sum to 1 in each.
stopifnot(units[, uniqueN(paste(region, biome)), by = scenario][, uniqueN(V1) == 1L])
stopifnot(weights[, .(s = sum(w_region_biome)), by = .(scenario, region, biome)][, all(abs(s - 1) < 1e-6)])

# N_eff is between 1 and the number of available candidates.
stopifnot(units[, all(n_effective >= 1 - 1e-9 & n_effective <= n_available + 1e-9)])

cat("\nUnits: ", nrow(all_units), " in the grid, ", nrow(weighted_units),
    " with weights, ", nrow(emptied_units), " emptied by the gate\n", sep = "")

cat("\nTies (top two probabilities equal) and N_eff, by scenario:\n")
print(
  units[
    ,
    .(
      ties = sum(tie),
      units = .N,
      neff_min = round(min(n_effective), 2),
      neff_median = round(median(n_effective), 2),
      neff_max = round(max(n_effective), 2)
    ),
    by = scenario
  ][match(SCENARIO_ORDER, scenario)]
)

cat("\nScenarios agreeing on the modal dataset (of ", N_VOTERS, ", ties and neutral do not vote):\n", sep = "")
print(robustness[, .(units = .N), by = n_agree][order(n_agree)])
cat("Units split between datasets: ", robustness[split == TRUE, .N], "\n", sep = "")

cat("\nCandidates surviving the gate per unit:\n")
print(gate[, .(units = .N), by = n_available][order(n_available)])

# Global summary (area-weighted mean probability per dataset and scenario; mean
# N_eff), not drawn in the figure: it belongs in the Supplement.
areas <- unit_area(grid_classes)
total_area <- areas[weighted_units, on = .(region, biome), sum(area)]

cat("\nArea-weighted mean probability (%) by scenario and dataset:\n")
print(dcast(
  merge(weights, areas, by = c("region", "biome"))[
    , .(share = round(100 * sum(area * w_region_biome) / total_area, 1)),
    by = .(scenario, dataset)
  ],
  scenario ~ dataset, value.var = "share"
)[match(SCENARIO_ORDER, scenario)])

cat("\nArea share of emptied units: ",
    round(100 * areas[emptied_units, on = .(region, biome), sum(area, na.rm = TRUE)] / areas[, sum(area)], 3),
    " %\n", sep = "")
