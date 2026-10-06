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
# (c) cross-scenario robustness: modal dominant dataset and the number of
#     scenarios that agree on it;
# (d) physics-gate coverage: how many of the five candidates are still available
#     in each unit before any weighting (the same for all scenarios).
# A small matrix (area-weighted mean probability per dataset and mean N_eff per
# scenario) is added as a global summary.
#
# Ties: a unit whose two highest probabilities differ by less than TIE_TOLERANCE
# has no dominant dataset and is shown as a tie (this includes many neutral
# units). Ties do not vote in (c). The neutral scenario never votes in (c)
# either: it is the no-preference reference, so its dominant dataset only
# reflects which candidates survive the gate, not a weighting choice. The vote
# is therefore over the remaining seven scenarios.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")

# Inputs =====================================================================

weights_region_biome <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "weights_region_biome.Rds")
)

grid_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
)

# Constants & Variables ======================================================

N_CANDIDATES <- length(CANDIDATES)

# Two top probabilities closer than this are an exact tie.
TIE_TOLERANCE <- 1e-6

# Scenarios that do not vote in the robustness count, and the scenario whose
# positive probabilities define which candidates are available in a unit.
ROBUSTNESS_EXCLUDED <- "neutral"
SUPPORT_SCENARIO <- "neutral"
N_VOTERS <- length(setdiff(SCENARIO_ORDER, ROBUSTNESS_EXCLUDED))

NEFF_LIMITS <- c(1, N_CANDIDATES)

# Used for exact ties in (a) and for units without a single modal dataset in (c).
LEVEL_TIE     <- "Tie (no single dominant dataset)"
LEVEL_NO_DATA <- "No candidate survives"

# Short scenario names for the summary matrix.
SCENARIO_SHORT <- c(
  base = "Base", clim_dominant = "Clim.", prec_dominant = "Prec.",
  evap_dominant = "Evap.", rank_linear = "Rk lin.", rank_exp = "Rk exp.",
  neutral = "Neutral", inverted = "Disagr."
)

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

map_legend_bottom <- function(plot) {
  plot + theme(legend.position = "bottom")
}

heading <- function(label) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = label, hjust = 0, size = 3.5, fontface = "bold") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-1, 1)) +
    coord_cartesian(clip = "off") +
    theme_void()
}

# Analysis ===================================================================

weights <- weights_region_biome[scenario %in% SCENARIO_ORDER]
weights[, dataset := as.character(dataset)]

stopifnot(setequal(unique(weights$scenario), SCENARIO_ORDER))
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

# (a) Dominant dataset per scenario ------------------------------------------------

dominant_levels <- c(CANDIDATES, LEVEL_TIE, LEVEL_NO_DATA)

dominant_scale <- scale_fill_manual(
  name = "Dominant dataset",
  values = c(DATASET_COLS, setNames(c(COL_TIE, COL_NO_DATA), c(LEVEL_TIE, LEVEL_NO_DATA))),
  limits = dominant_levels,
  drop = FALSE,
  guide = guide_legend(nrow = 1, keywidth = unit(0.4, "cm"), keyheight = unit(0.4, "cm"))
)

dominant_map <- function(scenario_name) {
  map_dt <- paint_units(
    cells,
    units[scenario == scenario_name, .(region, biome, dominant)]
  )

  map_dt[is.na(dominant), dominant := LEVEL_NO_DATA]
  map_dt[, dominant := factor(dominant, levels = dominant_levels)]

  cell_map(map_dt, "dominant", dominant_scale, title = SCENARIO_LABELS[[scenario_name]])
}

# (b) Effective number of datasets per scenario -------------------------------------

neff_scale <- scale_fill_viridis_c(
  name = expression(N[eff]),
  option = "mako",
  limits = NEFF_LIMITS,
  breaks = seq(NEFF_LIMITS[1], NEFF_LIMITS[2]),
  na.value = COL_NO_DATA,
  guide = guide_colourbar(barwidth = unit(5, "cm"), barheight = unit(0.3, "cm"))
)

neff_map <- function(scenario_name) {
  map_dt <- paint_units(
    cells,
    units[scenario == scenario_name, .(region, biome, n_effective)]
  )

  cell_map(map_dt, "n_effective", neff_scale, title = SCENARIO_LABELS[[scenario_name]])
}

# (c) Cross-scenario robustness ---------------------------------------------------------

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

robustness[
  ,
  modal_label := fifelse(split, LEVEL_TIE, modal)
]

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

# Same colours and legend as (a): the modal map needs no legend of its own.
p_modal <- cell_map(modal_dt, "modal_label", dominant_scale, title = "c  Modal dataset") +
  theme(legend.position = "none")

agree_scale <- scale_fill_viridis_d(
  name = paste0("Scenarios agreeing on the modal dataset (of ", N_VOTERS, ")"),
  option = "rocket",
  direction = -1,
  begin = 0.05,
  end = 0.85,
  drop = FALSE,
  na.translate = FALSE,
  guide = guide_legend(
    title.position = "top", nrow = 1,
    keywidth = unit(0.4, "cm"), keyheight = unit(0.4, "cm")
  )
)

# Units without a vote (none in practice) are left blank rather than drawn as NA.
p_agree <- cell_map(modal_dt[!is.na(n_agree_f)], "n_agree_f", agree_scale, title = "c  Agreement on the modal dataset") +
  theme(legend.position = "bottom", legend.title = element_text(size = 6.5))

# (d) Physics-gate coverage ---------------------------------------------------------------

gate <- units[scenario == SUPPORT_SCENARIO, .(region, biome, n_available)]

gate_dt <- paint_units(cells, gate)
gate_dt[, n_available_f := factor(n_available, levels = seq_len(N_CANDIDATES))]

# Cells of emptied units, marked with crosses (thinned if the units are large).
emptied_cells <- merge(cells, emptied_units, by = c("region", "biome"))

if (nrow(emptied_cells) > 2000L) {
  emptied_cells <- emptied_cells[
    (round(lon / 0.25) %% 3 == 0) & (round(lat / 0.25) %% 3 == 0)
  ]
}

emptied_cells[, marker := "Unit emptied by the gate"]

gate_scale <- scale_fill_viridis_d(
  name = "Candidates surviving the gate (of 5)",
  option = "cividis",
  drop = FALSE,
  na.translate = FALSE,
  guide = guide_legend(
    title.position = "top", nrow = 1, order = 1,
    keywidth = unit(0.4, "cm"), keyheight = unit(0.4, "cm")
  )
)

# Emptied units have no value; they are left blank and marked with crosses.
p_gate <- cell_map(gate_dt[!is.na(n_available_f)], "n_available_f", gate_scale, title = "d  Physics-gate coverage") +
  theme(legend.position = "bottom", legend.title = element_text(size = 6.5)) +
  geom_point(
    data = emptied_cells,
    aes(x = lon, y = lat, shape = marker),
    inherit.aes = FALSE,
    size = 1.1,
    stroke = 0.5
  ) +
  scale_shape_manual(
    name = NULL,
    values = c("Unit emptied by the gate" = 4),
    guide = guide_legend(order = 2)
  )

# Global summary matrix -------------------------------------------------------------------

areas <- unit_area(grid_classes)

summary_input <- merge(weights, areas, by = c("region", "biome"))

# Total area of the units that have weights (the same in every scenario).
total_area <- areas[weighted_units, on = .(region, biome), sum(area)]

dataset_share <- summary_input[
  ,
  .(share = sum(area * w_region_biome) / total_area),
  by = .(scenario, dataset)
]

mean_neff <- merge(units, areas, by = c("region", "biome"))[
  ,
  .(mean_neff = weighted.mean(n_effective, area)),
  by = scenario
]

summary_long <- rbindlist(list(
  dataset_share[, .(scenario, column = dataset, value = share, label = sprintf("%.0f%%", 100 * share))],
  mean_neff[, .(scenario, column = "Mean N_eff", value = NA_real_, label = sprintf("%.1f", mean_neff))]
))

summary_long[
  ,
  `:=`(
    scenario = factor(scenario, levels = SCENARIO_ORDER),
    column = factor(column, levels = rev(c(CANDIDATES, "Mean N_eff")))
  )
]

# Scenarios along x so that the matrix is wide and short, like the maps next to it.
p_summary <- ggplot(summary_long, aes(x = scenario, y = column)) +
  geom_tile(aes(fill = value), colour = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 2.1) +
  scale_fill_gradient(
    low = "white", high = "grey35", limits = c(0, 0.5), oob = scales::squish,
    na.value = "white", guide = "none"
  ) +
  scale_x_discrete(labels = SCENARIO_SHORT) +
  labs(x = NULL, y = NULL, title = "Global summary: mean probability (%) and N_eff") +
  theme_minimal(base_size = 7) +
  theme(
    aspect.ratio = MAP_ASPECT,
    panel.grid = element_blank(),
    plot.title = element_text(size = 8, face = "bold", hjust = 0),
    axis.text.x = element_text(angle = 40, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    plot.margin = margin(2, 2, 2, 2)
  )

# Assembly ----------------------------------------------------------------------------------

# Eight maps in two rows of four (share scenarios, then rank foils, neutral and
# inverted) and one shared legend underneath.
scenario_block <- function(map_function) {
  maps <- lapply(
    SCENARIO_ORDER,
    function(scenario_name) map_function(scenario_name) + theme(legend.position = "none")
  )

  wrap_plots(
    c(maps, list(legend_element(map_function(SCENARIO_ORDER[1])))),
    design = "ABCD\nEFGH\nIIII",
    heights = c(1, 1, 0.16)
  )
}

block_a <- scenario_block(dominant_map)
block_b <- scenario_block(neff_map)

block_c <- wrap_plots(p_modal, p_agree, p_gate, p_summary, nrow = 1)

figure_4 <- (
  heading("a  Dominant dataset (highest sampling probability) in each weighting scenario") /
    block_a /
    heading("b  Effective number of datasets, N_eff: concentration of the probability, not confidence") /
    block_b /
    block_c
) +
  plot_layout(heights = c(0.14, 2.3, 0.14, 2.3, 1.45)) +
  plot_annotation(
    caption = paste0(
      "N_eff = 1 / sum(w^2) over the candidates of a region x biome unit. ",
      "Tie: the two highest probabilities are equal. ",
      "Neutral (no weighting preference) and ties do not vote in c. ",
      "Units without any surviving candidate are grey."
    ),
    theme = theme(plot.caption = element_text(size = 6.5, colour = "grey30", hjust = 0))
  )

# Outputs ====================================================================

save_figure(figure_4, "fig04_weighting_sensitivity", width = 12.5, height = 11)

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

cat("\nArea share of emptied units: ",
    round(100 * areas[emptied_units, on = .(region, biome), sum(area, na.rm = TRUE)] / areas[, sum(area)], 3),
    " %\n", sep = "")
