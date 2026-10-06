# ============================================================================
# Figure 2: anatomy of MED region x biome sampling units, MC member 44.
#
# (a) Native 0.25-degree biome cells; (b) exact sampling weights with each unit's
# share of classified MED area and member 44's saved uniform draws;
# (c, d) annual P and E for each biome in that member, labelled by source.
# No smoothing, resampling, display renormalisation or uncertainty band.
# Dataset selection is fixed across years within each member x unit.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")
source("code/_figs.R") # supplies the mm-based save_figure used only here
library(ggrepel)      # keep every biome's source label readable

# Constants & Variables ======================================================

EXAMPLE_REGION <- "MED"
EXAMPLE_SCENARIO <- "base"
EXAMPLE_MEMBER <- 44L # requested worked realization; never generate new draws
EXPECTED_MEMBERS <- 100L
EXPECTED_YEARS <- seq.int(FULL_PERIOD[["START"]], FULL_PERIOD[["END"]])
WEIGHT_TOLERANCE <- 1e-6
VALUE_TOLERANCE <- 1e-8
GRID_RESOLUTION <- 0.25
MAP_PADDING <- 1 # degrees, context around the full MED grid
FIGURE_HEIGHT_MM <- 170
# Reserve an unplotted future interval for direct labels; no data extrapolation.
LABEL_SPACE_YEARS <- 17
LABEL_NUDGE_YEARS <- 3
EXAMPLE_STEM <- paste0("fig2_sampling_unit_", tolower(EXAMPLE_REGION), "_", EXAMPLE_SCENARIO,
                       "_member", EXAMPLE_MEMBER)
DATASET_LABELS <- c(
  ERA5L = "ERA5-Land", FLDAS = "FLDAS", GLEAM = "MSWEP-GLEAM",
  MERRA = "MERRA-2", TERRA = "TerraClimate"
)
stopifnot(identical(DATASET_COLS, PAL_DATASETS))

# Inputs =====================================================================

input_files <- file.path(PATH_OUTPUT_OUTPUT, c(
  "grid_classes.Rds", "weights_region_biome.Rds",
  "mc_selection_scenarios.Rds", "mc_region_year_scenarios.Rds",
  "dataset_region_biome_year.Rds", "weight_cdf_scenarios.Rds"
))
if (any(!file.exists(input_files))) {
  stop("Missing upstream outputs; run 01g, 03f, 03g, 04a and 04b first:\n",
       paste(input_files[!file.exists(input_files)], collapse = "\n"), call. = FALSE)
}
grid_classes <- as.data.table(readRDS(input_files[1]))
weights <- as.data.table(readRDS(input_files[2]))
selections <- as.data.table(readRDS(input_files[3]))
members <- as.data.table(readRDS(input_files[4]))
source_units <- as.data.table(readRDS(input_files[5]))
cdf <- as.data.table(readRDS(input_files[6]))

# Functions ==================================================================

require_columns <- function(dt, cols, name) {
  missing <- setdiff(cols, names(dt))
  if (length(missing)) stop(name, " lacks: ", toString(missing), call. = FALSE)
}
require_unique <- function(dt, key, name) {
  if (anyDuplicated(dt, by = key)) stop("Duplicate keys in ", name, call. = FALSE)
}
annual_panel <- function(variable, y_label) {
  year_breaks <- pretty(EXPECTED_YEARS, n = 6)
  year_breaks <- year_breaks[year_breaks >= min(EXPECTED_YEARS) & year_breaks <= max(EXPECTED_YEARS)]
  labels <- member44[year == max(EXPECTED_YEARS)]
  labels[, source_label := paste0(biome, ": ", DATASET_LABELS[as.character(dataset)])]
  ggplot() +
    geom_line(data = member44, aes(year, .data[[variable]], colour = biome, group = biome),
              linewidth = FIG_LINEWIDTH) +
    ggrepel::geom_text_repel(data = labels,
      aes(year, .data[[variable]], label = source_label),
      colour = COL_TEXT, family = FIG_FONT, size = FIG_GEOM_TEXT_SIZE,
      segment.color = COL_CONTEXT, segment.size = FIG_BOUNDARY_LINEWIDTH,
      box.padding = FIG_LABEL_PADDING, force = FIG_LABEL_FORCE,
      hjust = 0, direction = "y", nudge_x = LABEL_NUDGE_YEARS,
      xlim = c(max(EXPECTED_YEARS) + 1, max(EXPECTED_YEARS) + LABEL_SPACE_YEARS),
      seed = EXAMPLE_MEMBER, max.overlaps = Inf, min.segment.length = 0) +
    scale_colour_cat(values = PAL_BIOMES, breaks = sampled_order, name = "Biome") +
    scale_x_continuous(breaks = year_breaks, labels = label_minus, expand = expansion(mult = 0)) +
    scale_y_continuous(labels = label_minus) +
    coord_cartesian(xlim = c(min(EXPECTED_YEARS), max(EXPECTED_YEARS) + LABEL_SPACE_YEARS)) +
    labs(x = "Year", y = y_label) +
    theme_pub(legend_position = "none")
}

# Validation =================================================================

require_columns(grid_classes, c("lon", "lat", "region", "biome", "cell_weight"), "grid_classes")
require_columns(weights, c("scenario", "region", "biome", "dataset", "w_region_biome"), "weights")
require_columns(selections, c("scenario", "sim", "region", "biome", "dataset", "u"), "selections")
require_columns(members, c("scenario", "sim", "region", "year", "prec", "evap"), "members")
require_columns(source_units, c("dataset", "region", "biome", "year", "prec", "evap"), "source_units")
require_columns(cdf, c("scenario", "region", "biome", "dataset", "w_region_biome", "p_low", "p_high"), "cdf")

# Filter identifiers explicitly. Never change source files or scenario weights.
med_grid <- copy(grid_classes[as.character(region) == EXAMPLE_REGION & !is.na(biome)])
weights <- copy(weights[as.character(region) == EXAMPLE_REGION & scenario == EXAMPLE_SCENARIO])
selections <- copy(selections[as.character(region) == EXAMPLE_REGION & scenario == EXAMPLE_SCENARIO])
members <- copy(members[as.character(region) == EXAMPLE_REGION & scenario == EXAMPLE_SCENARIO])
source_units <- copy(source_units[as.character(region) == EXAMPLE_REGION])
cdf <- copy(cdf[as.character(region) == EXAMPLE_REGION & scenario == EXAMPLE_SCENARIO])
for (dt in list(med_grid, weights, selections, source_units, cdf)) dt[, biome := as.character(biome)]
for (dt in list(weights, selections, source_units, cdf)) dt[, dataset := as.character(dataset)]
if (any(vapply(list(med_grid, weights, selections, members, source_units), nrow, integer(1)) == 0L)) {
  stop("MED/base is absent from one or more upstream outputs.", call. = FALSE)
}
require_unique(med_grid, c("lon", "lat"), "MED grid")
require_unique(weights, c("biome", "dataset"), "MED weights")
require_unique(selections, c("sim", "biome"), "MED selections")
require_unique(members, c("sim", "year"), "MED members")
require_unique(source_units, c("dataset", "biome", "year"), "MED source units")
require_unique(cdf, c("biome", "dataset"), "MED CDF")
stopifnot(all(is.finite(med_grid$cell_weight) & med_grid$cell_weight > 0),
          all(is.finite(med_grid$lon)), all(is.finite(med_grid$lat)),
          all(abs(med_grid$lon) <= 180), all(abs(med_grid$lat) <= 90),
          all(is.finite(weights$w_region_biome) & weights$w_region_biome >= 0),
          all(weights$dataset %in% CANDIDATES),
          all(selections$dataset %in% CANDIDATES))
weight_check <- weights[, .(sum_weight = sum(w_region_biome)), by = biome]
stopifnot(all(abs(weight_check$sum_weight - 1) < WEIGHT_TOLERANCE))

sampled_biomes <- sort(unique(weights$biome))
stopifnot(setequal(sampled_biomes, selections$biome),
          all(sampled_biomes %in% med_grid$biome))
n_members <- uniqueN(members$sim)
stopifnot(n_members == EXPECTED_MEMBERS, setequal(members$sim, selections$sim))
selection_counts <- selections[, .N, by = sim]
stopifnot(all(selection_counts$N == length(sampled_biomes)))
year_check <- members[, .(ok = setequal(year, EXPECTED_YEARS)), by = sim]
stopifnot(all(year_check$ok),
          all(is.finite(members$prec)), all(is.finite(members$evap)))

# Analysis ===================================================================

# Same cell-area weights and regional denominator as 04b; area labels use ALL
# classified MED cells, including any unit without sampling weights.
area <- med_grid[, .(area_weight = sum(cell_weight), n_cells = .N), by = biome]
area[, area_share := area_weight / sum(area_weight)]
area[, sampled := biome %in% sampled_biomes]
coverage <- sum(area[sampled == TRUE, area_share])
if (coverage < 1 - WEIGHT_TOLERANCE) {
  warning(sprintf("Sampled units cover %.2f%% of classified MED area; unavailable units are grey.", 100 * coverage))
}
biome_levels <- area[order(-area_share, biome), biome]
sampled_order <- biome_levels[biome_levels %in% sampled_biomes]
unknown_biomes <- setdiff(biome_levels, names(PAL_BIOMES))
if (length(unknown_biomes)) {
  stop("Add an explicit PAL_BIOMES mapping in _figs.R for: ", toString(unknown_biomes))
}
med_grid[, mapped_biome := fifelse(biome %in% sampled_biomes, biome, NA_character_)]

# Reconstruct regional members from saved selections and compare to 04b. This
# also verifies that a single source choice applies to every year of a unit.
worlds <- source_units[biome %in% sampled_biomes & dataset %in% CANDIDATES]
stopifnot(nrow(worlds) == length(sampled_biomes) * length(CANDIDATES) * length(EXPECTED_YEARS),
          setequal(worlds$year, EXPECTED_YEARS),
          all(is.finite(worlds$prec)), all(is.finite(worlds$evap)))
world_year_check <- worlds[, .(ok = setequal(year, EXPECTED_YEARS)), by = .(dataset, biome)]
stopifnot(all(world_year_check$ok))
worlds <- merge(worlds, area[, .(biome, area_weight)], by = "biome", all.x = TRUE)
selected_worlds <- merge(selections[, .(sim, biome, dataset)], worlds,
                         by = c("biome", "dataset"), allow.cartesian = TRUE)
stopifnot(nrow(selected_worlds) == nrow(selections) * length(EXPECTED_YEARS))
reconstructed <- selected_worlds[, .(
  prec_check = weighted_mean_safe(prec, area_weight),
  evap_check = weighted_mean_safe(evap, area_weight)
), by = .(sim, year)]
comparison <- merge(members, reconstructed, by = c("sim", "year"))
stopifnot(nrow(comparison) == nrow(members),
          all(abs(comparison$prec - comparison$prec_check) < VALUE_TOLERANCE),
          all(abs(comparison$evap - comparison$evap_check) < VALUE_TOLERANCE))

# Bars are the actual saved CDF intervals, not ggplot's default reverse stack.
# Therefore each draw marker sits in the same source segment that 04a selected.
bars <- merge(weights[, .(biome, dataset, w_region_biome)],
              cdf[, .(biome, dataset, cdf_weight = w_region_biome, p_low, p_high)],
              by = c("biome", "dataset"), all = TRUE)
stopifnot(nrow(bars) == nrow(weights),
          all(is.finite(bars$p_low)), all(is.finite(bars$p_high)),
          all(is.finite(bars$cdf_weight)),
          all(abs(bars$w_region_biome - bars$cdf_weight) < VALUE_TOLERANCE),
          all(abs(bars$p_high - bars$p_low - bars$w_region_biome) < WEIGHT_TOLERANCE),
          all(bars$p_low >= 0), all(bars$p_high <= 1), all(bars$p_high >= bars$p_low))
setorder(bars, biome, dataset)
cdf_check <- bars[, .(ok = p_low[1] == 0 && p_high[.N] == 1 &&
                      all(abs(p_low[-1] - p_high[-.N]) < VALUE_TOLERANCE)), by = biome]
stopifnot(all(cdf_check$ok))

draws44 <- selections[sim == EXAMPLE_MEMBER, .(sim, biome, dataset, u)]
stopifnot(nrow(draws44) == length(sampled_biomes),
          all(is.finite(draws44$u) & draws44$u >= 0 & draws44$u < 1))
draw_check <- merge(draws44, bars, by = c("biome", "dataset"))
stopifnot(nrow(draw_check) == nrow(draws44),
          all(draw_check$u >= draw_check$p_low & draw_check$u < draw_check$p_high))
member44 <- selected_worlds[sim == EXAMPLE_MEMBER, .(sim, biome, dataset, year, prec, evap)]
require_unique(member44, c("biome", "year"), "Member 44 biome trajectories")
stopifnot(nrow(member44) == length(sampled_biomes) * length(EXPECTED_YEARS))
setorder(member44, biome, year)
bars[, y := match(biome, rev(sampled_order))]
draws44[, y := match(biome, rev(sampled_order))]
draw_label <- sprintf("MC member %d draw", EXAMPLE_MEMBER)
area_labels <- setNames(sprintf("%s (%.1f%%)", area$biome, 100 * area$area_share), area$biome)

# Figure =====================================================================

x_extent <- range(med_grid$lon) + c(-1, 1) * MAP_PADDING
y_extent <- range(med_grid$lat) + c(-1, 1) * MAP_PADDING
p_a <- ggplot(med_grid, aes(lon, lat, fill = mapped_biome)) +
  geom_tile(width = GRID_RESOLUTION, height = GRID_RESOLUTION) +
  geom_path(data = COASTLINES, aes(long, lat, group = group), inherit.aes = FALSE,
            colour = COL_REFERENCE, linewidth = FIG_BOUNDARY_LINEWIDTH) +
  scale_fill_cat(values = PAL_BIOMES, breaks = biome_levels, name = "Biome") +
  coord_quickmap(xlim = x_extent, ylim = y_extent, expand = FALSE) +
  scale_x_continuous(labels = label_lon) + scale_y_continuous(labels = label_lat) +
  theme_pub_map(legend_position = "bottom") +
  guides(fill = guide_legend(ncol = 2))
p_b <- ggplot() +
  geom_rect(data = bars, aes(xmin = p_low, xmax = p_high,
             ymin = y - FIG_BAR_HALF_HEIGHT, ymax = y + FIG_BAR_HALF_HEIGHT, fill = dataset)) +
  geom_segment(data = draws44, aes(x = u, xend = u,
               y = y - FIG_BAR_HALF_HEIGHT, yend = y + FIG_BAR_HALF_HEIGHT,
               linetype = draw_label),
               colour = COL_BLACK, linewidth = FIG_DRAW_LINEWIDTH) +
  scale_linetype_manual(name = NULL, values = setNames(FIG_LINETYPES[1], draw_label)) +
  scale_fill_cat(values = PAL_DATASETS, breaks = CANDIDATES,
                 labels = DATASET_LABELS, name = "Source P-E pair", drop = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1),
                     labels = scales::label_percent(accuracy = 1), expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = seq_along(sampled_order), labels = area_labels[rev(sampled_order)]) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Cumulative sampling probability", y = NULL) +
  theme_pub(legend_position = "bottom") +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE))
p_c <- annual_panel("prec", expression(P~(mm~yr^{-1})))
p_d <- annual_panel("evap", expression(E~(mm~yr^{-1})))
figure_example <- add_panel_tags(
  (p_a | p_b) / p_c / p_d + plot_layout(heights = c(1.25, 1, 1))
)

# Outputs ====================================================================

figure_stem <- file.path(PATH_OUTPUT_FIGURES, EXAMPLE_STEM)
save_figure(figure_example, file_stem = figure_stem,
            width_mm = FIG_WIDTH_DOUBLE, height_mm = FIGURE_HEIGHT_MM)
check_fonts(paste0(figure_stem, ".pdf"))
caption <- sprintf(paste0(
  "Anatomy of Mediterranean (IPCC MED) region x biome sampling units, %d-%d, base scenario, member %d. ",
  "(a) Biome cells on the native 0.25-degree common analysis grid; grey denotes units without sampling weights. ",
  "(b) Exact saved dataset sampling weights, summing to one within 1e-6 without display renormalisation; ",
  "parentheses give each biome's share of classified MED area, ordered largest to smallest from top to bottom. ",
  "Segments follow the alphabetical dataset CDF order of 04a from left to right; ",
  "each vertical black marker is the saved uniform draw u for member %d in that biome. ",
  "The segment containing u identifies the selected P-E pair (lower bound inclusive, upper exclusive). ",
  "Sampled units cover %.2f%% of classified MED area. ",
  "MSWEP-GLEAM pairs MSWEP precipitation with GLEAM evaporation. ",
  "(c, d) Annual biome-mean precipitation P and evaporation E for member %d only. ",
  "Each curve has its biome colour from (a); direct labels identify the biome and selected dataset. ",
  "Values are the upstream area-weighted means within each biome, not regional sums or area-share contributions. ",
  "One coherent P-E source is selected per unit and member and retained across all years. ",
  "Units are drawn independently by design; this is not an assertion of independent errors. ",
  "Weights are algorithmic sampling probabilities, not calibrated probabilities of truth. ",
  "No smoothing, new random draws, regional ensemble summary or uncertainty band is applied."
), min(EXPECTED_YEARS), max(EXPECTED_YEARS), EXAMPLE_MEMBER, EXAMPLE_MEMBER, 100 * coverage, EXAMPLE_MEMBER)
writeLines(caption, paste0(figure_stem, "_caption.txt"))
saveRDS(list(
  map = med_grid[, .(lon, lat, biome, mapped_biome)], weights = bars,
  area = area, member = member44, draws = draws44,
  metadata = list(region = EXAMPLE_REGION, scenario = EXAMPLE_SCENARIO,
    units = "mm/year", years = EXPECTED_YEARS, n_members = n_members,
    plotted_member = EXAMPLE_MEMBER, biome_order = sampled_order,
    coverage = coverage, inputs = basename(input_files))
), paste0(figure_stem, "_plot_data.Rds"))
write.csv(merge(draws44[, .(biome, dataset, u)], area[, .(biome, area_share)], by = "biome"),
          paste0(figure_stem, "_selections.csv"), row.names = FALSE)
check_limits(c(bars$p_low, bars$p_high, draws44$u), c(0, 1), "CDF intervals and member draw")
message("Data checks passed; inspect the exported PDF and PNG before manuscript use: ", figure_stem)
