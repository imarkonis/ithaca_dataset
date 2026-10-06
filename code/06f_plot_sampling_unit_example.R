# ============================================================================
# Supplementary worked example: anatomy of MED region x biome sampling units.
#
# (a) Native 0.25-degree biome cells; (b) exact sampling weights with each unit's
# share of classified MED area; (c) all annual P-E member trajectories, their
# pointwise median, and five source-world trajectories on the sampled support.
# No smoothing, resampling, display renormalisation or uncertainty band.
# Dataset selection is fixed across years within each member x unit.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")
source("code/_figs.R") # supplies the mm-based save_figure used only here

# Constants & Variables ======================================================

EXAMPLE_REGION <- "MED"
EXAMPLE_SCENARIO <- "base"
EXPECTED_MEMBERS <- 100L
EXPECTED_YEARS <- seq.int(FULL_PERIOD[["START"]], FULL_PERIOD[["END"]])
WEIGHT_TOLERANCE <- 1e-6
VALUE_TOLERANCE <- 1e-8
GRID_RESOLUTION <- 0.25
MAP_PADDING <- 1 # degrees, context around the full MED grid
FIGURE_HEIGHT_MM <- 150
EXAMPLE_STEM <- paste0("figS_sampling_unit_", tolower(EXAMPLE_REGION), "_", EXAMPLE_SCENARIO)
BIOME_ORDER <- c(
  "Polar", "Tundra", "B. Forests", "T. Forests", "T. Grasslands",
  "M. Grasslands", "Mediterranean", "Deserts", "T/S Grasslands",
  "T/S Forests", "Flooded", "Water"
)
DATASET_LABELS <- c(
  ERA5L = "ERA5-Land", FLDAS = "FLDAS", GLEAM = "MSWEP-GLEAM",
  MERRA = "MERRA-2", TERRA = "TerraClimate"
)
stopifnot(identical(DATASET_COLS, PAL_ITHACA_DATASETS))

# Inputs =====================================================================

input_files <- file.path(PATH_OUTPUT_OUTPUT, c(
  "grid_classes.Rds", "weights_region_biome.Rds",
  "mc_selection_scenarios.Rds", "mc_region_year_scenarios.Rds",
  "dataset_region_biome_year.Rds"
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

# Functions ==================================================================

require_columns <- function(dt, cols, name) {
  missing <- setdiff(cols, names(dt))
  if (length(missing)) stop(name, " lacks: ", toString(missing), call. = FALSE)
}
require_unique <- function(dt, key, name) {
  if (anyDuplicated(dt, by = key)) stop("Duplicate keys in ", name, call. = FALSE)
}
annual_panel <- function(variable, y_label, title) {
  ggplot() +
    geom_hline_ref() +
    geom_line(data = members, aes(year, .data[[variable]], group = sim),
              colour = COL_REFERENCE, linewidth = FIG_LINEWIDTH_MEMBER,
              alpha = FIG_ALPHA_MEMBER) +
    geom_line(data = source_region, aes(year, .data[[variable]], colour = dataset),
              linewidth = FIG_LINEWIDTH, alpha = FIG_ALPHA_REFERENCE) +
    geom_line(data = ensemble_median, aes(year, .data[[variable]]),
              colour = COL_TEXT, linewidth = FIG_LINEWIDTH_EMPH) +
    scale_colour_cat(values = PAL_ITHACA_DATASETS, breaks = CANDIDATES,
                     labels = DATASET_LABELS, name = "Source P-E pair") +
    scale_x_continuous(breaks = pretty(EXPECTED_YEARS, n = 6)) +
    labs(x = "Year", y = y_label, title = title,
         subtitle = sprintf("%d members (grey); ensemble median (black); source worlds (colour)", n_members)) +
    theme_pub_titled(legend_position = "none")
}

# Validation =================================================================

require_columns(grid_classes, c("lon", "lat", "region", "biome", "cell_weight"), "grid_classes")
require_columns(weights, c("scenario", "region", "biome", "dataset", "w_region_biome"), "weights")
require_columns(selections, c("scenario", "sim", "region", "biome", "dataset"), "selections")
require_columns(members, c("scenario", "sim", "region", "year", "prec", "evap"), "members")
require_columns(source_units, c("dataset", "region", "biome", "year", "prec", "evap"), "source_units")

# Filter identifiers explicitly. Never change source files or scenario weights.
med_grid <- copy(grid_classes[as.character(region) == EXAMPLE_REGION & !is.na(biome)])
weights <- copy(weights[as.character(region) == EXAMPLE_REGION & scenario == EXAMPLE_SCENARIO])
selections <- copy(selections[as.character(region) == EXAMPLE_REGION & scenario == EXAMPLE_SCENARIO])
members <- copy(members[as.character(region) == EXAMPLE_REGION & scenario == EXAMPLE_SCENARIO])
source_units <- copy(source_units[as.character(region) == EXAMPLE_REGION])
for (dt in list(med_grid, weights, selections, source_units)) dt[, biome := as.character(biome)]
for (dt in list(weights, selections, source_units)) dt[, dataset := as.character(dataset)]
if (any(vapply(list(med_grid, weights, selections, members, source_units), nrow, integer(1)) == 0L)) {
  stop("MED/base is absent from one or more upstream outputs.", call. = FALSE)
}
require_unique(med_grid, c("lon", "lat"), "MED grid")
require_unique(weights, c("biome", "dataset"), "MED weights")
require_unique(selections, c("sim", "biome"), "MED selections")
require_unique(members, c("sim", "year"), "MED members")
require_unique(source_units, c("dataset", "biome", "year"), "MED source units")
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
biome_levels <- c(BIOME_ORDER[BIOME_ORDER %in% area$biome],
                  sort(setdiff(area$biome, BIOME_ORDER)))
if (length(biome_levels) > length(PAL_CAT_8)) {
  stop("More than eight MED biomes: revise the categorical encoding before plotting.")
}
biome_colours <- setNames(PAL_CAT_8[seq_along(biome_levels)], biome_levels)
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

# Source curves use the same set of sampled biomes as the MC regional mean.
source_region <- worlds[, .(
  prec = weighted_mean_safe(prec, area_weight),
  evap = weighted_mean_safe(evap, area_weight)
), by = .(dataset, year)]
members[, avail := prec - evap]
source_region[, avail := prec - evap]
ensemble_median <- members[, lapply(.SD, median), by = year,
                           .SDcols = c("prec", "evap", "avail")]
setorder(members, sim, year)
setorder(source_region, dataset, year)
setorder(ensemble_median, year)

# Fill genuinely absent dataset entries with zero, not a renormalised weight.
bars <- merge(CJ(biome = sampled_biomes, dataset = CANDIDATES),
              weights[, .(biome, dataset, w_region_biome)],
              by = c("biome", "dataset"), all.x = TRUE)
bars[is.na(w_region_biome), w_region_biome := 0]
bars[, dataset := factor(dataset, levels = CANDIDATES)]
bars[, biome := factor(biome, levels = rev(biome_levels[biome_levels %in% sampled_biomes]))]
area_labels <- setNames(sprintf("%s (%.1f%%)", area$biome, 100 * area$area_share), area$biome)

# Figure =====================================================================

x_extent <- range(med_grid$lon) + c(-1, 1) * MAP_PADDING
y_extent <- range(med_grid$lat) + c(-1, 1) * MAP_PADDING
p_a <- ggplot(med_grid, aes(lon, lat, fill = mapped_biome)) +
  geom_tile(width = GRID_RESOLUTION, height = GRID_RESOLUTION) +
  geom_path(data = COASTLINES, aes(long, lat, group = group), inherit.aes = FALSE,
            colour = COL_REFERENCE, linewidth = FIG_BOUNDARY_LINEWIDTH) +
  scale_fill_cat(values = biome_colours, breaks = biome_levels, name = "Biome") +
  coord_quickmap(xlim = x_extent, ylim = y_extent, expand = FALSE) +
  labs(title = "Mediterranean sampling units", subtitle = "IPCC MED; native 0.25-degree grid") +
  theme_pub_map(legend_position = "bottom") +
  theme(plot.title = element_text(size = FIG_BASE_SIZE, face = "bold"),
        plot.subtitle = element_text(size = FIG_ANNOTATION_SIZE)) +
  guides(fill = guide_legend(ncol = 2))
p_b <- ggplot(bars, aes(w_region_biome, biome, fill = dataset)) +
  geom_col(orientation = "y") +
  scale_fill_cat(values = PAL_ITHACA_DATASETS, breaks = CANDIDATES,
                 labels = DATASET_LABELS, name = "Source P-E pair", drop = FALSE) +
  scale_x_continuous(limits = c(0, 1 + WEIGHT_TOLERANCE), breaks = c(0, 0.5, 1),
                     labels = scales::label_percent(accuracy = 1), expand = expansion(mult = 0)) +
  scale_y_discrete(labels = area_labels) +
  labs(x = "Sampling weight", y = NULL, title = "Dataset choice by biome",
       subtitle = "Parentheses: share of classified MED area") +
  theme_pub_titled(legend_position = "bottom") +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE))
p_c <- annual_panel("avail", expression(P-E~(mm~yr^{-1})), "Regional water availability")
figure_example <- add_panel_tags(
  (p_a | p_b) / p_c + plot_layout(heights = c(1.25, 1))
)
figure_components <- add_panel_tags(
  annual_panel("prec", expression(P~(mm~yr^{-1})), "Regional precipitation") /
    annual_panel("evap", expression(E~(mm~yr^{-1})), "Regional evaporation") +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")
)

# Outputs ====================================================================

dir.create(PATH_OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
figure_stem <- file.path(PATH_OUTPUT_FIGURES, EXAMPLE_STEM)
save_figure(figure_example, file_stem = figure_stem,
            width_mm = FIG_WIDTH_DOUBLE, height_mm = FIGURE_HEIGHT_MM)
save_figure(figure_components, file_stem = paste0(figure_stem, "_p_e"),
            width_mm = FIG_WIDTH_DOUBLE, height_mm = FIGURE_HEIGHT_MM)
caption <- sprintf(paste0(
  "Anatomy of Mediterranean (IPCC MED) region x biome sampling units, %d-%d, base scenario. ",
  "(a) Biome cells on the native 0.25-degree common analysis grid; grey denotes units without sampling weights. ",
  "(b) Exact saved dataset sampling weights, summing to one within 1e-6 without display renormalisation; ",
  "parentheses give each biome's share of classified MED area. Sampled units cover %.2f%% of that area. ",
  "MSWEP-GLEAM pairs MSWEP precipitation with GLEAM evaporation. ",
  "(c) Annual P-E regional means for all %d members (grey), pointwise ensemble median (black), ",
  "and five source worlds (coloured), using the same sampled units and cell-area weights. ",
  "One coherent P-E source is selected per unit and member and retained across all years. ",
  "Units are drawn independently by design; this is not an assertion of independent errors. ",
  "Spread describes dataset-selection uncertainty conditional on the candidates, base weights and sampling design; ",
  "weights are algorithmic sampling probabilities, not calibrated probabilities of truth. ",
  "P-E is net atmospheric moisture supply, not runoff or a closed basin water balance. ",
  "No smoothing or uncertainty band is applied. The companion figure shows P and E separately."
), min(EXPECTED_YEARS), max(EXPECTED_YEARS), 100 * coverage, n_members)
writeLines(caption, paste0(figure_stem, "_caption.txt"))
saveRDS(list(
  map = med_grid[, .(lon, lat, biome, mapped_biome)], weights = bars,
  area = area, members = members, median = ensemble_median, source_worlds = source_region,
  selections = selections, metadata = list(region = EXAMPLE_REGION, scenario = EXAMPLE_SCENARIO,
    units = "mm/year", years = EXPECTED_YEARS, n_members = n_members,
    coverage = coverage, inputs = basename(input_files))
), paste0(figure_stem, "_plot_data.Rds"))
message("Data checks passed; inspect the exported PDF and PNG before manuscript use: ", figure_stem)
