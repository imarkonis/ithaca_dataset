# ============================================================================
# Mediterranean P-E change and uncertainty: ITHACA CHANGE-weighted ensemble
#
# Panels:
#   (a) median member-wise Delta(P-E), late (2002-2021) minus early (1982-2001)
#   (b) fraction of members with Delta(P-E) < 0 (drying)
#   (c) 5th-95th percentile width of Delta(P-E), or effective dataset count
#   (d) source with maximum CHANGE sampling weight per MED x biome unit
#
# Inputs: 04c CHANGE member NetCDFs; 04c CSV lookups/manifest;
#         03f weights_region_biome.Rds
# Outputs: PATH_OUTPUT_FIGURES/fig_med_pe_change.{pdf,png}
#          PATH_OUTPUT_FIGURES/fig_med_pe_change_plot_data.Rds
#          PATH_OUTPUT_FIGURES/fig_med_pe_change_caption.txt
#
# CHANGE means change-oriented dataset weighting, not a future climate scenario.
# NOTE: AI-assisted draft (2026-10-13); run and visually check before publication.
# ============================================================================

# Libraries ===================================================================

source("code/_source.R")
source("code/_figure_helpers.R")
source("code/_figs.R")
library(ncdf4)

# Constants & Variables =======================================================

SCENARIO <- "change"
REGION <- "MED"

EARLY_YEARS <- seq.int(START_PERIOD_1, END_PERIOD_1)  # 1982-2001
LATE_YEARS  <- seq.int(START_PERIOD_2, END_PERIOD_2)  # 2002-2021
ALL_YEARS <- c(EARLY_YEARS, LATE_YEARS)

# Choose one representation for panel (c):
#   "delta_q90_width"   = width between member 5th and 95th percentiles
#   "effective_datasets" = 1 / sum(w_i^2), from CHANGE regional weights
# The former quantifies the spread in inferred water-balance change;
# the latter quantifies dispersion of weights, NOT water-balance uncertainty.
SPREAD_METRIC <- "delta_q90_width"
stopifnot(SPREAD_METRIC %in% c("delta_q90_width", "effective_datasets"))

EXPECTED_MEMBERS <- 100L  # current 04c gridded ensemble
WEIGHT_SUM_TOL <- 1e-6
TOP_WEIGHT_TOL <- 1e-12  # machine-precision tie, not an approximate winner
GRID_RESOLUTION <- 0.25  # native 04c grid, in geographic degrees
MAP_PADDING <- 1
FIGURE_HEIGHT_MM <- 135
FILE_STEM <- file.path(PATH_OUTPUT_FIGURES, "fig_med_pe_change")

# Inputs ======================================================================

manifest_path <- file.path(PATH_OUTPUT_DATASET, "manifest.csv")
region_lookup_path <- file.path(PATH_OUTPUT_DATASET, "region_lookup.csv")
biome_lookup_path <- file.path(PATH_OUTPUT_DATASET, "biome_lookup.csv")
weight_path <- file.path(PATH_OUTPUT_OUTPUT, "weights_region_biome.Rds")

input_paths <- c(manifest_path, region_lookup_path, biome_lookup_path, weight_path)
if (any(!file.exists(input_paths))) {
  stop("Missing upstream files:\n",
       paste(input_paths[!file.exists(input_paths)], collapse = "\n"),
       call. = FALSE)
}

manifest <- data.table::fread(manifest_path)
region_lookup <- data.table::fread(region_lookup_path)
biome_lookup <- data.table::fread(biome_lookup_path)
weights <- as.data.table(readRDS(weight_path))

require_cols <- function(dt, cols, label) {
  missing <- setdiff(cols, names(dt))
  if (length(missing)) stop(label, " missing: ", toString(missing), call. = FALSE)
}

require_cols(manifest, c("scenario", "member", "file"), "manifest")
require_cols(region_lookup, c("region", "region_id"), "region_lookup")
require_cols(biome_lookup, c("biome", "biome_id"), "biome_lookup")
require_cols(weights, c("scenario", "region", "biome", "dataset",
                        "w_region_biome"), "weights_region_biome")

if (anyDuplicated(region_lookup$region_id) || anyDuplicated(region_lookup$region) ||
    anyDuplicated(biome_lookup$biome_id) || anyDuplicated(biome_lookup$biome)) {
  stop("Duplicated values in region or biome lookups.", call. = FALSE)
}

members <- copy(manifest[as.character(scenario) == SCENARIO])
setorder(members, member)
if (!identical(as.integer(members$member), seq_len(EXPECTED_MEMBERS))) {
  stop("Expected exactly CHANGE gridded members 1:", EXPECTED_MEMBERS,
       "; found: ", toString(members$member), call. = FALSE)
}
if (anyDuplicated(members$file)) stop("Duplicated NetCDF names in manifest.")
member_files <- file.path(PATH_OUTPUT_DATASET, members$file)
if (any(!file.exists(member_files))) {
  stop("Missing 04c output files:\n",
       paste(member_files[!file.exists(member_files)], collapse = "\n"),
       call. = FALSE)
}

med_id <- region_lookup[as.character(region) == REGION, region_id]
if (length(med_id) != 1L || is.na(med_id)) {
  stop("MED must have one valid region_id.", call. = FALSE)
}

# Functions ===================================================================

read_member_delta <- function(file, cell_indices, expected_years,
                              med_region_id, coords, expected_biome_ids) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  
  required <- c("lon", "lat", "biome_id", "region_id", "precipitation", "evaporation")
  if (!all(required %in% names(nc$var)) || !("year" %in% names(nc$dim))) {
    stop("Unexpected NetCDF variables/dimensions: ", basename(file), call. = FALSE)
  }
  years <- as.integer(ncdf4::ncvar_get(nc, "year"))
  if (!identical(years, expected_years)) {
    stop("Year axis mismatch in ", basename(file), call. = FALSE)
  }
  
  # 04c writes cells sorted by latitude and longitude, and P/E as [cell,year].
  # Read just the contiguous slab covering MED indices; keep native cell order.
  start_cell <- min(cell_indices)
  n_read <- max(cell_indices) - start_cell + 1L
  idx <- cell_indices - start_cell + 1L
  
  read_static <- function(var) {
    as.numeric(ncdf4::ncvar_get(nc, var, start = start_cell, count = n_read))[idx]
  }
  if (!isTRUE(all(read_static("region_id") == med_region_id)) ||
      !identical(as.integer(read_static("biome_id")), expected_biome_ids) ||
      !isTRUE(all.equal(read_static("lon"), coords$lon, tolerance = 1e-5)) ||
      !isTRUE(all.equal(read_static("lat"), coords$lat, tolerance = 1e-5))) {
    stop("Cell coordinates or region/biome lookup differ in ",
         basename(file), call. = FALSE)
  }
  
  read_field <- function(var) {
    ncdf4::ncvar_get(
      nc, var, start = c(start_cell, 1L),
      count = c(n_read, length(years)), collapse_degen = FALSE
    )[idx, , drop = FALSE]
  }
  
  # One coherent P-E source is selected per member x region x biome in 04c.
  water_balance <- read_field("precipitation") - read_field("evaporation")
  # Complete 20-year means: missing annual values invalidate a period for a
  # member. Do not silently change the temporal denominator with na.rm=TRUE.
  early <- rowMeans(water_balance[, match(EARLY_YEARS, years), drop = FALSE])
  late  <- rowMeans(water_balance[, match(LATE_YEARS, years), drop = FALSE])
  late - early
}

# Same map frame and identical coastline treatment for all four panels.
make_med_map <- function(dt, column, fill_scale) {
  ggplot(dt, aes(x = lon, y = lat, fill = .data[[column]])) +
    geom_tile(width = GRID_RESOLUTION, height = GRID_RESOLUTION) +
    geom_path(data = COASTLINES_PUB,
              aes(x = long, y = lat, group = group), inherit.aes = FALSE,
              colour = COL_REFERENCE, linewidth = FIG_BOUNDARY_LINEWIDTH) +
    fill_scale +
    coord_quickmap(xlim = MED_XLIM, ylim = MED_YLIM, expand = FALSE) +
    scale_x_continuous(labels = label_lon) +
    scale_y_continuous(labels = label_lat) +
    theme_pub_map(legend_position = "right")
}

# Analysis: 04c MED cell and biome support =====================================

nc0 <- ncdf4::nc_open(member_files[1L])
required <- c("lon", "lat", "biome_id", "region_id", "precipitation", "evaporation")
if (!all(required %in% names(nc0$var)) || !("year" %in% names(nc0$dim))) {
  ncdf4::nc_close(nc0)
  stop("First member has unexpected NetCDF structure.", call. = FALSE)
}
years0 <- as.integer(ncdf4::ncvar_get(nc0, "year"))
region_ids <- as.integer(ncdf4::ncvar_get(nc0, "region_id"))
biome_ids <- as.integer(ncdf4::ncvar_get(nc0, "biome_id"))
lon0 <- as.numeric(ncdf4::ncvar_get(nc0, "lon"))
lat0 <- as.numeric(ncdf4::ncvar_get(nc0, "lat"))
ncdf4::nc_close(nc0)

if (!identical(years0, as.integer(ALL_YEARS))) {
  stop("The 04c record is not the expected 1982-2021 annual series.", call. = FALSE)
}
if (!identical(length(region_ids), length(biome_ids)) ||
    !identical(length(region_ids), length(lon0)) ||
    !identical(length(region_ids), length(lat0))) {
  stop("NetCDF cell coordinate lengths do not match.", call. = FALSE)
}
med_indices <- which(!is.na(region_ids) & region_ids == med_id)
if (length(med_indices) == 0L) stop("No IPCC MED land cells in NetCDF.")
med_biome_ids <- biome_ids[med_indices]
med_biomes <- as.character(biome_lookup$biome[
  match(med_biome_ids, biome_lookup$biome_id)
])

med_grid <- data.table(lon = lon0[med_indices], lat = lat0[med_indices],
                       biome = med_biomes)
if (anyNA(med_grid) || any(!is.finite(med_grid$lon)) ||
    any(!is.finite(med_grid$lat)) ||
    anyDuplicated(med_grid, by = c("lon", "lat"))) {
  stop("The mapped MED cell/biome lookup is invalid.", call. = FALSE)
}
coords <- list(lon = med_grid$lon, lat = med_grid$lat)

# Analysis: Monte Carlo Delta(P-E) =============================================

n_cell <- nrow(med_grid)
n_member <- nrow(members)
delta_matrix <- matrix(NA_real_, nrow = n_cell, ncol = n_member)
message(sprintf("Reading %d CHANGE members for %d IPCC MED land cells.",
                n_member, n_cell))

for (j in seq_len(n_member)) {
  delta_matrix[, j] <- read_member_delta(
    member_files[j], med_indices, years0, med_id, coords, med_biome_ids
  )
  if (j %% 10L == 0L || j == 1L) {
    message(sprintf("  CHANGE member %03d/%03d", j, n_member))
  }
}

# The 100 member realizations are dataset-selection draws, not 100 statistically
# independent estimates of truth. All four plotted panels use the same grid.
med_grid[, n_valid_change := rowSums(is.finite(delta_matrix))]
ok <- med_grid$n_valid_change == EXPECTED_MEMBERS
if (!any(ok)) {
  stop("No cells have all 100 complete member-wise period differences.", call. = FALSE)
}

med_grid[, `:=`(
  delta_median = NA_real_, p_drying = NA_real_, delta_q90_width = NA_real_
)]

# Restrict calculations to complete ensembles. The denominator of P(drying)
# is exactly 100 for every non-missing cell, never a variable sample size.
valid_delta <- delta_matrix[ok, , drop = FALSE]
med_grid[ok, delta_median := apply(valid_delta, 1L, stats::median)]
med_grid[ok, p_drying := rowMeans(valid_delta < 0)]
med_grid[ok, delta_q90_width := apply(valid_delta, 1L, function(x) {
  q <- stats::quantile(x, probs = c(0.05, 0.95),
                       names = FALSE, type = 7)
  q[2L] - q[1L]
})]
rm(delta_matrix, valid_delta)

# Analysis: dominant source and effective weight count =========================

weights[, `:=`(
  scenario = as.character(scenario), region = as.character(region),
  biome = as.character(biome), dataset = as.character(dataset)
)]
weights <- weights[scenario == SCENARIO & region == REGION]
if (nrow(weights) == 0L ||
    anyDuplicated(weights, by = c("biome", "dataset"))) {
  stop("CHANGE/MED weight rows are absent or duplicated.", call. = FALSE)
}
if (!setequal(unique(med_grid$biome), unique(weights$biome))) {
  stop("MED biomes in the NetCDF do not match the region-biome weight file. ",
       "Regenerate 04c from current 03f outputs or inspect the discrepancy.",
       call. = FALSE)
}

weight_summary <- weights[, {
  # 03f carries exactly one NA record for a non-Water unit with no eligible
  # candidates. Such a unit is NOT assigned an arbitrary dominant source.
  missing <- is.na(dataset)
  if (all(missing) && .N == 1L) {
    list(dominant_dataset = NA_character_, effective_datasets = NA_real_,
         top_probability = NA_real_, n_eligible = 0L, n_top = 0L)
  } else {
    if (any(missing) || any(!is.finite(w_region_biome)) ||
        any(w_region_biome < 0) ||
        abs(sum(w_region_biome) - 1) > WEIGHT_SUM_TOL) {
      stop("Invalid CHANGE weights in MED biome: ", biome[1L], call. = FALSE)
    }
    top_prob <- max(w_region_biome)
    top <- dataset[abs(w_region_biome - top_prob) <= TOP_WEIGHT_TOL]
    list(
      dominant_dataset = if (length(top) == 1L) top else "Tie",
      effective_datasets = 1 / sum(w_region_biome^2),
      top_probability = top_prob,
      n_eligible = .N,
      n_top = length(top)
    )
  }
}, by = biome]

if (anyDuplicated(weight_summary$biome)) stop("Duplicated MED weight units.")
med_grid[weight_summary, on = .(biome), `:=`(
  dominant_dataset = i.dominant_dataset,
  effective_datasets = i.effective_datasets,
  top_probability = i.top_probability,
  n_eligible = i.n_eligible,
  n_top = i.n_top
)]

# Panel (c) can show either MC spread or weight dispersion. Save both.
med_grid[, spread_value := get(SPREAD_METRIC)]

# Scientific colour scales: (a) zero-centred and symmetric with no clipping;
# (b) fixed 0-100%; (c) nonnegative with full finite range; (d) source colours.
MED_XLIM <- range(med_grid$lon) + c(-MAP_PADDING, MAP_PADDING)
MED_YLIM <- range(med_grid$lat) + c(-MAP_PADDING, MAP_PADDING)
change_lim <- max(abs(med_grid$delta_median[is.finite(med_grid$delta_median)]))
if (change_lim == 0) change_lim <- 1

if (SPREAD_METRIC == "delta_q90_width") {
  spread_limits <- c(0, max(med_grid$spread_value, na.rm = TRUE))
  if (spread_limits[2L] == 0) spread_limits[2L] <- 1
  spread_label <- "5-95% width (mm/yr)"
  spread_caption <- paste0(
    "(c) Difference between the 95th and 5th percentiles of member-specific ",
    "change in P-E (mm yr-1). Quantiles characterise the discrete dataset-",
    "selection ensemble, not a calibrated confidence interval. "
  )
} else {
  spread_limits <- c(1, length(CANDIDATES))
  spread_label <- "Effective sources (1/sum w^2)"
  spread_caption <- paste0(
    "(c) Inverse-Simpson effective number of eligible source datasets, ",
    "1/sum(w_i^2), computed from CHANGE weights for each MED x biome unit ",
    "and assigned to its native grid cells. This is weight concentration, ",
    "not uncertainty in P-E. "
  )
}

# Fixed dataset palette is shared across ITHACA figures; ties are explicit.
DATASET_PALETTE <- c(DATASET_COLS, Tie = COL_TIE)
SOURCE_LABELS <- c(
  ERA5L = "ERA5-Land", FLDAS = "FLDAS", GLEAM = "MSWEP-GLEAM",
  MERRA = "MERRA-2", TERRA = "TerraClimate", Tie = "Equal maxima"
)
if (!all(na.omit(unique(med_grid$dominant_dataset)) %in% names(DATASET_PALETTE))) {
  stop("Unknown dominant dataset ID; update the manuscript palette.", call. = FALSE)
}

# Figure ======================================================================

p_a <- make_med_map(med_grid, "delta_median",
                    scale_fill_div(
                      midpoint = 0, type = "water", limits = c(-change_lim, change_lim),
                      name = "Delta(P-E) (mm/yr)", na.value = COL_MISSING
                    )
)

p_b <- make_med_map(med_grid, "p_drying",
                    scale_fill_seq(
                      palette = "seq_evap", limits = c(0, 1),
                      breaks = seq(0, 1, by = 0.25),
                      labels = scales::label_percent(accuracy = 1),
                      name = "Fraction drying", na.value = COL_MISSING
                    )
)

p_c <- make_med_map(med_grid, "spread_value",
                    scale_fill_seq(
                      palette = "seq_default", limits = spread_limits,
                      name = spread_label, na.value = COL_MISSING
                    )
)

p_d <- make_med_map(med_grid, "dominant_dataset",
                    scale_fill_manual(
                      values = DATASET_PALETTE, breaks = names(DATASET_PALETTE),
                      labels = SOURCE_LABELS[names(DATASET_PALETTE)],
                      name = "Highest-weight source", na.value = COL_MISSING,
                      drop = TRUE
                    )
)

figure <- add_panel_tags(
  (p_a | p_b) / (p_c | p_d) +
    patchwork::plot_layout(guides = "keep")
)

# Outputs =====================================================================

dir.create(PATH_OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
save_figure(figure, file_stem = FILE_STEM,
            width_mm = FIG_WIDTH_DOUBLE, height_mm = FIGURE_HEIGHT_MM)

saveRDS(list(
  map = med_grid,
  weighting_units = weight_summary,
  metadata = list(
    scenario = SCENARIO, region = REGION, early_years = EARLY_YEARS,
    late_years = LATE_YEARS, spread_panel = SPREAD_METRIC,
    expected_members = EXPECTED_MEMBERS, complete_members_required = TRUE,
    delta_scale = c(-change_lim, change_lim), spread_scale = spread_limits,
    input_files = basename(member_files), weights_file = basename(weight_path)
  )
), paste0(FILE_STEM, "_plot_data.Rds"))

caption <- paste0(
  "Change in Mediterranean (IPCC MED) terrestrial atmospheric water balance ",
  "derived member by member from the ", EXPECTED_MEMBERS,
  "-member ITHACA CHANGE-weighted gridded P-E ensemble. ",
  "(a) Ensemble median change in mean annual P-E, 2002-2021 minus 1982-2001 ",
  "(mm yr-1; a negative value indicates drying). ",
  "(b) Fraction of CHANGE ensemble members with a negative change in P-E. ",
  spread_caption,
  "(d) Candidate source with the largest CHANGE sampling probability in each ",
  "IPCC region x biome unit; equal highest weights are shown as ties, ",
  "and units without eligible datasets are missing. ",
  "Sampling weights are estimated at region x biome scale and are constant ",
  "among grid cells of each biome within MED. ",
  "P and actual evaporation are paired within each realization; ",
  "MSWEP-GLEAM is the designated mixed-source candidate. ",
  "Both 20-year period means require complete annual values, and panels ",
  "(a)-(c) show grey for cells without all ", EXPECTED_MEMBERS,
  " member differences. Native 0.25-degree grid; no interpolation. ",
  "CHANGE denotes a dataset-weighting configuration, not a future ",
  "emissions scenario. Ensemble member fractions and spreads reflect ",
  "the prescribed sampling model and are not calibrated probabilities ",
  "or confidence intervals. P-E is an atmospheric balance indicator and ",
  "not a complete assessment of catchment water resources."
)
writeLines(caption, paste0(FILE_STEM, "_caption.txt"))

# Validation ==================================================================

stopifnot(
  all(med_grid$p_drying[is.finite(med_grid$p_drying)] >= 0),
  all(med_grid$p_drying[is.finite(med_grid$p_drying)] <= 1),
  all(med_grid$delta_q90_width[is.finite(med_grid$delta_q90_width)] >= -1e-12),
  all(med_grid$n_valid_change <= EXPECTED_MEMBERS),
  all(med_grid$effective_datasets[is.finite(med_grid$effective_datasets)] >= 1 - 1e-6),
  all(med_grid$effective_datasets[is.finite(med_grid$effective_datasets)] <=
        med_grid$n_eligible[is.finite(med_grid$effective_datasets)] + 1e-6)
)
message(sprintf(
  "Finished: %d MED cells; %d complete cells; %d tied-weight cells; panel (c): %s.",
  n_cell, sum(ok), sum(med_grid$dominant_dataset == "Tie", na.rm = TRUE), SPREAD_METRIC
))
message("Figure: ", FILE_STEM, ".pdf / .png")
check_fonts(paste0(FILE_STEM, ".pdf"))
