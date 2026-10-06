# ============================================================================
# Figure 2: input disagreement. Where do the observational products disagree?
#
# 4 x 2 maps (columns: precipitation, evaporation; rows: long-term mean spread,
# interannual SD spread, trend-direction agreement, trend-significance
# agreement) of the disagreement among the members of the two 8-member
# reference ensembles. This is before candidate ranking, gating, weighting or
# Monte Carlo sampling, and no dataset identities are shown.
#
# Nothing is re-estimated. Everything comes from the reference tables of 01f
# (prec_reference_values.fst, evap_reference_values.fst):
#   - Those tables are leave-one-out: each of the five candidate rows holds the
#     statistics of the reference ensemble with that candidate removed (if it is
#     a member). For the two continuous spreads we take, per cell, the median
#     over the five candidate-specific references of IQR / |median|.
#   - For trend direction and significance we recover the counts of the FULL
#     ensemble exactly: a member candidate's own result is added back to the
#     counts of its leave-one-out reference (n_significant, n_pos, n_neg).
#     The sums agree across all member candidates (checked in Validation).
#
# Trend direction keeps the majority logic of 01f (significant = Mann-Kendall
# p < 0.1; majority significant = more than half of the members; majority
# agrees = the larger sign group is more than half of the significant members).
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")
source("code/_figs.R") # current publication design system; restores modern export helpers

# Inputs =====================================================================

REFERENCE_COLUMNS <- c(
  "candidate_value_dataset", "lon", "lat",
  "candidate_sen_slope", "candidate_stat_sig",
  "ref_mean_median", "ref_mean_iqr", "ref_sd_median", "ref_sd_iqr",
  "n_significant", "n_pos", "n_neg"
)

prec_reference <- read_fst(
  file.path(PATH_OUTPUT_OUTPUT, "prec_reference_values.fst"),
  columns = REFERENCE_COLUMNS,
  as.data.table = TRUE
)

evap_reference <- read_fst(
  file.path(PATH_OUTPUT_OUTPUT, "evap_reference_values.fst"),
  columns = REFERENCE_COLUMNS,
  as.data.table = TRUE
)

grid_classes <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
))

stopifnot(
  all(c("lon", "lat", "cell_weight") %in% names(grid_classes)),
  !anyDuplicated(grid_classes[, .(lon, lat)]),
  all(is.finite(grid_classes$lon)),
  all(is.finite(grid_classes$lat)),
  all(is.finite(grid_classes$cell_weight) & grid_classes$cell_weight > 0)
)

# Constants & Variables ======================================================

# Relative spread (IQR / |median|) at which the colour scale saturates.
SPREAD_CAP <- 1

# Final ESSD/Copernicus figure height. Width comes from the active journal profile.
FIGURE_HEIGHT_MM <- 160

# Trend direction: products with a significant trend of EACH sign needed to call
# a cell "significant disagreement" when no sign has a majority.
MIN_OPPOSING <- 2L

# Trend significance: share of members with a significant trend.
SIG_SHARE_HIGH <- 0.75   # at or above: most products significant
SIG_SHARE_LOW  <- 0.25   # at or below: most products non-significant

DIRECTION_LEVELS <- c("positive", "negative", "disagreement", "none")
DIRECTION_LABELS <- c(
  "Agreement: positive trend",
  "Agreement: negative trend",
  "Significant disagreement",
  "No robust trend signal"
)
# Figure-specific categorical mapping using the shared ITHACA palette.
DIRECTION_COLS <- setNames(
  c(PAL_CAT_8[2], PAL_CAT_8[1], PAL_CAT_8[5], COL_MISSING),
  DIRECTION_LEVELS
)

SIGNIFICANCE_LEVELS <- c("most_sig", "disagree", "most_nonsig")
SIGNIFICANCE_LABELS <- c(
  "Most products significant",
  "Products disagree",
  "Most products non-significant"
)
SIGNIFICANCE_COLS <- setNames(
  c(PAL_CAT_8[2], PAL_CAT_8[6], COL_MISSING),
  SIGNIFICANCE_LEVELS
)

# Functions ==================================================================

# Per-cell ensemble summary from one leave-one-out reference table.
summarise_reference_ensemble <- function(reference, ensemble_names) {
  ref <- copy(reference)

  # Continuous spreads: relative spread of each candidate-specific reference,
  # then the median over the candidates. Zero medians give NA.
  ref[
    ,
    `:=`(
      spread_mean = fifelse(
        ref_mean_median != 0,
        ref_mean_iqr / abs(ref_mean_median),
        NA_real_
      ),
      spread_sd = fifelse(
        ref_sd_median > 0,
        ref_sd_iqr / ref_sd_median,
        NA_real_
      )
    )
  ]

  spread <- ref[
    ,
    .(
      spread_mean = median(spread_mean, na.rm = TRUE),
      spread_sd = median(spread_sd, na.rm = TRUE)
    ),
    by = .(lon, lat)
  ]

  # Trend counts of the full ensemble: add the member candidate back.
  members <- ref[candidate_value_dataset %in% ensemble_names]

  members[
    ,
    `:=`(
      n_sig_full = n_significant + as.integer(candidate_stat_sig),
      n_pos_full = n_pos + fifelse(
        candidate_stat_sig & !is.na(candidate_sen_slope) & candidate_sen_slope > 0,
        1L, 0L
      ),
      n_neg_full = n_neg + fifelse(
        candidate_stat_sig & !is.na(candidate_sen_slope) & candidate_sen_slope < 0,
        1L, 0L
      )
    )
  ]

  # Every member candidate must give the same full-ensemble counts.
  consistent <- members[
    ,
    .(
      ok = uniqueN(n_sig_full) == 1L &
        uniqueN(n_pos_full) == 1L &
        uniqueN(n_neg_full) == 1L
    ),
    by = .(lon, lat)
  ]

  stopifnot(all(consistent$ok))

  counts <- members[
    ,
    .(
      n_sig = n_sig_full[1],
      n_pos = n_pos_full[1],
      n_neg = n_neg_full[1]
    ),
    by = .(lon, lat)
  ]

  out <- merge(spread, counts, by = c("lon", "lat"))
  out[, n_ref := length(ensemble_names)]
  out[]
}

classify_direction <- function(n_sig, n_pos, n_neg, n_ref) {
  majority_significant <- n_sig > floor(n_ref / 2)
  majority_agrees <- majority_significant &
    pmax(n_pos, n_neg) > floor(n_sig / 2)

  fcase(
    majority_agrees & n_pos > n_neg, "positive",
    majority_agrees & n_neg > n_pos, "negative",
    pmin(n_pos, n_neg) >= MIN_OPPOSING, "disagreement",
    default = "none"
  )
}

classify_significance <- function(n_sig, n_ref) {
  share <- n_sig / n_ref

  fcase(
    share >= SIG_SHARE_HIGH, "most_sig",
    share <= SIG_SHARE_LOW, "most_nonsig",
    default = "disagree"
  )
}

add_classes <- function(summary_dt) {
  summary_dt[
    ,
    `:=`(
      direction = factor(
        classify_direction(n_sig, n_pos, n_neg, n_ref),
        levels = DIRECTION_LEVELS
      ),
      significance = factor(
        classify_significance(n_sig, n_ref),
        levels = SIGNIFICANCE_LEVELS
      )
    )
  ]

  summary_dt[]
}

# Colour scale shared by the two spread rows. Limits and the square-root
# transform are scientific display choices; colours and legend geometry come
# from the shared design system.
spread_scale <- function(name) {
  scale_fill_seq(
    name = name,
    palette = "seq_default",
    limits = c(0, SPREAD_CAP),
    transform = "sqrt",
    breaks = c(0.1, 0.25, 0.5, 1),
    labels = function(x) {
      ifelse(x >= SPREAD_CAP, "\u2265100%", scales::percent(x, accuracy = 1))
    },
    na.value = COL_MISSING
  )
}

category_scale <- function(name, levels, labels, cols) {
  scale_fill_cat(
    name = name,
    values = cols,
    labels = setNames(labels, levels),
    breaks = levels,
    limits = levels,
    drop = FALSE,
    na.value = COL_MISSING
  )
}

# Global raster map using the shared map frame and publication styling.
publication_map <- function(dt, fill, fill_scale) {
  ggplot(dt, aes(x = lon, y = lat, fill = .data[[fill]])) +
    geom_raster() +
    geom_path(
      data = COASTLINES,
      aes(x = long, y = lat, group = group),
      inherit.aes = FALSE,
      colour = COL_REFERENCE,
      linewidth = FIG_BOUNDARY_LINEWIDTH
    ) +
    fill_scale +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 4),
      labels = label_lon,
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 4),
      labels = label_lat,
      expand = expansion(mult = 0)
    ) +
    coord_quickmap(xlim = MAP_XLIM, ylim = MAP_YLIM, expand = FALSE) +
    theme_pub_map(legend_position = "right")
}

column_header <- function(label) {
  ggplot() +
    annotate(
      "text", x = 0, y = 0, label = label,
      family = FIG_FONT, size = FIG_GEOM_TEXT_SIZE,
      colour = COL_TEXT, fontface = "bold"
    ) +
    theme_void(base_size = FIG_BASE_SIZE, base_family = FIG_FONT)
}

# One row of the figure: P map, E map, and the row's shared legend.
figure_row <- function(map_prec, map_evap) {
  (map_prec + map_evap + guide_area()) +
    plot_layout(widths = c(1, 1, 0.34), guides = "collect")
}

# Analysis ===================================================================

prec_summary <- summarise_reference_ensemble(prec_reference, PREC_ENSEMBLE_NAMES_SHORT)
evap_summary <- summarise_reference_ensemble(evap_reference, EVAP_ENSEMBLE_NAMES_SHORT)

prec_summary <- add_classes(prec_summary)
evap_summary <- add_classes(evap_summary)

# One spatial mask for all eight maps: the cells of grid_classes.
mask <- grid_classes[, .(lon, lat, cell_weight)]

prec_map <- merge(mask, prec_summary, by = c("lon", "lat"), all.x = TRUE)
evap_map <- merge(mask, evap_summary, by = c("lon", "lat"), all.x = TRUE)

# Validation =================================================================

# Both ensembles have the full 8 members, and the mask is the same everywhere.
stopifnot(prec_summary[, all(n_ref == 8L)], evap_summary[, all(n_ref == 8L)])
stopifnot(nrow(prec_map) == nrow(mask), nrow(evap_map) == nrow(mask))
stopifnot(!anyNA(prec_map$n_ref), !anyNA(evap_map$n_ref))

# Counts are bounded by the ensemble size.
stopifnot(prec_summary[, all(n_pos + n_neg <= n_sig & n_sig <= n_ref)])
stopifnot(evap_summary[, all(n_pos + n_neg <= n_sig & n_sig <= n_ref)])

# Maps -------------------------------------------------------------------------

p_mean_prec <- publication_map(
  prec_map, "spread_mean",
  spread_scale("Long-term mean spread\n(IQR / |median|)")
)
p_mean_evap <- publication_map(
  evap_map, "spread_mean",
  spread_scale("Long-term mean spread\n(IQR / |median|)")
)

p_sd_prec <- publication_map(
  prec_map, "spread_sd",
  spread_scale("Interannual SD spread\n(IQR / |median|)")
)
p_sd_evap <- publication_map(
  evap_map, "spread_sd",
  spread_scale("Interannual SD spread\n(IQR / |median|)")
)

direction_scale <- category_scale(
  "Trend direction",
  DIRECTION_LEVELS, DIRECTION_LABELS, DIRECTION_COLS
)
p_dir_prec <- publication_map(prec_map, "direction", direction_scale)
p_dir_evap <- publication_map(evap_map, "direction", direction_scale)

significance_scale <- category_scale(
  "Trend significance",
  SIGNIFICANCE_LEVELS, SIGNIFICANCE_LABELS, SIGNIFICANCE_COLS
)
p_sig_prec <- publication_map(prec_map, "significance", significance_scale)
p_sig_evap <- publication_map(evap_map, "significance", significance_scale)

# The figure contains no panel subtitles or embedded caption. Panel tags and
# column headers carry only the structure needed to read the multi-panel layout.
map_grid <- figure_row(p_mean_prec, p_mean_evap) /
  figure_row(p_sd_prec, p_sd_evap) /
  figure_row(p_dir_prec, p_dir_evap) /
  figure_row(p_sig_prec, p_sig_evap)

map_grid <- add_panel_tags(map_grid)

figure_2 <- (
  (column_header("Precipitation (P)") |
     column_header("Evaporation (E)") |
     plot_spacer()) +
    plot_layout(widths = c(1, 1, 0.34))
) /
  map_grid +
  plot_layout(heights = c(0.06, 1))

# The spread scale is capped, so report any saturated values explicitly.
check_limits(
  c(prec_map$spread_mean, evap_map$spread_mean),
  c(0, SPREAD_CAP),
  "Long-term mean relative spread"
)
check_limits(
  c(prec_map$spread_sd, evap_map$spread_sd),
  c(0, SPREAD_CAP),
  "Interannual SD relative spread"
)

# Outputs ====================================================================

figure_stem <- file.path(PATH_OUTPUT_FIGURES, "fig02_input_disagreement")
quiet_raster_gaps(
  save_figure(
    figure_2,
    file_stem = figure_stem,
    width_mm = FIG_WIDTH_DOUBLE,
    height_mm = FIGURE_HEIGHT_MM
  )
)
check_fonts(paste0(figure_stem, ".pdf"))

caption <- paste0(
  "Input disagreement among the precipitation and evaporation reference products. ",
  "(a, b) Relative spread of the long-term mean and (c, d) relative spread of ",
  "interannual standard deviation, expressed as IQR / |median| across the ",
  "eight-member reference ensembles and summarized across the five leave-one-out ",
  "references. The colour scale uses a square-root transform and is capped at 100%; ",
  "larger values are shown with the maximum colour. (e, f) Trend-direction agreement ",
  "and (g, h) trend-significance agreement. Trends use the Mann-Kendall test with ",
  "p < 0.1 and the majority logic defined in 01f."
)
writeLines(caption, paste0(figure_stem, "_caption.txt"))

# Data QA ====================================================================

# Area-weighted share of land in each class, for the caption and the text.
class_share <- function(map_dt, column) {
  map_dt[
    !is.na(get(column)),
    .(share = round(100 * sum(cell_weight) / map_dt[!is.na(get(column)), sum(cell_weight)], 1)),
    by = column
  ][order(get(column))]
}

for (variable_name in c("prec", "evap")) {
  map_dt <- get(paste0(variable_name, "_map"))

  cat("\n", variable_name, ": trend direction, % of land area\n", sep = "")
  print(class_share(map_dt, "direction"))

  cat("\n", variable_name, ": trend significance, % of land area\n", sep = "")
  print(class_share(map_dt, "significance"))

  cat(
    "\n", variable_name, ": median relative spread, mean / SD = ",
    round(map_dt[, median(spread_mean, na.rm = TRUE)], 3), " / ",
    round(map_dt[, median(spread_sd, na.rm = TRUE)], 3), "\n",
    sep = ""
  )
}
