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

grid_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
)

# Constants & Variables ======================================================

# Relative spread (IQR / |median|) at which the colour scale saturates.
SPREAD_CAP <- 1

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
DIRECTION_COLS <- c("#2166ac", "#b35806", "#762a83", "grey88")

SIGNIFICANCE_LEVELS <- c("most_sig", "disagree", "most_nonsig")
SIGNIFICANCE_LABELS <- c(
  "Most products significant",
  "Products disagree",
  "Most products non-significant"
)
SIGNIFICANCE_COLS <- c("#01665e", "#dfc27d", "grey88")

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

# Colour scale shared by the two spread rows.
spread_scale <- function(name) {
  scale_fill_viridis_c(
    name = name,
    option = "magma",
    direction = -1,
    limits = c(0, SPREAD_CAP),
    transform = "sqrt",
    breaks = c(0.1, 0.25, 0.5, 1),
    labels = function(x) ifelse(x >= SPREAD_CAP, "≥100%", scales::percent(x, accuracy = 1)),
    oob = scales::squish,
    na.value = COL_NO_DATA,
    guide = guide_colourbar(barheight = unit(2.6, "cm"), barwidth = unit(0.3, "cm"))
  )
}

category_scale <- function(name, levels, labels, cols) {
  scale_fill_manual(
    name = name,
    values = setNames(cols, levels),
    labels = setNames(labels, levels),
    breaks = levels,
    limits = levels,
    drop = FALSE,
    na.value = COL_NO_DATA,
    guide = guide_legend(keywidth = unit(0.35, "cm"), keyheight = unit(0.35, "cm"))
  )
}

column_header <- function(label) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = label, size = 3.6, fontface = "bold") +
    theme_void()
}

# One row of the figure: P map, E map, and the row's legend in the third column.
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

# Maps -------------------------------------------------------------------------

p_mean_prec <- cell_map(prec_map, "spread_mean", spread_scale("Spread\n(IQR / |median|)")) +
  map_label("Mean spread")
p_mean_evap <- cell_map(evap_map, "spread_mean", spread_scale("Spread\n(IQR / |median|)")) +
  map_label("Mean spread")

p_sd_prec <- cell_map(prec_map, "spread_sd", spread_scale("Spread\n(IQR / |median|)")) +
  map_label("SD spread")
p_sd_evap <- cell_map(evap_map, "spread_sd", spread_scale("Spread\n(IQR / |median|)")) +
  map_label("SD spread")

direction_scale <- category_scale(
  "Trend direction\n(reference products)",
  DIRECTION_LEVELS, DIRECTION_LABELS, DIRECTION_COLS
)
p_dir_prec <- cell_map(prec_map, "direction", direction_scale) + map_label("Trend direction")
p_dir_evap <- cell_map(evap_map, "direction", direction_scale) + map_label("Trend direction")

significance_scale <- category_scale(
  "Trend significance\n(reference products)",
  SIGNIFICANCE_LEVELS, SIGNIFICANCE_LABELS, SIGNIFICANCE_COLS
)
p_sig_prec <- cell_map(prec_map, "significance", significance_scale) + map_label("Trend significance")
p_sig_evap <- cell_map(evap_map, "significance", significance_scale) + map_label("Trend significance")

figure_2 <- (
  (column_header("Precipitation (P)") | column_header("Evaporation (E)") | plot_spacer()) +
    plot_layout(widths = c(1, 1, 0.34))
) /
  figure_row(p_mean_prec, p_mean_evap) /
  figure_row(p_sd_prec, p_sd_evap) /
  figure_row(p_dir_prec, p_dir_evap) /
  figure_row(p_sig_prec, p_sig_evap) +
  plot_layout(heights = c(0.07, 1, 1, 1, 1)) +
  plot_annotation(
    caption = paste0(
      "Spread: IQR / |median| across the 8 reference products (median over the five leave-one-out references). ",
      "Trend: Mann-Kendall p < 0.1; direction after the majority logic of 01f."
    ),
    theme = theme(plot.caption = element_text(size = 6.5, colour = "grey30", hjust = 0))
  )

# Outputs ====================================================================

save_figure(figure_2, "fig02_input_disagreement", width = 11.5, height = 9.2)

# Validation =================================================================

# Both ensembles have the full 8 members, and the mask is the same everywhere.
stopifnot(prec_summary[, all(n_ref == 8L)], evap_summary[, all(n_ref == 8L)])
stopifnot(nrow(prec_map) == nrow(mask), nrow(evap_map) == nrow(mask))
stopifnot(!anyNA(prec_map$n_ref), !anyNA(evap_map$n_ref))

# Counts are bounded by the ensemble size.
stopifnot(prec_summary[, all(n_pos + n_neg <= n_sig & n_sig <= n_ref)])
stopifnot(evap_summary[, all(n_pos + n_neg <= n_sig & n_sig <= n_ref)])

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
