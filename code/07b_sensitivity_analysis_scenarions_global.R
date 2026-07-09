# ============================================================================
# Plot global mean changes in availability versus flux space
# ============================================================================

# Inputs ======================================================================

source("source/twc_change.R")

library(ggrepel)

scenario_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_global_yearly_prec_evap.Rds")
)

dataset_global_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_global_yearly_prec_evap.Rds")
)

dataset_unweighted_mean <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_unweighted_mean_global_yearly_prec_evap.Rds")
)


# Constants & Variables =======================================================

PERIOD_1 <- c(1982, 2001)
PERIOD_2 <- c(2002, 2021)

SCENARIO_LABELS <- c(
  "base" = "Base",
  "clim_dominant" = "Climate dominated",
  "evap_dominant" = "Evaporation dominated",
  "prec_dominant" = "Precipitation dominated",
  "rank_linear" = "Rank linear",
  "rank_exp" = "Rank exponential",
  "neutral" = "Neutral",
  "inverted" = "Disagreement"
)

SCENARIO_COLS <- c(
  "base" = "black",
  "clim_dominant" = "#1b9e77",
  "evap_dominant" = "#d95f02",
  "prec_dominant" = "#7570b3",
  "rank_linear" = "#66a61e",
  "rank_exp" = "#e6ab02",
  "neutral" = "#666666",
  "inverted" = "#e7298a"
)

DATASET_COLS <- c(
  "ERA5L" = "#1b9e77",
  "FLDAS" = "#d95f02",
  "GLEAM" = "#7570b3",
  "MERRA" = "#e7298a",
  "TERRA" = "#66a61e"
)

STORY_COLS <- c(
  "wetter_accelerated" = PALETTES$water_cycle_change[1],
  "wetter_decelerated" = PALETTES$water_cycle_change[2],
  "drier_accelerated" = PALETTES$water_cycle_change[3],
  "drier_decelerated" = PALETTES$water_cycle_change[4]
)


# Functions ===================================================================

add_twc_vars <- function(dt) {
  dt <- copy(as.data.table(dt))
  dt[, avail := prec - evap]
  dt[, flux := (prec + evap) / 2]
  dt[]
}

period_change <- function(dt, id_cols, value_cols = c("prec", "evap", "avail", "flux")) {
  dt <- add_twc_vars(dt)
  
  p1 <- dt[
    year >= PERIOD_1[1] & year <= PERIOD_1[2],
    lapply(.SD, mean, na.rm = TRUE),
    by = id_cols,
    .SDcols = value_cols
  ]
  
  p2 <- dt[
    year >= PERIOD_2[1] & year <= PERIOD_2[2],
    lapply(.SD, mean, na.rm = TRUE),
    by = id_cols,
    .SDcols = value_cols
  ]
  
  setnames(
    p1,
    value_cols,
    paste0(value_cols, "_p1")
  )
  
  setnames(
    p2,
    value_cols,
    paste0(value_cols, "_p2")
  )
  
  out <- merge(
    p1,
    p2,
    by = id_cols
  )
  
  for (v in value_cols) {
    out[
      ,
      paste0(v, "_change") := get(paste0(v, "_p2")) - get(paste0(v, "_p1"))
    ]
  }
  
  out[]
}

make_axis_limit <- function(x, pad = 1.25) {
  lim <- max(abs(x), na.rm = TRUE) * pad
  
  if (!is.finite(lim) || lim == 0) {
    lim <- 1
  }
  
  lim
}


# Analysis ====================================================================

setDT(scenario_global_yearly)
setDT(dataset_global_yearly)
setDT(dataset_unweighted_mean)

scenario_change <- period_change(
  scenario_global_yearly[scenario %in% names(SCENARIO_LABELS)],
  id_cols = "scenario"
)

scenario_change[
  ,
  `:=`(
    type = "Scenario",
    label = SCENARIO_LABELS[scenario]
  )
]

dataset_change <- period_change(
  dataset_global_yearly,
  id_cols = "dataset"
)

dataset_change[
  ,
  `:=`(
    type = "Dataset",
    label = dataset
  )
]

dataset_mean_change <- period_change(
  dataset_unweighted_mean,
  id_cols = "series"
)

dataset_mean_change[
  ,
  `:=`(
    type = "Unweighted mean",
    label = "Unweighted mean"
  )
]

plot_dt <- rbindlist(
  list(
    scenario_change[
      ,
      .(
        id = scenario,
        label,
        type,
        avail_change,
        flux_change,
        prec_change,
        evap_change
      )
    ],
    dataset_change[
      ,
      .(
        id = dataset,
        label,
        type,
        avail_change,
        flux_change,
        prec_change,
        evap_change
      )
    ],
    dataset_mean_change[
      ,
      .(
        id = series,
        label,
        type,
        avail_change,
        flux_change,
        prec_change,
        evap_change
      )
    ]
  ),
  use.names = TRUE
)

plot_dt[
  ,
  storyline := fcase(
    avail_change >= 0 & flux_change >= 0, "Wet and accelerated",
    avail_change <  0 & flux_change >= 0, "Dry and accelerated",
    avail_change >= 0 & flux_change <  0, "Wet and decelerated",
    avail_change <  0 & flux_change <  0, "Dry and decelerated"
  )
]

x_abs <- make_axis_limit(plot_dt$avail_change)
y_abs <- make_axis_limit(plot_dt$flux_change)

quad_labels <- data.table(
  x = c(-x_abs * 0.96, x_abs * 0.96, -x_abs * 0.96, x_abs * 0.96),
  y = c(y_abs * 0.96, y_abs * 0.96, -y_abs * 0.96, -y_abs * 0.96),
  hjust = c(0, 1, 0, 1),
  vjust = c(1, 1, 0, 0),
  label = c(
    "Dry and accelerated",
    "Wet and accelerated",
    "Dry and decelerated",
    "Wet and decelerated"
  ),
  colour = c(
    STORY_COLS["drier_accelerated"],
    STORY_COLS["wetter_accelerated"],
    STORY_COLS["drier_decelerated"],
    STORY_COLS["wetter_decelerated"]
  )
)


# Outputs =====================================================================

saveRDS(
  plot_dt,
  file.path(PATH_OUTPUT_DATA, "global_avail_flux_change_plot_dt.Rds")
)


# Plot ========================================================================

p_global_avail_flux <- ggplot() +
  
  # Quadrant fills
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = 0, ymax = Inf,
    fill = STORY_COLS["drier_accelerated"],
    alpha = 0.10
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = 0, ymax = Inf,
    fill = STORY_COLS["wetter_accelerated"],
    alpha = 0.10
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = -Inf, ymax = 0,
    fill = STORY_COLS["drier_decelerated"],
    alpha = 0.10
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = -Inf, ymax = 0,
    fill = STORY_COLS["wetter_decelerated"],
    alpha = 0.10
  ) +
  
  # Reference axes
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey35"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey35"
  ) +
  
  # Driver diagonals
  geom_abline(
    slope = -0.5,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey25"
  ) +
  geom_abline(
    slope = 0.5,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey25"
  ) +
  
  # Dataset points
  geom_point(
    data = plot_dt[type == "Dataset"],
    aes(
      x = avail_change,
      y = flux_change,
      colour = id
    ),
    size = 4.2,
    shape = 16,
    alpha = 0.95
  ) +
  geom_point(
    data = plot_dt[type == "Dataset"],
    aes(
      x = avail_change,
      y = flux_change
    ),
    size = 4.2,
    shape = 1,
    colour = "black",
    stroke = 0.55
  ) +
  geom_text_repel(
    data = plot_dt[type == "Dataset"],
    aes(
      x = avail_change,
      y = flux_change,
      label = label,
      colour = id
    ),
    size = 4.0,
    fontface = "bold",
    show.legend = FALSE,
    box.padding = 0.45,
    point.padding = 0.30,
    max.overlaps = Inf,
    segment.colour = "grey60",
    segment.linewidth = 0.25
  ) +
  
  # Scenario points
  geom_point(
    data = plot_dt[type == "Scenario"],
    aes(
      x = avail_change,
      y = flux_change,
      fill = id
    ),
    size = 4.4,
    shape = 21,
    colour = "black",
    stroke = 0.50,
    alpha = 0.95
  ) +
  geom_text_repel(
    data = plot_dt[type == "Scenario"],
    aes(
      x = avail_change,
      y = flux_change,
      label = label,
      fill = id
    ),
    colour = "black",
    size = 3.4,
    show.legend = FALSE,
    box.padding = 0.45,
    point.padding = 0.25,
    max.overlaps = Inf,
    segment.colour = "grey55",
    segment.linewidth = 0.25
  ) +
  
  # Unweighted mean
  geom_point(
    data = plot_dt[type == "Unweighted mean"],
    aes(
      x = avail_change,
      y = flux_change
    ),
    shape = 23,
    size = 5.8,
    fill = "white",
    colour = "black",
    stroke = 1.2
  ) +
  geom_text_repel(
    data = plot_dt[type == "Unweighted mean"],
    aes(
      x = avail_change,
      y = flux_change,
      label = label
    ),
    colour = "black",
    size = 4.0,
    fontface = "bold",
    show.legend = FALSE,
    box.padding = 0.55,
    point.padding = 0.35,
    max.overlaps = Inf,
    segment.colour = "black",
    segment.linewidth = 0.30
  ) +
  
  # Quadrant labels
  geom_label(
    data = quad_labels,
    aes(
      x = x,
      y = y,
      label = label,
      hjust = hjust,
      vjust = vjust
    ),
    inherit.aes = FALSE,
    size = 4.0,
    fontface = "bold",
    colour = quad_labels$colour,
    fill = "white",
    label.size = 0.25,
    alpha = 0.92
  ) +
  
  # Driver labels
  annotate(
    "label",
    x = 0,
    y = y_abs * 0.82,
    label = "ΔP > 0\nΔE > 0",
    size = 3.7,
    fontface = "bold",
    fill = "white",
    label.size = 0.25
  ) +
  annotate(
    "label",
    x = 0,
    y = -y_abs * 0.82,
    label = "ΔP < 0\nΔE < 0",
    size = 3.7,
    fontface = "bold",
    fill = "white",
    label.size = 0.25
  ) +
  annotate(
    "label",
    x = x_abs * 0.78,
    y = 0,
    label = "ΔP > 0\nΔE < 0",
    size = 3.7,
    fontface = "bold",
    fill = "white",
    label.size = 0.25
  ) +
  annotate(
    "label",
    x = -x_abs * 0.78,
    y = 0,
    label = "ΔP < 0\nΔE > 0",
    size = 3.7,
    fontface = "bold",
    fill = "white",
    label.size = 0.25
  ) +
  
  # Scales
  scale_colour_manual(
    name = "Dataset",
    values = DATASET_COLS,
    breaks = names(DATASET_COLS),
    drop = FALSE
  ) +
  scale_fill_manual(
    name = "Weighting scenario",
    values = SCENARIO_COLS,
    labels = SCENARIO_LABELS,
    breaks = names(SCENARIO_LABELS),
    drop = FALSE
  ) +
  scale_x_continuous(
    name = expression(Delta(P - E) ~ "[mm" ~ yr^{-1} * "]"),
    limits = c(-x_abs, x_abs),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = expression(Delta((P + E) / 2) ~ "[mm" ~ yr^{-1} * "]"),
    limits = c(-y_abs, y_abs),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "Global water cycle change in availability and flux space",
    subtitle = paste0(
      "Changes are period means for ",
      PERIOD_2[1], "-", PERIOD_2[2],
      " minus ",
      PERIOD_1[1], "-", PERIOD_1[2]
    ),
    caption = "Circles: original datasets. Filled circles: weighting scenario means. Diamond: unweighted dataset mean. Dashed diagonals mark ΔP = 0 and ΔE = 0."
  ) +
  guides(
    colour = guide_legend(
      order = 1,
      override.aes = list(size = 4.2, alpha = 1)
    ),
    fill = guide_legend(
      order = 2,
      override.aes = list(size = 4.2, alpha = 1)
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey93"),
    legend.position = "bottom",
    legend.box = "vertical",
    strip.background = element_rect(fill = "grey95"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(
      size = 8,
      colour = "grey45",
      hjust = 0,
      margin = margin(t = 6)
    ),
    axis.title = element_text(size = 11),
    plot.margin = margin(8, 18, 8, 8)
  )

p_global_avail_flux


# Outputs =====================================================================

ggsave(
  filename = file.path(PATH_FIGURES, "global_mean_avail_flux_change_space.png"),
  plot = p_global_avail_flux,
  width = 9.5,
  height = 7.5,
  dpi = 300
)


# Validation ==================================================================

print(
  plot_dt[
    ,
    .(
      id,
      type,
      avail_change = round(avail_change, 3),
      flux_change = round(flux_change, 3),
      prec_change = round(prec_change, 3),
      evap_change = round(evap_change, 3),
      storyline
    )
  ][order(type, id)]
)