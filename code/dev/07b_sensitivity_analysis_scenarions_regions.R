# ============================================================================
# Plot regional mean changes in availability versus flux space
#
# Uses only:
#   1. scenario_region_yearly
#   2. dataset_region_yearly
#
# No grid-cell prec_evap.
# No recomputation of regional means.
# Plots pages directly to screen, 9 regions per page.
# ============================================================================

# Inputs ======================================================================

source("source/twc_change.R")

library(data.table)
library(ggplot2)
library(ggrepel)

scenario_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "scenario_prec_evap_region.Rds")
)

dataset_region_yearly <- readRDS(
  file.path(PATH_OUTPUT_DATA, "dataset_region_yearly_prec_evap.Rds")
)


# Constants & Variables =======================================================

PERIOD_1 <- c(1982, 2001)
PERIOD_2 <- c(2002, 2021)

REGIONS_PER_PAGE <- 9
N_COLS <- 3
N_ROWS <- 3

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

DATASET_COLS_BASE <- c(
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

period_change <- function(dt,
                          id_cols,
                          value_cols = c("prec", "evap", "avail", "flux")) {
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
  
  setnames(p1, value_cols, paste0(value_cols, "_p1"))
  setnames(p2, value_cols, paste0(value_cols, "_p2"))
  
  out <- merge(p1, p2, by = id_cols)
  
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

make_dataset_colours <- function(dataset_ids) {
  dataset_cols <- DATASET_COLS_BASE[intersect(names(DATASET_COLS_BASE), dataset_ids)]
  missing_ids <- setdiff(dataset_ids, names(dataset_cols))
  
  if (length(missing_ids) > 0) {
    extra_cols <- RColorBrewer::brewer.pal(
      max(3, length(missing_ids)),
      "Set2"
    )[seq_along(missing_ids)]
    
    dataset_cols <- c(
      dataset_cols,
      setNames(extra_cols, missing_ids)
    )
  }
  
  dataset_cols[dataset_ids]
}

make_region_axis_dt <- function(dt_page, pad = 1.25) {
  axis_dt <- dt_page[
    ,
    .(
      x_abs = make_axis_limit(avail_change, pad = pad),
      y_abs = make_axis_limit(flux_change, pad = pad)
    ),
    by = region
  ]
  
  axis_dt[
    ,
    .(
      x = c(-x_abs, x_abs, -x_abs, x_abs),
      y = c(-y_abs, -y_abs, y_abs, y_abs)
    ),
    by = region
  ]
}

make_region_page_plot <- function(dt_page,
                                  page_id,
                                  dataset_cols) {
  dt_page <- copy(as.data.table(dt_page))
  
  axis_dt <- make_region_axis_dt(dt_page)
  
  ggplot() +
    
    # Invisible layer forcing symmetric free axes per region --------------------
  geom_blank(
    data = axis_dt,
    aes(
      x = x,
      y = y
    )
  ) +
    
    # Quadrant fills ------------------------------------------------------------
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = 0, ymax = Inf,
    fill = STORY_COLS["drier_accelerated"],
    alpha = 0.09
  ) +
    annotate(
      "rect",
      xmin = 0, xmax = Inf,
      ymin = 0, ymax = Inf,
      fill = STORY_COLS["wetter_accelerated"],
      alpha = 0.09
    ) +
    annotate(
      "rect",
      xmin = -Inf, xmax = 0,
      ymin = -Inf, ymax = 0,
      fill = STORY_COLS["drier_decelerated"],
      alpha = 0.09
    ) +
    annotate(
      "rect",
      xmin = 0, xmax = Inf,
      ymin = -Inf, ymax = 0,
      fill = STORY_COLS["wetter_decelerated"],
      alpha = 0.09
    ) +
    
    # Zero axes ----------------------------------------------------------------
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.35,
    colour = "grey35"
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.35,
      colour = "grey35"
    ) +
    
    # Driver diagonals ----------------------------------------------------------
  geom_abline(
    slope = -0.5,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.35,
    colour = "grey30"
  ) +
    geom_abline(
      slope = 0.5,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.35,
      colour = "grey30"
    ) +
    
    # Scenario points -----------------------------------------------------------
  geom_point(
    data = dt_page[type == "Scenario"],
    aes(
      x = avail_change,
      y = flux_change,
      fill = id
    ),
    shape = 21,
    size = 2.8,
    colour = "black",
    stroke = 0.35,
    alpha = 0.95
  ) +
    
    # Dataset points ------------------------------------------------------------
  geom_point(
    data = dt_page[type == "Dataset"],
    aes(
      x = avail_change,
      y = flux_change,
      colour = id
    ),
    size = 3.2,
    shape = 16,
    alpha = 0.95
  ) +
    geom_point(
      data = dt_page[type == "Dataset"],
      aes(
        x = avail_change,
        y = flux_change
      ),
      size = 3.2,
      shape = 1,
      colour = "black",
      stroke = 0.45
    ) +
    
    # Unweighted dataset mean ---------------------------------------------------
  geom_point(
    data = dt_page[type == "Unweighted mean"],
    aes(
      x = avail_change,
      y = flux_change
    ),
    shape = 23,
    size = 4.2,
    fill = "white",
    colour = "black",
    stroke = 1.0
  ) +
    
    # Dataset labels ------------------------------------------------------------
  geom_text_repel(
    data = dt_page[type == "Dataset"],
    aes(
      x = avail_change,
      y = flux_change,
      label = label,
      colour = id
    ),
    size = 2.7,
    fontface = "bold",
    show.legend = FALSE,
    box.padding = 0.28,
    point.padding = 0.18,
    max.overlaps = Inf,
    segment.colour = "grey60",
    segment.linewidth = 0.20,
    seed = 42
  ) +
    
    # Mean label ----------------------------------------------------------------
  geom_text_repel(
    data = dt_page[type == "Unweighted mean"],
    aes(
      x = avail_change,
      y = flux_change,
      label = label
    ),
    colour = "black",
    size = 2.7,
    fontface = "bold",
    show.legend = FALSE,
    box.padding = 0.32,
    point.padding = 0.22,
    max.overlaps = Inf,
    segment.colour = "black",
    segment.linewidth = 0.20,
    seed = 42
  ) +
    
    facet_wrap(
      ~ region,
      ncol = N_COLS,
      nrow = N_ROWS,
      scales = "free"
    ) +
    
    scale_colour_manual(
      name = "Dataset",
      values = dataset_cols,
      breaks = names(dataset_cols),
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
      labels = scales::comma,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = expression(Delta((P + E) / 2) ~ "[mm" ~ yr^{-1} * "]"),
      labels = scales::comma,
      expand = c(0, 0)
    ) +
    
    labs(
      title = paste0(
        "Regional water-cycle change in availability and flux space, page ",
        page_id
      ),
      subtitle = paste0(
        "Changes are ",
        PERIOD_2[1], "-", PERIOD_2[2],
        " minus ",
        PERIOD_1[1], "-", PERIOD_1[2]
      ),
      caption = paste0(
        "Circles: original datasets. Filled circles: weighting scenario means. ",
        "Diamond: unweighted dataset mean. ",
        "Dashed diagonals mark ΔP = 0 and ΔE = 0. ",
        "Axes vary by region and are symmetric around zero."
      )
    ) +
    
    guides(
      colour = guide_legend(
        order = 1,
        nrow = 1,
        override.aes = list(size = 3.4, alpha = 1)
      ),
      fill = guide_legend(
        order = 2,
        nrow = 2,
        override.aes = list(size = 3.4, alpha = 1)
      )
    ) +
    
    theme_bw(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey93"),
      legend.position = "bottom",
      legend.box = "vertical",
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(
        size = 7.5,
        colour = "grey45",
        hjust = 0,
        margin = margin(t = 5)
      ),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      plot.margin = margin(6, 12, 6, 6)
    )
}


# Analysis ====================================================================

setDT(scenario_region_yearly)
setDT(dataset_region_yearly)

# Unweighted dataset mean, estimated only from dataset_region_yearly ------------

dataset_region_mean_yearly <- dataset_region_yearly[
  ,
  .(
    prec = mean(prec, na.rm = TRUE),
    evap = mean(evap, na.rm = TRUE),
    n_datasets = uniqueN(dataset)
  ),
  by = .(region, year)
]

dataset_region_mean_yearly[
  ,
  series := "Mean"
]

# Scenario changes -------------------------------------------------------------

scenario_change <- period_change(
  scenario_region_yearly[
    scenario %in% names(SCENARIO_LABELS) &
      !is.na(region)
  ],
  id_cols = c("region", "scenario")
)

scenario_change[
  ,
  `:=`(
    type = "Scenario",
    id = scenario,
    label = SCENARIO_LABELS[scenario]
  )
]

# Dataset changes --------------------------------------------------------------

dataset_change <- period_change(
  dataset_region_yearly[
    !is.na(region) &
      !is.na(dataset)
  ],
  id_cols = c("region", "dataset")
)

dataset_change[
  ,
  `:=`(
    type = "Dataset",
    id = dataset,
    label = dataset
  )
]

# Unweighted dataset mean changes ---------------------------------------------

dataset_mean_change <- period_change(
  dataset_region_mean_yearly[
    !is.na(region)
  ],
  id_cols = c("region", "series")
)

dataset_mean_change[
  ,
  `:=`(
    type = "Unweighted mean",
    id = "unweighted_mean",
    label = "Mean"
  )
]

# Combined plot table ----------------------------------------------------------

plot_dt <- rbindlist(
  list(
    scenario_change[
      ,
      .(
        region,
        id,
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
        region,
        id,
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
        region,
        id,
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

plot_dt <- plot_dt[
  is.finite(avail_change) &
    is.finite(flux_change)
]

plot_dt[
  ,
  storyline := fcase(
    avail_change >= 0 & flux_change >= 0, "Wet and accelerated",
    avail_change <  0 & flux_change >= 0, "Dry and accelerated",
    avail_change >= 0 & flux_change <  0, "Wet and decelerated",
    avail_change <  0 & flux_change <  0, "Dry and decelerated"
  )
]

region_levels <- sort(unique(plot_dt$region))

plot_dt[
  ,
  region := factor(region, levels = region_levels)
]

dataset_ids <- sort(unique(plot_dt[type == "Dataset", id]))

DATASET_COLS <- make_dataset_colours(dataset_ids)

region_pages <- split(
  region_levels,
  ceiling(seq_along(region_levels) / REGIONS_PER_PAGE)
)


# Outputs =====================================================================

saveRDS(
  dataset_region_mean_yearly,
  file.path(PATH_OUTPUT_DATA, "dataset_region_unweighted_mean_yearly_prec_evap.Rds")
)

saveRDS(
  plot_dt,
  file.path(PATH_OUTPUT_DATA, "regional_avail_flux_change_plot_dt.Rds")
)

plots <- vector("list", length(region_pages))

for (i in seq_along(region_pages)) {
  page_regions <- region_pages[[i]]
  dt_page <- plot_dt[region %in% page_regions]
  
  p_page <- make_region_page_plot(
    dt_page = dt_page,
    page_id = i,
    dataset_cols = DATASET_COLS
  )
  
  plots[[i]] <- p_page
  
  print(p_page)
  
  if (interactive() && i < length(region_pages)) {
    readline(prompt = "Press [Enter] to show next page...")
  }
}


# Validation ==================================================================

message(sprintf(
  "Prepared %d regional pages with up to %d regions per page.",
  length(region_pages),
  REGIONS_PER_PAGE
))

print(
  plot_dt[
    ,
    .(
      n_points = .N,
      n_scenarios = uniqueN(id[type == "Scenario"]),
      n_datasets = uniqueN(id[type == "Dataset"]),
      has_mean = any(type == "Unweighted mean")
    ),
    by = region
  ][order(region)]
)

print(
  plot_dt[
    ,
    .N,
    by = .(type, storyline)
  ][order(type, storyline)]
)