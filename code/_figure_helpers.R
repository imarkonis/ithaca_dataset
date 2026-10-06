# ============================================================================
# Shared helpers for the figure scripts 06a-06d
#
# Sourced by 06a-06d right after _source.R. It holds only what several figure
# scripts need: the fixed dataset colours, the weighting-scenario set and its
# display order, the shared map frame (extent, coastlines, theme) and the save
# helper. It reads and writes no pipeline data.
# ============================================================================

# Libraries ==================================================================

library(patchwork)   # layout; cowplot is used for legend extraction (cowplot::get_legend)
source("code/_figs.R") # shared palettes; legacy helpers below retain their API

# Datasets ===================================================================

# The five coherent P-E candidates, in the order used in every legend.
CANDIDATES <- c("ERA5L", "FLDAS", "GLEAM", "MERRA", "TERRA")
stopifnot(setequal(CANDIDATES, EVAP_NAMES_SHORT))

# One fixed mapping from the main colset_mid palette across manuscript figures.
DATASET_COLS <- PAL_DATASETS

# Weighting scenarios ========================================================

# The eight scenarios of the sensitivity analysis (labelled in 07b), ordered by
# kind: the four share scenarios, the two rank foils, neutral, inverted.
# `change` and `trend_dominant` are also written by 03e/04a but are not part of
# the eight.
SCENARIO_ORDER <- c(
  "base", "clim_dominant", "prec_dominant", "evap_dominant",
  "rank_linear", "rank_exp",
  "neutral",
  "inverted"
)

SCENARIO_LABELS <- c(
  base          = "Base",
  clim_dominant = "Climate dominated",
  prec_dominant = "Precipitation dominated",
  evap_dominant = "Evaporation dominated",
  rank_linear   = "Rank linear",
  rank_exp      = "Rank exponential",
  neutral       = "Neutral",
  inverted      = "Disagreement"
)

# Map frame ==================================================================

# Same extent, coastlines and projection for every map in every figure.
MAP_XLIM <- c(-180, 180)
MAP_YLIM <- c(-58, 84)

# Height / width of a map panel under coord_quickmap, for panels that must line
# up with maps in the same row.
MAP_ASPECT <- diff(MAP_YLIM) / (diff(MAP_XLIM) * cos(mean(MAP_YLIM) * pi / 180))

# Cells that belong to the mask but have no value, and units without data.
COL_NO_DATA <- "grey93"
COL_TIE     <- "grey55"

COASTLINES <- ggplot2::map_data("world")

# Functions ==================================================================

coastline_layer <- function() {
  geom_path(
    data = COASTLINES,
    aes(x = long, y = lat, group = group),
    inherit.aes = FALSE,
    colour = "grey25",
    linewidth = 0.1
  )
}

theme_figure_map <- function(base_size = 8) {
  theme_void(base_size = base_size) +
    theme(
      plot.title = element_text(
        size = base_size, face = "bold", hjust = 0, margin = margin(b = 2)
      ),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1),
      panel.border = element_rect(colour = "grey60", fill = NA, linewidth = 0.3),
      plot.margin = margin(2, 2, 2, 2)
    )
}

# One map of grid cells (`dt` has lon, lat and the column named in `fill`).
cell_map <- function(dt, fill, fill_scale, title = NULL) {
  ggplot(dt, aes(x = lon, y = lat, fill = .data[[fill]])) +
    geom_raster() +
    coastline_layer() +
    fill_scale +
    coord_quickmap(xlim = MAP_XLIM, ylim = MAP_YLIM, expand = FALSE) +
    labs(title = title) +
    theme_figure_map()
}

# Label placed in the empty south-eastern Pacific corner of a map.
map_label <- function(label, size = 2.8) {
  annotate(
    "text",
    x = MAP_XLIM[1] + 3, y = MAP_YLIM[1] + 4,
    label = label, hjust = 0, vjust = 0,
    size = size, fontface = "bold"
  )
}

weighted_mean_safe <- function(value, weight) {
  ok <- is.finite(value) & is.finite(weight) & weight > 0

  if (!any(ok)) {
    return(NA_real_)
  }

  sum(value[ok] * weight[ok]) / sum(weight[ok])
}

# Area (summed cell_weight) of every region x biome unit, and the same split by
# hemisphere. Cells without region or biome are not part of any unit.
unit_area <- function(grid_classes) {
  grid_classes[
    !is.na(region) & !is.na(biome),
    .(area = sum(cell_weight)),
    by = .(region, biome)
  ]
}

unit_area_hemisphere <- function(grid_classes) {
  grid_classes[
    !is.na(region) & !is.na(biome),
    .(area = sum(cell_weight)),
    by = .(region, biome, hemisphere = as.character(hemisphere))
  ]
}

# The land mask has ocean-only longitude columns, so geom_raster reports an
# "uneven" grid although every cell sits on the regular 0.25 degree grid and is
# drawn correctly. Only that message is muffled.
quiet_raster_gaps <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("uneven horizontal intervals", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# The legend of a plot as a layout element (taken once, so it is not repeated).
legend_element <- function(plot) {
  wrap_elements(
    full = quiet_raster_gaps(
      cowplot::get_legend(plot + theme(legend.position = "bottom"))
    )
  )
}

save_figure <- function(plot, name, width, height, dpi = 300) {
  dir.create(PATH_OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

  png_file <- file.path(PATH_OUTPUT_FIGURES, paste0(name, ".png"))
  quiet_raster_gaps(
    ggsave(png_file, plot, width = width, height = height, units = "in",
           dpi = dpi, bg = "white")
  )

  # Vector copy for the manuscript; skipped where cairo is not available.
  if (isTRUE(capabilities("cairo"))) {
    pdf_file <- file.path(PATH_OUTPUT_FIGURES, paste0(name, ".pdf"))
    quiet_raster_gaps(
      ggsave(pdf_file, plot, width = width, height = height, units = "in",
             device = grDevices::cairo_pdf, bg = "white")
    )
  }

  invisible(png_file)
}
