# ============================================================================
# Shared helpers for the figure scripts 06a-06f
#
# Sourced by 06a-06f right after _source.R. It holds only what several figure
# scripts need: the fixed dataset colours, the weighting-scenario set and its
# display order, the shared map frame (extent, coastlines, theme) and the save
# helper. It reads and writes no pipeline data.
# ============================================================================

# Libraries ==================================================================

library(patchwork)   # layout; gtable (a ggplot2 dependency) is used for legend extraction
source("code/_figs.R") # shared palettes; legacy helpers below retain their API

# Datasets ===================================================================

# The five coherent P-E candidates, in the order used in every legend.
CANDIDATES <- c("ERA5L", "FLDAS", "GLEAM", "MERRA", "TERRA")
stopifnot(setequal(CANDIDATES, EVAP_NAMES_SHORT))

# One fixed mapping from the main colset_mid palette across manuscript figures.
DATASET_COLS <- PAL_DATASETS

# Names used in legends and labels (the pairs, as in the manuscript text).
DATASET_LABELS <- c(
  ERA5L = "ERA5-Land", FLDAS = "FLDAS", GLEAM = "MSWEP-GLEAM",
  MERRA = "MERRA-2", TERRA = "TerraClimate"
)

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

# Coastlines for the publication maps, simplified (Douglas-Peucker) to a tolerance
# well below the 0.25 degree grid: the vector paths are repeated in every map
# panel, and the full-resolution lines alone push multi-map figures over the
# 5 MB PDF limit of the journal profile.
COAST_SIMPLIFY_DEG <- 0.1

simplify_paths <- function(paths, tolerance) {
  parts <- split(paths[, c("long", "lat")], paths$group)
  lines <- sf::st_sfc(lapply(parts, function(p) sf::st_linestring(as.matrix(p))))
  simplified <- sf::st_simplify(lines, dTolerance = tolerance)
  xy <- sf::st_coordinates(simplified[!sf::st_is_empty(simplified)])
  data.frame(long = xy[, "X"], lat = xy[, "Y"], group = xy[, "L1"])
}

COASTLINES_PUB <- if (requireNamespace("sf", quietly = TRUE)) {
  simplify_paths(COASTLINES, COAST_SIMPLIFY_DEG)
} else {
  COASTLINES
}

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

# Global raster map in the publication style: shared frame and coastlines,
# colours from the fill scale. Coordinate labels can be dropped for small
# multiples (Copernicus does not require them; state the extent in the caption).
publication_map <- function(dt, fill, fill_scale, legend_position = "right",
                            coord_labels = TRUE) {
  ggplot(dt, aes(x = lon, y = lat, fill = .data[[fill]])) +
    geom_raster() +
    geom_path(
      data = COASTLINES_PUB,
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
    theme_pub_map(legend_position = legend_position, coord_labels = coord_labels)
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

# A plot as a gtable, built on a ragg device (the one the PNG export uses):
# Rscript's default PDF device does not know the system fonts (Arial), measures
# the text with a fallback font and warns for every label.
build_grob <- function(plot) {
  ragg::agg_png(tempfile(fileext = ".png"), width = 100, height = 100)
  on.exit(grDevices::dev.off(), add = TRUE)

  quiet_raster_gaps(ggplotGrob(plot))
}

# A whole plot as a layout element: it keeps its own axes and labels and is not
# aligned panel-to-panel with its neighbours (use it where a panel in the same
# column would otherwise inherit the margins of another panel's axis labels).
plot_element <- function(plot) {
  wrap_elements(full = build_grob(plot))
}

# The legend of a plot as a layout element (taken once, so it is not repeated).
legend_element <- function(plot) {
  built <- build_grob(plot + theme(legend.position = "bottom"))

  wrap_elements(full = gtable::gtable_filter(built, "guide-box-bottom", trim = TRUE))
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
