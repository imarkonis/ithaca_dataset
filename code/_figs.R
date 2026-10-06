# =============================================================================
# _figs.R
# Shared publication-figure design system
#
# Source this file in EVERY publication figure script.
# Keep reusable cosmetic constants here, not in individual figure scripts.
# Scientific choices (filtering, baselines, thresholds, projections, limits
# that affect interpretation, etc.) belong in the figure script.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Publication dimensions and export
# -----------------------------------------------------------------------------

FIG_WIDTH_SINGLE <- 89       # mm
FIG_WIDTH_DOUBLE <- 183      # mm
FIG_HEIGHT_MAX   <- 170      # mm
FIG_DPI          <- 300
FIG_BG           <- "white"

# -----------------------------------------------------------------------------
# 2. Typography
# -----------------------------------------------------------------------------

# Arial is the preferred publication font when available. "sans" is a portable
# fallback; override FIG_FONT in a project-specific copy only when necessary.
FIG_FONT <- "sans"

FIG_BASE_SIZE       <- 8
FIG_AXIS_TITLE_SIZE <- 8
FIG_AXIS_TEXT_SIZE  <- 7
FIG_LEGEND_TITLE_SIZE <- 7.5
FIG_LEGEND_TEXT_SIZE  <- 7
FIG_STRIP_SIZE      <- 8
FIG_TAG_SIZE        <- 9
FIG_ANNOTATION_SIZE <- 7

# -----------------------------------------------------------------------------
# 3. Neutral colours
# -----------------------------------------------------------------------------

COL_TEXT        <- "#222222"
COL_AXIS        <- "#333333"
COL_REFERENCE   <- "#6F6F6F"
COL_CONTEXT     <- "#A6A6A6"
COL_CONTEXT_LT  <- "#D0D0D0"
COL_GRID_MAJOR  <- "#DEDEDE"
COL_MISSING     <- "#D9D9D9"
COL_FACET_BG    <- "#F2F2F2"
COL_WHITE       <- "#FFFFFF"

# -----------------------------------------------------------------------------
# 4. Semantic colours
# -----------------------------------------------------------------------------

# Stable manuscript-wide semantics. Use these names in figure scripts.
COL_PRECIP    <- "#0072B2"   # blue
COL_EVAP      <- "#D55E00"   # vermillion/orange
COL_BALANCE   <- "#009E73"   # bluish green
COL_HIGHLIGHT <- "#CC79A7"   # reddish purple

# -----------------------------------------------------------------------------
# 5. Categorical palettes
# -----------------------------------------------------------------------------

# Okabe-Ito colour-blind-aware palette.
# Use named semantic palettes when categories recur across figures.
# If >8 important categories are needed, change the encoding rather than
# inventing additional arbitrary colours.
PAL_CAT_8 <- c(
  "#0072B2", # blue
  "#E69F00", # orange
  "#009E73", # bluish green
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#56B4E9", # sky blue
  "#F0E442", # yellow
  "#333333"  # dark neutral
)

PAL_CAT_6 <- PAL_CAT_8[1:6]

# ITHACA: preserve the established five-source mapping across manuscript figures.
PAL_ITHACA_DATASETS <- c(
  ERA5L = "#1b9e77", FLDAS = "#d95f02", GLEAM = "#7570b3",
  MERRA = "#e7298a", TERRA = "#66a61e"
)
FIG_LINEWIDTH_MEMBER <- 0.18
FIG_ALPHA_MEMBER <- 0.18

theme_pub_titled <- function(...) {
  theme_pub(...) + ggplot2::theme(
    plot.title = ggplot2::element_text(size = FIG_BASE_SIZE, face = "bold"),
    plot.subtitle = ggplot2::element_text(size = FIG_ANNOTATION_SIZE),
    legend.key.height = grid::unit(FIG_PANEL_SPACING_MM, "mm"),
    legend.key.width = grid::unit(FIG_PANEL_SPACING_MM * 2, "mm")
  )
}

# Example reusable named semantic palette.
PAL_WATER_COMPONENTS <- c(
  "P"   = COL_PRECIP,
  "E"   = COL_EVAP,
  "P-E" = COL_BALANCE
)

# -----------------------------------------------------------------------------
# 6. Sequential palettes
# -----------------------------------------------------------------------------

# Generic perceptually ordered scientific gradient.
# Fixed stops avoid hidden palette changes across package versions.
PAL_SEQ_VIRIDIS <- c(
  "#440154",
  "#46327E",
  "#365C8D",
  "#277F8E",
  "#1FA187",
  "#4AC16D",
  "#A0DA39",
  "#FDE725"
)

# Water / precipitation: light -> dark blue.
PAL_SEQ_BLUE <- c(
  "#F7FBFF",
  "#DEEBF7",
  "#C6DBEF",
  "#9ECAE1",
  "#6BAED6",
  "#4292C6",
  "#2171B5",
  "#08519C",
  "#08306B"
)

# Evaporation / atmospheric loss: light -> dark orange.
PAL_SEQ_ORANGE <- c(
  "#FFF5EB",
  "#FEE6CE",
  "#FDD0A2",
  "#FDAE6B",
  "#FD8D3C",
  "#F16913",
  "#D94801",
  "#A63603",
  "#7F2704"
)

PAL_SEQ_GREY <- c(
  "#F7F7F7",
  "#D9D9D9",
  "#BDBDBD",
  "#969696",
  "#737373",
  "#525252",
  "#252525"
)

# -----------------------------------------------------------------------------
# 7. Diverging palette
# -----------------------------------------------------------------------------

# Blue -> near-white -> red.
# The midpoint itself is a SCIENTIFIC parameter supplied by the figure script.
PAL_DIV_BLUE_RED <- c(
  "#2166AC",
  "#67A9CF",
  "#D1E5F0",
  "#F7F7F7",
  "#FDDBC7",
  "#EF8A62",
  "#B2182B"
)

COL_DIV_LOW  <- PAL_DIV_BLUE_RED[1]
COL_DIV_MID  <- PAL_DIV_BLUE_RED[4]
COL_DIV_HIGH <- PAL_DIV_BLUE_RED[7]

# -----------------------------------------------------------------------------
# 8. Lines, points, uncertainty, and spacing
# -----------------------------------------------------------------------------

FIG_LINEWIDTH        <- 0.45
FIG_LINEWIDTH_EMPH   <- 0.75
FIG_LINEWIDTH_REF    <- 0.35
FIG_AXIS_LINEWIDTH   <- 0.35
FIG_GRID_LINEWIDTH   <- 0.25
FIG_BOUNDARY_LINEWIDTH <- 0.25

FIG_POINT_SIZE       <- 1.8
FIG_POINT_SIZE_SMALL <- 1.2
FIG_POINT_STROKE     <- 0.25

FIG_ALPHA_DATA       <- 0.90
FIG_ALPHA_CONTEXT    <- 0.45
FIG_ALPHA_RIBBON     <- 0.20
FIG_ALPHA_REFERENCE  <- 0.70

FIG_ERRORBAR_WIDTH   <- 0.16

FIG_TICK_LENGTH_MM   <- 1.5
FIG_PANEL_SPACING_MM <- 2.5

FIG_MARGIN_TOP_PT    <- 4
FIG_MARGIN_RIGHT_PT  <- 5
FIG_MARGIN_BOTTOM_PT <- 4
FIG_MARGIN_LEFT_PT   <- 5

# -----------------------------------------------------------------------------
# 9. Shared line types and point shapes
# -----------------------------------------------------------------------------

FIG_LINETYPES <- c(
  "solid",
  "dashed",
  "dotted",
  "dotdash"
)

# Filled shapes work well with colour + fill and remain legible at small sizes.
FIG_SHAPES <- c(21, 22, 24, 23, 25, 16, 17, 15)

# -----------------------------------------------------------------------------
# 10. Publication theme
# -----------------------------------------------------------------------------

theme_pub <- function(
  base_size = FIG_BASE_SIZE,
  base_family = FIG_FONT,
  grid = FALSE,
  legend_position = "right"
) {
  th <- ggplot2::theme_classic(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        colour = COL_TEXT,
        family = base_family
      ),

      axis.title = ggplot2::element_text(
        size = FIG_AXIS_TITLE_SIZE,
        colour = COL_TEXT
      ),
      axis.text = ggplot2::element_text(
        size = FIG_AXIS_TEXT_SIZE,
        colour = COL_TEXT
      ),
      axis.line = ggplot2::element_line(
        colour = COL_AXIS,
        linewidth = FIG_AXIS_LINEWIDTH
      ),
      axis.ticks = ggplot2::element_line(
        colour = COL_AXIS,
        linewidth = FIG_AXIS_LINEWIDTH
      ),
      axis.ticks.length = grid::unit(FIG_TICK_LENGTH_MM, "mm"),

      legend.position = legend_position,
      legend.title = ggplot2::element_text(
        size = FIG_LEGEND_TITLE_SIZE,
        colour = COL_TEXT
      ),
      legend.text = ggplot2::element_text(
        size = FIG_LEGEND_TEXT_SIZE,
        colour = COL_TEXT
      ),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),

      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = FIG_STRIP_SIZE,
        colour = COL_TEXT
      ),

      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(
        size = FIG_ANNOTATION_SIZE,
        colour = COL_REFERENCE,
        hjust = 0
      ),
      plot.tag = ggplot2::element_text(
        size = FIG_TAG_SIZE,
        face = "bold",
        colour = COL_TEXT
      ),

      panel.spacing = grid::unit(FIG_PANEL_SPACING_MM, "mm"),
      plot.margin = ggplot2::margin(
        FIG_MARGIN_TOP_PT,
        FIG_MARGIN_RIGHT_PT,
        FIG_MARGIN_BOTTOM_PT,
        FIG_MARGIN_LEFT_PT,
        unit = "pt"
      )
    )

  if (isTRUE(grid)) {
    th <- th +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          colour = COL_GRID_MAJOR,
          linewidth = FIG_GRID_LINEWIDTH
        ),
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  th
}

# A map variant removes axes without altering map projection/data.
theme_pub_map <- function(
  base_size = FIG_BASE_SIZE,
  base_family = FIG_FONT,
  legend_position = "right"
) {
  theme_pub(
    base_size = base_size,
    base_family = base_family,
    grid = FALSE,
    legend_position = legend_position
  ) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank()
    )
}

# -----------------------------------------------------------------------------
# 11. Shared categorical scales
# -----------------------------------------------------------------------------

scale_colour_cat <- function(..., values = PAL_CAT_8, na.value = COL_MISSING) {
  ggplot2::scale_colour_manual(
    ...,
    values = values,
    na.value = na.value
  )
}

scale_color_cat <- scale_colour_cat

scale_fill_cat <- function(..., values = PAL_CAT_8, na.value = COL_MISSING) {
  ggplot2::scale_fill_manual(
    ...,
    values = values,
    na.value = na.value
  )
}

# -----------------------------------------------------------------------------
# 12. Shared sequential scales
# -----------------------------------------------------------------------------

scale_colour_seq <- function(..., palette = PAL_SEQ_VIRIDIS, na.value = COL_MISSING) {
  ggplot2::scale_colour_gradientn(
    ...,
    colours = palette,
    na.value = na.value
  )
}

scale_color_seq <- scale_colour_seq

scale_fill_seq <- function(..., palette = PAL_SEQ_VIRIDIS, na.value = COL_MISSING) {
  ggplot2::scale_fill_gradientn(
    ...,
    colours = palette,
    na.value = na.value
  )
}

scale_colour_precip <- function(..., na.value = COL_MISSING) {
  scale_colour_seq(..., palette = PAL_SEQ_BLUE, na.value = na.value)
}

scale_fill_precip <- function(..., na.value = COL_MISSING) {
  scale_fill_seq(..., palette = PAL_SEQ_BLUE, na.value = na.value)
}

scale_colour_evap <- function(..., na.value = COL_MISSING) {
  scale_colour_seq(..., palette = PAL_SEQ_ORANGE, na.value = na.value)
}

scale_fill_evap <- function(..., na.value = COL_MISSING) {
  scale_fill_seq(..., palette = PAL_SEQ_ORANGE, na.value = na.value)
}

# -----------------------------------------------------------------------------
# 13. Shared diverging scales
# -----------------------------------------------------------------------------

# Midpoint and limits are scientific parameters and must be supplied by the
# figure script when scientifically relevant. Colours are cosmetic and shared.
scale_colour_div <- function(
  ...,
  midpoint = 0,
  low = COL_DIV_LOW,
  mid = COL_DIV_MID,
  high = COL_DIV_HIGH,
  na.value = COL_MISSING
) {
  ggplot2::scale_colour_gradient2(
    ...,
    low = low,
    mid = mid,
    high = high,
    midpoint = midpoint,
    na.value = na.value
  )
}

scale_color_div <- scale_colour_div

scale_fill_div <- function(
  ...,
  midpoint = 0,
  low = COL_DIV_LOW,
  mid = COL_DIV_MID,
  high = COL_DIV_HIGH,
  na.value = COL_MISSING
) {
  ggplot2::scale_fill_gradient2(
    ...,
    low = low,
    mid = mid,
    high = high,
    midpoint = midpoint,
    na.value = na.value
  )
}

# -----------------------------------------------------------------------------
# 14. Panel tagging for patchwork
# -----------------------------------------------------------------------------

add_panel_tags <- function(plot, levels = "a") {
  plot +
    patchwork::plot_annotation(
      tag_levels = levels,
      tag_prefix = "(",
      tag_suffix = ")",
      theme = ggplot2::theme(
        plot.tag = ggplot2::element_text(
          family = FIG_FONT,
          size = FIG_TAG_SIZE,
          face = "bold",
          colour = COL_TEXT
        )
      )
    )
}

# -----------------------------------------------------------------------------
# 15. Reference-line helpers
# -----------------------------------------------------------------------------

geom_hline_ref <- function(yintercept = 0, ...) {
  ggplot2::geom_hline(
    yintercept = yintercept,
    colour = COL_REFERENCE,
    linewidth = FIG_LINEWIDTH_REF,
    linetype = "solid",
    ...
  )
}

geom_vline_ref <- function(xintercept = 0, ...) {
  ggplot2::geom_vline(
    xintercept = xintercept,
    colour = COL_REFERENCE,
    linewidth = FIG_LINEWIDTH_REF,
    linetype = "solid",
    ...
  )
}

# -----------------------------------------------------------------------------
# 16. Export helper
# -----------------------------------------------------------------------------

# file_stem should NOT include an extension.
# height_mm is deliberately required: height is figure-specific and should not
# be silently inferred from the active graphics device.
save_figure <- function(
  plot,
  file_stem,
  width_mm = FIG_WIDTH_DOUBLE,
  height_mm,
  save_png = TRUE,
  dpi = FIG_DPI,
  bg = FIG_BG
) {
  if (missing(height_mm)) {
    stop("`height_mm` must be supplied explicitly.")
  }

  if (width_mm <= 0 || height_mm <= 0) {
    stop("Figure width and height must be positive.")
  }

  pdf_file <- paste0(file_stem, ".pdf")

  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"

  ggplot2::ggsave(
    filename = pdf_file,
    plot = plot,
    device = pdf_device,
    width = width_mm,
    height = height_mm,
    units = "mm",
    bg = bg
  )

  if (isTRUE(save_png)) {
    png_file <- paste0(file_stem, ".png")

    ggplot2::ggsave(
      filename = png_file,
      plot = plot,
      width = width_mm,
      height = height_mm,
      units = "mm",
      dpi = dpi,
      bg = bg
    )
  }

  invisible(pdf_file)
}

# =============================================================================
# End of _figs.R
# =============================================================================
