# =============================================================================
# _figs.R -- shared publication-figure design system (COSMETICS ONLY)
#
# Source at the top of every figure script:
#   source(here::here("figures", "_figs.R"))
#
# This file decides how figures LOOK. It never decides what they SHOW:
# filtering, baselines, transformations, colour limits and midpoints, bin
# breaks, projections and significance criteria stay in the figure script
# (docs/SCIENTIFIC_FIGURE_WORKFLOW.md). Rationale for everything here:
# docs/SCIENTIFIC_FIGURE_STYLE.md.
#
# Requires: ggplot2, scales, systemfonts, ragg, scico, patchwork
# Optional: svglite (SVG export), pdftools (check_fonts())
# Tested:   R 4.3.3, ggplot2 3.4.4, scales 1.3.0, patchwork 1.2.0,
#           ragg 1.2.7, scico 1.5; avoids ggplot2 features newer than 3.4.
# Keep this file ASCII-only: write non-ASCII characters as \u escapes.
# =============================================================================

local({
  need <- c("ggplot2", "scales", "systemfonts", "ragg", "scico", "patchwork")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0) {
    stop("_figs.R needs these packages: ", paste(miss, collapse = ", "), call. = FALSE)
  }
})

# -----------------------------------------------------------------------------
# 0. Journal profile
# -----------------------------------------------------------------------------
# Project default below. To target another journal from one script, set
#   options(figs.profile = "nature")
# BEFORE sourcing this file. Choosing the journal is an editorial decision;
# everything the profile changes is cosmetic.

FIG_PROFILE <- getOption("figs.profile", "copernicus")

.fig_generic <- list(
  base = 8, axis_text = 7, legend_title = 7.5, legend_text = 7, strip = 8,
  tag = 9, annotation = 7, min_text = 6,          # font sizes in pt
  tag_prefix = "(", tag_suffix = ")",            # panel tags "(a)"
  width_single = 89, width_mid = 120, width_double = 183, height_max = 170,
  min_width = NA, raster_dpi = 300, max_file_mb = NA,
  deg_sep = ""                                    # "30\u00B0N"
)

FIG_PROFILES <- list(
  # Earth-science default when no journal has been chosen.
  generic = .fig_generic,
  # Copernicus/EGU (ESSD, HESS, GMD, ...): "(a)" panel labels, coordinates
  # written "30\u00B0 N", width >= 80 mm, 300 dpi, <= 5 MB per figure.
  copernicus = modifyList(.fig_generic, list(deg_sep = " ", min_width = 80, max_file_mb = 5)),
  # Nature portfolio: all text 5-7 pt, panel labels 8 pt bold without
  # brackets, widths 89/120 (or 136)/183 mm, height <= 170 mm.
  nature = modifyList(.fig_generic, list(
    base = 7, axis_text = 6, legend_title = 7, legend_text = 6, strip = 7,
    tag = 8, annotation = 6, min_text = 5, tag_prefix = "", tag_suffix = "")),
  # AGU (GRL, WRR, JGR): lower-case panel letters; no fixed widths published.
  agu = .fig_generic,
  # Elsevier: 90/140/190 mm; raster line art at 1000 dpi.
  elsevier = modifyList(.fig_generic, list(
    width_single = 90, width_mid = 140, width_double = 190, raster_dpi = 1000))
)

if (!FIG_PROFILE %in% names(FIG_PROFILES)) {
  stop("Unknown figs.profile '", FIG_PROFILE, "'. Use one of: ",
       paste(names(FIG_PROFILES), collapse = ", "), call. = FALSE)
}
FIG_SPEC <- FIG_PROFILES[[FIG_PROFILE]]

# -----------------------------------------------------------------------------
# 1. Units: points -> ggplot2 units
# -----------------------------------------------------------------------------
# ggplot2's linewidth unit is ~0.75 mm (not 1 mm, not 1 pt) and geom_text()
# size is in mm, while theme() text sizes are in pt. Always convert.

pt_to_lw   <- function(pt) pt / (ggplot2::.pt * 72 / 96)   # stroke width (pt) -> linewidth
pt_to_size <- function(pt) pt / ggplot2::.pt                # font size (pt) -> geom_text size

# -----------------------------------------------------------------------------
# 2. Dimensions and export
# -----------------------------------------------------------------------------

FIG_WIDTH_SINGLE <- FIG_SPEC$width_single   # mm
FIG_WIDTH_MID    <- FIG_SPEC$width_mid      # mm, 1.5 columns
FIG_WIDTH_DOUBLE <- FIG_SPEC$width_double   # mm
FIG_HEIGHT_MAX   <- FIG_SPEC$height_max     # mm, routine maximum
FIG_DPI_PREVIEW  <- 300                     # PNG preview
FIG_DPI_RASTER   <- FIG_SPEC$raster_dpi     # TIFF when a journal wants raster
FIG_BG           <- "white"

# -----------------------------------------------------------------------------
# 3. Typography
# -----------------------------------------------------------------------------
# Journals ask for Arial/Helvetica. On Linux servers "sans" can resolve to
# DejaVu Sans or a TeX font; Liberation Sans and Arimo are metric-compatible
# Arial stand-ins.

pick_font <- function(candidates = c("Arial", "Helvetica", "Liberation Sans", "Arimo")) {
  available <- unique(systemfonts::system_fonts()$family)
  hit <- intersect(candidates, available)
  if (length(hit) == 0) {
    warning("_figs.R: no Arial-like font installed; using 'sans'. Run check_fonts() on the PDF.",
            call. = FALSE)
    return("sans")
  }
  hit[1]
}

FIG_FONT              <- pick_font()
FIG_BASE_SIZE         <- FIG_SPEC$base           # pt
FIG_AXIS_TITLE_SIZE   <- FIG_SPEC$base
FIG_AXIS_TEXT_SIZE    <- FIG_SPEC$axis_text
FIG_LEGEND_TITLE_SIZE <- FIG_SPEC$legend_title
FIG_LEGEND_TEXT_SIZE  <- FIG_SPEC$legend_text
FIG_STRIP_SIZE        <- FIG_SPEC$strip
FIG_TAG_SIZE          <- FIG_SPEC$tag
FIG_ANNOTATION_SIZE   <- FIG_SPEC$annotation     # pt, for theme text
FIG_MIN_TEXT_SIZE     <- FIG_SPEC$min_text
FIG_GEOM_TEXT_SIZE    <- pt_to_size(FIG_ANNOTATION_SIZE)  # use in geom_text()/annotate()

# Number labels with a true minus sign (U+2212) and no thousands separator
# (keeps years as 1990, not "1 990").
label_minus <- scales::label_number(style_negative = "minus", big.mark = "")

# Coordinate labels; the spacing follows the profile ("30\u00B0 N" for Copernicus).
.fig_deg <- function(x, pos, neg) {
  num  <- format(abs(x), trim = TRUE, drop0trailing = TRUE)
  hemi <- ifelse(x > 0, pos, ifelse(x < 0, neg, ""))
  out  <- paste0(num, "\u00B0", ifelse(hemi == "", "", paste0(FIG_SPEC$deg_sep, hemi)))
  out[is.na(x)] <- NA
  out
}
label_lon <- function(x) .fig_deg(x, "E", "W")
label_lat <- function(x) .fig_deg(x, "N", "S")

# -----------------------------------------------------------------------------
# 4. Neutral colours
# -----------------------------------------------------------------------------

COL_TEXT       <- "#222222"
COL_AXIS       <- "#333333"
COL_REFERENCE  <- "#6F6F6F"
COL_CONTEXT    <- "#A6A6A6"
COL_CONTEXT_LT <- "#D0D0D0"
COL_GRID_MAJOR <- "#DEDEDE"
COL_MISSING    <- "#D9D9D9"
COL_FACET_BG   <- "#F2F2F2"
COL_WHITE      <- "#FFFFFF"
COL_BLACK      <- "#000000"

# -----------------------------------------------------------------------------
# 5. Semantic colours (manuscript-wide meanings; use the names in scripts)
# -----------------------------------------------------------------------------

COL_PRECIP    <- "#0072B2"   # blue: precipitation, water input
COL_EVAP      <- "#D55E00"   # vermillion: evaporation, atmospheric loss
COL_BALANCE   <- "#009E73"   # bluish green: P - E, balance terms
COL_HIGHLIGHT <- "#CC79A7"   # reddish purple: focal element

# -----------------------------------------------------------------------------
# 6. Categorical palettes (ITHACA graphics.R; optional Okabe-Ito)
# -----------------------------------------------------------------------------
# More than 8 important categories: change the encoding (facets, direct
# labels, grey-out), do not invent colours.

PAL_OKABE_ITO <- c(
  "#0072B2", # blue
  "#E69F00", # orange
  "#009E73", # bluish green
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#56B4E9", # sky blue
  "#F0E442", # yellow (weak on white: avoid for thin lines and small points)
  "#333333"  # dark neutral
)
# Project default, copied from imarkonis/ithaca/source/graphics.R (colset_mid).
# Fixed values, not fetched over the network when a figure is run.
colset_mid <- c(
  "#4D648D", "#337BAE", "#97B8C2", "#739F3D", "#ACBD78",
  "#F4CC70", "#EBB582", "#BF9A77", "#E38B75", "#CE5A57",
  "#CA3433", "#785A46"
)
colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]

PAL_CAT_8 <- colset_mid_qual[1:8]
PAL_CAT_6 <- colset_mid_qual[1:6]
PAL_DATASETS <- setNames(colset_mid[c(3, 5, 6, 8, 10)], c("GLEAM", "TERRA", "FLDAS", "ERA5L", "MERRA"))

# Fixed display names for the five candidate datasets. Keep internal IDs in the
# data/scales; use these labels in every manuscript figure.
DATASET_LABELS <- c(
  ERA5L = "ERA5L",
  FLDAS = "FLDAS",
  GLEAM = "GLEAM",
  MERRA = "MERRA2",
  TERRA = "TERRA"
)

# Exact named biome palette from graphics.R. Its unnamed short palette includes
# an out-of-range index (13), so use the complete named colset_biome instead.
colset_biome <- c(
  "B. Forests" = "#4D648D", "Deserts" = "#EBB582", "Flooded" = "#337BAE",
  "Mangroves" = "#064470", "M. Grasslands" = "#D24136", "Mediterranean" = "#F4CC70",
  "T. Coni. Forests" = "#32520B", "T. BL Forests" = "#739F3D", "T. Grasslands" = "#785A46",
  "T/S Coni. Forests" = "#576b16", "T/S Dry BL Forests" = "#ACBD78",
  "T/S Moist BL Forests" = "#97BA23", "T/S Grasslands" = "#E38B75", "Tundra" = "#97B8C2"
)
# Explicit aliases for the coarser classes used by pRecipe's biome_short_class.
# Water takes the source palette's dark aquatic (Mangroves) colour.
PAL_BIOMES <- c(colset_biome,
  "T. Forests" = unname(colset_biome["T. BL Forests"]),
  "T/S Forests" = unname(colset_biome["T/S Moist BL Forests"]),
  "Water" = unname(colset_biome["Mangroves"]),
  "Polar" = unname(colset_biome["Tundra"])
)

PAL_WATER_COMPONENTS <- c("P" = COL_PRECIP, "E" = COL_EVAP, "P-E" = COL_BALANCE)

# Project identity palettes: one fixed colour per dataset/scenario, defined
# once here and used by name in every figure. Fill in per project, e.g.
# PAL_DATASETS  <- c(ERA5 = "#0072B2", GLEAM = "#D55E00", FLUXCOM = "#009E73")
# PAL_SCENARIOS <- c(historical = "#333333", ssp245 = "#E69F00", ssp585 = "#D55E00")

# -----------------------------------------------------------------------------
# 7. Continuous palettes (scico: Crameri's perceptually uniform maps)
# -----------------------------------------------------------------------------
# begin/end trim the extremes (no pure black; light end stays distinct from
# COL_MISSING). Diverging directions encode semantics: below the midpoint is
# drier (water) or colder (temp).

PAL_CONT <- list(
  seq_default = list(palette = "batlow",  direction =  1, begin = 0.00, end = 1.00),
  seq_water   = list(palette = "oslo",    direction = -1, begin = 0.15, end = 0.95),
  seq_evap    = list(palette = "lajolla", direction = -1, begin = 0.15, end = 1.00),
  seq_grey    = list(palette = "grayC",   direction = -1, begin = 0.10, end = 0.95),
  div_change  = list(palette = "vik",     direction =  1, begin = 0.00, end = 1.00),
  div_water   = list(palette = "broc",    direction = -1, begin = 0.00, end = 1.00),
  div_temp    = list(palette = "vik",     direction =  1, begin = 0.00, end = 1.00),
  cyc_time    = list(palette = "romaO",   direction =  1, begin = 0.00, end = 1.00)
)

fig_colours <- function(name, n = 256, reverse = FALSE) {
  s <- PAL_CONT[[name]]
  if (is.null(s)) stop("Unknown palette '", name, "'.", call. = FALSE)
  cols <- scico::scico(n, palette = s$palette, direction = s$direction,
                       begin = s$begin, end = s$end)
  if (isTRUE(reverse)) rev(cols) else cols
}

# -----------------------------------------------------------------------------
# 8. Lines, points, uncertainty and spacing (stroke widths set in pt)
# -----------------------------------------------------------------------------

FIG_LINEWIDTH          <- pt_to_lw(0.6)    # data lines
FIG_LINEWIDTH_EMPH     <- pt_to_lw(1.0)    # emphasised series
FIG_LINEWIDTH_REF      <- pt_to_lw(0.4)    # zero/reference lines
FIG_AXIS_LINEWIDTH     <- pt_to_lw(0.35)   # axes and ticks
FIG_GRID_LINEWIDTH     <- pt_to_lw(0.25)   # major gridlines
FIG_BOUNDARY_LINEWIDTH <- pt_to_lw(0.25)   # coastlines, borders
FIG_DRAW_LINEWIDTH     <- pt_to_lw(1.0)    # selected MC draw on a weight bar
FIG_BAR_HALF_HEIGHT    <- 0.4             # categorical-axis units
FIG_LABEL_PADDING     <- 0.25            # ggrepel text padding in lines
FIG_LABEL_FORCE       <- 1

FIG_POINT_SIZE       <- 1.2    # ggplot2 size; ~1 mm marker
FIG_POINT_SIZE_SMALL <- 0.6
FIG_POINT_STROKE     <- 0.3
FIG_STIPPLE_SIZE     <- 0.3    # significance stippling on maps (stroke 0)

FIG_ALPHA_DATA      <- 0.90
FIG_ALPHA_CONTEXT   <- 0.45
FIG_ALPHA_RIBBON    <- 0.20
FIG_ALPHA_REFERENCE <- 0.70

FIG_ERRORBAR_WIDTH   <- 0.16
FIG_TICK_LENGTH_MM   <- 1.2
FIG_PANEL_SPACING_MM <- 2.5
FIG_LEGEND_KEY_MM      <- 3   # key size; vertical colourbars are 5x as long
FIG_LEGEND_KEY_LONG_MM <- 8   # key width for top/bottom legends (colourbar ~40 mm)
FIG_MARGIN_PT        <- c(top = 4, right = 5, bottom = 4, left = 5)

# -----------------------------------------------------------------------------
# 9. Line types and point shapes for non-colour encoding
# -----------------------------------------------------------------------------

FIG_LINETYPES <- c("solid", "dashed", "dotted", "dotdash")
FIG_SHAPES    <- c(21, 22, 24, 23, 25, 16, 17, 15)   # fillable first

# -----------------------------------------------------------------------------
# 10. Themes
# -----------------------------------------------------------------------------

theme_pub <- function(base_size = FIG_BASE_SIZE, base_family = FIG_FONT,
                      grid = FALSE, legend_position = "right") {
  th <- ggplot2::theme_classic(
    base_size = base_size, base_family = base_family,
    base_line_size = FIG_AXIS_LINEWIDTH, base_rect_size = FIG_AXIS_LINEWIDTH
  ) +
    ggplot2::theme(
      text              = ggplot2::element_text(colour = COL_TEXT, family = base_family),
      axis.title        = ggplot2::element_text(size = FIG_AXIS_TITLE_SIZE, colour = COL_TEXT),
      axis.text         = ggplot2::element_text(size = FIG_AXIS_TEXT_SIZE, colour = COL_TEXT),
      axis.line         = ggplot2::element_line(colour = COL_AXIS, linewidth = FIG_AXIS_LINEWIDTH),
      axis.ticks        = ggplot2::element_line(colour = COL_AXIS, linewidth = FIG_AXIS_LINEWIDTH),
      axis.ticks.length = grid::unit(FIG_TICK_LENGTH_MM, "mm"),
      legend.position   = legend_position,
      legend.box        = "vertical", # separate guide blocks instead of clipping side by side
      legend.title      = ggplot2::element_text(size = FIG_LEGEND_TITLE_SIZE, colour = COL_TEXT),
      legend.text       = ggplot2::element_text(size = FIG_LEGEND_TEXT_SIZE, colour = COL_TEXT),
      legend.key        = ggplot2::element_blank(),
      legend.key.size   = grid::unit(FIG_LEGEND_KEY_MM, "mm"),
      legend.key.width  = grid::unit(if (legend_position %in% c("top", "bottom"))
                                       FIG_LEGEND_KEY_LONG_MM else FIG_LEGEND_KEY_MM, "mm"),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      strip.background  = ggplot2::element_blank(),
      strip.text        = ggplot2::element_text(size = FIG_STRIP_SIZE, colour = COL_TEXT),
      # Titles belong in the manuscript caption, never inside the figure.
      plot.title        = ggplot2::element_blank(),
      plot.subtitle     = ggplot2::element_blank(),
      plot.caption      = ggplot2::element_text(size = FIG_ANNOTATION_SIZE, colour = COL_REFERENCE, hjust = 0),
      plot.tag          = ggplot2::element_text(size = FIG_TAG_SIZE, face = "bold", colour = COL_TEXT),
      panel.spacing     = grid::unit(FIG_PANEL_SPACING_MM, "mm"),
      plot.margin       = ggplot2::margin(FIG_MARGIN_PT[1], FIG_MARGIN_PT[2],
                                          FIG_MARGIN_PT[3], FIG_MARGIN_PT[4], unit = "pt")
    )
  # Titles above horizontal legends (theme element exists from ggplot2 3.5.0).
  if (legend_position %in% c("top", "bottom") && utils::packageVersion("ggplot2") >= "3.5.0") {
    th <- th + ggplot2::theme(legend.title.position = "top")
  }
  if (isTRUE(grid)) {
    th <- th + ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = COL_GRID_MAJOR, linewidth = FIG_GRID_LINEWIDTH),
      panel.grid.minor = ggplot2::element_blank()
    )
  }
  th
}

# Maps keep coordinate labels by default (AGU requires latitude/longitude);
# axis titles and lines are dropped, a thin frame is drawn.
theme_pub_map <- function(base_size = FIG_BASE_SIZE, base_family = FIG_FONT,
                          legend_position = "right", coord_labels = TRUE, frame = TRUE) {
  th <- theme_pub(base_size = base_size, base_family = base_family,
                  grid = FALSE, legend_position = legend_position) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.line  = ggplot2::element_blank())
  if (!isTRUE(coord_labels)) {
    th <- th + ggplot2::theme(axis.text = ggplot2::element_blank(),
                              axis.ticks = ggplot2::element_blank())
  }
  if (isTRUE(frame)) {
    th <- th + ggplot2::theme(panel.border = ggplot2::element_rect(
      colour = COL_AXIS, fill = NA, linewidth = FIG_AXIS_LINEWIDTH))
  }
  th
}

# -----------------------------------------------------------------------------
# 11. Scales
# -----------------------------------------------------------------------------
# Colour limits are scientific (set them in the script). Values beyond them
# are drawn in the end colours (squish) rather than turning into the
# "missing" colour; ggplot2's default (censor) would make real data look
# like no-data. Squishing is silent, so whenever a script sets limits that
# may cut the data, it must call check_limits() and say so in the caption.
# (The oob function cannot warn by itself: ggplot2 >= 4.0 also passes
# candidate breaks through it.)

check_limits <- function(x, limits, what = deparse(substitute(x))) {
  n <- sum(x < limits[1] | x > limits[2], na.rm = TRUE)
  if (n > 0) {
    warning(sprintf("%s: %d value(s) (%.1f%%) outside [%g, %g] are drawn in the end colours; say so in the caption.",
                    what, n, 100 * n / sum(!is.na(x)), limits[1], limits[2]), call. = FALSE)
  } else {
    message(sprintf("%s: all values within [%g, %g].", what, limits[1], limits[2]))
  }
  invisible(n)
}

.fig_mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to = to, from = from, mid = mid)
  }
}

# Categorical ----------------------------------------------------------------
scale_colour_cat <- function(..., values = PAL_MAIN, na.value = COL_MISSING) {
  ggplot2::scale_colour_manual(..., values = values, na.value = na.value)
}
scale_fill_cat <- function(..., values = PAL_MAIN, na.value = COL_MISSING) {
  ggplot2::scale_fill_manual(..., values = values, na.value = na.value)
}

# Sequential -----------------------------------------------------------------
scale_colour_seq <- function(..., palette = "seq_default", na.value = COL_MISSING,
                             oob = scales::oob_squish) {
  ggplot2::scale_colour_gradientn(..., colours = fig_colours(palette),
                                  na.value = na.value, oob = oob)
}
scale_fill_seq <- function(..., palette = "seq_default", na.value = COL_MISSING,
                           oob = scales::oob_squish) {
  ggplot2::scale_fill_gradientn(..., colours = fig_colours(palette),
                                na.value = na.value, oob = oob)
}
scale_colour_precip <- function(...) scale_colour_seq(..., palette = "seq_water")
scale_fill_precip   <- function(...) scale_fill_seq(..., palette = "seq_water")
scale_colour_evap   <- function(...) scale_colour_seq(..., palette = "seq_evap")
scale_fill_evap     <- function(...) scale_fill_seq(..., palette = "seq_evap")

# Diverging: midpoint is REQUIRED -- it is a scientific choice. -----------------
# type: "change" (generic), "water" (drier brown <-> wetter blue),
#       "temp" (colder blue <-> warmer red). reverse = TRUE flips the semantics
# when the variable's sign convention is opposite (decide in the script).
.fig_div_check <- function(midpoint_missing, fun) {
  if (midpoint_missing) {
    stop(fun, "(): `midpoint` is a scientific choice; pass it explicitly (e.g. midpoint = 0).",
         call. = FALSE)
  }
}
scale_colour_div <- function(..., midpoint, type = c("change", "water", "temp"),
                             reverse = FALSE, na.value = COL_MISSING, oob = scales::oob_squish) {
  .fig_div_check(missing(midpoint), "scale_colour_div")
  type <- match.arg(type)
  ggplot2::scale_colour_gradientn(..., colours = fig_colours(paste0("div_", type), reverse = reverse),
                                  rescaler = .fig_mid_rescaler(midpoint),
                                  na.value = na.value, oob = oob)
}
scale_fill_div <- function(..., midpoint, type = c("change", "water", "temp"),
                           reverse = FALSE, na.value = COL_MISSING, oob = scales::oob_squish) {
  .fig_div_check(missing(midpoint), "scale_fill_div")
  type <- match.arg(type)
  ggplot2::scale_fill_gradientn(..., colours = fig_colours(paste0("div_", type), reverse = reverse),
                                rescaler = .fig_mid_rescaler(midpoint),
                                na.value = na.value, oob = oob)
}
# Binned version for maps read by class; `breaks` are scientific too.
scale_fill_div_binned <- function(..., midpoint, breaks, type = c("change", "water", "temp"),
                                  reverse = FALSE, na.value = COL_MISSING, oob = scales::oob_squish) {
  .fig_div_check(missing(midpoint), "scale_fill_div_binned")
  if (missing(breaks)) stop("scale_fill_div_binned(): `breaks` (class boundaries) must be explicit.",
                            call. = FALSE)
  type <- match.arg(type)
  ggplot2::scale_fill_stepsn(..., breaks = breaks,
                             colours = fig_colours(paste0("div_", type), reverse = reverse),
                             rescaler = .fig_mid_rescaler(midpoint),
                             na.value = na.value, oob = oob)
}

# Cyclic (day of year, month, timing, direction): limits must span one cycle.
# Out-of-cycle values are censored to the missing colour so they stay visible
# as errors; validate the period in the script.
scale_colour_cyclic <- function(..., limits, na.value = COL_MISSING) {
  if (missing(limits)) stop("scale_colour_cyclic(): `limits` must span one full cycle, e.g. c(1, 366).",
                            call. = FALSE)
  ggplot2::scale_colour_gradientn(..., colours = fig_colours("cyc_time"), limits = limits,
                                  na.value = na.value, oob = scales::oob_censor)
}
scale_fill_cyclic <- function(..., limits, na.value = COL_MISSING) {
  if (missing(limits)) stop("scale_fill_cyclic(): `limits` must span one full cycle, e.g. c(1, 366).",
                            call. = FALSE)
  ggplot2::scale_fill_gradientn(..., colours = fig_colours("cyc_time"), limits = limits,
                                na.value = na.value, oob = scales::oob_censor)
}

# British/American aliases
scale_color_cat <- scale_colour_cat
scale_color_seq <- scale_colour_seq
scale_color_div <- scale_colour_div
scale_color_cyclic <- scale_colour_cyclic

# -----------------------------------------------------------------------------
# 12. Reference lines, stippling, panel tags
# -----------------------------------------------------------------------------

geom_hline_ref <- function(yintercept = 0, ...) {
  ggplot2::geom_hline(yintercept = yintercept, colour = COL_REFERENCE,
                      linewidth = FIG_LINEWIDTH_REF, ...)
}
geom_vline_ref <- function(xintercept = 0, ...) {
  ggplot2::geom_vline(xintercept = xintercept, colour = COL_REFERENCE,
                      linewidth = FIG_LINEWIDTH_REF, ...)
}

# Stippling style only; WHICH cells are stippled is decided in the script.
geom_stipple <- function(mapping = NULL, data = NULL, ...) {
  ggplot2::geom_point(mapping = mapping, data = data, shape = 16, stroke = 0,
                      size = FIG_STIPPLE_SIZE, colour = COL_TEXT, inherit.aes = FALSE, ...)
}

# Tag format and size follow the profile: "(a)" by default, "a" for Nature.
add_panel_tags <- function(plot, levels = "a") {
  plot +
    patchwork::plot_annotation(tag_levels = levels,
                               tag_prefix = FIG_SPEC$tag_prefix,
                               tag_suffix = FIG_SPEC$tag_suffix) &
    ggplot2::theme(plot.tag = ggplot2::element_text(family = FIG_FONT, size = FIG_TAG_SIZE,
                                                    face = "bold", colour = COL_TEXT))
}

# -----------------------------------------------------------------------------
# 13. Export
# -----------------------------------------------------------------------------
# cairo_pdf embeds fonts and keeps text editable; mapping the symbol font to
# the main font stops plotmath (yr^{-1}, degree) from embedding a second
# typeface.

fig_pdf_device <- function(family = FIG_FONT) {
  if (!isTRUE(capabilities("cairo"))) {
    warning("cairo is unavailable: falling back to pdf(); fonts may not be embedded.",
            call. = FALSE)
    return(grDevices::pdf)
  }
  function(filename, ...) {
    grDevices::cairo_pdf(filename, ..., family = family,
                         symbolfamily = grDevices::cairoSymbolFont(family, usePUA = FALSE))
  }
}

# file_stem has no extension. height_mm is required: height is a
# figure-specific decision and must never come from the open device.
save_figure <- function(plot, file_stem, width_mm = FIG_WIDTH_DOUBLE, height_mm,
                        formats = c("pdf", "png"), dpi_png = FIG_DPI_PREVIEW,
                        dpi_raster = FIG_DPI_RASTER, bg = FIG_BG) {
  if (missing(height_mm)) stop("`height_mm` must be supplied explicitly.", call. = FALSE)
  if (!inherits(plot, c("ggplot", "patchwork"))) {
    stop("Pass an explicit plot object (never last_plot()).", call. = FALSE)
  }
  if (width_mm <= 0 || height_mm <= 0) stop("Width and height must be positive.", call. = FALSE)
  if (width_mm > FIG_WIDTH_DOUBLE) {
    warning(sprintf("Width %g mm exceeds the %s full width (%g mm).", width_mm, FIG_PROFILE,
                    FIG_WIDTH_DOUBLE), call. = FALSE)
  }
  if (!is.na(FIG_SPEC$min_width) && width_mm < FIG_SPEC$min_width) {
    warning(sprintf("Width %g mm is below the %s minimum (%g mm).", width_mm, FIG_PROFILE,
                    FIG_SPEC$min_width), call. = FALSE)
  }
  if (height_mm > FIG_HEIGHT_MAX) {
    warning(sprintf("Height %g mm exceeds the routine maximum (%g mm); check the journal.",
                    height_mm, FIG_HEIGHT_MAX), call. = FALSE)
  }
  dir.create(dirname(file_stem), recursive = TRUE, showWarnings = FALSE)
  files <- character(0)
  for (fmt in formats) {
    f <- paste0(file_stem, ".", fmt)
    args <- list(filename = f, plot = plot, width = width_mm, height = height_mm,
                 units = "mm", bg = bg)
    args <- switch(fmt,
      pdf  = c(args, list(device = fig_pdf_device())),
      png  = c(args, list(device = ragg::agg_png, dpi = dpi_png)),
      tiff = c(args, list(device = ragg::agg_tiff, dpi = dpi_raster, compression = "lzw")),
      svg  = c(args, list(device = svglite::svglite)),
      stop("Unsupported format: ", fmt, call. = FALSE))
    do.call(ggplot2::ggsave, args)
    if (!is.na(FIG_SPEC$max_file_mb) && file.size(f) > FIG_SPEC$max_file_mb * 1024^2) {
      warning(sprintf("%s is larger than %g MB; rasterise dense layers (ggrastr::rasterise()).",
                      basename(f), FIG_SPEC$max_file_mb), call. = FALSE)
    }
    files <- c(files, f)
  }
  invisible(files)
}

# One font family, all embedded? Uses pdftools if installed, else pdffonts.
check_fonts <- function(pdf_file) {
  if (requireNamespace("pdftools", quietly = TRUE)) {
    f <- pdftools::pdf_fonts(pdf_file)
    fam <- unique(sub("[-,].*$", "", sub("^[A-Z]{6}\\+", "", f$name)))
    ok <- length(fam) == 1 && all(f$embedded)
    msg <- sprintf("%s: font families = %s; all embedded = %s", basename(pdf_file),
                   paste(fam, collapse = ", "), all(f$embedded))
    if (ok) message(msg) else warning(msg, call. = FALSE)
    return(invisible(ok))
  }
  if (nzchar(Sys.which("pdffonts"))) {
    system2("pdffonts", shQuote(pdf_file))
    return(invisible(NA))
  }
  message("Install pdftools (or poppler's pdffonts) to check embedded fonts.")
  invisible(NA)
}

# =============================================================================
# End of _figs.R
# =============================================================================
