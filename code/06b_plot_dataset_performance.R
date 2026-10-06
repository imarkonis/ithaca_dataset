# ============================================================================
# Figure 3: dataset performance structure. Which products perform best where?
#
# (a) Best-performing candidate by IPCC region, drawn on the hexagonal region
#     cartogram.
# (b) Mean performance rank by biome, shown as a circle matrix, with an
#     "All land" row.
#
# Nothing is re-estimated. The performance synthesis is the plain mean of the
# six comparable per-cell ranks that 03a already saved in dataset_ranks.Rds:
# precipitation and evaporation x climatological mean, interannual SD and
# Sen-slope rank.
#
# Every component enters with the same weight. It does not use scenario
# weights, and Boolean significance checks are not part of it.
# Ranks run from 1 (best) to 5 (worst) among the five candidates in a cell.
#
# Cell ranks are averaged to IPCC regions and biomes using cell-area weights
# from grid_classes.Rds.
# ============================================================================


# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")
source("code/_figs.R")

stopifnot(
  setequal(names(PAL_DATASETS), CANDIDATES),
  setequal(names(DATASET_LABELS), CANDIDATES)
)


# Inputs =====================================================================

RANK_COLUMNS <- c(
  "prec_mean_rank", "prec_sd_rank", "prec_rank_slope",
  "evap_mean_rank", "evap_sd_rank", "evap_rank_slope"
)

dataset_ranks <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "dataset_ranks.Rds")
))

grid_classes <- as.data.table(readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
))

rank_required <- c("lon", "lat", "dataset", RANK_COLUMNS)
grid_required <- c("lon", "lat", "region", "biome", "cell_weight")

stopifnot(
  all(rank_required %in% names(dataset_ranks)),
  all(grid_required %in% names(grid_classes)),
  !anyDuplicated(dataset_ranks[, .(lon, lat, dataset)]),
  !anyDuplicated(grid_classes[, .(lon, lat)]),
  all(as.character(dataset_ranks$dataset) %in% CANDIDATES),
  all(is.finite(grid_classes$cell_weight) & grid_classes$cell_weight > 0)
)

dataset_ranks <- dataset_ranks[, ..rank_required]


# Constants & Variables =======================================================

# A cell-dataset needs at least this many of the six ranks.
MIN_RANK_COMPONENTS <- 4L

# Use only cells for which all five candidates have an overall rank.
REQUIRE_ALL_CANDIDATES <- TRUE

# Final ESSD/Copernicus size.
FIGURE_HEIGHT_MM <- 105

# Biome order.
BIOME_ORDER <- c(
  "Polar", "Tundra", "B. Forests", "T. Forests", "T. Grasslands",
  "M. Grasslands", "Mediterranean", "Deserts", "T/S Grasslands",
  "T/S Forests", "Flooded", "Water"
)

# Mean rank = 3 is the neutral expectation among five candidates.
# Lower = better; higher = worse.
RANK_LIMITS <- c(2, 4)
RANK_MIDPOINT <- 3

# Figure-specific pastel rank palette.
RANK_COLOUR_BETTER <- "#8FC4D8"
RANK_COLOUR_NEUTRAL <- "#F6F3EE"
RANK_COLOUR_WORSE <- "#E99A86"

# Established IPCC hexagon layout.
HEX_LAYOUT_FILES <- c(
  "docs/figures/ipcc_ref_hexagons.csv",
  "/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv"
)


# Functions ==================================================================

# Overall performance rank per cell and dataset.
overall_rank <- function(ranks) {
  ranks <- copy(ranks)
  
  ranks[
    ,
    `:=`(
      n_components = rowSums(!is.na(.SD)),
      overall = rowMeans(.SD, na.rm = TRUE)
    ),
    .SDcols = RANK_COLUMNS
  ]
  
  ranks[n_components < MIN_RANK_COMPONENTS, overall := NA_real_]
  
  if (REQUIRE_ALL_CANDIDATES) {
    complete <- ranks[
      !is.na(overall),
      .N,
      by = .(lon, lat)
    ][N == length(CANDIDATES)]
    
    ranks <- ranks[
      complete[, .(lon, lat)],
      on = .(lon, lat),
      nomatch = 0L
    ]
  }
  
  ranks[
    !is.na(overall),
    .(lon, lat, dataset = as.character(dataset), overall)
  ]
}


# Area-weighted mean overall rank by one grouping column.
mean_rank_by <- function(ranked, group_column) {
  ranked[
    !is.na(get(group_column)),
    .(mean_rank = weighted_mean_safe(overall, cell_weight)),
    by = c(group_column, "dataset")
  ]
}


# Best-performing candidate per region.
region_winners <- function(region_ranks) {
  setorder(region_ranks, region, mean_rank)
  
  region_ranks[
    ,
    .(
      winner = dataset[1],
      rank_winner = mean_rank[1]
    ),
    by = region
  ][]
}


# Established hexagons --------------------------------------------------------

# Same layout shifts used by build_region_hex_map() in the ithaca repository.
shift_ipcc_hexagons <- function(hexagons) {
  hexagons <- copy(hexagons)
  
  shifts <- list(
    list(regions = "GIC", dx = -7, dy = -4),
    list(regions = "MDG", dx = -7, dy = -3),
    list(regions = c("NAU", "CAU", "EAU", "SAU"), dx = 5, dy = 12),
    list(regions = "NZ", dx = 10, dy = 9)
  )
  
  for (shift in shifts) {
    rows <- hexagons$Acronym %in% shift$regions
    
    hexagons[rows, `:=`(
      long = long + shift$dx,
      V1 = V1 + shift$dx,
      lat = lat + shift$dy,
      V2 = V2 + shift$dy
    )]
  }
  
  hexagons[]
}


read_established_hexagons <- function(path) {
  hexagons <- as.data.table(read.csv(path))
  
  missing_columns <- setdiff(
    c("Acronym", "long", "lat", "group", "V1", "V2"),
    names(hexagons)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Hexagon file ", path, " lacks column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  shift_ipcc_hexagons(hexagons)
}


# Generated fallback layout ---------------------------------------------------

# Area-weighted region centroid.
# Longitude is treated as a circular variable.
region_centroids <- function(grid_classes) {
  grid_classes[
    !is.na(region),
    {
      angle <- lon * pi / 180
      
      .(
        lon_c = atan2(
          sum(cell_weight * sin(angle)),
          sum(cell_weight * cos(angle))
        ) * 180 / pi,
        lat_c = weighted.mean(lat, cell_weight)
      )
    },
    by = .(Acronym = region)
  ]
}


# Assign every region to a unique hexagonal grid cell.
assign_regions_to_cells <- function(target_x, target_y, cell_x, cell_y) {
  cost <- outer(target_x, cell_x, "-")^2 +
    outer(target_y, cell_y, "-")^2
  
  n_regions <- length(target_x)
  assigned <- rep(NA_integer_, n_regions)
  free <- rep(TRUE, length(cell_x))
  todo <- seq_len(n_regions)
  
  while (length(todo) > 0) {
    sub_cost <- cost[todo, free, drop = FALSE]
    pick <- which(sub_cost == min(sub_cost), arr.ind = TRUE)[1, ]
    
    region_i <- todo[pick[1]]
    cell_j <- which(free)[pick[2]]
    
    assigned[region_i] <- cell_j
    free[cell_j] <- FALSE
    todo <- setdiff(todo, region_i)
  }
  
  repeat {
    improved <- FALSE
    
    for (i in seq_len(n_regions)) {
      free_cells <- which(free)
      
      if (length(free_cells) > 0) {
        best <- free_cells[which.min(cost[i, free_cells])]
        
        if (cost[i, best] < cost[i, assigned[i]] - 1e-12) {
          free[assigned[i]] <- TRUE
          assigned[i] <- best
          free[best] <- FALSE
          improved <- TRUE
        }
      }
      
      for (k in setdiff(seq_len(n_regions), i)) {
        swapped <- cost[i, assigned[k]] + cost[k, assigned[i]]
        current <- cost[i, assigned[i]] + cost[k, assigned[k]]
        
        if (swapped < current - 1e-12) {
          held <- assigned[i]
          assigned[i] <- assigned[k]
          assigned[k] <- held
          improved <- TRUE
        }
      }
    }
    
    if (!improved) break
  }
  
  assigned
}


generate_hexagons <- function(grid_classes) {
  centroids <- region_centroids(grid_classes)
  n_regions <- nrow(centroids)
  
  n_row <- ceiling(sqrt(1.7 * n_regions / 2.4))
  n_col <- ceiling(1.7 * n_regions / n_row)
  
  cells <- CJ(
    col = 0:(n_col - 1),
    row = 0:(n_row - 1)
  )
  
  cells[, `:=`(
    x = sqrt(3) * (col + 0.5 * (row %% 2)),
    y = 1.5 * (n_row - 1 - row)
  )]
  
  centroids[, `:=`(
    target_x = scales::rescale(lon_c, to = range(cells$x)),
    target_y = scales::rescale(lat_c, to = range(cells$y))
  )]
  
  centroids[
    ,
    cell := assign_regions_to_cells(
      target_x, target_y, cells$x, cells$y
    )
  ]
  
  corner_angle <- (30 + 60 * (0:5)) * pi / 180
  
  centroids[
    ,
    {
      cx <- cells$x[cell]
      cy <- cells$y[cell]
      
      .(
        group = Acronym,
        long = cx + 0.97 * cos(corner_angle),
        lat = cy + 0.97 * sin(corner_angle),
        V1 = cx,
        V2 = cy
      )
    },
    by = Acronym
  ]
}


load_hexagons <- function(files, grid_classes) {
  found <- files[file.exists(files)]
  
  if (length(found) > 0) {
    message("Hexagon layout: ", found[1])
    
    return(list(
      data = read_established_hexagons(found[1]),
      source = "established",
      path = found[1]
    ))
  }
  
  warning(
    "Hexagon layout file not found (", paste(files, collapse = ", "), "): ",
    "using a generated layout, NOT the established IPCC hexagons.",
    call. = FALSE
  )
  
  list(
    data = generate_hexagons(grid_classes),
    source = "generated",
    path = NA_character_
  )
}


# White text on dark fills, black text on light fills.
label_colour <- function(fill) {
  luminance <- colSums(
    col2rgb(fill) * c(0.299, 0.587, 0.114)
  ) / 255
  
  ifelse(luminance < 0.55, COL_WHITE, COL_BLACK)
}


# Analysis ===================================================================

ranked <- overall_rank(dataset_ranks)

ranked <- merge(
  ranked,
  grid_classes[, .(lon, lat, region, biome, cell_weight)],
  by = c("lon", "lat")
)


# Regions --------------------------------------------------------------------

region_ranks <- mean_rank_by(ranked, "region")
winners <- region_winners(copy(region_ranks))


# Biomes ---------------------------------------------------------------------

biome_ranks <- mean_rank_by(ranked, "biome")

all_land <- ranked[
  ,
  .(
    biome = "All land",
    mean_rank = weighted_mean_safe(overall, cell_weight)
  ),
  by = dataset
]

heat_ranks <- rbindlist(
  list(biome_ranks, all_land),
  use.names = TRUE
)

biome_levels <- c(
  intersect(BIOME_ORDER, biome_ranks$biome),
  setdiff(unique(biome_ranks$biome), BIOME_ORDER)
)

heat_ranks[, `:=`(
  biome = factor(
    biome,
    levels = rev(c(biome_levels, "All land"))
  ),
  dataset = factor(
    dataset,
    levels = CANDIDATES
  ),
  block = factor(
    fifelse(biome == "All land", "All land", "Biomes"),
    levels = c("Biomes", "All land")
  )
)]

# Distance from neutral rank 3 controls circle area.
heat_ranks[, rank_distance := abs(mean_rank - RANK_MIDPOINT)]

# Best candidate in every row.
heat_best <- heat_ranks[
  ,
  .SD[which.min(mean_rank)],
  by = biome
]


# Validation before rendering ================================================

stopifnot(
  nrow(ranked) > 0L,
  ranked[, all(overall >= 1 & overall <= length(CANDIDATES))],
  region_ranks[
    ,
    uniqueN(dataset),
    by = region
  ][, all(V1 == length(CANDIDATES))],
  all(is.finite(heat_ranks$mean_rank)),
  setequal(names(PAL_DATASETS), CANDIDATES),
  setequal(names(DATASET_LABELS), CANDIDATES)
)


# Panel (a): IPCC hexagons ====================================================

hex_layout <- load_hexagons(
  HEX_LAYOUT_FILES,
  grid_classes
)

hexagons <- hex_layout$data

missing_hexagons <- setdiff(
  winners$region,
  hexagons$Acronym
)

if (length(missing_hexagons) > 0) {
  warning(
    "Regions without a hexagon (not drawn): ",
    paste(missing_hexagons, collapse = ", "),
    call. = FALSE
  )
}

hex_polygons <- merge(
  hexagons,
  winners[, .(Acronym = region, winner)],
  by = "Acronym"
)

hex_polygons[, winner := factor(
  winner,
  levels = CANDIDATES
)]

hex_labels <- unique(
  hex_polygons[, .(Acronym, V1, V2, winner)]
)

hex_labels[, text_colour := label_colour(
  PAL_DATASETS[as.character(winner)]
)]


# Explicit five-dataset legend.
# The invisible layer retains all five categories even when a dataset wins
# no region; key_glyph = "rect" gives proper rectangular colour swatches.
dataset_legend_keys <- data.table(
  winner = factor(CANDIDATES, levels = CANDIDATES),
  x = mean(range(hex_polygons$long)),
  y = mean(range(hex_polygons$lat))
)

dataset_fill <- scale_fill_cat(
  name = "Dataset",
  values = PAL_DATASETS,
  breaks = CANDIDATES,
  limits = CANDIDATES,
  labels = DATASET_LABELS[CANDIDATES],
  drop = FALSE,
  guide = guide_legend(
    nrow = 1,
    byrow = TRUE,
    override.aes = list(
      alpha = 1,
      colour = NA
    )
  )
)

p_hex <- ggplot() +
  geom_polygon(
    data = hex_polygons,
    aes(x = long, y = lat, group = group, fill = winner),
    colour = COL_WHITE,
    linewidth = FIG_BOUNDARY_LINEWIDTH,
    show.legend = FALSE
  ) +
  geom_point(
    data = dataset_legend_keys,
    aes(x = x, y = y, fill = winner),
    alpha = 0,
    inherit.aes = FALSE,
    show.legend = TRUE,
    key_glyph = "rect"
  ) +
  geom_text(
    data = hex_labels,
    aes(x = V1, y = V2, label = Acronym, colour = text_colour),
    family = FIG_FONT,
    size = FIG_GEOM_TEXT_SIZE,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  dataset_fill +
  coord_equal(expand = FALSE) +
  theme_pub(legend_position = "bottom") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank()
  )


# Panel (b): rank circle matrix ===============================================

best_key <- "Best dataset in row"

rank_distance_max <- max(
  abs(RANK_LIMITS - RANK_MIDPOINT)
)

p_heat <- ggplot(
  heat_ranks,
  aes(x = dataset, y = biome)
) +
  # Quiet matrix grid.
  geom_tile(
    fill = NA,
    colour = COL_CONTEXT_LT,
    linewidth = FIG_BOUNDARY_LINEWIDTH
  ) +
  # Circle colour = mean rank; area = distance from neutral rank 3.
  geom_point(
    aes(
      fill = mean_rank,
      size = rank_distance
    ),
    shape = 21,
    colour = COL_CONTEXT,
    stroke = FIG_POINT_STROKE
  ) +
  # Outline the best-performing dataset in each row.
  geom_tile(
    data = heat_best,
    aes(colour = best_key),
    fill = NA,
    linewidth = FIG_LINEWIDTH_EMPH
  ) +
  scale_colour_manual(
    name = NULL,
    values = setNames(COL_BLACK, best_key)
  ) +
  scale_fill_gradient2(
    name = "Mean rank\n(1 = best)",
    low = RANK_COLOUR_BETTER,
    mid = RANK_COLOUR_NEUTRAL,
    high = RANK_COLOUR_WORSE,
    midpoint = RANK_MIDPOINT,
    limits = RANK_LIMITS,
    breaks = seq(RANK_LIMITS[1], RANK_LIMITS[2], by = 0.5),
    labels = label_minus,
    oob = scales::squish
  ) +
  # Circle area, rather than radius, is proportional to distance from rank 3.
  scale_size_area(
    name = expression("|rank - 3|"),
    max_size = FIG_POINT_SIZE * 6,
    limits = c(0, rank_distance_max),
    breaks = c(0.25, 0.5, 1)
  ) +
  scale_x_discrete(
    position = "top",
    labels = DATASET_LABELS[CANDIDATES]
  ) +
  facet_grid(
    block ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  labs(x = NULL, y = NULL) +
  theme_pub(legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    panel.spacing.y = grid::unit(FIG_PANEL_SPACING_MM, "mm"),
    strip.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face = "bold")
  )


# Assemble ====================================================================

figure_3 <- add_panel_tags(
  (p_hex + p_heat) +
    plot_layout(widths = c(1.8, 1))
)


# Colour-scale QA =============================================================

check_limits(
  heat_ranks$mean_rank,
  RANK_LIMITS,
  "Biome/all-land mean performance rank"
)


# Outputs ====================================================================

figure_stem <- file.path(
  PATH_OUTPUT_FIGURES,
  "fig03_dataset_performance"
)

save_figure(
  figure_3,
  file_stem = figure_stem,
  width_mm = FIG_WIDTH_DOUBLE,
  height_mm = FIGURE_HEIGHT_MM
)

check_fonts(
  paste0(figure_stem, ".pdf")
)


# Caption =====================================================================

layout_sentence <- if (identical(hex_layout$source, "established")) {
  paste0(
    "Hexagon positions follow the established ",
    "IPCC reference-region cartogram. "
  )
} else {
  paste0(
    "The established IPCC hexagon file was unavailable at rendering; ",
    "a centroid-based fallback cartogram was used. "
  )
}

caption <- paste0(
  "Dataset performance structure. ",
  "(a) Best-performing candidate in each IPCC region, defined as the ",
  "lowest area-weighted mean of six per-cell ranks: precipitation and ",
  "evaporation climatological mean, interannual SD and Sen slope. ",
  layout_sentence,
  "(b) Area-weighted mean performance rank by biome and for all land; ",
  "rank 1 is best and rank 5 worst. Rank 3 is the neutral expectation. ",
  "Circle area represents the absolute departure from rank 3, while colour ",
  "shows the mean rank, from better-than-neutral blue to worse-than-neutral ",
  "tomato. The outlined cell is the lowest mean rank in each row. ",
  "The colour scale is displayed over ranks ",
  RANK_LIMITS[1], "\u2013", RANK_LIMITS[2],
  "; values outside this range are shown using the corresponding end colour."
)

writeLines(
  caption,
  paste0(figure_stem, "_caption.txt")
)


# Data QA =====================================================================

cat(
  "\nMean overall rank over all cells and datasets: ",
  round(ranked[, mean(overall)], 3),
  " (3 expected for complete ranks)\n",
  sep = ""
)

cat(
  "\nRegions won, by dataset (",
  nrow(winners),
  " regions):\n",
  sep = ""
)

print(
  winners[
    ,
    .(regions = .N),
    by = winner
  ][order(-regions)]
)

cat("\nAll land, area-weighted mean rank:\n")

print(
  all_land[
    order(mean_rank)
  ][
    ,
    mean_rank := round(mean_rank, 3)
  ][]
)

if (REQUIRE_ALL_CANDIDATES) {
  kept <- uniqueN(ranked[, .(lon, lat)])
  total <- uniqueN(dataset_ranks[, .(lon, lat)])
  
  cat(
    "\nCells with all five candidates ranked: ",
    kept, " of ", total, "\n",
    sep = ""
  )
}