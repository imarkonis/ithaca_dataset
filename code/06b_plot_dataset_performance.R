# ============================================================================
# Figure 3: dataset performance structure. Which products perform best where?
#
# (a) Best-performing candidate by IPCC region, drawn on the hexagonal region
#     cartogram, and (b) mean performance rank by biome (heatmap, with an
#     "All land" row).
#
# Nothing is re-estimated. The performance synthesis is the plain mean of the six
# comparable per-cell ranks that 03a already saved in dataset_ranks.Rds:
#   precipitation and evaporation  x  climatological mean, interannual SD and
#   Sen-slope rank.
# Every component enters with the same weight. It does not use the scenario
# weights, and the Boolean significance checks are deliberately not part of it
# (they are not ranks). Ranks run from 1 (best) to 5 (worst) among the five
# candidates in a cell.
#
# The cell ranks are averaged to IPCC regions and biomes with the cell-area
# weight (cell_weight) of grid_classes.Rds.
#
# Hexagon layout: the established IPCC hexagon polygons (gloabl_ipcc_ref_hexagons.csv,
# used by build_region_hex_map in the ithaca repository) are read if the file
# can be found (see HEX_LAYOUT_FILES). If not, a layout is generated from the
# region centroids so that the figure still runs; it is NOT the established one.
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")
source("code/_figure_helpers.R")

# Inputs =====================================================================

RANK_COLUMNS <- c(
  "prec_mean_rank", "prec_sd_rank", "prec_rank_slope",
  "evap_mean_rank", "evap_sd_rank", "evap_rank_slope"
)

dataset_ranks <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "dataset_ranks.Rds")
)[, c("lon", "lat", "dataset", RANK_COLUMNS), with = FALSE]

grid_classes <- readRDS(
  file.path(PATH_OUTPUT_OUTPUT, "grid_classes.Rds")
)

# Constants & Variables ======================================================

# A cell-dataset needs at least this many of the six ranks to get an overall rank.
MIN_RANK_COMPONENTS <- 4L

# Use only cells in which all five candidates are ranked, so that every rank is
# on the same 1-5 scale.
REQUIRE_ALL_CANDIDATES <- TRUE

# Difference in mean rank between first and second candidate below which a region
# is called a close call (runner-up shown as a dot).
CLOSE_CALL_TOLERANCE <- 0.05

# Biomes ordered along the environmental gradient from cold to warm and from
# azonal to open water; biomes missing from this list are appended.
BIOME_ORDER <- c(
  "Polar", "Tundra", "B. Forests", "T. Forests", "T. Grasslands",
  "M. Grasslands", "Mediterranean", "Deserts", "T/S Grasslands",
  "T/S Forests", "Flooded", "Water"
)

# Colour range of the heatmap; the mean rank expected by chance is 3.
RANK_LIMITS <- c(2, 4)

# Established hexagon layout, first file that exists is used. Copy the CSV into
# docs/figures/ to keep it with the repository.
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
    complete <- ranks[!is.na(overall), .N, by = .(lon, lat)][N == length(CANDIDATES)]
    ranks <- ranks[complete[, .(lon, lat)], on = .(lon, lat), nomatch = 0L]
  }

  ranks[!is.na(overall), .(lon, lat, dataset = as.character(dataset), overall)]
}

# Area-weighted mean overall rank by one grouping column.
mean_rank_by <- function(ranked, group_column) {
  ranked[
    !is.na(get(group_column)),
    .(mean_rank = weighted_mean_safe(overall, cell_weight)),
    by = c(group_column, "dataset")
  ]
}

# Winner, runner-up and their difference per region.
region_winners <- function(region_ranks) {
  setorder(region_ranks, region, mean_rank)

  region_ranks[
    ,
    .(
      winner = dataset[1],
      runner_up = dataset[2],
      rank_winner = mean_rank[1],
      margin = mean_rank[2] - mean_rank[1]
    ),
    by = region
  ][
    ,
    close_call := margin < CLOSE_CALL_TOLERANCE
  ][]
}

# Established hexagons ---------------------------------------------------------

# Standard manuscript layout shifts, as in build_region_hex_map() of the ithaca
# repository (source/plot_functions.R): Greenland and Madagascar are moved
# towards their neighbours, Australia and New Zealand up and to the right.
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
      long = long + shift$dx, V1 = V1 + shift$dx,
      lat = lat + shift$dy, V2 = V2 + shift$dy
    )]
  }

  hexagons[]
}

read_established_hexagons <- function(path) {
  hexagons <- as.data.table(read.csv(path))

  missing_columns <- setdiff(c("Acronym", "long", "lat", "group", "V1", "V2"), names(hexagons))

  if (length(missing_columns) > 0) {
    stop(
      "Hexagon file ", path, " lacks column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  shift_ipcc_hexagons(hexagons)
}

# Generated fallback layout ----------------------------------------------------

# Area-weighted region centroid (longitude as circular mean, so regions that
# cross the date line are not pulled to the Greenwich meridian).
region_centroids <- function(grid_classes) {
  grid_classes[
    !is.na(region),
    {
      angle <- lon * pi / 180
      .(
        lon_c = atan2(sum(cell_weight * sin(angle)), sum(cell_weight * cos(angle))) * 180 / pi,
        lat_c = weighted.mean(lat, cell_weight)
      )
    },
    by = .(Acronym = region)
  ]
}

# Assign every region to its own hexagon cell, minimising the squared distance
# between the region centroid and the cell: greedy start, then moves and swaps.
assign_regions_to_cells <- function(target_x, target_y, cell_x, cell_y) {
  cost <- outer(target_x, cell_x, "-")^2 + outer(target_y, cell_y, "-")^2
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

  # Pointy-top hexagon grid with about 1.7 cells per region.
  n_row <- ceiling(sqrt(1.7 * n_regions / 2.4))
  n_col <- ceiling(1.7 * n_regions / n_row)

  cells <- CJ(col = 0:(n_col - 1), row = 0:(n_row - 1))
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
    cell := assign_regions_to_cells(target_x, target_y, cells$x, cells$y)
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
    return(read_established_hexagons(found[1]))
  }

  message(
    "Hexagon layout file not found (", paste(files, collapse = ", "), "): ",
    "using a generated layout, NOT the established IPCC hexagons."
  )

  generate_hexagons(grid_classes)
}

# White text on dark fills, black text on light fills.
label_colour <- function(fill) {
  luminance <- colSums(col2rgb(fill) * c(0.299, 0.587, 0.114)) / 255
  ifelse(luminance < 0.55, "white", "black")
}

# Analysis ===================================================================

ranked <- overall_rank(dataset_ranks)

ranked <- merge(
  ranked,
  grid_classes[, .(lon, lat, region, biome, cell_weight)],
  by = c("lon", "lat")
)

# Regions: best candidate and how decisive it is ---------------------------------

region_ranks <- mean_rank_by(ranked, "region")
winners <- region_winners(copy(region_ranks))

# Biomes, plus an "All land" row ---------------------------------------------------

biome_ranks <- mean_rank_by(ranked, "biome")

all_land <- ranked[
  ,
  .(biome = "All land", mean_rank = weighted_mean_safe(overall, cell_weight)),
  by = dataset
]

heat_ranks <- rbindlist(list(biome_ranks, all_land), use.names = TRUE)

biome_levels <- c(
  intersect(BIOME_ORDER, biome_ranks$biome),
  setdiff(unique(biome_ranks$biome), BIOME_ORDER)
)

heat_ranks[
  ,
  `:=`(
    biome = factor(biome, levels = rev(c(biome_levels, "All land"))),
    dataset = factor(dataset, levels = CANDIDATES),
    block = factor(
      fifelse(biome == "All land", "All land", "Biomes"),
      levels = c("Biomes", "All land")
    )
  )
]

# Best candidate within every biome row, for the outline.
heat_best <- heat_ranks[, .SD[which.min(mean_rank)], by = biome]

# Hexagon map ------------------------------------------------------------------------

hexagons <- load_hexagons(HEX_LAYOUT_FILES, grid_classes)

missing_hexagons <- setdiff(winners$region, hexagons$Acronym)

if (length(missing_hexagons) > 0) {
  warning(
    "Regions without a hexagon (not drawn): ",
    paste(missing_hexagons, collapse = ", "),
    call. = FALSE
  )
}

hex_polygons <- merge(
  hexagons,
  winners[, .(Acronym = region, winner, runner_up, margin, close_call)],
  by = "Acronym"
)

hex_polygons[, winner := factor(winner, levels = CANDIDATES)]

hex_labels <- unique(hex_polygons[, .(Acronym, V1, V2, winner, runner_up, close_call)])
hex_labels[, text_colour := label_colour(DATASET_COLS[as.character(winner)])]

# Runner-up dot for close calls, placed in the lower right of the hexagon.
hex_extent <- hex_polygons[
  ,
  .(width = diff(range(long)), height = diff(range(lat))),
  by = Acronym
]

close_calls <- merge(hex_labels[close_call == TRUE], hex_extent, by = "Acronym")
close_calls[, `:=`(
  dot_x = V1 + 0.26 * width,
  dot_y = V2 - 0.30 * height,
  runner_up = factor(runner_up, levels = CANDIDATES)
)]

dataset_fill <- scale_fill_manual(
  name = "Best-performing dataset",
  values = DATASET_COLS,
  limits = CANDIDATES,
  drop = FALSE,
  guide = guide_legend(nrow = 1, keywidth = unit(0.45, "cm"), keyheight = unit(0.45, "cm"))
)

# Legend keys for all five candidates, also those that win no region.
legend_keys <- data.table(
  winner = factor(CANDIDATES, levels = CANDIDATES),
  x = NA_real_,
  y = NA_real_
)

p_hex <- ggplot() +
  geom_polygon(
    data = hex_polygons,
    aes(x = long, y = lat, group = group, fill = winner),
    colour = "white",
    linewidth = 0.5,
    show.legend = FALSE
  ) +
  geom_text(
    data = hex_labels,
    aes(x = V1, y = V2, label = Acronym, colour = text_colour),
    size = 2.7,
    fontface = "bold"
  ) +
  geom_point(
    data = close_calls,
    aes(x = dot_x, y = dot_y, fill = runner_up),
    shape = 21,
    colour = "white",
    stroke = 0.5,
    size = 2.2,
    show.legend = FALSE
  ) +
  geom_point(
    data = legend_keys,
    aes(x = x, y = y, fill = winner),
    shape = 22,
    size = 4,
    na.rm = TRUE
  ) +
  scale_colour_identity() +
  dataset_fill +
  coord_equal(expand = FALSE) +
  labs(
    caption = paste0(
      "Lowest mean of the six performance ranks (P and E: mean, SD, slope), area-weighted over each region. ",
      "Dot: runner-up within ", CLOSE_CALL_TOLERANCE, " mean rank (effectively tied)."
    )
  ) +
  theme_void(base_size = 8) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    plot.caption = element_text(size = 6.5, colour = "grey30", hjust = 0),
    plot.margin = margin(2, 2, 2, 2)
  )

# Biome heatmap -------------------------------------------------------------------------

heat_ranks[, text_colour := fifelse(mean_rank < mean(RANK_LIMITS) - 0.35, "white", "black")]

p_heat <- ggplot(heat_ranks, aes(x = dataset, y = biome)) +
  geom_tile(aes(fill = mean_rank), colour = "white", linewidth = 0.6) +
  geom_tile(
    data = heat_best,
    fill = NA,
    colour = "black",
    linewidth = 0.7
  ) +
  geom_text(
    aes(label = sprintf("%.2f", mean_rank), colour = text_colour),
    size = 2.6
  ) +
  scale_colour_identity() +
  scale_fill_viridis_c(
    name = "Mean rank\n(1 = best)",
    option = "mako",
    limits = RANK_LIMITS,
    oob = scales::squish,
    breaks = seq(RANK_LIMITS[1], RANK_LIMITS[2], by = 0.5),
    guide = guide_colourbar(
      title.position = "top",
      barwidth = unit(3.5, "cm"),
      barheight = unit(0.3, "cm")
    )
  ) +
  scale_x_discrete(position = "top") +
  facet_grid(block ~ ., scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, caption = "Outline: best dataset in the row.") +
  theme_minimal(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    panel.spacing.y = unit(0.15, "cm"),
    strip.text = element_blank(),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    plot.caption = element_text(size = 6.5, colour = "grey30", hjust = 0),
    plot.margin = margin(2, 2, 2, 2)
  )

figure_3 <- (p_hex + p_heat) +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 11))

# Outputs ====================================================================

save_figure(figure_3, "fig03_dataset_performance", width = 11, height = 5.6)

# Validation =================================================================

# Ranks are on the 1-5 scale and every dataset is present in every region.
stopifnot(ranked[, all(overall >= 1 & overall <= length(CANDIDATES))])
stopifnot(region_ranks[, uniqueN(dataset), by = region][, all(V1 == length(CANDIDATES))])

# Within a cell the five overall ranks average to 3 (ranks are a permutation of
# 1-5 per component, apart from ties and missing components).
cat(
  "\nMean overall rank over all cells and datasets: ",
  round(ranked[, mean(overall)], 3), " (3 expected for complete ranks)\n",
  sep = ""
)

cat("\nRegions won, by dataset (", nrow(winners), " regions):\n", sep = "")
print(winners[, .(regions = .N, close_calls = sum(close_call)), by = winner][order(-regions)])

cat("\nMargin between first and second (mean rank), quantiles:\n")
print(round(quantile(winners$margin, c(0, 0.25, 0.5, 0.75, 1)), 3))

cat("\nAll land, area-weighted mean rank:\n")
print(all_land[order(mean_rank)][, mean_rank := round(mean_rank, 3)][])

if (REQUIRE_ALL_CANDIDATES) {
  kept <- uniqueN(ranked[, .(lon, lat)])
  total <- uniqueN(dataset_ranks[, .(lon, lat)])
  cat("\nCells with all five candidates ranked: ", kept, " of ", total, "\n", sep = "")
}
