# ============================================================================
# Build most and least dataset maps
#
# This script:
#   1. reads grid-cell dataset rankings and biases
#   2. identifies the most and least dataset for selected properties
#   3. saves the most/least map data
#   4. plots the resulting spatial patterns
#
# Outputs:
#   - dataset_most_least_maps.Rds
# ============================================================================

# Libraries ==================================================================

source("code/_source.R")

# Inputs =====================================================================

dataset_ranks <- readRDS(
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_ranks.Rds"
  )
)

# Constants & Variables ======================================================

PROPERTY_BIAS <- c(
  prec_mean = "prec_mean_bias",
  prec_trend = "prec_bias_slope",
  evap_mean = "evap_mean_bias",
  evap_trend = "evap_bias_slope"
)

# Functions ==================================================================

most_least_at <- function(dt, bias_col) {
  
  dt[
    is.finite(get(bias_col)),
    {
      v <- get(bias_col)
      
      .(
        position = c(
          "most",
          "least"
        ),
        dataset = c(
          dataset[which.min(v)],
          dataset[which.max(v)]
        ),
        n_avail = .N
      )
    },
    by = .(
      lon,
      lat
    )
  ]
}


build_most_least_maps <- function(dt, property_bias) {
  
  rbindlist(
    lapply(
      names(property_bias),
      function(prop) {
        
        most_least_at(
          dt = dt,
          bias_col = property_bias[[prop]]
        )[
          ,
          property := prop
        ]
      }
    ),
    use.names = TRUE
  )
}

# Analysis ===================================================================

ml_maps <- build_most_least_maps(
  dt = dataset_ranks,
  property_bias = PROPERTY_BIAS
)

ml_maps[
  ,
  property := factor(
    property,
    levels = names(PROPERTY_BIAS)
  )
]

ml_maps[
  ,
  position := factor(
    position,
    levels = c(
      "most",
      "least"
    )
  )
]

ml_maps[
  ,
  dataset := factor(dataset)
]

# Outputs ====================================================================

saveRDS(
  ml_maps,
  file.path(
    PATH_OUTPUT_OUTPUT,
    "dataset_most_least_maps.Rds"
  )
)

# Plot =======================================================================
world <- map_data("world")

ggplot(
  ml_maps,
  aes(
    x = lon,
    y = lat,
    fill = dataset
  )
) +
  geom_tile() +
  geom_polygon(
    data = world,
    aes(
      x = long,
      y = lat,
      group = group
    ),
    inherit.aes = FALSE,
    fill = NA,
    colour = "grey20",
    linewidth = 0.15
  ) +
  facet_grid(
    position ~ property
  ) +
  coord_equal(
    expand = FALSE
  ) +
  labs(
    title = "Most and least representative dataset by grid cell",
    x = NULL,
    y = NULL,
    fill = "Dataset"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    panel.spacing = unit(0.4, "lines")
  )

# Validation =================================================================

stopifnot(nrow(ml_maps) > 0)

stopifnot(
  all(
    names(PROPERTY_BIAS) %in%
      unique(
        as.character(
          ml_maps$property
        )
      )
  )
)

cat("\nmost/least counts:\n")

print(
  ml_maps[
    ,
    .N,
    by = .(
      dataset,
      position
    )
  ]
)

cat("\nFinished most/least maps.\n")