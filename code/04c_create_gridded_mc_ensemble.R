# ============================================================================
# Build public BASE and CHANGE land-cell Monte Carlo ensembles.
#
# Creates 200 independent NetCDF files:
#
#   base/twc_base_001.nc   ... base/twc_base_100.nc
#   change/twc_change_001.nc ... change/twc_change_100.nc
#
# Each file is one complete ensemble realization containing:
#
#   precipitation[cell, year]
#   evaporation[cell, year]
#   source_dataset_id[cell]
#   region_id[cell]
#   biome_id[cell]
#   lon[cell]
#   lat[cell]
#
# BASE and CHANGE member numbers are paired because both originate from the
# same upstream random draws:
#
#   twc_base_037.nc <-> twc_change_037.nc
#
# Input prec_evap is LONG:
#
#   lon | lat | year | dataset | prec | evap
#
# Members are written in batches and candidate fields are processed one year
# at a time, so no full cell x year x member ensemble is held in memory.
#
# N_MEMBERS = 3 is currently used for timing/testing.
# Change to 100 for the final public ensemble.
# ============================================================================


# Libraries ==================================================================

source("code/_source.R")

library(ncdf4)


# Inputs ======================================================================

prec_evap <- read_fst(
  file.path(PATH_OUTPUT_OUTPUT, "prec_evap.fst"),
  as.data.table = TRUE
)

grid_classes <- readRDS(
  file.path(
    PATH_OUTPUT_OUTPUT,
    "grid_classes.Rds"
  )
)

mc_selection <- readRDS(
  file.path(
    PATH_OUTPUT_OUTPUT,
    "mc_selection_scenarios.Rds"
  )
)


# Constants ===================================================================

PUBLIC_SCENARIOS <- c(
  "base",
  "change"
)

# TEST RUN --------------------------------------------------------------------

N_MEMBERS <- 100L
MEMBER_BATCH_SIZE <- 20L

# FINAL RUN -------------------------------------------------------------------
# N_MEMBERS <- 100L
# MEMBER_BATCH_SIZE <- 20L

NC_MISSING <- -9999


# Output directories ==========================================================

for (scenario in PUBLIC_SCENARIOS) {
  
  dir.create(
    file.path(
      PATH_OUTPUT_DATASET,
      scenario
    ),
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# Prepare inputs ==============================================================

grid_classes <- as.data.table(
  grid_classes
)

mc_selection <- as.data.table(
  mc_selection
)[
  scenario %chin% PUBLIC_SCENARIOS
]

mc_selection[
  ,
  `:=`(
    scenario = as.character(scenario),
    region = as.character(region),
    biome = as.character(biome),
    dataset = as.character(dataset)
  )
]


# Dataset lookup ==============================================================

datasets <- sort(
  unique(
    mc_selection$dataset
  )
)

dataset_lookup <- data.table(
  dataset_id = seq_along(datasets),
  dataset = datasets
)

mc_selection[
  ,
  dataset_id := match(
    dataset,
    datasets
  )
]

prec_evap[
  ,
  dataset := as.character(dataset)
]

prec_evap[
  ,
  dataset_id := match(
    dataset,
    datasets
  )
]

# Keep only datasets used by BASE / CHANGE.

prec_evap <- prec_evap[
  !is.na(dataset_id)
]


# Published land cells ========================================================

data_cells <- unique(
  prec_evap[
    ,
    .(
      lon,
      lat
    )
  ]
)

classified_cells <- grid_classes[
  !is.na(region) &
    !is.na(biome),
  .(
    lon,
    lat,
    region = as.character(region),
    biome = as.character(biome)
  )
]

# Keep only region-biome units represented in the MC selections.

selected_units <- unique(
  mc_selection[
    ,
    .(
      region,
      biome
    )
  ]
)

grid_cells <- merge(
  data_cells,
  classified_cells,
  by = c(
    "lon",
    "lat"
  )
)

grid_cells <- merge(
  grid_cells,
  selected_units,
  by = c(
    "region",
    "biome"
  )
)

setorder(
  grid_cells,
  lat,
  lon
)

grid_cells[
  ,
  cell_id := .I
]

N_CELLS <- nrow(
  grid_cells
)


# Region / biome lookup =======================================================

region_lookup <- data.table(
  region_id = seq_along(
    sort(
      unique(
        grid_cells$region
      )
    )
  ),
  region = sort(
    unique(
      grid_cells$region
    )
  )
)

biome_lookup <- data.table(
  biome_id = seq_along(
    sort(
      unique(
        grid_cells$biome
      )
    )
  ),
  biome = sort(
    unique(
      grid_cells$biome
    )
  )
)

grid_cells[
  ,
  region_id := region_lookup$region_id[
    match(
      region,
      region_lookup$region
    )
  ]
]

grid_cells[
  ,
  biome_id := biome_lookup$biome_id[
    match(
      biome,
      biome_lookup$biome
    )
  ]
]


# Region-biome unit lookup ====================================================

unit_lookup <- unique(
  grid_cells[
    ,
    .(
      region,
      biome
    )
  ]
)

setorder(
  unit_lookup,
  region,
  biome
)

unit_lookup[
  ,
  unit_id := .I
]

grid_cells[
  unit_lookup,
  unit_id := i.unit_id,
  on = .(
    region,
    biome
  )
]


# Years =======================================================================

years <- sort(
  unique(
    prec_evap$year
  )
)

# Repeated extraction is by year.

setkey(
  prec_evap,
  year
)


# Save lookup tables ===========================================================

fwrite(
  dataset_lookup,
  file.path(
    PATH_OUTPUT_DATASET,
    "dataset_lookup.csv"
  )
)

fwrite(
  region_lookup,
  file.path(
    PATH_OUTPUT_DATASET,
    "region_lookup.csv"
  )
)

fwrite(
  biome_lookup,
  file.path(
    PATH_OUTPUT_DATASET,
    "biome_lookup.csv"
  )
)


# Functions ===================================================================

make_cell_selection_matrix <- function(
    scenario_name
) {
  
  sel <- unit_lookup[
    mc_selection[
      scenario == scenario_name &
        sim <= N_MEMBERS,
      .(
        sim,
        region,
        biome,
        dataset_id
      )
    ],
    on = .(
      region,
      biome
    ),
    nomatch = 0L
  ]
  
  sel[
    ,
    sim := as.integer(sim)
  ]
  
  
  # Region-biome unit x member matrix.
  
  sel_wide <- dcast(
    sel,
    unit_id ~ sim,
    value.var = "dataset_id"
  )
  
  member_cols <- as.character(
    seq_len(N_MEMBERS)
  )
  
  
  # Add member columns if required.
  
  missing_cols <- setdiff(
    member_cols,
    names(sel_wide)
  )
  
  if (length(missing_cols)) {
    
    sel_wide[
      ,
      (missing_cols) := NA_integer_
    ]
  }
  
  
  # Ensure every published spatial unit is represented.
  
  sel_wide <- merge(
    unit_lookup[
      ,
      .(
        unit_id
      )
    ],
    sel_wide,
    by = "unit_id",
    all.x = TRUE,
    sort = TRUE
  )
  
  setcolorder(
    sel_wide,
    c(
      "unit_id",
      member_cols
    )
  )
  
  selection_unit <- as.matrix(
    sel_wide[
      ,
      ..member_cols
    ]
  )
  
  storage.mode(
    selection_unit
  ) <- "integer"
  
  
  # Expand region-biome selections to individual land cells.
  
  selection_unit[
    match(
      grid_cells$unit_id,
      sel_wide$unit_id
    ),
    ,
    drop = FALSE
  ]
}


member_filename <- function(
    scenario_name,
    member
) {
  
  file.path(
    PATH_OUTPUT_DATASET,
    scenario_name,
    sprintf(
      "twc_%s_%03d.nc",
      scenario_name,
      member
    )
  )
}


create_member_nc <- function(
    scenario_name,
    member,
    source_dataset_id
) {
  
  filename <- member_filename(
    scenario_name,
    member
  )
  
  
  # Remove previous test output if present.
  
  if (file.exists(filename)) {
    
    removed <- file.remove(
      filename
    )
    
    if (!removed) {
      
      stop(
        "Existing NetCDF cannot be removed, probably because it is locked: ",
        filename
      )
    }
  }
  
  
  # Dimensions ----------------------------------------------------------------
  
  dim_cell <- ncdim_def(
    name = "cell",
    units = "1",
    vals = seq_len(N_CELLS)
  )
  
  dim_year <- ncdim_def(
    name = "year",
    units = "year",
    vals = years
  )
  
  
  # Coordinate / provenance variables ----------------------------------------
  
  var_lon <- ncvar_def(
    name = "lon",
    units = "degrees_east",
    dim = list(dim_cell),
    missval = NC_MISSING,
    longname = "longitude of land cell",
    prec = "float",
    compression = 4
  )
  
  var_lat <- ncvar_def(
    name = "lat",
    units = "degrees_north",
    dim = list(dim_cell),
    missval = NC_MISSING,
    longname = "latitude of land cell",
    prec = "float",
    compression = 4
  )
  
  var_dataset <- ncvar_def(
    name = "source_dataset_id",
    units = "1",
    dim = list(dim_cell),
    missval = -32767,
    longname = "source dataset selected for this ensemble member",
    prec = "short",
    compression = 4
  )
  
  var_region <- ncvar_def(
    name = "region_id",
    units = "1",
    dim = list(dim_cell),
    missval = -32767,
    longname = "IPCC region identifier",
    prec = "short",
    compression = 4
  )
  
  var_biome <- ncvar_def(
    name = "biome_id",
    units = "1",
    dim = list(dim_cell),
    missval = -32767,
    longname = "biome identifier",
    prec = "short",
    compression = 4
  )
  
  
  # P/E variables -------------------------------------------------------------
  
  var_prec <- ncvar_def(
    name = "precipitation",
    units = "mm year-1",
    dim = list(
      dim_cell,
      dim_year
    ),
    missval = NC_MISSING,
    longname = "annual precipitation",
    prec = "float",
    compression = 4,
    chunksizes = c(
      min(
        16384L,
        N_CELLS
      ),
      1L
    )
  )
  
  var_evap <- ncvar_def(
    name = "evaporation",
    units = "mm year-1",
    dim = list(
      dim_cell,
      dim_year
    ),
    missval = NC_MISSING,
    longname = "annual actual evaporation",
    prec = "float",
    compression = 4,
    chunksizes = c(
      min(
        16384L,
        N_CELLS
      ),
      1L
    )
  )
  
  
  # Create file ---------------------------------------------------------------
  
  nc <- nc_create(
    filename,
    vars = list(
      var_lon,
      var_lat,
      var_dataset,
      var_region,
      var_biome,
      var_prec,
      var_evap
    ),
    force_v4 = TRUE
  )
  
  
  # Static cell information ---------------------------------------------------
  
  ncvar_put(
    nc,
    "lon",
    grid_cells$lon
  )
  
  ncvar_put(
    nc,
    "lat",
    grid_cells$lat
  )
  
  ncvar_put(
    nc,
    "source_dataset_id",
    source_dataset_id
  )
  
  ncvar_put(
    nc,
    "region_id",
    grid_cells$region_id
  )
  
  ncvar_put(
    nc,
    "biome_id",
    grid_cells$biome_id
  )
  
  
  # Variable metadata ---------------------------------------------------------
  
  ncatt_put(
    nc,
    "precipitation",
    "coordinates",
    "lon lat"
  )
  
  ncatt_put(
    nc,
    "evaporation",
    "coordinates",
    "lon lat"
  )
  
  
  # Global metadata -----------------------------------------------------------
  
  ncatt_put(
    nc,
    0,
    "title",
    paste(
      "Terrestrial water-cycle observational ensemble",
      toupper(scenario_name),
      sprintf(
        "member %03d",
        member
      )
    )
  )
  
  ncatt_put(
    nc,
    0,
    "scenario",
    scenario_name
  )
  
  ncatt_put(
    nc,
    0,
    "ensemble_member",
    member
  )
  
  ncatt_put(
    nc,
    0,
    "paired_member",
    paste0(
      "Member ",
      sprintf(
        "%03d",
        member
      ),
      " is paired between BASE and CHANGE through common random numbers."
    )
  )
  
  ncatt_put(
    nc,
    0,
    "selection_scale",
    paste0(
      "One source dataset is selected per region-biome unit and ensemble ",
      "member and applied to all land cells and years belonging to that unit."
    )
  )
  
  ncatt_put(
    nc,
    0,
    "dataset_lookup_file",
    "dataset_lookup.csv"
  )
  
  ncatt_put(
    nc,
    0,
    "region_lookup_file",
    "region_lookup.csv"
  )
  
  ncatt_put(
    nc,
    0,
    "biome_lookup_file",
    "biome_lookup.csv"
  )
  
  
  if (scenario_name == "base") {
    
    ncatt_put(
      nc,
      0,
      "weighting_definition",
      paste0(
        "Balanced weighting: climatology/change = 50/50; ",
        "mean/SD within climatology = 50/50; ",
        "significance/slope within change = 50/50; ",
        "precipitation/evaporation = 50/50."
      )
    )
    
  } else {
    
    ncatt_put(
      nc,
      0,
      "weighting_definition",
      paste0(
        "Change-oriented weighting: climatology/change = 25/75; ",
        "mean/SD within climatology = 50/50; ",
        "significance/slope within change = 75/25; ",
        "precipitation/evaporation = 50/50."
      )
    )
  }
  
  
  nc
}


write_scenario <- function(
    scenario_name
) {
  
  cat(
    "\nBuilding ",
    toupper(scenario_name),
    " ensemble\n",
    sep = ""
  )
  
  
  # Selected source dataset for every land cell x member.
  
  selection_cell <- make_cell_selection_matrix(
    scenario_name
  )
  
  cell_index <- seq_len(
    N_CELLS
  )
  
  
  # Process members in batches.
  
  batch_starts <- seq(
    1L,
    N_MEMBERS,
    by = MEMBER_BATCH_SIZE
  )
  
  
  for (batch_start in batch_starts) {
    
    members <- batch_start:min(
      batch_start + MEMBER_BATCH_SIZE - 1L,
      N_MEMBERS
    )
    
    cat(
      "  members ",
      min(members),
      "-",
      max(members),
      "\n",
      sep = ""
    )
    
    
    # Keep creation inside tryCatch so successfully opened files are closed
    # even if a later file or processing step fails.
    
    nc_files <- list()
    
    tryCatch(
      {
        
        # Create/open NetCDF files for this batch.
        
        for (member in members) {
          
          nc_files[[length(nc_files) + 1L]] <- create_member_nc(
            scenario_name = scenario_name,
            member = member,
            source_dataset_id = selection_cell[
              ,
              member
            ]
          )
        }
        
        
        # Process one year at a time.
        
        for (year_i in seq_along(years)) {
          
          year_now <- years[year_i]
          
          cat(
            sprintf(
              "Scenario: %s | Members: %03d-%03d | Year: %d\n",
              scenario_name,
              min(members),
              max(members),
              year_now
            )
          )
          
          
          # All source datasets for this year.
          #
          # prec_evap is LONG:
          #
          #   lon | lat | year | dataset | prec | evap | dataset_id
          
          year_dt <- prec_evap[
            .(year_now)
          ]
          
          
          # Attach the fixed published cell ID.
          
          year_dt <- grid_cells[
            year_dt,
            on = .(
              lon,
              lat
            ),
            nomatch = 0L,
            .(
              cell_id,
              dataset_id = i.dataset_id,
              prec = i.prec,
              evap = i.evap
            )
          ]
          
          
          # Convert the long annual values to compact:
          #
          #   cell x source-dataset
          #
          # matrices.
          
          prec_candidates <- matrix(
            NA_real_,
            nrow = N_CELLS,
            ncol = length(datasets)
          )
          
          evap_candidates <- matrix(
            NA_real_,
            nrow = N_CELLS,
            ncol = length(datasets)
          )
          
          idx <- cbind(
            year_dt$cell_id,
            year_dt$dataset_id
          )
          
          prec_candidates[idx] <- year_dt$prec
          evap_candidates[idx] <- year_dt$evap
          
          
          # Write each realization in the current batch.
          
          for (j in seq_along(members)) {
            
            member <- members[j]
            
            selected_dataset <- selection_cell[
              ,
              member
            ]
            
            ok <- !is.na(
              selected_dataset
            )
            
            
            # Precipitation ----------------------------------------------------
            
            prec_member <- rep(
              NA_real_,
              N_CELLS
            )
            
            prec_member[ok] <- prec_candidates[
              cbind(
                cell_index[ok],
                selected_dataset[ok]
              )
            ]
            
            ncvar_put(
              nc_files[[j]],
              "precipitation",
              prec_member,
              start = c(
                1L,
                year_i
              ),
              count = c(
                N_CELLS,
                1L
              )
            )
            
            
            # Evaporation ------------------------------------------------------
            
            evap_member <- rep(
              NA_real_,
              N_CELLS
            )
            
            evap_member[ok] <- evap_candidates[
              cbind(
                cell_index[ok],
                selected_dataset[ok]
              )
            ]
            
            ncvar_put(
              nc_files[[j]],
              "evaporation",
              evap_member,
              start = c(
                1L,
                year_i
              ),
              count = c(
                N_CELLS,
                1L
              )
            )
          }
          
          
          rm(
            year_dt,
            prec_candidates,
            evap_candidates,
            prec_member,
            evap_member
          )
        }
        
      },
      
      finally = {
        
        for (nc in nc_files) {
          
          try(
            nc_close(nc),
            silent = TRUE
          )
        }
      }
    )
  }
  
  
  invisible(
    TRUE
  )
}


# Analysis ====================================================================

write_scenario(
  "base"
)

write_scenario(
  "change"
)


# Outputs =====================================================================
# The 200 NetCDF ensemble members themselves are written above, incrementally
# during Analysis (see the file header note on batched writing); this is the
# manifest of what that step produced.

manifest <- CJ(
  scenario = PUBLIC_SCENARIOS,
  member = seq_len(N_MEMBERS)
)

manifest[
  ,
  file := sprintf(
    "%s/twc_%s_%03d.nc",
    scenario,
    scenario,
    member
  )
]

manifest[
  ,
  paired_member := member
]

fwrite(
  manifest,
  file.path(
    PATH_OUTPUT_DATASET,
    "manifest.csv"
  )
)


# Validation ==================================================================

manifest_paths <- file.path(PATH_OUTPUT_DATASET, manifest$file)
stopifnot(all(file.exists(manifest_paths)))

# Summary =====================================================================

cat(
  "\nFinished public land-cell ensemble.\n",
  "\nFiles: ",
  N_MEMBERS * length(PUBLIC_SCENARIOS),
  "\nLand cells: ",
  N_CELLS,
  "\nYears: ",
  length(years),
  "\n",
  sep = ""
)