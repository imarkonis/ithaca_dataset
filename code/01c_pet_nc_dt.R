# ============================================================================
# Convert MERRA2 and MSWX-Past NetCDF forcing into monthly data.tables
#
# First stage of the PET production chain. Reads the raw monthly 0.25 degree
# NetCDF meteorological forcing of MERRA2 and MSWX-Past (plus the ERA5-Land
# albedo used with MSWX-Past), converts each variable to long format, merges
# the variables of each dataset on the common x/y/date key, clips them to the
# 1980-2024 study period and stores one .fst table per dataset.
#
# The 12 PET method/input combinations are estimated in the next script
# (01d_estimate_pet.R), which uses the two tables written here.
#
# Inputs : monthly NetCDF files listed in FORCING_FILES, read from
#          PATH_OUTPUT_RAW_OTHER
# Outputs: the .fst tables listed in OUTPUT_FILES, written to
#          PATH_OUTPUT_RAW_OTHER
# ============================================================================
source(file.path("code", "00_initialize.R"))

library(data.table)
library(terra)
library(fst)

# Constants & Variables ======================================================
PERIOD_FIRST_YEAR <- 1980
PERIOD_LAST_YEAR <- 2024
PERIOD_START <- as.Date(paste0(PERIOD_FIRST_YEAR, "-01-01"))
PERIOD_END <- as.Date(paste0(PERIOD_LAST_YEAR, "-12-31"))
MONTHS_PER_YEAR <- 12
EXPECTED_MONTH_COUNT <- MONTHS_PER_YEAR * 
  (PERIOD_LAST_YEAR - PERIOD_FIRST_YEAR + 1)

FORCING_FILES <- list( merra2 = MERRA2_FILES, mswx_past = MSWX_FILES)

OUTPUT_FILES <- c( merra2 = "merra2_pet-forcing_mixed_1980_2024_025_monthly.fst", 
                   mswx_past = "mswx-past_pet-forcing_mixed_1980_2024_025_monthly.fst" 
                   )
# Coordinates are rounded before merging: the same 0.25 degree grid is written
# with slightly different floating point values across products, which
# silently drops rows in the joins below.
COORDINATE_DIGITS <- 4
MERGE_KEYS <- c("x", "y", "date")

# Target column name -> source file. Units are the ones carried by the file
# names: temperature degC, radiation W m-2, pressure Pa, wind m s-1,
# specific humidity kg kg-1, relative humidity %, albedo fraction.
MERRA2_FILES <- c( tavg = "merra2_t2m_degC_land_198001_202511_025_monthly.nc", 
                   tmax = "merra2_tmax_degC_land_198001_202512_025_monthly.nc", 
                   tmin = "merra2_tmin_degC_land_198001_202512_025_monthly.nc", 
                   sw_rad = "merra2_ssrd_wm-2_land_198001_202511_025_monthly.nc", 
                   lw_rad = "merra2_strd_wm-2_land_198001_202511_025_monthly.nc", 
                   pres = "merra2_sp_Pa_land_198001_202511_025_monthly.nc", 
                   u_wind = "merra2_u2m_ms-1_land_198001_202511_025_monthly.nc", 
                   v_wind = "merra2_v2m_ms-1_land_198001_202511_025_monthly.nc", 
                   spec_hum = "merra2_2sh_kgkg-1_land_198001_202511_025_monthly.nc"
                   )

# MSWX-Past carries no albedo, so the ERA5-Land product is used instead.
# MSWX wind is at 10 m while MERRA2 components are at 2 m; the reduction to a
# common reference height is left to the PET script, not done here.
MSWX_FILES <- c( tavg = "mswx-past_t2m_degC_land_197901_202512_025_monthly.nc", 
                 tmax = "mswx-past_tmax_degC_land_197901_202512_025_monthly.nc", 
                 tmin = "mswx-past_tmin_degC_land_197901_202512_025_monthly.nc", 
                 sw_rad = "mswx-past_ssrd_Wm-2_land_197902_202512_025_monthly.nc", 
                 lw_rad = "mswx-past_strd_Wm-2_land_197902_202512_025_monthly.nc", 
                 pres = "mswx-past_sp_pa_land_197901_202412_025_monthly.nc", 
                 wind_speed = "mswx-past_u10_ms-1_land_197901_202412_025_monthly.nc", 
                 rel_hum = "mswx-past_r_pct_land_197901_202512_025_monthly.nc", 
                 albedo = "era5-land_albedo_198001_202501_025_monthly.nc"
                 )

# Functions ==================================================================
read_nc_as_data_table <- function(nc_path, value_name) {
# Converts one single-variable monthly NetCDF into a long data.table, on the
# first day of each month and clipped to the study period.
# Args:
#  nc_path: full path of the NetCDF file.
#  value_name: name given to the value column of the output.
# Returns:
#  data.table with columns x, y, date and value_name, keyed on x, y, date.
  variable_raster <- rast(nc_path)
  variable_table <- as.data.table(
    as.data.frame(variable_raster, xy = TRUE, time = TRUE, wide = FALSE,
                  na.rm = TRUE, row.names = NULL)
  )
  
  if (!"time" %in% names(variable_table)) {
    layer_day_count <- as.numeric(sub(".*=", "", variable_table$layer))
    if (anyNA(layer_day_count)) {
      stop("Could not parse dates from layer names of ", nc_path,
           call. = FALSE)
    }
    variable_table[, time := as.Date(layer_day_count, origin = "1970-01-01")]
  }
  variable_table[, layer := NULL]
  setnames(variable_table, c("values", "time"), c(value_name, "date"))

  # Monthly stamps sit on different days across products, so every date is
  # moved to the first of its month before the variables are merged.
  variable_table[, date := as.Date(date)]
  variable_table[, date := as.Date(format(date, "%Y-%m-01"))]
  variable_table[, x := round(x, COORDINATE_DIGITS)]
  variable_table[, y := round(y, COORDINATE_DIGITS)]
  variable_table <- variable_table[date >= PERIOD_START & date <= PERIOD_END]

  setcolorder(variable_table, c(MERGE_KEYS, value_name))
  setkeyv(variable_table, MERGE_KEYS)
  variable_table[]
}

# Analysis ===================================================================
# Every input is checked first, so a missing file stops the run in seconds
# instead of after an hour of conversion work.
required_paths <- file.path(PATH_OUTPUT_RAW_OTHER, unlist(FORCING_FILES))
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop("Missing NetCDF inputs:\n  ",
       paste(missing_paths, collapse = "\n  "), call. = FALSE)
}

forcing_tables <- vector("list", length(FORCING_FILES))
names(forcing_tables) <- names(FORCING_FILES)

for (dataset_name in names(FORCING_FILES)) {
  message(dataset_name, " forcing")
  dataset_files <- FORCING_FILES[[dataset_name]]

  variable_tables <- vector("list", length(dataset_files))
  names(variable_tables) <- names(dataset_files)
  for (variable_name in names(dataset_files)) {
    nc_path <- file.path(PATH_OUTPUT_RAW_OTHER, dataset_files[[variable_name]])
    variable_tables[[variable_name]] <- read_nc_as_data_table(nc_path,
                                                              variable_name)
    message("  ", variable_name, ": ",
            format(nrow(variable_tables[[variable_name]]), big.mark = " "),
            " rows")
    gc()
  }

  forcing_table <- variable_tables[[1]]
  for (variable_index in seq_along(variable_tables)[-1]) {
    forcing_table <- merge(forcing_table, variable_tables[[variable_index]],
                           by = MERGE_KEYS)
  }
  rm(variable_tables)
  gc()

  row_count_merged <- nrow(forcing_table)
  forcing_table <- na.omit(forcing_table)
  message("  merged: ", format(row_count_merged, big.mark = " "),
          " rows, dropped ", row_count_merged - nrow(forcing_table),
          " incomplete rows")

  setkeyv(forcing_table, MERGE_KEYS)
  forcing_tables[[dataset_name]] <- forcing_table
}

# Validation ==================================================================
for (dataset_name in names(forcing_tables)) {
  forcing_table <- forcing_tables[[dataset_name]]
  if (nrow(forcing_table) == 0) {
    stop(dataset_name, ": empty forcing table, the variable merge failed.",
         call. = FALSE)
  }
  if (anyDuplicated(forcing_table, by = MERGE_KEYS) > 0) {
    stop(dataset_name, ": duplicated x/y/date records.", call. = FALSE)
  }
  month_count <- uniqueN(forcing_table$date)
  if (month_count != EXPECTED_MONTH_COUNT) {
    warning(dataset_name, ": ", month_count, " months instead of ",
            EXPECTED_MONTH_COUNT, ", check the coverage of the inputs.",
            call. = FALSE)
  }
  message(dataset_name, ": ", uniqueN(forcing_table, by = c("x", "y")),
          " grid cells over ", month_count, " months")
}

# Outputs ====================================================================
for (dataset_name in names(forcing_tables)) {
  write_fst(forcing_tables[[dataset_name]],
            file.path(PATH_OUTPUT_RAW_OTHER, OUTPUT_FILES[[dataset_name]]))
}
