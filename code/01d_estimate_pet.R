# ============================================================================
# Estimate yearly potential evapotranspiration from MERRA2 and MSWX-Past
#
# from the MERRA2 and from the MSWX-Past forcing:
#   pet_hs   Hargreaves and Samani (1985), temperature and top of atmosphere
#            radiation
#   pet_od   Oudin et al. (2005), temperature and top of atmosphere radiation
#   pet_hm   Hamon (1961), temperature and day length
#   pet_eop  Energy only, net radiation scaled by a fixed coefficient
#   pet_pt   Priestley and Taylor (1972), net radiation and the slope of the
#            saturation vapour pressure curve
#   pet_pm   FAO-56 Penman-Monteith (Allen et al., 1998), full forcing
#
# Inputs : the .fst tables listed in INPUT_FILES, read from
#          PATH_OUTPUT_RAW_OTHER
# Outputs: merra2_mswx_pet_mm_1980_2024_yearly.fst, written to
#          PATH_OUTPUT_RAW_OTHER
# ============================================================================
source(file.path("code", "00_initialize.R"))

library(data.table)
library(fst)

# Constants & Variables ======================================================
INPUT_FILES <- c( "merra2" = "merra2_pet-forcing_mixed_1980_2024_025_monthly.fst", 
                  "mswx-past" = "mswx-past_pet-forcing_mixed_1980_2024_025_monthly.fst"
                  )
FILE_PET_OUT <- "merra2_mswx_pet_mm_1980_2024_yearly.fst"

PERIOD_FIRST_YEAR <- 1980
PERIOD_LAST_YEAR <- 2024
MONTHS_PER_YEAR <- 12
EXPECTED_YEAR_COUNT <- PERIOD_LAST_YEAR - PERIOD_FIRST_YEAR + 1

PET_METHOD_COLUMNS <- c("pet_hs", "pet_od", "pet_hm", "pet_eop", "pet_pt",
                        "pet_pm")
EXPECTED_COMBINATION_COUNT <- length(PET_METHOD_COLUMNS) * length(INPUT_FILES)
PET_DECIMAL_DIGITS <- 1

# Day of each month whose top of atmosphere radiation is closest to the
# monthly mean, following the standard FAO-56 table.
REPRESENTATIVE_DAYS <- c(17, 16, 16, 15, 15, 11, 17, 16, 15, 15, 14, 10)
DAYS_IN_MONTH <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
DAYS_IN_LEAP_FEBRUARY <- 29
DAYS_PER_YEAR <- 365.25

# Physical constants, in the units of FAO-56 Irrigation and Drainage Paper 56.
SOLAR_CONSTANT <- 0.0820           # MJ m-2 min-1
W_TO_MJ_PER_DAY <- 0.0864          # W m-2 to MJ m-2 day-1
STEFAN_BOLTZMANN <- 4.903e-09      # MJ K-4 m-2 day-1
SURFACE_EMISSIVITY <- 0.98
VAPOUR_MASS_RATIO <- 0.622         # water vapour over dry air
PSYCHROMETRIC_COEFFICIENT <- 0.00163
WIND_MEASUREMENT_HEIGHT <- 10      # m, MSWX-Past wind reference height

# Method coefficients and the temperature below which the temperature based
# methods return no evaporative demand.
HARGREAVES_COEFFICIENT <- 0.0023
HARGREAVES_TEMPERATURE_LIMIT <- -17.8
OUDIN_TEMPERATURE_LIMIT <- -5
OUDIN_SCALE <- 100
HAMON_COEFFICIENT <- 0.1651 * 216.7
ENERGY_ONLY_COEFFICIENT <- 0.8
PRIESTLEY_TAYLOR_ALPHA <- 1.26

# Analysis ===================================================================
required_paths <- file.path(PATH_OUTPUT_RAW_OTHER, INPUT_FILES)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop("Missing forcing tables, run 01c_prepare_pet_forcing.R first:\n  ",
       paste(missing_paths, collapse = "\n  "), call. = FALSE)
}

pet_tables <- vector("list", length(INPUT_FILES))
names(pet_tables) <- names(INPUT_FILES)

for (dataset_name in names(INPUT_FILES)) {
  message(dataset_name, " potential evapotranspiration")
  pet_table <- read_fst(
    file.path(PATH_OUTPUT_RAW_OTHER, INPUT_FILES[[dataset_name]]),
    as.data.table = TRUE
  )

  # Thermodynamic terms shared by both datasets. Latent heat of vaporisation
  # is used throughout to convert MJ m-2 day-1 into mm day-1.
  pet_table[, latent_heat := 2.501 - 0.002361 * tavg]
  pet_table[, sat_vp_slope := 4098 * 0.6108 *
              exp(17.27 * tavg / (tavg + 237.3)) / (tavg + 237.3)^2]
  pet_table[, sat_vp := 0.6108 * (exp(17.27 * tmax / (tmax + 237.3)) +
                                    exp(17.27 * tmin / (tmin + 237.3))) / 2]
  pet_table[, psy_const := PSYCHROMETRIC_COEFFICIENT * pres /
              (1000 * latent_heat)]

  # Terms whose derivation depends on the variables each dataset provides.
  if (dataset_name == "merra2") {
    # MERRA2 wind components are given at 2 m, no height reduction needed,
    # and humidity is given as specific humidity.
    pet_table[, wind_2m := sqrt(u_wind^2 + v_wind^2)]
    pet_table[, act_vp := spec_hum * pres / (VAPOUR_MASS_RATIO * 1000)]
    pet_table[, net_rad := W_TO_MJ_PER_DAY * (sw_rad + lw_rad)]
    pet_table[, c("u_wind", "v_wind", "spec_hum") := NULL]
  } else {
    # MSWX-Past wind is given at 10 m and reduced to 2 m with FAO-56
    # equation 47. Net radiation uses the ERA5-Land albedo for the reflected
    # shortwave and a grey body approximation for the outgoing longwave.
    pet_table[, wind_2m := wind_speed * 4.87 /
                log(67.8 * WIND_MEASUREMENT_HEIGHT - 5.42)]
    pet_table[, act_vp := sat_vp * rel_hum / 100]
    pet_table[, net_rad := W_TO_MJ_PER_DAY * sw_rad * (1 - albedo) +
                W_TO_MJ_PER_DAY * lw_rad -
                SURFACE_EMISSIVITY * STEFAN_BOLTZMANN * (tavg + 273.15)^4]
    pet_table[, c("wind_speed", "rel_hum", "albedo") := NULL]
  }

  # Calendar terms. Every month is represented by the day of REPRESENTATIVE_
  # DAYS, and monthly rates are accumulated over the true month length.
  pet_table[, year_number := year(date)]
  pet_table[, month_number := month(date)]
  pet_table[, day_count := DAYS_IN_MONTH[month_number]]
  pet_table[month_number == 2 &
              ((year_number %% 4 == 0 & year_number %% 100 != 0) |
                 year_number %% 400 == 0),
            day_count := DAYS_IN_LEAP_FEBRUARY]
  pet_table[, representative_date := as.Date(paste(
    year_number, month_number, REPRESENTATIVE_DAYS[month_number], sep = "-"
  ))]
  pet_table[, julian_day := yday(representative_date)]

  # Top of atmosphere radiation, FAO-56 equations 21 to 25.
  pet_table[, latitude_rad := y * pi / 180]
  pet_table[, earth_sun_distance := 1 + 0.0330 *
              cos(2 * pi * julian_day / DAYS_PER_YEAR)]
  pet_table[, solar_declination := 0.409 *
              sin(2 * pi * julian_day / DAYS_PER_YEAR - 1.39)]
  pet_table[, sunset_angle := acos(pmin(pmax(
    -tan(latitude_rad) * tan(solar_declination), -1), 1))]
  pet_table[, ext_rad := (24 * 60 / pi) * SOLAR_CONSTANT *
              earth_sun_distance *
              (sunset_angle * sin(latitude_rad) * sin(solar_declination) +
                 cos(latitude_rad) * cos(solar_declination) *
                 sin(sunset_angle))]

  # The six methods, as daily rates in mm day-1. Negative demand is not
  # physical and is set to zero before the monthly accumulation.
  pet_table[, pet_hs := 0]
  pet_table[tavg > HARGREAVES_TEMPERATURE_LIMIT,
            pet_hs := HARGREAVES_COEFFICIENT * ext_rad *
              sqrt(abs(tmax - tmin)) *
              (tavg - HARGREAVES_TEMPERATURE_LIMIT) / latent_heat]

  pet_table[, pet_od := 0]
  pet_table[tavg > OUDIN_TEMPERATURE_LIMIT,
            pet_od := ext_rad * (tavg - OUDIN_TEMPERATURE_LIMIT) /
              (latent_heat * OUDIN_SCALE)]

  pet_table[, pet_hm := pmax(HAMON_COEFFICIENT * (2 * sunset_angle / pi) *
                               6.108 * exp(17.27 * tavg / (tavg + 237.3)) /
                               (tavg + 273.3), 0)]

  pet_table[, pet_eop := pmax(ENERGY_ONLY_COEFFICIENT * net_rad /
                                latent_heat, 0)]

  pet_table[, pet_pt := pmax(PRIESTLEY_TAYLOR_ALPHA * sat_vp_slope * net_rad /
                               (latent_heat * (sat_vp_slope + psy_const)), 0)]

  pet_table[, pet_pm := pmax(
    (0.408 * sat_vp_slope * net_rad +
       psy_const * (900 / (tavg + 273)) * wind_2m * (sat_vp - act_vp)) /
      (sat_vp_slope + psy_const * (1 + 0.34 * wind_2m)), 0)]

  # Daily rates to monthly totals, then to yearly totals in mm.
  pet_table[, (PET_METHOD_COLUMNS) := lapply(.SD, "*", day_count),
            .SDcols = PET_METHOD_COLUMNS]
  pet_yearly <- pet_table[, c(lapply(.SD, sum), list(month_count = .N)),
                          by = .(x, y, year_number),
                          .SDcols = PET_METHOD_COLUMNS]
  rm(pet_table)
  gc()

  incomplete_count <- nrow(pet_yearly[month_count != MONTHS_PER_YEAR])
  pet_yearly <- pet_yearly[month_count == MONTHS_PER_YEAR]
  pet_yearly[, month_count := NULL]
  message("  dropped ", incomplete_count, " incomplete cell years")

  pet_yearly[, (PET_METHOD_COLUMNS) := lapply(.SD, round,
                                              PET_DECIMAL_DIGITS),
             .SDcols = PET_METHOD_COLUMNS]
  pet_yearly[, date := as.Date(paste0(year_number, "-01-01"))]
  pet_yearly[, year_number := NULL]
  pet_yearly[, source := dataset_name]
  pet_tables[[dataset_name]] <- pet_yearly
  gc()
}

pet_yearly_all <- rbindlist(pet_tables)
rm(pet_tables)
gc()

pet_yearly_all <- pet_yearly_all[
  date >= as.Date(paste0(PERIOD_FIRST_YEAR, "-01-01")) &
    date <= as.Date(paste0(PERIOD_LAST_YEAR, "-01-01"))
]
setcolorder(pet_yearly_all, c("date", "x", "y", "source", PET_METHOD_COLUMNS))
setkeyv(pet_yearly_all, c("source", "x", "y", "date"))

# Validation =================================================================
if (nrow(pet_yearly_all) == 0) {
  stop("Empty PET table, no cell year survived the estimation.", call. = FALSE)
}
if (anyDuplicated(pet_yearly_all, by = c("source", "x", "y", "date")) > 0) {
  stop("Duplicated source/x/y/date records.", call. = FALSE)
}

combination_count <- uniqueN(pet_yearly_all$source) * length(PET_METHOD_COLUMNS)
if (combination_count != EXPECTED_COMBINATION_COUNT) {
  stop("Found ", combination_count, " PET combinations instead of ",
       EXPECTED_COMBINATION_COUNT, ", 03e expects the full set.",
       call. = FALSE)
}

year_count <- uniqueN(pet_yearly_all$date)
if (year_count != EXPECTED_YEAR_COUNT) {
  warning("Found ", year_count, " years instead of ", EXPECTED_YEAR_COUNT,
          ", check the coverage of the forcing.", call. = FALSE)
}

for (method_name in PET_METHOD_COLUMNS) {
  method_values <- pet_yearly_all[[method_name]]
  if (anyNA(method_values) || any(!is.finite(method_values))) {
    stop(method_name, ": missing or non finite yearly totals.", call. = FALSE)
  }
  if (min(method_values) < 0) {
    stop(method_name, ": negative yearly totals.", call. = FALSE)
  }
  message(method_name, ": median ",
          round(median(method_values), PET_DECIMAL_DIGITS), " mm per year")
}

# Outputs ====================================================================
write_fst(pet_yearly_all, file.path(PATH_OUTPUT_RAW_OTHER, FILE_PET_OUT))
