source('source/twc_change.R')
source('source/data_registry.R')
library(pRecipe)
library(doParallel)

registerDoParallel(N_DATASETS_PREC)

# Datasets - Main

PATH_OUTPUT_RAW_PREC <- paste0(PATH_OUTPUT_RAW, "prec/") 
PATH_OUTPUT_RAW_EVAP <- paste0(PATH_OUTPUT_RAW, "evap/") 
PATH_OUTPUT_RAW_OTHER <- paste0(PATH_OUTPUT_RAW, "other/")
dir.create(PATH_OUTPUT_RAW_PREC)
dir.create(PATH_OUTPUT_RAW_EVAP)
dir.create(PATH_OUTPUT_RAW_OTHER)

prec_datasets <- filter_datasets(var = 'precip', tstep = 'yearly', area = 'land')
prec_names_ensemble <- prec_datasets[name %in% PREC_ENSEMBLE_NAMES_SHORT]$fname 
prec_names_analysis <- prec_datasets[name %in% PREC_NAMES_SHORT]$fname 
prec_names_used <- unique(c(prec_names_ensemble, prec_names_analysis))

prec_datasets_used <- data.table(name = prec_datasets[fname %in% prec_names_used]$name,
                                 fname = prec_datasets[fname %in% prec_names_used]$fname,
                                 file_raw = prec_datasets[fname %in% prec_names_used]$file,
                                 file = paste0(PATH_OUTPUT_RAW_PREC, 
                                               prec_datasets[fname %in% prec_names_used]$fname,
                                               "_yearly.nc"))

foreach(dataset_count = 1:N_DATASETS_PREC) %dopar% {
  result <- subset_data(prec_datasets_used$file_raw[dataset_count], 
                        yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
  saveNC(result, prec_datasets_used$file[dataset_count])
}

evap_datasets <- filter_datasets(var = 'evap', var2 = 'e', tstep = 'yearly', area = 'land')
evap_names_ensemble <- evap_datasets[name %in% EVAP_ENSEMBLE_NAMES_SHORT]$fname 
evap_names_analysis <- evap_datasets[name %in% EVAP_NAMES_SHORT]$fname 
evap_names_used <- unique(c(evap_names_ensemble, evap_names_analysis))

evap_datasets_used <- data.table(name = evap_datasets[fname %in% evap_names_used]$name,
                                 fname = evap_datasets[fname %in% evap_names_used]$fname,
                                 file_raw = evap_datasets[fname %in% evap_names_used]$file,
                                 file = paste0(PATH_OUTPUT_RAW_EVAP, evap_datasets_used$fname,
                                               "_yearly.nc"))

foreach(dataset_count = 1:N_DATASETS_EVAP) %dopar% {
  result <- subset_data(evap_datasets_used$file_raw[dataset_count], 
                        yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
  saveNC(result, evap_datasets_used$file[dataset_count])
}

#Precipitation
dataset_to_dt <- brick(prec_datasets_used$file[1])
dataset_dt <- tabular(dataset_to_dt)
dataset_dt[, variable := PREC_NAME]
dataset_dt[, dataset := factor(prec_datasets_used$name[1])]

for(dataset_count in 2:N_DATASETS_PREC){
  dataset_to_dt <- brick(prec_datasets_used$file[dataset_count]) 
  dummy <- tabular(dataset_to_dt)
  dummy[, variable := PREC_NAME]
  dummy[, dataset := factor(prec_datasets_used$name[dataset_count])]
  dummy[, date := as.Date(date)]
  print(prec_datasets_used$name[dataset_count])
  dataset_dt <- rbind(dataset_dt, dummy)
}

#Evaporation
for(dataset_count in 1:N_DATASETS_EVAP){
  dataset_to_dt <- brick(evap_datasets_used$file[dataset_count]) 
  dummy <- tabular(dataset_to_dt)
  dummy[, variable := EVAP_NAME]
  dummy[, dataset := factor(evap_datasets_used$name[dataset_count])]
  dummy[, date := as.Date(date)]
  print(evap_datasets_used$name[dataset_count])
  dataset_dt <- rbind(dataset_dt, dummy)
}

saveRDS(dataset_dt, paste0(PATH_OUTPUT_RAW, 'prec_evap_raw.Rds'))

#PET
dummy <- brick("~/shared/data/sim/evap/raw/gleam-v4-1a_pet_mm_land_198001_202312_025_yearly.nc")
dummy_dt <- tabular(dummy)
dummy_annual_dt <- dummy_dt[, .(value = mean(value)), .(lon, lat, year = year(date))] 

saveRDS(dummy_annual_dt[year <= 2019], paste0(PATH_OUTPUT_RAW_OTHER, 'gleam_pet_yearly.Rds'))

#Water storage/Soil moisture

dummy <- brick("~/shared/data/obs/other/waterstorage/raw/grace-gfz_ws_mm_global_200204_202112_025_monthly.nc")
dummy_dt <- tabular(dummy)
dummy_annual_dt <- dummy_dt[, .(value = mean(value)), .(lon, lat, year = year(date))] 

saveRDS(dummy_annual_dt[year <= 2019], paste0(PATH_OUTPUT_RAW_OTHER, 'grace_yearly_2019.Rds'))

dummy_annual_dt[vakues]
dummy <- subset_data("~/shared/data/obs/soilmoisture/raw/esa-cci-sm-v07-1_swv_m3m-3_land_197811_202112_025_yearly.nc", yrs = c(year(START_PERIOD_1), year(END_PERIOD_2))) 
dummy_dt <- tabular(dummy)

saveRDS(dummy_dt, paste0(PATH_OUTPUT_RAW_OTHER, 'esa-cci_yearly.Rds'))

#Runoff
library(sf)
library(dplyr)
runoff_robin_shp <- st_read('~/shared/data/geodata/robin_v1_Jan2025/ROBIN_V1_Shapefiles_Jan2025.shp')
runoff_robin_shp <- st_make_valid(runoff_robin_shp)
runoff_robin_shp <- runoff_robin_shp[st_is_valid(runoff_robin_shp), ]
runoff_robin_meta <- fread('~/shared/data/stations/robin_v1/supporting-documents/robin_station_metadata_public_v1-1.csv')

csv_files <- list.files("~/shared/data/stations/robin_v1/source/", pattern = "\\.csv$", full.names = TRUE)
runoff_robin <- rbindlist(lapply(csv_files, fread), use.names = TRUE, fill = TRUE)

runoff_robin_day <- merge(runoff_robin, runoff_robin_meta[, .(robin_id = ROBIN_ID, area = AREA)])
runoff_robin_day[, flow_mm := (flow_cumecs * SEC_IN_DAY / (area * 10^6)) * 1000][, area := NULL][flow_cumecs := NULL]
runoff_robin_day[, flow_mm := round(flow_mm, 2)]
setnames(runoff_robin_day, 'flow_mm', 'flow')
dir.create('~/shared/data/stations/robin_v1/raw')
saveRDS(runoff_robin_day, '~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_daily.rds')

runoff_robin_day[, year := as.integer(format(date, "%Y"))]
runoff_robin_day[, month := as.integer(format(date, "%m"))]

runoff_robin_month <- runoff_robin_day[
  , .(flow = sum(flow, na.rm = TRUE)), 
  by = .(robin_id, year, month)
]

runoff_robin_year <- runoff_robin_day[
  , .(flow = sum(flow, na.rm = TRUE)), 
  by = .(robin_id, year)
]

saveRDS(runoff_robin_month, '~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_monthly.rds')
saveRDS(runoff_robin_year, '~/shared/data/stations/robin_v1/raw/robin-v1_q_mm_land_18630101_20221231_station_yearly.rds')

prec_evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'prec_evap.Rds'))
prec_evap_grids <- unique(prec_evap[, .(lon, lat)])
prec_evap_sf <- st_as_sf(prec_evap_grids, coords = c("lon", "lat"), crs = 4326)
prec_evap_in_robin <- st_join(prec_evap_sf, runoff_robin_shp, left = FALSE)

prec_evap_in_robin$lon <- st_coordinates(prec_evap_in_robin)[,1]
prec_evap_in_robin$lat <- st_coordinates(prec_evap_in_robin)[,2]

#basins with no intersections to grid cells
runoff_rep <- st_point_on_surface(runoff_robin_shp)
idx <- st_nearest_feature(runoff_rep, prec_evap_sf)
prec_evap_sf_small <- prec_evap_sf %>%
  mutate(grid_id = row_number())
runoff_with_grid <- runoff_robin_shp %>%
  mutate(grid_id = prec_evap_sf_small$grid_id[idx])

prec_evap_in_robin_small <- left_join(st_drop_geometry(runoff_with_grid), prec_evap_sf2, by = "grid_id")
prec_evap_in_robin_small$lon <- st_coordinates(prec_evap_in_robin_small$geometry)[,1]
prec_evap_in_robin_small$lat <- st_coordinates(prec_evap_in_robin_small$geometry)[,2]
prec_evap_in_robin_small <- as.data.table(prec_evap_in_robin_small)
prec_evap_in_robin_small <- prec_evap_in_robin_small[, .(lon, lat, robin_id = ROBIN_ID)]

#unification of basins
robin_coords <- as.data.table(prec_evap_in_robin)
robin_coords <- robin_coords[, .(lon, lat, robin_id = ROBIN_ID)]

robin_coords <- merge(prec_evap_in_robin_small, robin_coords, by = c('lon', 'lat', 'robin_id'), 
                      all = TRUE, allow.cartesian = TRUE)

basin_ids_with_flow_data <- unique(runoff_robin$robin_id)
basins_with_flow_data <- robin_coords[robin_id %in% basin_ids_with_flow_data]

saveRDS(robin_coords, paste0(PATH_OUTPUT_RAW, 'robin_coords_all.rds'))
saveRDS(basins_with_flow_data, paste0(PATH_OUTPUT_RAW, 'robin_coords.rds'))
saveRDS(prec_evap_in_robin, paste0(PATH_OUTPUT_RAW, 'prec_evap_robin.rds'))
