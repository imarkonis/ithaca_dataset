source('source/twc_change.R')
source('source/data_registry.R')
library(pRecipe)
library(doParallel)

registerDoParallel(N_DATASETS_PREC)

# Datasets - Main

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