# Libraries ====================================================================

source('source/twc_change.R')

# Inputs =======================================================================

prec_evap_stats <- readRDS(file.path(PATH_OUTPUT_DATA, 'prec_evap_stats.Rds'))
dataset_ranks <- readRDS(file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))


# Constants & Variables ========================================================

ARIDITY_THRES <- 0.9

prec_evap_stats[, pe_ratio := prec_mean / evap_mean]
prec_evap_stats[, pe_ratio_check := TRUE]

# Analysis =====================================================================

prec_evap_stats[pe_ratio < ARIDITY_THRES, pe_ratio_check := FALSE]

prec_evap_stats[pe_ratio_check == TRUE, .N, dataset]
prec_evap_stats[pe_ratio_check == FALSE, .N, dataset]

dataset_ranks <- merge(dataset_ranks, 
                       prec_evap_stats[, .(lon, lat, dataset, pe_ratio_check)],
                       by = c('lon', 'lat', 'dataset'))

# Output =======================================================================

saveRDS(dataset_ranks, file.path(PATH_OUTPUT_DATA, 'dataset_ranks.Rds'))    

# Validation ===================================================================

ggplot(prec_evap_stats) +
  geom_point(aes(x = lon, y = lat, col = pe_ratio_check)) +
  facet_wrap(~dataset)
