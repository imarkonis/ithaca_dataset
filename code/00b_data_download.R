# Downloads data from Zenodo repositories -----
## Requires folder set up from initialize.R
source("code/_source.R")

# libraries ----
library(jsonlite)

## Precipitation datasets -----
PREC_NAMES_SHORT_lcase <- c("era5-land", "fldas", "merra", "terraclimate", "mswep")
PREC_ENSEMBLE_NAMES_SHORT_lcase <- c("cpc", "gpcc", "em-earth", "era5-land", "fldas", "merra", "precl",  "terraclimate")
PREC_ALL_NAMES_SHORT_lcase <- unique(c(PREC_NAMES_SHORT_lcase, PREC_ENSEMBLE_NAMES_SHORT_lcase))
prec_names_grep <- paste0(PREC_ALL_NAMES_SHORT_lcase, collapse = "|")

## Evaporation datasets -----
EVAP_ENSEMBLE_NAMES_SHORT_lcase <- c("bess", "era5-land", "etmonitor", 
                                     "etsynthesis", "fldas", "gleam-v4-1a", "merra", 
                                     "terraclimate")

evap_names_grep <- paste0(EVAP_ENSEMBLE_NAMES_SHORT_lcase, collapse = "|")

# Download data ----
options(timeout = max(600, getOption("timeout")))

## Precipe data ----
doi <- "10.5281/zenodo.14290970"
record_id <- sub(".*zenodo\\.", "", doi)
api_url <- paste0("https://zenodo.org/api/records/", record_id)
record <- fromJSON(api_url, simplifyVector = TRUE)
files <- record$files

selected_files_prec <- files[
  grepl("land", files$key) &
    grepl("yearly", files$key) &
    grepl(prec_names_grep, files$key),
]

for (i in seq_len(nrow(selected_files_prec))) { 
  file <- selected_files_prec[i,]
  file_name <- file$key 
  download_url <- file$links$self 
  out_path <- file.path(PATH_OUTPUT_RAW_PREC, file_name)

  if (file.exists(out_path)) {
    message("Already exists, skipping: ", file_name)
    next
  }

  message("Downloading: ", file_name)
  download.file(url = download_url, destfile = out_path, mode = "wb", quiet = FALSE ) 
}

## Evapore data ----
doi <- "10.5281/zenodo.14622177"
record_id <- sub(".*zenodo\\.", "", doi)
api_url <- paste0("https://zenodo.org/api/records/", record_id)
record <- fromJSON(api_url, simplifyVector = TRUE)
files <- record$files

selected_files_evap <- files[
  grepl("land", files$key) &
    grepl("yearly", files$key) &
    grepl("_e_", files$key) &
    grepl(evap_names_grep, files$key),
]

for (i in seq_len(nrow(selected_files_evap))) { 
  file <- selected_files_evap[i,]
  file_name <- file$key 
  download_url <- file$links$self 
  out_path <- file.path(PATH_OUTPUT_RAW_EVAP, file_name)

  if (file.exists(out_path)) {
    message("Already exists, skipping: ", file_name)
    next
  }

  message("Downloading: ", file_name)
  download.file(url = download_url, destfile = out_path, mode = "wb", quiet = FALSE ) 
}
