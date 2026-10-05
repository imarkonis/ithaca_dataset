Written against commit 646336d. To see which scripts changed since: `git diff --stat 646336d -- code/`

# Script inputs and outputs

For every pipeline script, in run order: what it does, which files it reads and writes, and which columns each file has. Run order and the read/write links come from [pipeline_manifest.md](pipeline_manifest.md). The columns were traced by reading the code, from where they are created (`data.table()`, `.( )`, `:=`, `setnames`, merges and joins, `rbindlist`, `melt`, `dcast`, helper functions) through every change up to the save. The column lists were then checked by running the real code of `00a`, the reshaping part of `01b`, and `01d` to `07b` in a scratch copy of `code/` on a small synthetic data set (500 land cells, made-up values) and comparing every saved file with this report. `07a`, `07b_global` and `07b_regions` needed small test-only patches or stand-in inputs to run (see their notes). `00b`, `01a`, `01c` and the NetCDF reading of `01b` were not run; their descriptions come from the code and `docs/` alone.

## How to read this report

- **Folders.** All folders are under `<PATH_SAVE>/ithaca_dataset/`, as set in `00a`. `PATH_OUTPUT_RAW_PREC`, `PATH_OUTPUT_RAW_EVAP` and `PATH_OUTPUT_RAW_OTHER` are `data/raw/prec`, `data/raw/evap` and `data/raw/other`; `PATH_OUTPUT_INPUT` is `data/input`; `PATH_OUTPUT_OUTPUT` is `data/output`; `PATH_OUTPUT_DATASET` is `data/dataset`; `PATH_OUTPUT_DATA` is `data`.
- **File types.** "data.table" means the file holds one data.table (`.fst` written with `write_fst`, `.Rds` with `saveRDS`).
- **Links.** An input file name links to the description of that file under the script that writes it. A script name links to its section.
- **Meaning and units.** Given only where the code, its comments, `docs/` or `manuscript/manuscript.tex` state them; otherwise "?". Precipitation and evaporation (`prec`, `evap`, `value` of those variables) are annual totals in mm yr⁻¹: `docs/input-dataset-metadata-table.csv` gives mm for the yearly input files, and `04c` writes `mm year-1` into its NetCDF metadata. `01b` itself does not state units. The manuscript text does not yet state any units.
- **Study period.** 1982–2021 (`FULL_PERIOD` in `_source.R`), 0.25° grid, land only (`docs/methodology_long_v1.1.md`). Period 1 is 1982–2001, period 2 is 2002–2021.
- **(check).** Marks anything I am unsure of. All of them are listed at the end.
- **Shared setup.** `_machine_paths.R` defines `PATH_SAVE` for the machine in use (`ACTIVE_MACHINE`). `_source.R` loads data.table, ggplot2 and fst, sources `_machine_paths.R`, loads [`paths.Rdata`](#f_paths_Rdata), and defines the dataset name vectors (`PREC_NAMES_SHORT`, `PREC_ENSEMBLE_NAMES_SHORT`, `EVAP_NAMES_SHORT`, `EVAP_ENSEMBLE_NAMES_SHORT`), the period constants and `MY_PALETTE`. Every script except `00a` sources it; it writes nothing.
- **Not covered.** The parked scripts `code/XX_global_prec_evap.R` and `code/XX_plot_bw_maps.R` and the three scripts in `code/archive/` are not part of the pipeline.

## Scripts

[00a](#s00a) · [00b](#s00b) · [01a](#s01a) · [01b](#s01b) · [01c](#s01c) · [01d](#s01d) · [01e](#s01e) · [01f](#s01f) · [01g](#s01g) · [03a](#s03a) · [03c](#s03c) · [03d](#s03d) · [03e](#s03e) · [03f](#s03f) · [03g](#s03g) · [04a](#s04a) · [04b](#s04b) · [04c](#s04c) · [05a_global](#s05a_global) · [05a_region](#s05a_region) · [07a](#s07a) · [07b_global](#s07b_global) · [07b_regions](#s07b_regions)

---

### <a name="s00a"></a>00a_initialize.R

Defines the project folders under `PATH_SAVE`, creates them, and saves the path constants for all other scripts.

**Inputs**
- `_machine_paths.R` (sourced, not a data file): `PATH_SAVE`.

**Outputs**
- <a name="f_paths_Rdata"></a>`paths.Rdata` (`<PATH_SAVE>/ithaca_dataset/`): R workspace written with `save()`, not a table. It holds 11 character objects: `PATH_OUTPUT`, `PATH_OUTPUT_DATA`, `PATH_OUTPUT_RAW`, `PATH_OUTPUT_RAW_PREC`, `PATH_OUTPUT_RAW_EVAP`, `PATH_OUTPUT_RAW_OTHER`, `PATH_OUTPUT_INPUT`, `PATH_OUTPUT_OUTPUT`, `PATH_OUTPUT_DATASET`, `PATH_OUTPUT_FIGURES`, `PATH_OUTPUT_TABLES`. Read by `_source.R`. The empty folders themselves are also created.

### <a name="s00b"></a>00b_data_download.R

Downloads the raw yearly land precipitation and evaporation NetCDF files from the pRecipe and evapoRe Zenodo records, plus GPCC and the MERRA-2/MSWX PET table from the ITHACA Zenodo record, and checks each file against its Zenodo size.

**Inputs**
- Zenodo records pRecipe `14290970`, evapoRe `21369628`, ITHACA `21353180` (web, not files from a script).
- [`paths.Rdata`](#f_paths_Rdata) through `_source.R` (from [00a](#s00a)).

**Outputs**
- <a name="f_raw_prec_nc"></a>`*.nc` in `PATH_OUTPUT_RAW_PREC`: raw yearly land precipitation NetCDFs, named `<dataset>_tp_mm_land_<start>_<end>_025_yearly.nc`: the pRecipe files whose names contain `land`, `yearly` and one of era5-land, fldas, merra, terraclimate, mswep, cpc, em-earth, precl, plus `gpcc-v2022_tp_mm_land_198101_202012_025_yearly.nc` from the ITHACA record. NetCDF, so no columns; the variable and dimension names inside are those of the Zenodo files and are not defined in this repo (check).
- <a name="f_raw_evap_nc"></a>`*.nc` in `PATH_OUTPUT_RAW_EVAP`: raw yearly land evaporation NetCDFs, named `<dataset>_e_mm_land_<start>_<end>_025_yearly.nc`: the evapoRe files whose names contain `land`, `yearly`, `_e_` and one of bess, era5-land, etmonitor, etsynthesis, fldas, gleam-v4-1a, merra, terraclimate. Same remark on the contents (check).
- `merra2_mswx_pet_mm_1980_2024_yearly.fst` in `PATH_OUTPUT_RAW_OTHER`: downloaded from the ITHACA record. It is the file that [01d](#s01d) writes; columns are described [there](#f_pet_yearly_fst).

### <a name="s01a"></a>01a_crop_data.R

Crops every raw yearly NetCDF to 1982–2021 and saves the cropped copy in `PATH_OUTPUT_INPUT`, leaving the raw files untouched.

**Inputs**
- [`*.nc` in `PATH_OUTPUT_RAW_PREC`](#f_raw_prec_nc) and [`*.nc` in `PATH_OUTPUT_RAW_EVAP`](#f_raw_evap_nc), from [00b](#s00b).

**Outputs**
- <a name="f_cropped_nc"></a>`*.nc` in `PATH_OUTPUT_INPUT`: one cropped NetCDF per raw file, with the same name except that the period in the name is rewritten to the years the file now contains (for example GPCC `gpcc-v2022_tp_mm_land_198201_202012_025_yearly.nc`, which ends in 2020 because the raw file does). Same variables as the raw file, time subset to `FULL_PERIOD`. The file names are built at run time, and the variable names are written by pRecipe's `subset_data` and `saveNC`, not by this repo (check).

### <a name="s01b"></a>01b_prepare_prec_evap.R

Reads each cropped NetCDF into a long land-only table, keeps the grid cells covered by every dataset, pairs MSWEP precipitation with GLEAM evaporation, and widens precipitation and evaporation into one row per cell, year and dataset.

**Inputs**
- [`*.nc` in `PATH_OUTPUT_INPUT`](#f_cropped_nc), from [01a](#s01a). Files with `_e_` in the name are evaporation, all others precipitation; the dataset is taken from the name token (cpc, gpcc, em-earth, era5-land, fldas, merra, precl, terraclimate, mswep, bess, etmonitor, etsynthesis, gleam-v4-1a).

**Outputs** (all in `PATH_OUTPUT_OUTPUT`)
- <a name="f_prec_evap_raw_fst"></a>`prec_evap_raw.fst`: data.table, long, with lon (degrees east, cell centre), lat (degrees north), year (integer), dataset (short name: CPC, GPCC, EARTH, ERA5L, FLDAS, MERRA, PRECL, TERRA, MSWEP for precipitation; BESS, ERA5L, ETMON, ETSYN, FLDAS, GLEAM, MERRA, TERRA for evaporation; whichever of these files are in `PATH_OUTPUT_INPUT`), variable (`"prec"` or `"evap"`) and value (annual precipitation or evaporation, mm yr⁻¹, rounded to whole numbers). Land cells covered by every dataset only (check). Sorted by variable, dataset, lon, lat, year.
- <a name="f_twc_complete_grid_Rds"></a>`twc_complete_grid.Rds`: data.table with lon, lat: the cells kept above (the "TWC grid").
- <a name="f_prec_evap_fst"></a>`prec_evap.fst`: data.table, wide, with lon, lat, year, dataset, prec (annual precipitation, mm yr⁻¹), evap (annual evaporation, mm yr⁻¹). `dataset` is one of ERA5L, FLDAS, MERRA, TERRA, GLEAM, where GLEAM is MSWEP precipitation paired with GLEAM evaporation. Only rows with both `prec` and `evap`. Sorted by dataset, lon, lat, year.
- The header of the script says it saves `prec_evap.Rds`; the code writes `prec_evap.fst` (check).

### <a name="s01c"></a>01c_prepare_pet_forcing.R

Converts the monthly MERRA-2 and MSWX-Past forcing NetCDFs (plus ERA5-Land albedo for MSWX-Past) into one monthly data.table per source, clipped to 1980–2024.

**Inputs**
- 18 monthly 0.25° NetCDFs in `PATH_OUTPUT_RAW_OTHER`, named in `MERRA2_FILES` (9 files) and `MSWX_FILES` (8 MSWX-Past files plus `era5-land_albedo_198001_202501_025_monthly.nc`). No script in the repo writes them and `00b` does not download them (check). `docs/pet_input.md` describes them and `docs/input-dataset-metadata-table.csv` links some of them to Zenodo records.

**Outputs** (both in `PATH_OUTPUT_RAW_OTHER`; units are the ones in the input file names, as the script's comment says; rows with any missing variable are dropped)
- <a name="f_forcing_merra2"></a>`merra2_pet-forcing_mixed_1980_2024_025_monthly.fst`: data.table keyed on x, y, date, with x (longitude, rounded to 4 decimals), y (latitude), date (Date, one per month, 1980–2024), tavg (2 m mean air temperature, °C), tmax (2 m daily maximum, °C), tmin (2 m daily minimum, °C), sw_rad (downwelling shortwave, W m⁻²), lw_rad (downwelling longwave, W m⁻²), pres (surface pressure, Pa), u_wind (2 m eastward wind, m s⁻¹), v_wind (2 m northward wind, m s⁻¹), spec_hum (2 m specific humidity, kg kg⁻¹). The comment in the script says every date is moved to the first of its month before merging; the code only converts it with `as.Date` (check).
- <a name="f_forcing_mswx"></a>`mswx-past_pet-forcing_mixed_1980_2024_025_monthly.fst`: data.table keyed on x, y, date, with x, y, date as above, tavg, tmax, tmin, sw_rad, lw_rad, pres as above, wind_speed (10 m wind speed, m s⁻¹), rel_hum (2 m relative humidity, %), albedo (surface albedo from ERA5-Land, dimensionless 0–1).

### <a name="s01d"></a>01d_estimate_pet.R

Estimates yearly potential evapotranspiration for every grid cell with six methods (Hargreaves-Samani, Oudin, Hamon, energy-only, Priestley-Taylor, FAO-56 Penman-Monteith) from each of the two forcing tables.

**Inputs**
- [`merra2_pet-forcing_mixed_1980_2024_025_monthly.fst`](#f_forcing_merra2) and [`mswx-past_pet-forcing_mixed_1980_2024_025_monthly.fst`](#f_forcing_mswx) in `PATH_OUTPUT_RAW_OTHER`, from [01c](#s01c).

**Outputs**
- <a name="f_pet_yearly_fst"></a>`merra2_mswx_pet_mm_1980_2024_yearly.fst` (`PATH_OUTPUT_RAW_OTHER`): data.table keyed on source, x, y, date, with date (Date, 1 January of each year, 1980–2024), x (longitude), y (latitude), source (`"merra2"` or `"mswx-past"`, the forcing the row was computed from) and six PET columns, one per method, each the yearly total in mm rounded to 0.1: pet_hs (Hargreaves-Samani), pet_od (Oudin), pet_hm (Hamon), pet_eop (energy-only), pet_pt (Priestley-Taylor), pet_pm (FAO-56 Penman-Monteith). Two sources times six methods give the 12 PET combinations used in [03d](#s03d). `00b` downloads the same file.

### <a name="s01e"></a>01e_prepare_pet.R

Restricts the yearly PET table to 1982–2021 and the TWC grid, reshapes it to long format by method, and computes full-period means per cell, source and method.

**Inputs**
- [`merra2_mswx_pet_mm_1980_2024_yearly.fst`](#f_pet_yearly_fst) in `PATH_OUTPUT_RAW_OTHER`, from [01d](#s01d) (or downloaded by [00b](#s00b)).
- [`twc_complete_grid.Rds`](#f_twc_complete_grid_Rds), from [01b](#s01b).

**Outputs** (in `PATH_OUTPUT_OUTPUT`; x, y and source of the input are renamed lon, lat, dataset)
- <a name="f_pet_fst"></a>`pet.fst`: data.table, long, with lon, lat, year (1982–2021), dataset (`"merra2"` or `"mswx-past"`), method (one of pet_hs, pet_od, pet_hm, pet_eop, pet_pt, pet_pm, as character) and value (yearly PET, mm). TWC grid cells only. Not read by any later script.
- <a name="f_pet_mean_fst"></a>`pet_mean.fst`: data.table with lon, lat, dataset, method, value (1982–2021 mean of the yearly PET, mm yr⁻¹).

### <a name="s01f"></a>01f_prepare_validation_ensemble.R

For each of the five analysis datasets, builds a leave-one-out reference from the 8-member precipitation and evaporation ensembles (per-cell mean, SD, Sen slope and Mann-Kendall significance) and attaches the dataset's own values.

**Inputs**
- [`prec_evap_raw.fst`](#f_prec_evap_raw_fst), from [01b](#s01b).

**Outputs** (in `PATH_OUTPUT_OUTPUT`)
- <a name="f_reference_values"></a>`prec_reference_values.fst` and `evap_reference_values.fst`: two data.tables with the same 23 columns, one row per candidate dataset and grid cell, sorted by candidate_dataset, lon, lat. In the precipitation table the reference ensemble is CPC, GPCC, EARTH, ERA5L, FLDAS, MERRA, PRECL, TERRA; in the evaporation table it is BESS, ERA5L, ETMON, ETSYN, FLDAS, GLEAM, MERRA, TERRA. A candidate is removed from its own reference when it is a member (so GLEAM is not removed from the precipitation reference). Columns:
  - candidate_dataset: the analysis dataset being evaluated (ERA5L, FLDAS, MERRA, TERRA, GLEAM);
  - candidate_value_dataset: the dataset whose values are used for it (the same name, except in `prec_reference_values.fst`, where GLEAM uses MSWEP);
  - lon, lat;
  - candidate_mean, candidate_sd: mean and SD of the candidate's yearly values over 1982–2021 (same units as the values); candidate_n_years_mean: number of finite yearly values;
  - candidate_sen_slope: Sen slope of the yearly values, per year; candidate_p_value: Mann-Kendall p-value; candidate_stat_sig: TRUE if p < 0.1; candidate_n_years_slope: years used. Slope and p-value are NA when fewer than 35 valid years or all values equal;
  - ref_mean_median, ref_mean_iqr: median and IQR, across the reference datasets, of their per-cell means; ref_sd_median, ref_sd_iqr: the same for their SDs; ref_slope_median, ref_slope_iqr: the same for their Sen slopes;
  - n_significant: number of reference datasets with a significant slope (p < 0.1); n_pos, n_neg: how many of those have a positive or negative slope;
  - majority_significant: TRUE if n_significant is more than half the reference datasets; majority_agrees: TRUE if majority_significant and the larger of n_pos and n_neg is more than half of n_significant; majority_sign: 1 if n_pos > n_neg, -1 if n_neg > n_pos, otherwise 0.

### <a name="s01g"></a>01g_set_region_classes.R

Attaches IPCC AR6 region, biome, Köppen-Geiger and land-cover classes (from the pRecipe masks) and area weights to every TWC grid cell, derives hemisphere and latitude zone, and finds the dominant class of each region.

**Inputs**
- [`twc_complete_grid.Rds`](#f_twc_complete_grid_Rds), from [01b](#s01b).
- `pRecipe_masks()` from the pRecipe package (not a file in the pipeline).

**Outputs** (in `PATH_OUTPUT_OUTPUT`)
- <a name="f_grid_classes_Rds"></a>`grid_classes.Rds`: data.table, one row per TWC grid cell, with lon, lat, region (IPCC AR6 short code, e.g. MED), region_full (region name), continent (IPCC continent), biome (pRecipe short biome class, e.g. "B. Forests"), kg_class (Köppen-Geiger class), kg_class_1 (Köppen-Geiger level-1 class: A to E, and "Ocean" for some cells that the mask calls land), land_cover (land-cover class), cell_weight (cos(lat), relative cell area), area_weight (cell_weight divided by its sum over the whole table, so all rows sum to 1), hemisphere (factor: north, south), lat_zone (factor on |lat|: highlatitude ≥ 60, midlatitude ≥ 35, subtropical ≥ 23.5, tropical). Ocean regions (code ending in `O`) and BOB, ARS, GIC are removed. Cells without a mask match keep NA in the class columns (check). The class values come from `pRecipe_masks()` and may differ between pRecipe versions (check). The code labels `kg_class_1` "level-1" in its header and "level 3" in a plot (check).
- <a name="f_region_classes_Rds"></a>`region_classes.Rds`: data.table, one row per region, with region, region_full, continent, hemisphere, lat_zone, biome, kg_class_1, kg_class, land_cover (the class covering the largest `cell_weight` in the region), and hemisphere_share, lat_zone_share, biome_share, kg_class_share, kg_class_1_share, land_cover_share (fraction of the region's classified `cell_weight` in that dominant class). Not read by any later script. The hemisphere here is north/south, while `docs/methodology_long_v1.1.md` describes north/tropics/south from the latitudinal extent of each region (check).

### <a name="s03a"></a>03a_dataset_ranking.R

Compares each candidate's per-cell mean, SD and Sen slope with its leave-one-out reference, scores the relative bias and the agreement on trend significance, and ranks the five datasets within each cell.

**Inputs**
- [`prec_reference_values.fst` and `evap_reference_values.fst`](#f_reference_values), from [01f](#s01f).

**Outputs** (in `PATH_OUTPUT_OUTPUT`)
- <a name="f_dataset_ranks_03a"></a>`dataset_ranks.Rds` (03a version, written here and rewritten by [03c](#s03c) and [03d](#s03d)): data.table, 23 columns, one row per cell and dataset, sorted by lon, lat, dataset: lon, lat, dataset (factor: ERA5L, FLDAS, GLEAM, MERRA, TERRA), then the evaporation block, then the same ten columns for precipitation.
  - Evaporation block: evap_value_dataset (dataset whose evaporation values were used); evap_mean_bias, evap_sd_bias (relative absolute bias of the candidate's mean and SD against the reference median, |candidate - reference| / (|reference| + 1e-6), dimensionless, smaller is closer); evap_bias_slope (the same relative absolute bias for the Sen slope); evap_diff_slope (absolute difference between the candidate's slope and the reference median slope, in slope units); evap_mean_rank, evap_sd_rank, evap_rank_slope (rank among the datasets in the cell, 1 = smallest value, ties take the lowest rank, NA stays NA; the slope rank is ranked on evap_diff_slope, not evap_bias_slope); evap_check_significance (when a reference majority has a significant trend of one sign: TRUE if the candidate's trend is significant, FALSE if not, NA otherwise); evap_check_non_significance (when no reference majority has a significant trend: TRUE if the candidate's trend is not significant, FALSE if it is, NA otherwise).
  - Precipitation block: prec_value_dataset (MSWEP for GLEAM, otherwise the dataset itself), prec_mean_bias, prec_sd_bias, prec_bias_slope, prec_diff_slope, prec_mean_rank, prec_sd_rank, prec_rank_slope, prec_check_significance, prec_check_non_significance, defined as above.
- <a name="f_prec_evap_stats_Rds"></a>`prec_evap_stats.Rds`: data.table, 25 columns, one row per cell and dataset, sorted by lon, lat, dataset: lon, lat, dataset (factor), then evap_value_dataset, evap_mean, evap_sd, evap_n_years_mean, evap_sen_slope, evap_p_value, evap_stat_sig, evap_n_years_slope, evap_ref_mean_median, evap_ref_sd_median, evap_ref_slope_median, then the same eleven columns with the prefix `prec_`. They are the candidate's own statistics and the reference medians from [01f](#s01f) (columns without the `candidate_` prefix).

### <a name="s03c"></a>03c_dataset_aridity_check.R

Flags the cells where a dataset's mean precipitation divided by its mean evaporation is below 0.9, and adds the flag to `dataset_ranks.Rds`.

**Inputs**
- [`prec_evap_stats.Rds`](#f_prec_evap_stats_Rds) and [`dataset_ranks.Rds`](#f_dataset_ranks_03a) (03a version), from [03a](#s03a).

**Outputs**
- <a name="f_dataset_ranks_03c"></a>`dataset_ranks.Rds` (03c version, overwrites the 03a file in place): the 23 columns of the 03a version plus one last column, pe_ratio_check (logical: TRUE where prec_mean / evap_mean is 0.9 or more, FALSE where it is below `ARIDITY_THRES = 0.9`). 24 columns. The ratio itself and the flag on `prec_evap_stats.Rds` are computed in memory and not saved.

### <a name="s03d"></a>03d_pet_check.R

Counts, for each cell and dataset, in how many of the 12 PET products the dataset's mean evaporation is below the PET, and adds the count to `dataset_ranks.Rds`.

**Inputs**
- [`prec_evap_stats.Rds`](#f_prec_evap_stats_Rds), from [03a](#s03a).
- [`pet_mean.fst`](#f_pet_mean_fst), from [01e](#s01e).
- [`dataset_ranks.Rds`](#f_dataset_ranks_03c) (03c version), from [03c](#s03c).

**Outputs**
- <a name="f_dataset_ranks_03d"></a>`dataset_ranks.Rds` (03d version, overwrites the 03c file in place): the 24 columns of the 03c version plus one last column, n_below_pet (integer, 0 to 12: the number of PET source and method combinations for which the dataset's evap_mean is below the PET mean). 25 columns. The rows are those of the `pet_check` table the script joins on, so a cell without PET data would drop out (check). If 03c was not run first the file has no pe_ratio_check, and [03e](#s03e) stops.

### <a name="s03e"></a>03e_dataset_agreement_weights.R

Drops the dataset-cells that fail the physics filter (pe_ratio_check is TRUE and n_below_pet is above 6) and turns the ranks, biases and significance checks of the rest into per-cell dataset sampling probabilities under ten weighting scenarios.

**Inputs**
- [`dataset_ranks.Rds`](#f_dataset_ranks_03d) (03d version), from [03d](#s03d).

**Outputs** (in `PATH_OUTPUT_OUTPUT`)
- <a name="f_dataset_weights_Rds"></a>`dataset_weights.Rds`: data.table with lon, lat, dataset (factor), weight (probability of sampling the dataset in the cell; the weights of the datasets in a cell sum to 1 within each scenario; the code returns NA when no component is available), scenario (character: base, change, trend_dominant, clim_dominant, evap_dominant, prec_dominant, rank_linear, rank_exp, inverted, neutral). Only dataset-cells that passed the filter. The documentation describes eight scenarios (check).
- <a name="f_dataset_weights_base_detailed_Rds"></a>`dataset_weights_base_detailed.Rds`: data.table, base scenario only, with lon, lat, dataset, and then the weights at each step of the hierarchy (all per-cell probabilities over the datasets in the cell): weight_prec_mean, weight_prec_sd, weight_evap_mean, weight_evap_sd, weight_prec_slope, weight_evap_slope (inverse-loss probabilities from prec_mean_bias, prec_sd_bias, evap_mean_bias, evap_sd_bias, prec_bias_slope, evap_bias_slope); weight_prec_sig, weight_evap_sig (from the significance checks: agreement scores 1, disagreement 0.1); weight_prec_clim, weight_evap_clim (mean and SD combined); weight_prec_trend, weight_evap_trend (significance and slope combined); weight_prec, weight_evap (climatology and trend combined); weight (precipitation and evaporation combined, the base `weight` of `dataset_weights.Rds`). All shares are 0.5 in the base scenario.
- <a name="f_dataset_weight_diagnostics_Rds"></a>`dataset_weight_diagnostics.Rds`: data.table with scenario, lon, lat, n_candidates (datasets with a finite weight in the cell), dataset_top (the dataset with the highest weight), weight_top (its weight), eff_n (effective number of datasets, 1 / sum of squared weights).

### <a name="s03f"></a>03f_estimate_regional_weights.R

Aggregates the grid-cell dataset weights to IPCC region × biome probabilities, weighting by cell area and dividing by the area of all cells retained in the unit.

**Inputs**
- [`dataset_weights.Rds`](#f_dataset_weights_Rds), from [03e](#s03e).
- [`grid_classes.Rds`](#f_grid_classes_Rds), from [01g](#s01g).

**Outputs**
- <a name="f_weights_region_biome_Rds"></a>`weights_region_biome.Rds` (`PATH_OUTPUT_OUTPUT`): data.table sorted by scenario, region, biome, dataset, with scenario, region (IPCC short code), biome, dataset (factor) and w_region_biome (probability that the dataset is chosen in that region-biome unit under that scenario; sums to 1 over the datasets of a scenario, region and biome). Cells with a missing region or biome are left out.

### <a name="s03g"></a>03g_region_biome_prec_evap.R

Estimates the area-weighted annual mean precipitation and evaporation of each dataset in each IPCC region × biome.

**Inputs**
- [`prec_evap.fst`](#f_prec_evap_fst), from [01b](#s01b).
- [`grid_classes.Rds`](#f_grid_classes_Rds), from [01g](#s01g) (lon, lat, region, biome, area_weight used).

**Outputs**
- <a name="f_dataset_region_biome_year_Rds"></a>`dataset_region_biome_year.Rds` (`PATH_OUTPUT_OUTPUT`): data.table keyed on dataset, region, biome, year, with those four columns and prec (area-weighted mean of the unit's cells, mm yr⁻¹) and evap (the same for evaporation, mm yr⁻¹). Cells with a missing region or biome are not removed here, so such groups may exist (check).

### <a name="s04a"></a>04a_run_mc_simulation.R

Draws one uniform random number for each simulation and region-biome unit and uses it to pick a dataset from each scenario's probabilities: 100 simulations for every scenario and 1000 for the change scenario.

**Inputs**
- [`weights_region_biome.Rds`](#f_weights_region_biome_Rds), from [03f](#s03f).

**Outputs** (in `PATH_OUTPUT_OUTPUT`; scenario, region, biome and dataset are character columns)
- <a name="f_weight_cdf_Rds"></a>`weight_cdf_scenarios.Rds`: data.table keyed on scenario, region, biome, dataset, with those four columns, w_region_biome (as in 03f), p_low, p_high (the cumulative probability interval of the dataset, datasets in alphabetical order, the last p_high is exactly 1).
- <a name="f_mc_random_Rds"></a>`mc_random_region_biome_scenarios.Rds`: data.table keyed on region, biome, sim, with region, biome, sim (1 to 100) and u (uniform random number in [0, 1), the same for all scenarios).
- <a name="f_mc_selection_scenarios_Rds"></a>`mc_selection_scenarios.Rds`: data.table keyed on sim, scenario, region, biome, with those four columns, dataset (the dataset selected, the one whose [p_low, p_high) contains u) and u. One row per simulation, scenario, region and biome.
- <a name="f_mc_selection_matrix_Rds"></a>`mc_selection_matrix_scenarios.Rds`: data.table with sim, region, biome, u, then one column per scenario (the ten scenario names, alphabetically), each holding the dataset selected in that scenario.
- <a name="f_mc_selection_change_Rds"></a>`mc_selection_change.Rds`: data.table with sim (1 to 1000), region, biome, dataset, u: the change scenario only. Its first 100 simulations equal the change rows of `mc_selection_scenarios.Rds`. The documentation describes a 500-member base ensemble and a 100-member all-scenario ensemble (check).

### <a name="s04b"></a>04b_create_aggregated_mc_ensemble.R

Joins the selected datasets to the dataset-region-biome-year values and area-weights them to region × biome × year, region × year and global × year ensembles.

**Inputs**
- [`dataset_region_biome_year.Rds`](#f_dataset_region_biome_year_Rds), from [03g](#s03g).
- [`grid_classes.Rds`](#f_grid_classes_Rds), from [01g](#s01g) (cell_weight summed per region-biome as the area weight).
- [`mc_selection_scenarios.Rds`](#f_mc_selection_scenarios_Rds), from [04a](#s04a).

**Outputs** (in `PATH_OUTPUT_OUTPUT`)
- <a name="f_mc_region_biome_year_fst"></a>`mc_region_biome_year_scenarios.fst`: data.table with sim, scenario, region, biome, dataset (the dataset selected for that simulation, scenario and unit), year, prec, evap (that dataset's values for the unit, mm yr⁻¹).
- `mc_region_year_scenarios.Rds`: data.table with sim, scenario, region, year, prec, evap (area-weighted mean over the region's biomes, mm yr⁻¹).
- `mc_global_year_scenarios.Rds`: data.table with sim, scenario, year, prec, evap (area-weighted mean over all region-biome units, mm yr⁻¹).

### <a name="s04c"></a>04c_create_gridded_mc_ensemble.R

Writes the public BASE and CHANGE land-cell ensembles as 100 NetCDF members per scenario, where each region-biome unit takes its precipitation and evaporation from the dataset selected for that member in 04a.

**Inputs**
- [`prec_evap.fst`](#f_prec_evap_fst), from [01b](#s01b).
- [`grid_classes.Rds`](#f_grid_classes_Rds), from [01g](#s01g) (lon, lat, region, biome used).
- [`mc_selection_scenarios.Rds`](#f_mc_selection_scenarios_Rds), from [04a](#s04a) (scenarios base and change, simulations 1 to 100).

**Outputs** (in `PATH_OUTPUT_DATASET`)
- <a name="f_ensemble_nc"></a>`base/twc_base_001.nc` to `base/twc_base_100.nc` and `change/twc_change_001.nc` to `change/twc_change_100.nc`: 200 NetCDF files. Member *n* of base and change use the same random draw. Dimensions: cell (1 to the number of land cells, sorted by lat then lon) and year (1982–2021). Variables: lon[cell] (degrees_east), lat[cell] (degrees_north), source_dataset_id[cell] (dataset selected for the cell, see `dataset_lookup.csv`), region_id[cell] (see `region_lookup.csv`), biome_id[cell] (see `biome_lookup.csv`), precipitation[cell, year] (annual precipitation, mm year-1), evaporation[cell, year] (annual actual evaporation, mm year-1). The global attributes are title, scenario, ensemble_member, paired_member, selection_scale, the three lookup file names and weighting_definition. The file names are built at run time. The header says `N_MEMBERS = 3` is used for testing; the code sets 100 (check).
- `dataset_lookup.csv`: dataset_id (1, 2, ...), dataset (the datasets selected in base or change, alphabetical).
- `region_lookup.csv`: region_id, region (the regions of the published cells, alphabetical).
- `biome_lookup.csv`: biome_id, biome (the biomes of the published cells, alphabetical).
- `manifest.csv`: scenario (base, change), member (1 to 100), file (path relative to `PATH_OUTPUT_DATASET`), paired_member (equal to member).

### <a name="s05a_global"></a>05a_plot_p_e_global.R

Compares the original datasets and the Monte Carlo scenarios in P-E and ΔP-ΔE space for the globe and each hemisphere.

**Inputs**
- [`dataset_region_biome_year.Rds`](#f_dataset_region_biome_year_Rds), from [03g](#s03g).
- [`grid_classes.Rds`](#f_grid_classes_Rds), from [01g](#s01g) (cell_weight summed per region-biome, global and by hemisphere of the cell).
- [`mc_region_biome_year_scenarios.fst`](#f_mc_region_biome_year_fst), from [04b](#s04b).

**Outputs** (all in `PATH_OUTPUT_OUTPUT`, including the two figures). Early period = the first half of the years common to the dataset and Monte Carlo tables (1982–2001), late period = the second half (2002–2021); dprec and devap are late minus early means (mm yr⁻¹).
- `pe_dataset_vs_scenarios.png` and `delta_pe_dataset_vs_scenarios.png`: figures. The hemisphere panels use the whole region-biome mean weighted by the area the unit has in that hemisphere (check).
- `dataset_scope_mean.Rds`: data.table with dataset, scope (factor: Global, Northern Hemisphere, Southern Hemisphere), prec, evap (1982–2021 mean of the yearly area-weighted values).
- `mc_scope_mean_scenarios.Rds`: data.table with sim, scenario (factor), scope (factor), prec, evap (same, per Monte Carlo member).
- `dataset_scope_change.Rds`: data.table with dataset, scope, prec_early, prec_late, evap_early, evap_late (period means), dprec, devap (late minus early).
- `mc_scope_change_scenarios.Rds`: data.table with sim, scenario, scope, prec_early, prec_late, evap_early, evap_late, dprec, devap.
- `mc_scope_mean_centroids.Rds`: data.table with scenario, scope, prec, evap (mean over the simulations of `mc_scope_mean_scenarios.Rds`).
- `mc_scope_change_centroids.Rds`: data.table with scenario, scope, dprec, devap (mean over the simulations).

### <a name="s05a_region"></a>05a_plot_p_e_region.R

Compares the original datasets and the Monte Carlo scenarios in P-E and ΔP-ΔE space for each IPCC region.

**Inputs**
- [`dataset_region_biome_year.Rds`](#f_dataset_region_biome_year_Rds), from [03g](#s03g).
- [`grid_classes.Rds`](#f_grid_classes_Rds), from [01g](#s01g) (cell_weight summed per region-biome).
- [`mc_region_biome_year_scenarios.fst`](#f_mc_region_biome_year_fst), from [04b](#s04b).

**Outputs** (all in `PATH_OUTPUT_OUTPUT`, including the two figures; early and late periods and dprec, devap as in [05a_global](#s05a_global))
- `pe_dataset_vs_scenarios_ipcc_regions.png` and `delta_pe_dataset_vs_scenarios_ipcc_regions.png`: figures.
- `dataset_region_mean.Rds`: data.table with dataset, region (factor), prec, evap (1982–2021 mean of the yearly area-weighted values).
- `mc_region_mean_scenarios.Rds`: data.table with sim, scenario (factor), region (factor), prec, evap.
- `dataset_region_change.Rds`: data.table with dataset, region, prec_early, prec_late, evap_early, evap_late, dprec, devap.
- `mc_region_change_scenarios.Rds`: data.table with sim, scenario, region, prec_early, prec_late, evap_early, evap_late, dprec, devap.

### <a name="s07a"></a>07a_estimate_scenario_means.R

Builds, for each weighting scenario, the weighted mean precipitation and evaporation over the datasets (using the region-biome probabilities as fixed weights) at grid-cell, region-biome and region level.

This script cannot run as written: it reads `prec_evap.Rds` and `twc_grid_classes.Rds`, which no script writes, and `weights_region_biome.Rds` from `PATH_OUTPUT_DATA` while [03f](#s03f) writes it to `PATH_OUTPUT_OUTPUT` (see the manifest). The columns below assume it reads the files the manifest names as the closest matches: `prec_evap.fst` for the first and `grid_classes.Rds` for the second (check).

**Inputs**
- `prec_evap.Rds` (`PATH_OUTPUT_OUTPUT`; not written by any script): lon, lat, year, dataset, prec, evap are used. Probably meant to be [`prec_evap.fst`](#f_prec_evap_fst) from [01b](#s01b) (check).
- [`weights_region_biome.Rds`](#f_weights_region_biome_Rds) (read from `PATH_OUTPUT_DATA`), from [03f](#s03f).
- `twc_grid_classes.Rds` (`PATH_OUTPUT_DATA`; not written by any script): lon, lat, region, biome are used. Probably meant to be [`grid_classes.Rds`](#f_grid_classes_Rds) from [01g](#s01g) (check).

**Outputs** (in `PATH_OUTPUT_DATA`; prec and evap are weighted means over datasets, mm yr⁻¹; cell_area is cos(lat))
- `scenario_prec_evap_grid.Rds`: data.table keyed on scenario, lon, lat, year, with scenario, lon, lat, year, region, biome, prec, evap, cell_area.
- `scenario_prec_evap_region_biome.Rds`: data.table keyed on scenario, region, biome, year, with those four columns, prec, evap (cell_area-weighted means of the grid values), area_sum (summed cell_area of the cells with a finite prec or evap), n_cells (number of cells).
- <a name="f_scenario_region"></a>`scenario_prec_evap_region.Rds`: data.table keyed on scenario, region, year, with those three columns, prec, evap (area_sum-weighted means over the region's biomes), area_sum (summed), n_region_biomes (number of biomes), n_cells (summed).

### <a name="s07b_global"></a>07b_sensitivity_analysis_scenarions_global.R

Plots the change in water availability, Δ(P−E), against the change in flux, Δ((P+E)/2), for the original datasets, the weighting scenarios and the unweighted dataset mean.

This script cannot run as written: it reads three files that no script writes, and uses `PALETTES` and `PATH_FIGURES`, which are defined nowhere (see the manifest). The input columns below are inferred from how the script uses them (check).

**Inputs** (all in `PATH_OUTPUT_DATA`, none written by any script)
- `scenario_global_yearly_prec_evap.Rds`: scenario, year, prec, evap (global yearly values per weighting scenario) (check).
- `dataset_global_yearly_prec_evap.Rds`: dataset, year, prec, evap (global yearly values per dataset) (check).
- `dataset_unweighted_mean_global_yearly_prec_evap.Rds`: series, year, prec, evap (global yearly unweighted dataset mean) (check).

**Outputs**
- `global_avail_flux_change_plot_dt.Rds` (`PATH_OUTPUT_DATA`): data.table with id (scenario name, dataset name or series; only the eight scenarios that have a label in the script, so not `change` or `trend_dominant`), label (display name), type ("Scenario", "Dataset" or "Unweighted mean"), avail_change (period 2 mean minus period 1 mean of P−E, mm yr⁻¹), flux_change (the same for (P+E)/2, mm yr⁻¹), prec_change, evap_change (the same for P and E), storyline ("Wet and accelerated", "Dry and accelerated", "Wet and decelerated" or "Dry and decelerated", from the signs of avail_change and flux_change).
- `global_mean_avail_flux_change_space.png` (`PATH_FIGURES`, undefined) (check): figure.

### <a name="s07b_regions"></a>07b_sensitivity_analysis_scenarions_regions.R

Plots the regional change in water availability against the change in flux for the original datasets, the weighting scenarios and the unweighted dataset mean, nine regions per page.

This script cannot run as written: one input is written by no script and `PALETTES` is defined nowhere (see the manifest). The columns of that input are inferred from how the script uses it (check).

**Inputs**
- [`scenario_prec_evap_region.Rds`](#f_scenario_region) (`PATH_OUTPUT_DATA`), from [07a](#s07a): scenario, region, year, prec, evap are used.
- `dataset_region_yearly_prec_evap.Rds` (`PATH_OUTPUT_DATA`; not written by any script): dataset, region, year, prec, evap (regional yearly values per dataset) (check).

**Outputs** (in `PATH_OUTPUT_DATA`; the figures are only printed, not saved)
- `dataset_region_unweighted_mean_yearly_prec_evap.Rds`: data.table with region, year, prec, evap (mean over the datasets), n_datasets (number of datasets), series (always "Mean").
- `regional_avail_flux_change_plot_dt.Rds`: data.table with region (factor), id (scenario name, dataset name or "unweighted_mean"; only the eight labelled scenarios, as in 07b_global), label, type ("Scenario", "Dataset" or "Unweighted mean"), avail_change, flux_change, prec_change, evap_change (period 2 minus period 1, as in [07b_global](#s07b_global)), storyline (as in 07b_global). Rows with a non-finite avail_change or flux_change are removed.

---

## Everything marked (check)

- **00b, raw NetCDF contents:** the variable and dimension names inside the downloaded NetCDFs are those of the Zenodo files and are not defined in this repo.
- **01a, cropped NetCDF contents:** the cropped files' variable names are written by pRecipe's `subset_data` and `saveNC`, not by this repo.
- **01b, coverage test:** `prec_evap_raw.fst` and `twc_complete_grid.Rds` keep the cells covered by every dataset, but the code counts distinct dataset names across precipitation and evaporation together (`uniqueN(dataset)`), so a cell missing, say, ERA5L precipitation but having ERA5L evaporation still counts ERA5L.
- **01b, header comment:** says `prec_evap.Rds` is saved; the code writes `prec_evap.fst`.
- **01c, input files:** the 18 forcing NetCDFs are not written by any script and `00b` does not download them.
- **01c, dates:** the comment says dates are moved to the first of their month before merging; the code only converts them with `as.Date`.
- **01g, NA classes:** cells of the TWC grid with no match in the pRecipe land mask keep NA in region and the other class columns, and the region filter does not remove them.
- **01g, pRecipe values:** the class values (region codes, biome names, Köppen-Geiger and land-cover classes) come from `pRecipe_masks()` and may differ between pRecipe versions.
- **01g, `kg_class_1` label:** "level-1" in the header, "level 3" in a plot label.
- **01g, hemisphere:** the code gives north/south from the latitude of each cell, while `docs/methodology_long_v1.1.md` §4 describes north/tropics/south from each region's latitudinal extent.
- **03d, row set:** the output keeps the rows of `pet_check`, so cells without PET data in `pet_mean.fst` would drop out of `dataset_ranks.Rds`.
- **03e, scenario count:** the code defines ten scenarios (including `change` and `trend_dominant`); `docs/methodology_long_v1.1.md` describes eight.
- **03g, NA region or biome:** unlike 03f, this script does not remove cells with a missing region or biome, so such groups may be in `dataset_region_biome_year.Rds`.
- **04a, ensemble sizes:** `docs/methodology_long_v1.1.md` §8 describes a 500-member base ensemble and a 100-member all-scenario ensemble; the code makes 100 simulations for all scenarios and 1000 for `change`.
- **04c, member count:** the header says `N_MEMBERS = 3` is used for testing; the code sets 100.
- **05a_global, hemispheres:** hemisphere values weight the whole region-biome mean (computed over both hemispheres) by the area the unit has in that hemisphere, rather than recomputing the mean from the cells of that hemisphere.
- **07a, inputs:** it cannot read its inputs as written; the columns assume `prec_evap.Rds` is `prec_evap.fst` and `twc_grid_classes.Rds` is `grid_classes.Rds`.
- **07b_global, inputs and undefined names:** the three input files are written by no script and their columns are inferred from use; `PALETTES` and `PATH_FIGURES` are undefined, so the figure's destination is unclear.
- **07b_regions, input and undefined name:** `dataset_region_yearly_prec_evap.Rds` is written by no script and its columns are inferred from use; `PALETTES` is undefined.
