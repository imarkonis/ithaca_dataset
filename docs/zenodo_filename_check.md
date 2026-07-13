# Zenodo filename check

|short_name   |server basename                                     |pRecipe/evapoRe Zenodo key                          |filename match? |version match? |time contained? |source (DOI)                            |
|:------------|:---------------------------------------------------|:---------------------------------------------------|:---------------|:--------------|:---------------|:---------------------------------------|
|cpc          |cpc_tp_mm_land_197901_202208_025_yearly.nc          |cpc-global_tp_mm_land_197901_202309_025_yearly.nc   |no              |no             |yes             |https://doi.org/10.5281/zenodo.14290970 |
|em-earth     |em-earth_tp_mm_land_195001_201912_025_yearly.nc     |em-earth_tp_mm_land_195001_201912_025_yearly.nc     |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14290970 |
|era5-land    |era5-land_tp_mm_land_195001_202112_025_yearly.nc    |era5-land_tp_mm_land_195001_202112_025_yearly.nc    |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14290970 |
|fldas        |fldas_tp_mm_land_198201_2022112_025_yearly.nc       |fldas_tp_mm_land_198201_202410_025_yearly.nc        |no              |yes            |yes             |https://doi.org/10.5281/zenodo.14290970 |
|gpcc-v2022   |gpcc-v2022_tp_mm_land_198101_202012_025_yearly.nc   |gpcc-v2022_tp_mm_land_198201_202012_025_yearly.nc   |no              |yes            |no              |https://doi.org/10.5281/zenodo.14290970 |
|merra2-land  |merra2-land_tp_mm_land_198001_202308_025_yearly.nc  |merra-2_tp_mm_land_198001_202410_025_yearly.nc      |no              |no             |yes             |https://doi.org/10.5281/zenodo.14290970 |
|mswep-v2-8   |mswep-v2-8_tp_mm_land_197902_202301_025_yearly.nc   |mswep-v2-8_tp_mm_land_197901_202411_025_yearly.nc   |no              |yes            |yes             |https://doi.org/10.5281/zenodo.14290970 |
|precl        |precl_tp_mm_land_194801_202208_025_yearly.nc        |precl_tp_mm_land_194801_202410_025_yearly.nc        |no              |yes            |yes             |https://doi.org/10.5281/zenodo.14290970 |
|terraclimate |terraclimate_tp_mm_land_195801_202112_025_yearly.nc |terraclimate_tp_mm_land_195801_202312_025_yearly.nc |no              |yes            |yes             |https://doi.org/10.5281/zenodo.14290970 |
|bess         |bess_e_mm_land_198201_201912_025_yearly.nc          |bess_e_mm_land_198201_201912_025_yearly.nc          |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14622177 |
|era5-land    |era5-land_e_mm_land_195001_202112_025_yearly.nc     |era5-land_e_mm_land_195001_202112_025_yearly.nc     |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14622177 |
|etmonitor    |etmonitor_e_mm_land_200006_201912_025_yearly.nc     |etmonitor_e_mm_land_200006_201912_025_yearly.nc     |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14622177 |
|etsynthesis  |etsynthesis_e_mm_land_200001_201912_025_yearly.nc   |etsynthesis_e_mm_land_200001_201912_025_yearly.nc   |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14622177 |
|fldas        |fldas_e_mm_land_198201_202212_025_yearly.nc         |fldas_e_mm_land_198201_202212_025_yearly.nc         |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14622177 |
|gleam-v4-1a  |gleam-v4-1a_e_mm_land_198001_202312_025_yearly.nc   |gleam-v4-1a_e_mm_land_198001_202312_025_yearly.nc   |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14622177 |
|merra2       |merra2_e_mm_land_198001_202301_025_yearly.nc        |merra2_e_mm_land_198001_202301_025_yearly.nc        |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14622177 |
|terraclimate |terraclimate_e_mm_land_195801_202112_025_yearly.nc  |terraclimate_e_mm_land_195801_202112_025_yearly.nc  |yes             |yes            |yes             |https://doi.org/10.5281/zenodo.14622177 |

**Recommendation:** inputs sufficiently on pRecipe/evapoRe Zenodo -> close #2 and point #3 at these DOIs.

**Warning:** Time cover: GPCC start in precipe is 1982 but server start is 1981.

**Warning:** Minor naming inconsistencies, e.g. merra2-land vs merra-2.
