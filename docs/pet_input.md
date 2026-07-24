# PET input variables (MERRA-2 and MSWX)

## Related issue
Issue #47: *Compile input-dataset metadata table (units, grids, versions, links, citations) for PET estimation*.

## Purpose
PET is produced from two parallel forcing sources so the estimates can be compared:

- a **MERRA-2**-driven PET, and
- an **MSWX-Past**-driven PET (with surface albedo taken from ERA5-Land, since MSWX does not distribute albedo).

- `domain` = `land` (land-masked)
- `resolution` = `025` → **0.25° regular grid** (both sources regridded to a common 0.25° grid using ITHACA approach)
- `frequency` = `monthly`

---

## 1. MERRA-2 input variables

| # | File | Variable | Description | Unit | Coverage (YYYYMM) |
|---|------|----------|-------------|------|-------------------|
| 1 | `merra2_2sh_kgkg-1_land_198001_202511_025_monthly.nc` | sh | 2 m specific humidity | kg kg⁻¹ | 198001–202511 |
| 2 | `merra2_albedo_01_land_198001_202512_025_monthly.nc` | albedo | Surface albedo | dimensionless (0–1) | 198001–202512 |
| 3 | `merra2_sp_Pa_land_198001_202511_025_monthly.nc` | sp | Surface air pressure | Pa | 198001–202511 |
| 4 | `merra2_ssrd_wm-2_land_198001_202511_025_monthly.nc` | ssrd | Surface downwelling shortwave (solar) radiation | W m⁻² | 198001–202511 |
| 5 | `merra2_strd_wm-2_land_198001_202511_025_monthly.nc` | strd | Surface downwelling longwave (thermal) radiation | W m⁻² | 198001–202511 |
| 6 | `merra2_t2m_degC_land_198001_202511_025_monthly.nc` | t2m | 2 m air temperature (mean) | °C | 198001–202511 |
| 7 | `merra2_tmax_degC_land_198001_202512_025_monthly.nc` | tmax | 2 m daily maximum air temperature | °C | 198001–202512 |
| 8 | `merra2_tmin_degC_land_198001_202512_025_monthly.nc` | tmin | 2 m daily minimum air temperature | °C | 198001–202512 |
| 9 | `merra2_u2m_ms-1_land_198001_202511_025_monthly.nc` | u2m | 2 m eastward (u) wind component | m s⁻¹ | 198001–202511 |
| 10 | `merra2_v2m_ms-1_land_198001_202511_025_monthly.nc` | v2m | 2 m northward (v) wind component | m s⁻¹ | 198001–202511 |

> **Wind note (MERRA-2):** wind speed is derived from the two components at 2 m, `wind = sqrt(u2m² + v2m²)`.

---

## 2. MSWX-Past input variables

| # | File | Variable | Description | Unit | Coverage |
|---|------|----------|-------------|------|-------------------|
| 1 | `mswx-past_r_pct_land_197901_202512_025_monthly.nc` | r | 2 m relative humidity | % |197901–202512 |
| 2 | `mswx-past_sp_pa_land_197901_202412_025_monthly.nc` | sp | Surface air pressure | Pa | 197901–202412 |
| 3 | `mswx-past_ssrd_Wm-2_land_197902_202512_025_monthly.nc` | ssrd | Surface downwelling shortwave (solar) radiation | W m⁻² | 197902–202512 |
| 4 | `mswx-past_strd_Wm-2_land_197902_202512_025_monthly.nc` | strd | Surface downwelling longwave (thermal) radiation | W m⁻² | 197902–202512 |
| 5 | `mswx-past_t2m_degC_land_197901_202512_025_monthly.nc` | t2m | 2 m air temperature (mean) | °C | 197901–202512 |
| 6 | `mswx-past_tmax_degC_land_197901_202512_025_monthly.nc` | tmax | 2 m daily maximum air temperature | °C | 197901–202512 |
| 7 | `mswx-past_tmin_degC_land_197901_202512_025_monthly.nc` | tmin | 2 m daily minimum air temperature | °C | 197901–202512 |
| 8 | `mswx-past_u10_ms-1_land_197901_202412_025_monthly.nc` | u10 | 10 m wind speed (magnitude) | m s⁻¹ | 197901–202412 |
| 9 | `era5-land_albedo_198001_202501_025_monthly.nc` | albedo | Surface albedo (**from ERA5-Land**, not MSWX) | dimensionless (0–1) | 198001–202501 |


> **Wind note (MSWX):** MSWX distributes a single **wind-speed magnitude at 10 m** (`u10`), not u/v components.
>
> **Albedo note (MSWX pipeline):** MSWX does not include surface albedo, so albedo is sourced from **ERA5-Land** for the MSWX-based PET.

---


## 4. Dataset other info (versions, grids, links, citations)

| Dataset | Version / product | Native resolution | Provider | Data link | DOI |
|---------|-------------------|-------------------|----------|-----------|----------|
| MERRA-2 | GEOS-5.12.4 (M2 reanalysis) | 0.5° lat × 0.625° lon, hourly, 1980–present | NASA GMAO | https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/ | https://doi.org/10.1175/JCLI-D-16-0758.1 |
| MSWX (MSWX-Past) | MSWX-Past (ERA5-based historical stream) | 0.1°, 3-hourly, 1979–present | GloH2O | https://www.gloh2o.org/mswx/ | https://doi.org/10.1175/BAMS-D-21-0145.1 |
| ERA5-Land (albedo only) | ERA5-Land monthly averaged | ≈0.1° (~9 km), hourly, 1950–present | ECMWF / Copernicus C3S | https://doi.org/10.24381/cds.68d2bb30 | https://doi.org/10.5194/essd-13-4349-2021 |

---


## 5. Key differences between the two sources
These are the points that made the input list "unclear" and are worth documenting explicitly:

1. **Humidity representation** — MERRA-2 provides **specific humidity** (kg kg⁻¹); MSWX provides **relative humidity** (%). Vapour pressure is derived differently in each pipeline.
2. **Wind representation & height** — MERRA-2 provides **u/v components at 2 m**; MSWX provides a **single wind-speed magnitude at 10 m**. Reference-height handling differs.
3. **Albedo source** — MERRA-2 has its **own** albedo; the MSWX pipeline borrows albedo from **ERA5-Land**.
4. **Native grid before regridding** — MERRA-2 native 0.5° × 0.625°; MSWX-Past native 0.1°; ERA5-Land native ≈0.1° (~9 km). All are regridded to a common **0.25°** grid (`025`) for PET.
5. **Temporal coverage** — start dates differ (MERRA-2 from 1980; MSWX from 1979); end months vary slightly per variable, as shown in the coverage columns.

---
