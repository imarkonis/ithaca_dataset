# Methods

*A global probabilistic ensemble dataset for land–atmospheric water exchange*

Study period 1982–2021 · 0.25° annual · global land · IPCC AR6 region × biome stratification.
Pipeline scripts `00_initialize.R` – `04e_global_twc_change_storylines.R`.

---

## 1. Design principles

- The dataset is a **probabilistic** representation of P and E in the 1982–2021 period over global land with emphasis in its change, built so that no single input product is privileged: dataset choice is resolved by Monte Carlo sampling from performance-based probabilities rather than by a single best-estimate merge.
- Sampling is stratified by **IPCC AR6 region × biome**, so spatial coherence of dataset selection is preserved within physically meaningful units rather than drawn independently per grid cell.
- Weights reflect different weighting strategies, with the main one (base) used for the final dataset prioritizing change. The other 7 weight approaches are used for sensitivity analysis.

## 2. Input datasets

*Scripts `01a`–`01c`; constants in `source/twc_change.R`.*

- Five coherent precipitation–evaporation **analysis pairs** at 0.25°, annual, land-only: ERA5-Land, FLDAS, MERRA-2, TerraClimate, and GLEAM/MSWEP. Note the deliberate pairing: MSWEP precipitation is joined to GLEAM evaporation and relabelled `GLEAM` so each of the five worlds is a self-consistent P–E pair (`01e`).
- Two larger **reference ensembles** used only for evaluation, not sampling:
  - Precipitation (8): CPC, GPCC, GPCP/EARTH, ERA5-Land, FLDAS, MERRA-2, PREC/L, TerraClimate.
  - Evaporation (8): BESS, ERA5-Land, ETMonitor, SynthesizedET, FLDAS, GLEAM, MERRA-2, TerraClimate.
- **PET products** (GLEAM v4.1a plus a MERRA-2/MSWX suite) for water/energy-limitation and plausibility screening.

## 3. Harmonisation and common grid

*Scripts `01a`, `01b`, `01d`.*

- All products subset to 1982–2021, converted to annual land fields on the common 0.25° grid, and tabulated.
- A `twc_complete_grid` mask retains only cells where every analysis dataset has valid coverage, so all members are defined on an identical spatial grid.

## 4. Spatial stratification

*Script `01g`.*

- Each grid cell is tagged with its IPCC AR6 region (44 land regions after dropping ocean regions and BOB/ARS/GIC) and biome derived automatically from each region's latitudinal extent.
- Additional descriptive layers are attached for downstream stratification and figures: atmospheric circulation regime, latitude zone, main climate class, hydrobelt, and continent.
- Grid-cell area weights are computed and normalised.

## 5. Derived variables and per-dataset change

*Scripts `01h`, `02a`, `02b`, `02c`.*

- Per dataset: area-weighted annual P and E at global and region × biome levels

## 6. Dataset evaluation

*Scripts `01f`, `03a`, `03c`, `03d`.*

- A **leave-one-out reference** is built per candidate: per-cell mean, SD, Sen slope, and Mann–Kendall significance from the large reference ensemble, with the candidate removed from its own reference when present (e.g. GLEAM removed from the evap reference but retained in the prec reference, since prec comes from MSWEP).
- Each candidate is scored per cell by relative absolute bias in mean, SD, and trend, plus agreement with the reference majority on trend significance and sign; datasets are ranked within each cell.
- Two physical-plausibility filters:
  - an aridity / P–E ratio check (flagging P/E < 0.9);
  - a PET consistency check counting, per cell, how many PET products place AET below PET (`n_below_pet`, out of 12 combinations).
- Slope-bias metrics are stored as non-negative magnitudes upstream to avoid silently dropping underestimating datasets.

## 7. Weighting machinery — the primary deliverable

*Scripts `03e`, `03f`.*

Per-cell dataset performance is mapped to a sampling probability under **eight scenarios spanning two orthogonal axes**, and *all eight weight fields are distributed as first-class products*, keeping the dataset epistemically consistent with the no-privileged-world stance.

- **Axis 1 — what is weighted** (blend shares, inverse-loss transform): `base`, `clim_dominant`, `evap_dominant`, `prec_dominant`. These share the transform and differ only in the ordered `COMBINE_SPEC` blend (climatology vs trend share, and final prec vs evap share).
- **Axis 2 — how performance maps to probability** (single-knob foils): `rank_linear`, `rank_exp` (steep, near winner-take-all), `inverted` (adversarial mirror), and `neutral` (exact 1/n substrate for provenance analysis).

Procedure:

- A **physics gate** is applied before weighting: cells are kept only where the aridity check passes **and** `n_below_pet > 6` (strict majority of PET combinations consistent). This is the main source of high-latitude coverage holes and must be documented as such.
- Grid weights are aggregated to region × biome sampling probabilities using an **all-cell mean** denominator (absent dataset-cells count as zero), so a dataset that survives only in a few harsh cells is penalised for absence rather than rewarded; biome fractions and per-unit cell counts are stored for provenance.

> **Flags for the paper.**
> 1. `trend_dominant` from the earlier five-scenario design has been intentionally dropped (its clim↔trend span is already covered by `base`↔`clim_dominant`), and the axis-2 scenarios are new — so any methods text/table describing "five scenarios" needs updating to the current **eight**.
> 2. Confirm the `base` climatology share: the constants set `PREC/EVAP_CLIM_SHARE = 0.3` (trend 0.7) but the `base` block leaves `0.5`. Every base-scenario number downstream moves with this, so it must be pinned before submission.

## 8. Monte Carlo ensemble generation

*Scripts `04a`, `04b`.*

- Region × biome probabilities are converted to cumulative-probability intervals; a **shared** uniform draw per simulation × region × biome selects exactly one dataset per unit by inverse-CDF sampling (verified to yield one and only one selection per unit).
- Two runs are produced from a fixed seed:
  - a **500-member base-scenario ensemble** (primary product);
  - a **100-member all-scenario ensemble** (8 scenarios, for sensitivity / robustness).
- Selected units are joined to the per-dataset region × biome annual P and E and area-weighted up to IPCC-region and global annual P, E, avail, flux ensembles.

## 9. Ensemble change products and storylines

*Scripts `04d`, `04e`.*

- For every member and level (global, region, region × biome): period difference (absolute and %), a two-sample t-test p-value, and a full-period OLS trend with its p-value and significance flag.
- Region × biome metrics are summarised into distributional products — 5/25/50/75/95 quantiles, mean, SD, positive/negative fractions, and significance fractions — and extrapolated to grid cells to give the gridded change record.
- Each base-ensemble member is assigned a **global change storyline** from the joint signs of ΔP, ΔE, Δflux, Δavail (wet-accelerated, dry-accelerated ↑E variants, dry-decelerated variants, other), providing a discrete label per world for downstream storyline analysis.

> **Gap to note.** `04c_create_gridded_mc_ensemble.R` is currently empty, so the gridded ensemble is produced by region × biome → grid extrapolation (`04d`), not by a native per-cell MC. For a data paper this provenance distinction (region-biome-coherent, then mapped down) should be stated explicitly in the Data Records section.

## 10. Validation

*Embedded across `01b`–`01f`, `02c`, `03*`.*

- Time-series and mean-field consistency checks per dataset at random cells.
- Reference-agreement diagnostics (majority sign / significance).
- PET-vs-AET consistency.
- Per-scenario weight diagnostics: effective number of datasets (inverse-Simpson), dominant-dataset maps, adversarial base-vs-inverted agreement.

## 11. Data records (proposed distribution)

- Harmonised per-dataset annual P/E fields on the common grid.
- The **eight scenario weight fields** at grid and region × biome level (the headline deliverable).
- MC selection tables / matrices.
- Aggregated annual P / E / avail ensembles (base 500 + scenarios 100).
- Per-member and summarised change / trend metrics at global, region, and grid level.
