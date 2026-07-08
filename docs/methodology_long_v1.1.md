# Methods

*A global probabilistic ensemble dataset for land–atmospheric water exchange*

Study period 1982–2021 · 0.25° annual · global land · IPCC AR6 region × biome stratification.
Pipeline scripts `00_initialize.R` – `04e_global_twc_change_storylines.R`, plus scenario-mean sensitivity `07a`–`07b`.

---

## 1. Design principles

- The dataset is a **probabilistic** representation of P and E over global land for 1982–2021, with emphasis on their change, built so that no single input product is privileged: dataset choice is resolved by Monte Carlo sampling from performance-based probabilities rather than by a single best-estimate merge.
- Sampling is stratified by **IPCC AR6 region × biome**, so spatial coherence of dataset selection is preserved within physically meaningful units rather than drawn independently per grid cell.
- Weights reflect different weighting strategies. The **base** strategy is used for the released ensemble and prioritises change; the other seven strategies are retained for **sensitivity analysis** (`07`) and are distributed alongside base.
- Two derived quantities carry the analysis: net atmospheric moisture supply **Δ(P−E)** ("water availability") and land–atmosphere water exchange intensity **Δ(P+E)/2** ("water flux"). Flux is an exchange-intensity metric — not a velocity or residence time.
- Change is defined throughout as the difference of two equal 20-year means, **2002–2021 minus 1982–2001**, with full-period (40-yr) linear trends provided as a complementary metric.

## 2. Input datasets

*Scripts `01a`–`01c`; constants in `source/twc_change.R`.*

- Five coherent precipitation–evaporation **analysis pairs** at 0.25°, annual, land-only: ERA5-Land, FLDAS, MERRA-2, TerraClimate, and GLEAM/MSWEP. Note the deliberate pairing: MSWEP precipitation is joined to GLEAM evaporation and relabelled `GLEAM` so each of the five worlds is a self-consistent P–E pair (`01e`).
- Two larger **reference ensembles** used only for evaluation, not sampling:
  - Precipitation (8): CPC, GPCC, GPCP/EARTH, ERA5-Land, FLDAS, MERRA-2, PREC/L, TerraClimate.
  - Evaporation (8): BESS, ERA5-Land, ETMonitor, SynthesizedET, FLDAS, GLEAM, MERRA-2, TerraClimate.
- **PET products** (GLEAM v4.1a plus a MERRA-2/MSWX suite) for water/energy-limitation and plausibility screening.
- **Auxiliary observational constraints, validation only:** GRACE terrestrial water storage, ESA CCI soil moisture, and ROBIN in-situ runoff. GRACE is not an ensemble member — it is a process-completeness / validation overlay rather than an analytical object.

## 3. Harmonisation and common grid

*Scripts `01a`, `01b`, `01d`.*

- All products subset to 1982–2021, converted to annual land fields on the common 0.25° grid, and tabulated.
- A `twc_complete_grid` mask retains only cells where every analysis dataset has valid coverage, so all members are defined on an identical spatial support.
- ROBIN daily discharge is converted cumecs→mm using catchment area, aggregated to monthly/annual, and matched to grid cells (direct intersection plus a nearest-feature fallback for small catchments) to enable a P−E−Q water-balance check.

## 4. Spatial stratification

*Script `01g`.*

- Each grid cell is tagged with its IPCC AR6 region (44 land regions after dropping ocean regions and BOB/ARS/GIC) and its **biome** (from the pRecipe land masks).
- A **hemisphere** label (north / tropics / south) is derived automatically from each region's latitudinal extent.
- Additional descriptive layers are attached for downstream stratification and figures: atmospheric circulation regime, latitude zone, main climate class, hydrobelt, and continent.
- Grid-cell area weights are computed and normalised.

## 5. Derived variables and per-dataset change

*Scripts `01h`, `02a`, `02b`, `02c`.*

- Per dataset: area-weighted annual P and E at global and region × biome levels; water availability `avail = P − E` and water flux `flux = (P + E)/2`.
- Period means (1982–2001, 2002–2021) and their differences give per-dataset ΔP, ΔE, Δavail, Δflux fields.
- Each cell/dataset receives a compound class from the sign of the two axes: wetter/drier × accelerated/decelerated → the four-class `flux_avail` typology, plus a water- vs energy-limitation regime and its transition (`02c`, PET > P screened against AET).

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

- **Axis 1 — what is weighted** (blend shares, inverse-loss transform): `base` (Base), `clim_dominant` (Climate-dominated), `evap_dominant` (Evaporation-dominated), `prec_dominant` (Precipitation-dominated). These share the transform and differ only in the ordered `COMBINE_SPEC` blend (climatology vs trend share, and final prec vs evap share).
- **Axis 2 — how performance maps to probability** (single-knob foils): `rank_linear` (Rank-linear), `rank_exp` (Rank-exponential; steep, near winner-take-all), `inverted` (Disagreement; adversarial mirror), and `neutral` (Neutral; exact 1/n substrate for provenance analysis).

Procedure:

- A **physics gate** is applied before weighting: cells are kept only where the aridity check passes **and** `n_below_pet > 6` (strict majority of PET combinations consistent). This is the main source of high-latitude coverage holes and must be documented as such.
- Grid weights are aggregated to region × biome sampling probabilities using an **all-cell mean** denominator (absent dataset-cells count as zero), so a dataset that survives only in a few harsh cells is penalised for absence rather than rewarded; biome fractions and per-unit cell counts are stored for provenance.

> **Flags for the paper (unresolved — code unchanged from the analysis repo).**
> 1. `trend_dominant` from the earlier five-scenario design was intentionally dropped (its clim↔trend span is already covered by `base`↔`clim_dominant`); the axis-2 scenarios are new. Any methods text or table describing "five scenarios" must be updated to the current **eight**. Human-readable labels are fixed in `07b` (Base, Climate-dominated, Evaporation-dominated, Precipitation-dominated, Rank-linear, Rank-exponential, Neutral, Disagreement).
> 2. Confirm the `base` climatology share: the constants set `PREC/EVAP_CLIM_SHARE = 0.3` (trend 0.7) but the `base` block still leaves `0.5`. Every base-scenario number downstream moves with this, so it must be pinned before submission.

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
- Each base-ensemble member is assigned a **global change storyline** from the joint signs of ΔP, ΔE, Δflux, Δavail. `04e` uses six labels (wet-accelerated ↑P↑E; dry-accelerated ↓P↑E and ↑P↑E; dry-decelerated ↓P↓E and ↓P↑E; other), i.e. the four availability × flux quadrants further split by the ΔP/ΔE driver signs.

> **Gap to note.** `04c_create_gridded_mc_ensemble.R` is still empty, so the gridded *MC* ensemble is produced by region × biome → grid extrapolation (`04d`), not by a native per-cell MC. A genuine per-grid field does exist deterministically via `07a` (§10). The Data Records section must state which gridded product is which (stochastic-mapped vs deterministic scenario mean).

## 10. Scenario-mean sensitivity analysis

*Scripts `07a`, `07b`.*

- `07a` builds, for **each of the eight scenarios**, a **deterministic scenario-weighted mean** of P and E by applying the region × biome weights as fixed weights over datasets — i.e. the analytic expectation of the sampling distribution that the Monte Carlo ensemble converges to, computed without sampling. Outputs are produced at three scales: grid cell, region × biome, and IPCC region (`scenario_prec_evap_grid` / `_region_biome` / `_region`).
- `07b` positions the change signal in **availability–flux space**: the x-axis is Δ(P−E), the y-axis is Δ(P+E)/2, the four quadrants are the wet/dry × accelerated/decelerated storylines, and the ±½ diagonals mark the pure-precipitation (ΔE = 0) and pure-evaporation (ΔP = 0) driver directions. Each panel overlays the individual datasets, the eight scenario means, and the unweighted dataset mean, globally and per IPCC region.
- Purpose: quantify how sensitive the diagnosed change is to the weighting choice, and show that the base scenario sits sensibly relative to the raw datasets and their unweighted mean rather than being driven by any single product.

> **Wiring gap (reproducibility).** `07b` reads global/region per-dataset and per-scenario yearly files (`scenario_global_yearly_prec_evap.Rds`, `dataset_global_yearly_prec_evap.Rds`, `dataset_region_yearly_prec_evap.Rds`) whose names do not match the outputs of `07a` (`scenario_prec_evap_region.Rds` matches; the global-yearly and per-dataset-yearly inputs do not appear to be produced by `01h`/`07a` under these names). The `07a → 07b` inputs need reconciling before the sensitivity figures are reproducible end-to-end.

## 11. Validation

*Embedded across `01b`–`01f`, `02c`, `03*`, `07`.*

- Time-series and mean-field consistency checks per dataset at random cells; reference-agreement diagnostics (majority sign / significance).
- PET-vs-AET consistency; ROBIN P−E−Q water-balance closure; GRACE and ESA CCI as independent storage / soil-moisture cross-checks.
- Per-scenario weight diagnostics: effective number of datasets (inverse-Simpson), dominant-dataset maps, adversarial base-vs-Disagreement agreement.
- `07` checks: region × biome weights sum to 1 per scenario; every scenario yields all three spatial scales; per-scenario coverage summaries.

## 12. Data records (proposed distribution)

- Harmonised per-dataset annual P/E fields on the common grid.
- The **eight scenario weight fields** at grid and region × biome level (the headline deliverable).
- MC selection tables / matrices; aggregated annual P / E / avail / flux ensembles (base 500 + scenarios 100).
- Per-member and summarised change / trend metrics at global, region, and grid level; global storyline labels.
- **Deterministic scenario-mean P/E fields** at grid, region × biome, and region scales (`07a`) — the reproducible gridded product while `04c` is unimplemented.
- The region / biome / hemisphere / limitation class atlas.
- The five single-dataset "coherent worlds" are recoverable as degenerate members and can be shipped as a reference layer.

---

*Version 1.1 — changes from v1.0: extended scope to the `07` scenario-mean sensitivity stage (new §10); restored the availability/flux derived-variable framing and the change definition in §1/§5; corrected §4 (biome is from the pRecipe masks, hemisphere is the latitude-extent derivation); re-flagged the two unresolved base-share / scenario-count items (core scripts unchanged from the analysis repo); added the `07a → 07b` input-wiring gap. Open decisions unchanged: whether to include the downstream locked/flipping trustworthiness atlas (`05*`/`06*`, absent from this repo) in the dataset paper, and settling the base climatology share.*
