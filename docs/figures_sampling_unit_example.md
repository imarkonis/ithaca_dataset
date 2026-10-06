# Figure 2: Mediterranean sampling-unit example, member 44

Related issue: #7. Run from the repository root:

```r
source("code/06f_plot_sampling_unit_example.R")
```

Configure `_machine_paths.R` and run `00a_initialize.R` as for other scripts.
Dependencies include the shared `_figs.R` requirements and `ggrepel`, which
places direct source labels without dropping overlapping labels.

## Panels

- (a) Native 0.25-degree MED biome map, with the named `colset_biome` colours
  from `imarkonis/ithaca/source/graphics.R`.
- (b) Exact saved CDF intervals, coloured by source P-E pair; biome rows ordered
  by decreasing share of classified MED area, largest at the top. Black vertical
  markers show the saved member-44 uniform draw in each unit. Left-to-right
  source order is the alphabetical CDF order from 04a, not ggplot's reverse stack.
- (c) Member-44 annual precipitation, one area-weighted biome-mean curve per unit.
- (d) The corresponding evaporation curves. Both time panels use biome colours
  from (a); neutral direct labels identify each biome and its selected dataset.

No titles or subtitles, smoothing, P-E curves, other members, ensemble summaries
or uncertainty bands appear. Panel descriptions are in the generated caption.
The empty future interval at the right of the time axis is label space, not
extrapolation. P and E have independent vertical scales and explicit units.

The complete named source biome palette is used because its short positional
palette contains an out-of-range index. The coarser `T. Forests` class uses
`T. BL Forests`; `T/S Forests` uses `T/S Moist BL Forests`; `Water` uses the
source dark aquatic `Mangroves` colour; `Polar` uses `Tundra`. These aliases
are explicit in `_figs.R`; unknown labels stop the script.

The main categorical palette is `colset_mid`. Dataset colours use its first
five entries in the fixed ERA5L, FLDAS, GLEAM, MERRA, TERRA order.
`_figure_helpers.R` reuses `PAL_DATASETS` so existing figure scripts share that
mapping. It now loads `_figs.R`'s dependencies while retaining its legacy APIs.

## Inputs

All are in `PATH_OUTPUT_OUTPUT`:

| Input | Producer | Use |
| --- | --- | --- |
| `grid_classes.Rds` | 01g | Biome map and unit area shares |
| `weights_region_biome.Rds` | 03f | Exact dataset weights |
| `weight_cdf_scenarios.Rds` | 04a | Actual bar-segment bounds |
| `mc_selection_scenarios.Rds` | 04a | Member 44's source choices and saved `u` |
| `dataset_region_biome_year.Rds` | 03g | Selected biome P and E series |
| `mc_region_year_scenarios.Rds` | 04b | Reconstruction check only, never plotted |

Area percentages use all MED cells with a biome classification. Units without
sampling weights appear grey on the map and are excluded from sampled support;
coverage is reported. The map retains the repository's longitude/latitude
quick-map convention; area shares use upstream cell weights, not apparent map
area. P and E are biome means, not contributions scaled by biome area share.

## Outputs and checks

Outputs in `PATH_OUTPUT_FIGURES` start with
`fig2_sampling_unit_med_base_member44`: `.pdf`, `.png` (183 x 170 mm, 300 dpi),
`_caption.txt`, `_plot_data.Rds`, and `_selections.csv` (biome, dataset, `u`, area share).

The script checks required columns, keys, finite values, weight sums, source
support and full-year coverage. Regional reconstruction is compared with 04b
at 1e-8 mm/year. Saved CDF weights must match 03f weights, adjacent intervals
must be contiguous, and every saved member-44 draw must lie inside the interval
of its saved source choice (`p_low <= u < p_high`). No weights are normalised
for display and no Monte Carlo draws are regenerated.

`check_limits()` checks the probability range and `check_fonts()` checks the
PDF after export. Execution against actual processed inputs and final visual
inspection are performed locally by the user, as requested. Check that all
source labels are legible and match the saved selection CSV before manuscript
use. The script uses the saved base scenario and does not change its definition.
