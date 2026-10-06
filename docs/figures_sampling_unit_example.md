# Mediterranean sampling-unit worked example

Related issue: #7. The proposed placement is supplementary; no numeric figure
label is embedded, so final numbering does not alter the script or filenames.

Run from the repository root after the pipeline outputs are available:

```r
source("code/06f_plot_sampling_unit_example.R")
```

Configure `code/_machine_paths.R` and run `00a_initialize.R` as for the other
figure scripts. Packages are inherited from `_source.R` and `_figure_helpers.R`
(data.table, ggplot2, fst, patchwork, maps); scales is used for axis labels.

The script sources `_figs.R`, based on the shared scientific figure standard,
with the existing ITHACA dataset colours retained. It does not change the
cosmetics or export API of scripts 06a-06e.

## Inputs and panels

All inputs are read from `PATH_OUTPUT_OUTPUT`:

| Input | Producer | Use |
| --- | --- | --- |
| `grid_classes.Rds` | 01g | MED biome map and relative cell-area weights |
| `weights_region_biome.Rds` | 03f | Exact base-scenario sampling weights |
| `mc_selection_scenarios.Rds` | 04a | Membership and reconstruction checks |
| `mc_region_year_scenarios.Rds` | 04b | Annual member trajectories |
| `dataset_region_biome_year.Rds` | 03g | Source-world curves and reconstruction |

The top row contains (a) the native-grid MED biome map and (b) horizontal stacked
sampling-weight bars. The full-width lower panel (c) contains annual P-E:
all 100 members in transparent grey, the pointwise median in black, and five
source worlds in the established dataset colours. The MSWEP-GLEAM pair is
labelled explicitly. There is no smoothing or interval ribbon.

Biome labels follow the same environmental ordering as 06b. Area percentages
use all MED cells with a biome classification. Units without sampling weights
are grey in the map and excluded from the regional ensemble support; their
area is reported. Source worlds use the same sampled units as the ensemble.
The map uses the repository's longitude/latitude quick-map convention; numerical
area percentages use the upstream cell weights, not apparent map area.

The script displays the saved base scenario; it does not resolve the separate
decision about whether the 0.5 climatology share is final. Weights are sampling
probabilities, not calibrated probabilities that a product is true. Independent
unit draws are a modelling assumption, not evidence of independent errors.

## Outputs

In `PATH_OUTPUT_FIGURES`, all filenames start with `figS_sampling_unit_med_base`:

- `.pdf` and `.png`: 183 x 150 mm worked example, PNG at 300 dpi;
- `_p_e.pdf` and `_p_e.png`: separate P and E companion panels;
- `_caption.txt`: caption populated with actual coverage and member count;
- `_plot_data.Rds`: compact plotted inputs, selections and metadata.

## Validation and completion

Before export the script checks required columns, duplicate keys, finite values,
weight sums, 100-member coverage, all years in the configured 1982-2021 period,
source support, and one source selection per member and biome. It reconstructs
the regional P and E series from the saved selections and compares them to 04b
at an absolute tolerance of 1e-8 mm/year. Missing inputs or mismatched runs stop
the figure instead of silently dropping data or normalising plotted bars.

The regional processed inputs are external to GitHub and were unavailable in
the authoring environment. Actual-data rendering, biome-count verification and
visual QA remain required. Inspect both exports at final size for map extent,
labels, legend placement and trajectory visibility before manuscript use.

If MED contains more than eight biomes, the script stops so the categorical
encoding can be revised explicitly rather than silently excluding units.
