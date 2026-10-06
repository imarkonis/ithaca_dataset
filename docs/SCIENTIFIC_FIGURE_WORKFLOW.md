# Scientific Figure Workflow Standard for R

**Purpose:** scientific, analytical and reproducibility rules for an AI coding assistant (or a person) creating or revising publication figures in R.

**This file does not define cosmetics.** Typography, palettes, dimensions, line widths, themes, journal profiles and export styling are defined in `SCIENTIFIC_FIGURE_STYLE.md` and implemented in `_figs.R`.

**The dividing test:** if changing a value could change what a reader concludes, it is a *scientific* choice — it belongs in the figure script, is set explicitly, and is stated in the caption. If it only changes appearance, it is *cosmetic* and belongs in `_figs.R`.

---

## 1. Priority order

When instructions conflict, follow:

1. Explicit instructions in the current figure request.
2. Scientific meaning of the data and analysis.
3. Target-journal requirements (see §24 and the journal profiles in the style standard).
4. Existing repository conventions and established figure semantics.
5. This workflow standard.
6. The shared cosmetic standard.

An explicit request never overrides data integrity (§5). If a request would misrepresent the data, say so, explain why in one or two sentences, and propose an alternative.

Never sacrifice scientific correctness for visual attractiveness.

---

## 2. Core rule: plotting decisions can be scientific decisions

Treat the following as **scientific choices**, not cosmetics:

- filtering or exclusion of observations;
- aggregation in space or time;
- normalization, standardization, anomalies, ratios, ranks or transformations;
- smoothing, interpolation, binning or resampling;
- axis transformations and axis limits that affect interpretation;
- **colour-scale limits, the midpoint of diverging scales, colour-bin breaks, and how values beyond the limits are drawn**;
- uncertainty intervals;
- trend estimation;
- reference periods or baselines;
- map projection, masking and region definitions;
- significance or model-agreement criteria and what is stippled or hatched;
- ordering of scientifically meaningful categories;
- choice of scenarios, datasets, regions, variables or periods to display;
- statistical annotations;
- any operation that changes values or their apparent interpretation.

**Never invent or silently introduce these choices.** If a scientifically material choice is not specified:

1. infer it from the repository, manuscript, upstream analysis or existing figures;
2. if it still cannot be inferred, use the least-transformative defensible option;
3. report the assumption explicitly.

Never hide a scientific decision inside plotting code. `_figs.R` deliberately refuses to make some of these choices for you (for example, diverging scales have no default midpoint).

---

## 3. Inspect before coding

Before creating or revising a figure:

- inspect the relevant repository structure and the most relevant existing figure script;
- identify the actual upstream data object/file;
- inspect columns, units, dimensions, factor levels, missing-value codes, time coverage, spatial extent and CRS where relevant;
- inspect manuscript terminology and scientific category ordering;
- identify whether a derived figure-data product already exists;
- reuse established analytical conventions unless explicitly changed.

Do **not** guess file names, object names, columns, units, CRS, factor levels or data structures when they can be inspected.

If you cannot run R or cannot reach the data (for example, analyses run on a server you have no access to), say so plainly, ask for `str()`, `summary()` and `head()` output of the inputs, write against the columns the user names, list your assumptions, and give the user the validation and QA checks to run.

If an existing script already performs most of the requested task, modify or extend it rather than rewriting the workflow.

---

## 4. Establish the figure contract

Before substantial coding, determine (and state in 3–6 lines when the user has not):

- the scientific question/message, in one sentence;
- panel structure and reading order;
- variables shown in each panel;
- comparison/reference groups;
- datasets/scenarios/regions/time periods included;
- uncertainty representation;
- target journal, and therefore the `_figs.R` journal profile;
- whether scales must be shared across panels;
- main-text or supplementary figure;
- intended width (single, 1.5 or double column);
- required output formats.

The figure architecture must follow the scientific argument, not whichever geom is easiest to code.

---

## 5. Preserve data integrity

- Never modify raw/source data files.
- Never remove inconvenient values for visual cleanliness.
- Never silently drop observations because they fall outside display limits.
- Handle missing values deliberately. ggplot2's "Removed n rows" warnings are information: find out why before adding `na.rm = TRUE`, and say why in a comment.
- Make gaps visible: complete the time index (a keyed join onto the full sequence) so `geom_line()` breaks at missing periods. Rows that are simply absent are bridged by a straight line without any warning.
- Do not alter values merely to make panels visually comparable.
- Never replace observations with smoothed/interpolated values unless the analysis explicitly requires it.
- Never fabricate data to make code run. Synthetic data are acceptable only for testing code: label them as synthetic and never save the result under the real figure's file name.
- Never silently substitute a different dataset.
- Never manually edit plotted values after export.

When the intention is only to zoom a ggplot2 panel, use `coord_cartesian()` (or `coord_sf()` limits): `xlim()`, `ylim()` and `scale_*(limits = )` delete observations before statistics are computed, silently changing boxplots, smoothers and summaries.

Colour scales: values beyond colour limits must never turn into the "missing" colour. The shared scales in `_figs.R` squish them to the end colours; whenever a script sets colour limits that may cut the data, call `check_limits(x, limits)` and state the saturation in the caption ("values beyond ±20 mm decade⁻¹ are shown in the darkest colour").

---

## 6. Transformations must be explicit

Any transformation must be visible in code and understandable from the figure and caption:

- log scales must be identified, and the handling of zeros stated;
- anomalies must state the baseline/reference period;
- standardized values must state the standardization;
- percentages must have a clear denominator when ambiguous;
- ranks must not be presented as physical magnitudes;
- ratios must define numerator and denominator;
- climatologies must define the averaging period.

Do not add LOESS, GAMs, regression, rolling means, interpolation, spatial smoothing or resampling merely because they make the figure cleaner.

---

## 7. Statistical honesty

- Define every uncertainty interval, and distinguish SD, SE, confidence intervals, credible intervals, ensemble spread and quantile ranges.
- Do not calculate or display significance tests unless they belong to the analysis, and do not imply significance through styling.
- A fitted line names its estimator and test (e.g., Theil–Sen slope with Mann–Kendall test) and sets `method` and `formula` explicitly in code.
- Significance or agreement on maps: stippling or hatching with the criterion stated in the caption. Consider field significance (e.g., false discovery rate; Wilks 2016) rather than per-cell p < 0.05.
- Prefer displaying observations/distributions when their structure matters scientifically.
- Show or report sample size where unequal or small samples affect interpretation.
- Do not use visual emphasis to imply evidence stronger than the analysis supports.

---

## 8. Axes and scales as scientific choices

- Every quantitative axis identifies variable and unit.
- Use meaningful tick intervals and defensible numeric precision.
- Include zero/reference lines when scientifically important.
- Never truncate bar-chart axes; bars start at zero.
- Non-zero origins for line/scatter plots are acceptable only when they do not distort interpretation.
- Do not use two independent y-axes. A secondary axis is allowed only for a true one-to-one transformation of the primary scale.
- Use common axis ranges across panels when direct magnitude comparison is intended; if ranges differ, make it obvious and justified.
- Colour scales follow the same logic: the diverging midpoint, the limits (symmetric or not), and bin breaks are set in the script with a rationale. Symmetric limits are appropriate when positive and negative magnitudes should be compared; asymmetric limits when the physical range is asymmetric.
- Do not define colour-bin thresholds solely for attractive visual balance.

Display limits may be cosmetic only when they do not alter analysis or interpretation. Otherwise they are scientific parameters and belong in the figure script with an explicit rationale.

---

## 9. Maps and spatial figures

### 9.1 CRS and projection

- Inspect the CRS of every spatial input; never assume longitude/latitude when the CRS can be read.
- Choose the projection for the scientific purpose. Prefer equal-area projections for spatial-area comparisons or totals — e.g. `EPSG:3035` (LAEA Europe), `EPSG:8857` (Equal Earth) or `ESRI:54009` (Mollweide); `ESRI:54030` (Robinson) for global overview maps; plain longitude–latitude only for small domains.
- Use `coord_sf()` for `sf` workflows unless a different approach is justified.
- Regular grids: do not push `geom_raster()`/`geom_tile()` through a map projection. Convert the grid to `sf` polygons, or draw a `terra` raster with `tidyterra::geom_spatraster()`; for plain longitude–latitude maps `geom_raster()` + `coord_quickmap()` is fine.

### 9.2 Spatial integrity

- Do not smooth, interpolate, aggregate or resample only to improve appearance; if resampling is necessary, make the method explicit.
- Preserve native resolution when important.
- Handle the antimeridian explicitly for global maps.
- Distinguish no-data areas from low values.
- Avoid unnecessary administrative boundaries; follow journal requirements for disputed boundaries and naming.
- Label latitude and longitude (AGU requires it); the shared map theme keeps coordinate labels by default.
- Credit third-party map or aerial imagery as the journal requires.

Map projection, raster aggregation, region definitions, bin thresholds and spatial masking are scientific/workflow choices, not cosmetics.

---

## 10. R script structure

Every figure script runs from a clean R session (or a documented project environment) and has this structure:

```r
# -------------------------------------------------------------------------
# Figure X: Short descriptive name
# Purpose : one-sentence scientific purpose
# Inputs  : upstream files/objects
# Outputs : final figure + optional figure-data product
# Note    : drafted with AI assistance (<tool>, <date>); checked by <name>
# -------------------------------------------------------------------------

# 1. Packages and shared design system (source _figs.R)
# 2. Scientific parameters specific to this figure
# 3. Read upstream data
# 4. Validate inputs
# 5. Prepare plotting data
# 6. Build panels
# 7. Assemble and export with save_figure()
# 8. Data + visual QA (check_limits(), check_fonts(), inspection)
```

Every figure script sources the shared design system before building plots:

```r
source(here::here("figures", "_figs.R"))
```

Adjust the repository-relative path to the actual project structure. To target a journal other than the project default, set `options(figs.profile = "nature")` *before* sourcing.

The note line supports AI-use disclosure, which some journals require (§24).

---

## 11. Paths and environment

- Do not use `setwd()`; use repository-relative paths (`here::here()` when the project uses `here`).
- Do not depend on objects left in the Global Environment.
- Do not call `install.packages()` in figure scripts.
- Do not write outside the repository unless explicitly instructed, and never overwrite upstream analytical products.
- Keep derived plotting data separate from source data.
- Set seeds for anything random (`set.seed()`, `position_jitter(seed = )`).

---

## 12. Dependencies and code idiom

- **Data manipulation: follow the repository's existing idiom** (data.table or dplyr/tidyr). Do not introduce a second framework without reason, and never mix frameworks within one script.
- Plotting stack: `ggplot2`, `patchwork`, `scales`; `sf` for vector data; `terra`/`tidyterra` for rasters; `here` if the project uses it. `_figs.R` additionally needs `systemfonts`, `ragg` and `scico`.
- Add another package only when it provides a clear scientific or technical benefit — never just to save a few lines.
- Use current ggplot2 syntax that also runs on older installations: `.data[[var]]` instead of `aes_string()`, `after_stat()` instead of `..count..`, `linewidth` instead of `size` for lines. Check `packageVersion("ggplot2")` before using features newer than 3.4 (analysis servers often lag behind).

---

## 13. Object naming

Assign panels and final figures to named objects (`p_a`, `p_b`, `fig_03`); never rely on `last_plot()` for export. Use descriptive names for important objects (`plot_data`, `region_summary`, `trend_estimates`), not `df2`, `x` or `tmp`.

---

## 14. Scientific parameters vs cosmetic parameters

### Figure script

Defines **scientific parameters specific to that figure**, in one block near the top, each with a one-line rationale:

```r
baseline_years     <- 1991:2020                        # anomaly reference period
selected_scenarios <- c("historical", "ssp245", "ssp585")
map_crs            <- "ESRI:54009"                     # equal-area: totals compared
trend_limit        <- 20                               # mm/decade; symmetric: wetting vs drying
trend_midpoint     <- 0                                # no change
signif_level       <- 0.05                             # with FDR control (Wilks 2016)
```

### `_figs.R`

Holds **reusable presentation parameters**: colours and palettes, typography, line widths, point sizes, alpha values, panel tags, theme settings, legend spacing, plot margins, figure widths, DPI, missing-data colour, journal profiles and export helpers.

**Do not hard-code reusable cosmetic values inside figure scripts.** If a cosmetic choice is genuinely unique to one figure, keep it local and comment why; if it appears in a second figure, promote it to `_figs.R`.

### Example of the split

```r
library(data.table)                       # the repository's idiom
library(ggplot2)
library(patchwork)
source(here::here("figures", "_figs.R"))

# Scientific parameters
trend_limit    <- 20    # mm/decade, symmetric
trend_midpoint <- 0     # no change

trend <- as.data.table(readRDS(here::here("data", "derived", "precip_trend_grid.rds")))
stopifnot(all(c("lon", "lat", "trend", "signif") %in% names(trend)))
check_limits(trend$trend, c(-trend_limit, trend_limit))   # saturated values -> caption

p_b <- ggplot(trend, aes(lon, lat)) +
  geom_raster(aes(fill = trend)) +
  geom_stipple(data = trend[signif == TRUE], aes(lon, lat)) +   # which cells: science
  scale_fill_div(type = "water", midpoint = trend_midpoint,      # colours: _figs.R
                 limits = c(-trend_limit, trend_limit),
                 labels = label_minus, name = expression("Trend (mm decade"^{-1}*")")) +
  scale_x_continuous(labels = label_lon, expand = c(0, 0)) +
  scale_y_continuous(labels = label_lat, expand = c(0, 0)) +
  coord_quickmap() +
  theme_pub_map(legend_position = "bottom")
```

Everything that looks like a style decision (palette, stroke widths, fonts, missing colour, legend geometry, coordinate-label format) comes from `_figs.R`; everything that decides what the reader sees (limits, midpoint, stippled cells, map extent) is visible in the script.

---

## 15. Choosing geoms

Choose the representation from the scientific question.

- **Relationships/comparisons:** scatterplots; density-aware scatterplots (`geom_hex()`, `geom_bin_2d()`) for very large samples; line plots for ordered/time dimensions; dot/range plots for estimates with uncertainty.
- **Distributions:** raw points, boxplots, violins/densities when shape matters, ECDFs when useful, distribution + raw points when sample size permits. Do not default to bars with error bars for continuous data (Weissgerber et al. 2015).
- **Time series:** preserve temporal spacing; do not connect across missing periods unless justified (§5); distinguish observations, models, scenarios and uncertainty; avoid unreadable spaghetti plots by highlighting, faceting, summarising or small multiples.
- **Heatmaps:** scientifically meaningful row/column ordering; missing distinct from low values; avoid printing values in every tile unless exact values are needed and legible.

### Hydroclimate patterns

- **Anomalies:** name the baseline period in the axis title or caption and keep it identical across panels and figures; make the time unit explicit (mm d⁻¹ vs mm yr⁻¹).
- **Ensembles and dataset intercomparison:** show the spread, not only a mean — interquartile and min–max ribbons, or thin translucent member lines under a bold median. Harmonise period, units, aggregation and grid before comparing, and state how. Never average away disagreement without showing it. Keep dataset order and identity colours fixed across all figures of a paper.
- **Heavy-tailed variables** (discharge, intensities): log axes when justified, with the zero handling stated.
- **Extremes and thresholds:** state the threshold definition (absolute, percentile, period) — it is part of the result.
- **Part-of-whole** (pie, donut): only for a handful of parts, with values labelled directly; prefer stacked bars or dot plots for comparisons across groups.

---

## 16. Multi-panel logic

Use multiple panels only when they form one scientific argument. Arrange panels in reading order; share scales when comparison requires it but do not force shared scales that erase important structure; avoid redundant panels; keep scientific category ordering consistent; assemble panels reproducibly in R (patchwork), not manually in external software.

Layout geometry and spacing are cosmetic and come from `_figs.R`; panel content and order remain scientific/storytelling decisions.

---

## 17. Reproducible figure data

For complex figures, write compact derived data containing exactly what is plotted:

```text
data/derived/figure_data/fig03_data.csv
figures/fig03_scenarios.R
figures/output/fig03_scenarios.pdf
```

Project structure overrides the example. Derived figure data contain plotted values, identifiers, uncertainty and units; are generated by code; document transformations; and are never edited by hand. Use an appropriate reproducible format for large rasters/spatial objects.

Journals increasingly expect this: AGU requires the data behind every figure to be deposited in a repository and cited, and Copernicus journals encourage plot data as a supplement.

---

## 18. Input validation

Add checks where they can prevent a scientifically wrong figure:

```r
stopifnot(all(required_cols %in% names(plot_data)))
stopifnot(!anyDuplicated(plot_data[, key_columns, with = FALSE]))   # data.table; adapt to idiom
stopifnot(all(is.finite(plot_data$value[!is.na(plot_data$value)])))
stopifnot(all(plot_data$doy >= 1 & plot_data$doy <= 366, na.rm = TRUE))   # cyclic period
```

Also validate where relevant: expected datasets/scenarios present; temporal coverage; units; complete factor levels; lower ≤ estimate ≤ upper; plausible spatial extent; CRS defined; row counts not unexpectedly reduced. Use checks appropriate to the figure rather than generic assertions copied blindly.

---

## 19. Mandatory data QA

Before finalising, compare plotted data against upstream data, as relevant: record counts before/after preparation; missing-value counts; ranges and quantiles; unique categories/datasets/scenarios; temporal range; spatial extent; category ordering; baseline/reference values; sign convention; units; uncertainty bounds; duplicated keys; values beyond colour limits (`check_limits()`).

If a transformation changes the data count, explain why.

---

## 20. Mandatory visual QA

**Code execution is not completion.** After rendering the actual final export, inspect it at or near publication size:

- no warnings indicating dropped data or failed aesthetics;
- no clipping or overlapping labels;
- correct panel order, units, category order and colour mapping;
- missing values represented as intended;
- uncertainty visible but not dominant;
- legends match plotted aesthetics;
- shared scales genuinely shared where intended;
- maps not stretched or cropped;
- scientific interpretation not changed through presentation.

Appearance checks (text size, strokes, fonts, colour-vision safety) are listed in the cosmetic QA of `SCIENTIFIC_FIGURE_STYLE.md`. If you could not render the figure, say so explicitly and hand the user both QA lists.

---

## 21. AI completion report

After creating/revising a figure, report concisely:

1. **What changed** — panels/layout/scales/annotations.
2. **Scientific choices** — filtering, transformations, aggregation, baseline, projection, statistics, colour limits/midpoint/breaks; flag every assumption you made.
3. **Inputs** — actual upstream files/objects used.
4. **Outputs** — script and figure paths.
5. **Validation** — what was rendered and inspected, which data checks ran, and their results.
6. **Draft caption** — what is shown; data sources and period; method in one clause; baseline; definition of bands/bars/markers; n; significance or agreement criterion; any saturated colour range.
7. **Unresolved issues** — anything that remains uncertain; at most one question to the user.
8. **Disclosure reminder** — if the target journal requires disclosure of AI assistance in producing graphics (AGU does), one line saying so.

Do not describe the figure as complete if it has not been rendered and checked.

---

## 22. Prohibited shortcuts

Do not:

- fabricate data, or guess unavailable values and present them as real;
- silently substitute datasets, change units, aggregate, remove outliers or reorder scientific categories;
- zoom with scale limits (`ylim()`, `scale_*(limits = )`) without considering data removal;
- let lines bridge missing periods;
- add smoothing for aesthetics, or use a default `geom_smooth()` without stating method and formula;
- show distributions as bars with error bars when the data can be shown;
- cap a colour scale without `check_limits()` and a caption note, or let out-of-range values turn into the missing-data colour;
- use independent dual y-axes;
- manually alter final figures in a non-reproducible way;
- stop after generating code without rendering and validating.

---

## 23. Definition of done

A figure is complete only when:

- [ ] the script runs from a clean/documented environment;
- [ ] `_figs.R` is sourced and no reusable cosmetic literals remain in the script;
- [ ] upstream inputs are real and traceable;
- [ ] scientific parameters sit in one block, each with a rationale;
- [ ] scientific transformations are explicit;
- [ ] no observations were silently lost or altered (warnings explained, gaps visible, `check_limits()` run where limits are set);
- [ ] the figure answers the intended scientific question;
- [ ] scientifically meaningful scales/orderings are correct;
- [ ] plotted values were checked against upstream data;
- [ ] the final export was visually inspected (data QA here, cosmetic QA in the style standard);
- [ ] a draft caption and the completion report were provided, including assumptions and unresolved issues.

---

## 24. Journal requirements that affect content

Checked October 2026; verify against the journal's current guidelines before submission. Cosmetic requirements (sizes, fonts, formats) are in the style standard's journal profiles.

- **AGU** (GRL, WRR, JGR): data behind every figure deposited in a repository and cited in the Open Research section, and the software used described and cited; latitude and longitude on maps; no information in the figure that belongs in the caption; GRL Research Letters ≤ 12 publication units (one figure = 500 words); use of AI tools to produce images or graphical elements must be disclosed in the Methods.
- **Copernicus/EGU** (ESSD, HESS, GMD, …): a legend inside the figure must explain all symbols — no verbal explanations such as "dashed line" in captions; plot data encouraged as a supplement; credits for third-party map/aerial imagery; colour schemes checked for colour-vision deficiency during technical review.
- **Nature portfolio:** keys inside the figure rather than colour descriptions in the legend text; image-integrity policy for any image data.

---

## 25. Workflow philosophy

**Preserve the data. Expose the assumption. Separate science from cosmetics. Make every figure reproducible.**

When in doubt, choose the least-transformative scientifically defensible option and make the choice explicit.

---

### References

- Weissgerber, T. L. et al. (2015). Beyond bar and line graphs: time for a new data presentation paradigm. *PLoS Biology* 13, e1002128.
- Wilks, D. S. (2016). "The stippling shows statistically significant grid points": how research results are routinely overstated and overinterpreted, and what to do about it. *BAMS* 97, 2263–2273.
- AGU Text & Graphics Requirements: https://www.agu.org/publications/authors/journals/text-graphics-requirements
- Copernicus manuscript preparation (figures): https://publications.copernicus.org/for_authors/manuscript_preparation.html
