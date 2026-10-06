# Scientific Figure Cosmetic Standard for R

**Purpose:** shared visual design system for publication figures.

**Implementation:** every reusable cosmetic constant and helper in this document lives in `figures/_figs.R`. Figure scripts source `_figs.R` and do not recreate the design system.

Scientific transformations, filtering, aggregation, projection choices, uncertainty definitions, colour limits and midpoints, bin breaks and other analytical decisions belong in `SCIENTIFIC_FIGURE_WORKFLOW.md`, not here. The dividing test: *if changing it could change what a reader concludes, it is not cosmetic.*

---

## 1. Core visual principle

Figures should be **quiet, precise, consistent, readable at final size, and subordinate to the science.**

Default style: white background; flat 2-D graphics; dark neutral text; one sans-serif font family; no decorative gradients, shadows, bevels, icons or 3-D effects; no minor gridlines; major gridlines only when they improve quantitative reading; minimal borders; thin consistent strokes; restrained colour; consistent semantics across the whole paper.

A publication figure is not an infographic.

---

## 2. Single source of truth: `_figs.R`

All figure scripts source `_figs.R`. Do not hard-code reusable values for: colours, palettes, fonts, font sizes, line widths, point sizes, alpha values, gridline widths, tick lengths, plot margins, legend spacing, panel labels, missing-value colour, figure widths, DPI, the theme, colour/fill scales, export settings or journal-specific formatting.

If a cosmetic constant is used in two figures, it belongs in `_figs.R`. Keep `_figs.R` ASCII-only (write non-ASCII characters as `\u` escapes).

---

## 3. Journal profiles

Journal formatting differs, so `_figs.R` holds **profiles**. Choosing the journal is an editorial decision; everything a profile changes is cosmetic.

- The project default is set once at the top of `_figs.R`: `FIG_PROFILE <- getOption("figs.profile", "copernicus")`. Set it per repository (e.g. `"copernicus"` for ESSD/HESS projects, `"nature"` for a Nature Climate Change paper).
- One script can target another journal with `options(figs.profile = "nature")` **before** `source(...)`.
- The active settings are in `FIG_SPEC`; all `FIG_*` constants derive from it.

| Profile | Base / axis text / min (pt) | Panel tag | Widths (mm) | Other |
|---|---|---|---|---|
| `generic` | 8 / 7 / 6 | **(a)** 9 pt bold | 89, 120, 183 | Earth-science default |
| `copernicus` | 8 / 7 / 6 | **(a)** 9 pt bold | 89, 120, 183; ≥ 80 | "30° N" coordinates; ≤ 5 MB per figure; 300 dpi |
| `nature` | 7 / 6 / 5 | **a** 8 pt bold | 89, 120 (or 136), 183 | all text 5–7 pt; height ≤ 170 mm; editable vector main figures |
| `agu` | 8 / 7 / 6 | **(a)** 9 pt bold | 89, 120, 183 | no fixed widths published; submit near final size |
| `elsevier` | 8 / 7 / 6 | **(a)** 9 pt bold | 90, 140, 190 | raster line art 1000 dpi |

Requirements behind the profiles (checked October 2026 — verify before submission):

- **Nature portfolio:** widths 89 mm (single), 120 or 136 mm (1.5), 183 mm (double); height ≤ 170 mm; Arial or Helvetica, all text 5–7 pt, panel labels 8 pt bold upright lower case; main figures as editable vector files (.pdf, .eps, .ai) with embedded fonts and live text — JPEG/TIFF/PNG not accepted for main figures; Extended Data as RGB JPEG (preferred), TIFF or EPS at ≤ 300 dpi and ≤ 10 MB; avoid red/green combinations, rainbow scales and coloured text.
- **Copernicus/EGU:** one file per figure with all panels; .pdf, .ps, .eps, .jpg, .png or .tif at 300 dpi; width ≥ 8 cm; fonts embedded; individual figures ≤ 5 MB; panel labels "(a)"; ranges with an en dash and no spaces ("1–10"); coordinates with a degree sign and a space before the direction ("30° N"); a space between number and unit ("1 %", "1 m"); units with negative exponents ("W m⁻²"); "h" not "hr"; only the first word of a label capitalised; avoid parallel use of red and green.
- **AGU:** JPG, TIFF, EPS, PS or PDF, all parts of a figure in one file; no titles or "Figure 1" inside the graphic; lower-case panel letters; avoid rainbow-like and red–green palettes and separate series by line type or symbol as well as colour.
- **Elsevier:** widths 90, 140 and 190 mm (minimum 30 mm); raster at final size 300 dpi (photographs), 500 dpi (combination art), 1000 dpi (line art).

Content-related journal requirements (data deposition, AI disclosure, captions) are in the workflow standard.

---

## 4. Typography

### 4.1 Font

Arial when available, else Helvetica, Liberation Sans or Arimo (metric-compatible Arial stand-ins) — `pick_font()` picks the first installed. Do not rely on `"sans"`: on Linux servers it can resolve to DejaVu Sans or a TeX font. Every PDF must contain **one** embedded font family: run `check_fonts()` on the export.

### 4.2 Sizes

Sizes come from the profile (points): base/axis titles, axis text, legend title and text, strip text, panel tags and annotations (`FIG_*_SIZE`). All text must be checked at final size and must not fall below `FIG_MIN_TEXT_SIZE`.

`theme()` text sizes are in points, but `geom_text()`/`annotate()` sizes are in millimetres: use `size = FIG_GEOM_TEXT_SIZE` (or `pt_to_size(pt)`).

### 4.3 Notation

- **Units:** SI with negative exponents, consistent with the manuscript text.
- **Superscripts:** plotmath written from ASCII source, e.g. `expression("Anomaly (mm yr"^{-1}*")")`. Never type Unicode superscripts (⁻¹): many fonts lack U+207B, so the PDF silently embeds a fallback typeface. `_figs.R` maps the plotmath symbol font to the main font, so the minus and degree signs stay in one typeface.
- **Minus signs:** negative tick labels with a true minus via `labels = label_minus` (the default is a hyphen). `label_minus` also omits thousands separators, so years read 1990, not "1 990".
- **Coordinates:** `labels = label_lon` / `label_lat`; the profile sets the format ("30° N" for Copernicus). They also work with `coord_sf()`.
- **Other non-ASCII characters** in strings as escapes: `"\u00B0"` (°), `"\u2212"` (−), `"\u2013"` (– for ranges), `"\u00B5"` (µ), `"\u2030"` (‰). Literal UTF-8 in a script turns into garbage in a non-UTF-8 locale (e.g. batch jobs under a C/POSIX locale).
- Avoid coloured text for categorical encoding.

---

## 5. Figure dimensions

- Widths: `FIG_WIDTH_SINGLE`, `FIG_WIDTH_MID` (1.5 columns) and `FIG_WIDTH_DOUBLE`, from the profile.
- Height is chosen from the content, not from a fixed aspect ratio, and is always passed explicitly (`save_figure()` refuses to guess). `FIG_HEIGHT_MAX` (170 mm) is the routine maximum; `save_figure()` warns above it.
- Build at final size from the start; never rescale an exported figure (fonts and strokes scale with it).
- PNG previews at `FIG_DPI_PREVIEW` (300 dpi); raster submissions (TIFF) at `FIG_DPI_RASTER` from the profile.

---

## 6. Neutral colours

Fixed neutral colours for text (`COL_TEXT`), axes (`COL_AXIS`), reference lines (`COL_REFERENCE`), context (`COL_CONTEXT`, `COL_CONTEXT_LT`), major gridlines (`COL_GRID_MAJOR`), missing/no-data values (`COL_MISSING`), facet backgrounds (`COL_FACET_BG`) and white (`COL_WHITE`). These come from `_figs.R`, never from local hex codes.

---

## 7. Categorical palette

Okabe–Ito colour-blind-aware palette for up to eight important categories (`PAL_CAT_8`), in this order: blue, orange, bluish green, vermillion, reddish purple, sky blue, yellow, dark neutral. Yellow is weak on white; avoid it for thin lines and small points.

Rules:

- Keep category-to-colour assignments consistent throughout a manuscript. When categories recur (datasets, scenarios, components), define a **named palette** in `_figs.R` — `PAL_DATASETS`, `PAL_SCENARIOS`, `PAL_WATER_COMPONENTS` — and use it by name: `scale_colour_cat(values = PAL_DATASETS)`.
- Do not use red versus green as the only distinction, and never rainbow/jet/turbo palettes or ggplot2's default hue palette.
- Do not invent a ninth or tenth colour. With more important categories, use faceting, grouping, line type (`FIG_LINETYPES`), point shape (`FIG_SHAPES`), direct labels, or emphasis of focal categories with the rest in neutral grey.
- When the distinction carries the message, encode it twice (colour plus line type or shape).

Colour is not required to encode every category.

---

## 8. Semantic colours

Stable manuscript-wide meanings: precipitation/water input `COL_PRECIP` (blue); evaporation/atmospheric loss `COL_EVAP` (vermillion); balance terms `COL_BALANCE` (bluish green); focal element `COL_HIGHLIGHT` (reddish purple); reference/context in greys; missing/no-data `COL_MISSING`.

Do not assign moral meaning ("good"/"bad") to green/red. Add project-specific semantic colours as named constants in `_figs.R`; use names, not hex values, in scripts.

---

## 9. Continuous colour maps

All continuous scales use Fabio Crameri's perceptually uniform, colour-vision-deficiency-friendly *Scientific colour maps* via the `scico` package, with extremes trimmed so there is no pure black and the light end stays distinct from `COL_MISSING`.

| Name (`PAL_CONT`) | Map | Use | Scale helpers |
|---|---|---|---|
| `seq_default` | batlow | generic magnitudes, counts, intensities | `scale_fill_seq()`, `scale_colour_seq()` |
| `seq_water` | oslo, reversed | precipitation, water storage | `scale_fill_precip()`, `scale_colour_precip()` |
| `seq_evap` | lajolla, reversed | evaporation, water loss | `scale_fill_evap()`, `scale_colour_evap()` |
| `seq_grey` | grayC, reversed | visually secondary fields | `scale_fill_seq(palette = "seq_grey")` |
| `div_change` | vik | generic differences and changes | `scale_fill_div(type = "change")` |
| `div_water` | broc, reversed | drier (brown) ↔ wetter (blue) | `scale_fill_div(type = "water")` |
| `div_temp` | vik | colder (blue) ↔ warmer (red) | `scale_fill_div(type = "temp")` |
| `cyc_time` | romaO | day of year, timing of maxima, month, direction | `scale_fill_cyclic()`, `scale_colour_cyclic()` |

Rules:

- **Sequential:** lightness changes monotonically with magnitude; do not reverse a scale without a scientific reason. With `seq_grey`, the missing-value colour is also grey — mark missing values differently.
- **Diverging:** only when there is a meaningful centre (zero, no change, a baseline). The **midpoint is required** — `scale_fill_div()` errors without it, because it is a scientific choice. The script also sets the limits (symmetric or not), any bin breaks (`scale_fill_div_binned(breaks = )`), and `reverse = TRUE` if the variable's sign convention is opposite to the palette semantics.
- **Values beyond the limits** are squished to the end colours instead of becoming the missing colour; the script reports them with `check_limits()` (workflow standard §5).
- **Cyclic:** limits span exactly one cycle (`limits = c(1, 366)`); out-of-cycle values are drawn as missing so they show up as errors.
- **Binned scales** are allowed when readers must read values by class; do not discretise a continuous field merely for appearance. Bin breaks are scientific.

---

## 10. Line, point and uncertainty constants

ggplot2's `linewidth` unit is about 0.75 mm — not 1 mm, not 1 pt — so `_figs.R` defines stroke widths in points and converts them with `pt_to_lw()`. Never type raw `linewidth` numbers.

| Constant | Value | Use |
|---|---|---|
| `FIG_LINEWIDTH` | 0.6 pt | data lines |
| `FIG_LINEWIDTH_EMPH` | 1.0 pt | emphasised series |
| `FIG_LINEWIDTH_REF` | 0.4 pt | zero/reference lines (`geom_hline_ref()`, `geom_vline_ref()`) |
| `FIG_AXIS_LINEWIDTH` | 0.35 pt | axes, ticks, map frame |
| `FIG_GRID_LINEWIDTH` | 0.25 pt | major gridlines |
| `FIG_BOUNDARY_LINEWIDTH` | 0.25 pt | coastlines, borders |
| `FIG_POINT_SIZE` / `_SMALL` | 1.2 / 0.6 | markers about 1 mm / 0.5 mm across |
| `FIG_STIPPLE_SIZE` | 0.3 | significance dots about 0.25 mm (`geom_stipple()`) |

Also shared: point stroke, error-bar width and alphas for data, context, ribbons and references (`FIG_ALPHA_*`). Nothing is drawn thinner than 0.25 pt; emphasis comes from a few controlled contrasts, not arbitrary size changes.

---

## 11. Line types and shapes

Stable vectors for non-colour encoding: `FIG_LINETYPES` (solid, dashed, dotted, dotdash) and `FIG_SHAPES` (fillable shapes first, so colour and fill can both be used). Do not use more distinct line types or shapes than readers can decode.

---

## 12. Gridlines and reference lines

No minor gridlines; no major gridlines unless they help reading (`theme_pub(grid = TRUE)` adds thin light ones). Scientifically meaningful zero/reference lines are darker than gridlines but lighter than data, and are drawn before the data.

---

## 13. Panel labels

Panel tags follow the profile: **(a)**, (b), … in 9 pt bold by default; **a**, b, … in 8 pt bold for Nature. Lower-case, same position in every panel, reading order. Always use `add_panel_tags()`.

---

## 14. Legends

- Prefer direct labels when they reduce eye movement without clutter; otherwise compact legends.
- Use manuscript terminology; legend titles include units.
- Collect shared legends in multi-panel figures (`plot_layout(guides = "collect")`); avoid identical repeated legends.
- Key sizes come from `_figs.R`; top/bottom legends get longer keys so colour bars are about 40 mm long, with the title above them on ggplot2 ≥ 3.5.
- Do not place legends over dense data. Inside placement: `legend.position = "inside", legend.position.inside = c(x, y)` on ggplot2 ≥ 3.5.0; `legend.position = c(x, y)` before that.
- Legend labels must not collide; thin the breaks or widen the figure rather than shrinking text.

---

## 15. Facets

Strips use the shared typography, no decorative background and consistent spacing. Facet label content and order are scientific; strip styling is cosmetic.

---

## 16. Multi-panel composition

Aligned plot regions, consistent panel spacing and tags, shared legends where appropriate, minimal redundant axis titles, efficient white space, no decorative outer border. Use patchwork unless the project specifies otherwise. Panel content and order are scientific/storytelling decisions.

---

## 17. Maps

- `theme_pub_map()` keeps latitude/longitude labels (AGU requires them; `coord_labels = FALSE` drops them where a journal allows), removes axis titles and lines, and draws a thin frame.
- Coastlines and boundaries are thin (`FIG_BOUNDARY_LINEWIDTH`) and neutral, visually subordinate to the data; no decorative basemaps; no administrative borders unless needed; sparse labels; north arrows and scale bars only when useful.
- Missing data use `COL_MISSING`; significance stippling uses `geom_stipple()` (style only — which cells are stippled is decided in the script).

CRS, projection, masking, interpolation, raster resolution and region definitions are workflow decisions.

---

## 18. Export

Use `save_figure(plot, file_stem, width_mm, height_mm, formats = c("pdf", "png"))`:

- vector PDF first, through `cairo_pdf` with the plotmath symbol font mapped to the main font — fonts embedded, text editable, one typeface;
- PNG preview at 300 dpi (ragg); optional `"tiff"` (LZW, profile raster dpi) and `"svg"`;
- explicit plot object (never `last_plot()`), explicit width and height in mm, white background, deterministic file names;
- warnings when the width or height leaves the profile's range or a file exceeds the profile's size limit.

After exporting, run `check_fonts("<file>.pdf")` (one family, all embedded). For dense point clouds or fine rasters, rasterise only the dense layer (`ggrastr::rasterise(geom_point(...), dpi = 600)`) so text and axes stay vector and files stay small.

---

## 19. Figure-script cosmetic rule

A normal figure script looks approximately like:

```r
source(here::here("figures", "_figs.R"))

p_a <- ggplot(plot_data, aes(year, value, colour = dataset)) +
  geom_hline_ref(0) +
  geom_line(linewidth = FIG_LINEWIDTH) +
  scale_colour_cat(values = PAL_DATASETS, name = NULL) +
  scale_y_continuous(labels = label_minus) +
  labs(x = NULL, y = expression("Anomaly (mm yr"^{-1}*")")) +
  theme_pub(legend_position = "bottom")

fig_03 <- add_panel_tags(p_a + p_b + plot_layout(widths = c(1.15, 1)))

save_figure(fig_03, here::here("figures", "output", "fig03_example"),
            width_mm = FIG_WIDTH_DOUBLE, height_mm = 75)
```

It should **not** contain cosmetic literals such as:

```r
colour = "#2166AC"
size = 1.8
linewidth = 0.45          # ggplot2 units: this is ~1 pt, not 0.45 pt
theme_minimal(base_size = 9)
legend.key.height = unit(...)
```

unless the value is genuinely unique to that figure and documented.

---

## 20. Technical pitfalls

| Instead of | Use | Why |
|---|---|---|
| `geom_line(size = 1)` or raw `linewidth = 0.45` | `linewidth = FIG_LINEWIDTH` | `size` for lines is deprecated; the linewidth unit is ≈ 0.75 mm |
| `ggsave("f.pdf", width = 89)` | `save_figure(p, stem, width_mm, height_mm)` | default unit is inches; `pdf()` does not embed standard fonts |
| `labs(title = ...)` | the manuscript caption | journals set titles in captions (`theme_pub()` blanks titles) |
| `"mm yr⁻¹"`, literal `°` in code | plotmath `yr^{-1}`, `"\u00B0"` | missing glyphs and locale-dependent rendering |
| hyphens in negative tick labels | `labels = label_minus` | typographic minus |
| rainbow, jet, turbo, `"Set1"`, default hue | `PAL_CAT_8`, scico scales | colour-vision deficiency, perceptual uniformity |
| `family = "sans"` | `FIG_FONT` | `"sans"` may resolve to a non-Arial font |
| ad-hoc `theme()` tweaks per script | `theme_pub()` and edits in `_figs.R` | one paper, one visual language |

---

## 21. Cosmetic QA

At final publication size, verify:

- text is readable and no text is below `FIG_MIN_TEXT_SIZE`;
- `check_fonts()` reports one font family, all embedded;
- line widths are neither hairline nor heavy; points remain visible;
- panel labels are aligned and follow the profile;
- legends are compact and readable, with no colliding labels;
- colour mappings are consistent with the other figures of the manuscript;
- the figure survives colour-vision-deficiency simulation — `colorspace::swatchplot(pal, cvd = TRUE)` for palettes, Color Oracle or Coblis on the rendered PNG — and greyscale (`colorspace::desaturate()`), without relying on red/green;
- missing values are distinct;
- gradients are perceptually ordered;
- no clipping or overlap; balanced white space;
- no decorative element competes with the data;
- file formats and sizes match the profile.

---

## 22. Cosmetic philosophy

**One paper, one visual language.** `_figs.R` makes the routine aesthetic decisions once, so each figure script can focus on the science.

---

### Sources

- Nature research figure guide: https://research-figure-guide.nature.com/figures/building-and-exporting-figure-panels/ and guide to preparing final artwork: https://nature.com/documents/nature-final-artwork.pdf
- Copernicus manuscript preparation (figure composition and content guidelines): https://publications.copernicus.org/for_authors/manuscript_preparation.html
- AGU Text & Graphics Requirements: https://www.agu.org/publications/authors/journals/text-graphics-requirements
- Elsevier artwork sizing: https://www.elsevier.com/authors/policies-and-guidelines/artwork-and-media-instructions/artwork-sizing
- ggplot2 aesthetic specifications (linewidth and text units): https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
- Crameri, F., Shephard, G. E. & Heron, P. J. (2020). The misuse of colour in science communication. *Nature Communications* 11, 5444. Scientific colour maps: https://www.fabiocrameri.ch/colourmaps/
- Stoelzle, M. & Stein, L. (2021). Rainbow color map distorts and misleads research in hydrology. *HESS* 25, 4549–4565.
- Okabe, M. & Ito, K. (2008). Color Universal Design (CUD): how to make figures and presentations that are friendly to colorblind people.
