# Figure 2 polish — issue #69

15 October 2026. Authorized copy: `code/06f_plot_sampling_unit_example_polish.R`.
The original `06f_plot_sampling_unit_example.R`, shared styling files, input data
and original exports were not edited. All new exports include `_polish`.

## Presentation changes

- Retained map/weights top row and full-width P/E panels below.
- Extracted the top legends into their own compact, untagged row.
- Used shorter categorical legend keys from the shared design constants.
- Reduced total height from170to150mm; retained the shared183mm width and fonts.
- Exported PNG at600dpi; PDF remains vector.
- Reused shared `build_grob()` to measure Arial legend text on a ragg device;
  the initial default-device measurement caused warnings and a collision, which
  were corrected before final inspection.
- Added comments and a named row-height setting; no scientific refactoring.

## Checks

Only the copied figure script ran; no upstream analysis was rerun. Its existing
checks passed (native-grid support, saved CDF draws,100members,40years, complete
member trajectories, weights and value agreement). `check_limits()` passed.
Original and polished saved plot data compare equal for map, weights, area,
member, draws and metadata. Captions match exactly. No smoothing, new random
draws, selection changes, aggregation changes or map rasterisation were added.

Final PDF rendered with Poppler and visually inspected: four correct panel tags,
no clipped text, source labels readable and legend collision resolved. PDF is
approximately183x150mm (reported182.74x149.93mm from its integer-point page box).
Arial regular/bold fonts are embedded, independently verified with pypdf;
R's `check_fonts()` could not check because pdftools/pdffonts is unavailable.
PNG4322x3543pixels at600dpi; inspected for consistency with PDF.

Runtime emitted pre-existing LC_CTYPE and package-built-under-newer-R warnings;
no data-loss or font-measurement warnings remain in the final run. Original
Preview tile-seam artifact was intentionally left unchanged at the user's request.

This is a reviewable layout revision. Caption and all scientific content remain
unchanged. Source-family palette, MED/base/member44 and all seven units retained.

## Final interactive polish

- Increased all figure text by 0.5 pt using figure-local profile overrides.
- Set top panels to 60:40, retaining independent legend widths.
- Used three-column legends, a vertical MC draw key, and source legend before draw legend.
- Aligned legend headings and moved the source legend left for right-edge breathing room.
- Adjusted panel (b) tag and reduced the P/E inter-panel gap.
- Manually anchored only T. Forests: FLDAS and Flooded: TerraClimate labels in panel (c); retained automatic layout for other labels.
- Regenerated PDF and 600 dpi PNG; visually inspected exports. Existing scientific assertions pass.
- Original script and shared styles remain unchanged; no upstream analysis rerun.
