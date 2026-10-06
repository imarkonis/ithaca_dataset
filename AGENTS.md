# Agent instructions

## Figures (R)

This repository uses a shared standard for publication figures. Before creating or modifying any figure:

1. Read `docs/SCIENTIFIC_FIGURE_WORKFLOW.md` (scientific choices, data integrity, validation, completion report) and `docs/SCIENTIFIC_FIGURE_STYLE.md` (journal profiles, typography, colour maps, export).
2. Source `code/_figs.R` at the top of every figure script and use its theme, scales, constants and `save_figure()`. Do not hard-code colours, sizes, fonts or line widths in figure scripts.
3. Keep scientific choices (baselines, filters, transformations, colour limits and midpoints, bin breaks, projections, significance criteria) in one commented block of the figure script, and state them in the caption.
4. Inspect the real data before coding. Never fabricate, alter or silently drop data.
5. Render the final export and check it (`check_limits()`, `check_fonts()`, visual inspection at final size), then give the completion report from the workflow standard. If you cannot run R, say so.

Use the repository's existing data idiom (data.table or dplyr). The journal profile is `FIG_PROFILE` in `code/_figs.R`.

Claude Code: the same standard is available as the skill `.claude/skills/r-publication-figures/`.
