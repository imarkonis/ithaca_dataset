# A global probabilistic ensemble for land-atmospheric water exchange

This folder contains the working material for the standalone dataset paper on terrestrial water-cycle fluxes.

The goal is to prepare a reproducible dataset paper for submission to ESSD. The analysis uses Monte Carlo simulation to generate a probabilistic ensemble of P and E, based on IPCC region × biome sampling and weighted by dataset agreement that can be used to derive water availability & water flux changes.

## Working principle

This project should be coordinated through **GitHub Issues and Pull Requests only**.

We avoid using Teams, email, or private messages for scientific or technical decisions. If a discussion happens outside GitHub, the conclusion must be copied back into the relevant issue.

GitHub Issues are the official record of:

* tasks
* decisions
* bugs
* methodological questions
* script changes
* manuscript changes
* figure & table requests
* review comments

This avoids fragmented communication and keeps the dataset paper reproducible.

## Folder structure

```text
projects/twc_dataset/
  README.md
  code/
    dev/
      active scripts and reproducibility workflow
    stable/
      final scripts ready for release
  docs/
    documents for internal communication
  manuscript/
    manuscript.tex
      main manuscript file (Copernicus/ESSD template), git-synced to Overleaf
    fig_main/
      figures and captions for manuscirpt 
    fig_sm/
      figures and captions for supplementary material 
    tab_main/  
      tables and captions for manuscirpt 
    tab_sm/  
      tables and captions for supplementary material 
```

## Important documents

Before starting work, please read:

```text
docs/methodology_short.md
docs/methodology_long.md
docs/group_roles.md
```

## Branching rules

We do not commit directly to `main`.

We use one branch per task or issue.

Branch names should be short and descriptive:

```text
doc/data-records-table
fix/base-weighting-shares
fix/04c-gridded-ensemble-note
feat/workflow-figure
review/reproducibility-00-04e
```

After finishing the task, we open a Pull Request into `main`.

## Issue-first workflow

Every meaningful change should start from an issue.

We use issues for:

* new scripts;
* script revisions;
* manuscript sections;
* methodological decisions;
* data-record descriptions;
* figure planning;
* validation checks;
* reproducibility checks;
* bugs or inconsistencies.

We do not start substantial work without an issue. We do not create issues for minro things.

Each issue should include:

```text
## Goal
What should be done?

## Context
Why is this needed?

## Expected output
What file, figure, script, table, or decision should exist when this is done?

## Relevant files
List scripts, docs, or outputs involved.

## Completion criteria
How do we know this issue is done?
```
## .gitignore

Please add this to .gitignore file locally.

```text
.Rproj
.Rproj.user
.Rhistory
.RData
.Ruserdata
.gitignore
code\dev\00_initialize.R
ithaca_dataset.Rproj
```

## Pull request rules

Each Pull Request should correspond to one issue.

A Pull Request should:

* link the issue using `Closes #issue_number` or `Related to #issue_number`;
* have a clear title;
* describe what changed;
* list affected files;
* mention whether outputs need to be regenerated;
* mention whether the methodology document needs updating.

Example Pull Request description:

```text
Closes #12

## What changed
- Updated the Methods outline to reflect the eight weighting scenarios.
- Added a note that gridded MC products are derived by region–biome-to-grid extrapolation.

## Files changed
- docs/dataset_paper_methodology.md

## Checks
- No scripts changed.
- No outputs need to be regenerated.
```

## Working with scripts

Active scripts should be placed in:

```text
dev/
```

Use `dev/` for scripts that are still changing.

Scripts should follow a consistent structure:

```r
# ============================================================================
# Script title
# ============================================================================

# Libraries ===================================================================

# Inputs ======================================================================

# Constants & Variables =======================================================

# Functions ===================================================================

# Analysis ====================================================================

# Outputs =====================================================================

# Validation ==================================================================
```

When editing scripts:

* we keep each script focused on one task;
* we avoid hidden dependencies;
* we use explicit input and output file names;
* we document any changed output;
* we add simple validation checks where possible;
* we do not change methodology-critical constants without an issue;

## Working with manuscript text

Manuscript text should be developed in:

```text
manuscript/
```

Use `docs/` for internal notes and `manuscript/` for paper-ready sections.

## Overleaf manuscript

The manuscript is also available on Overleaf for collaborative editing and compilation:

```text
https://www.overleaf.com/project/6a4ce889a16e7824bdfe9153
```

Overleaf is git-synced to this repository through Overleaf's GitHub integration (Menu > GitHub). The main file is `manuscript/manuscript.tex`, built on the Copernicus (ESSD) template.

* This repository is the source of truth for coordination (issues, PRs, decisions); Overleaf holds the compiled/rendered view of the manuscript. We do not let the two diverge — we treat Overleaf comments the same way as any other out-of-GitHub discussion: copy conclusions back into the relevant issue if needed.
* Manuscript sections follow the same one-owner-per-section model as the rest of the project: each team/person edits their own section on its own branch; conflicts are resolved at merge time, never by editing another team's section directly in Overleaf.
* Because the Overleaf sync can push edits back to GitHub, we avoid free-editing directly in Overleaf outside of our own section.

## Communication rules

Use GitHub Issues for all project communication.

Good issue comments are short, specific, and actionable.

Preferred style:

```text
I checked script 03e. The text says five scenarios, but the code now defines eight:
base, clim_dominant, evap_dominant, prec_dominant, rank_linear, rank_exp, inverted, neutral.

I suggest updating the Methods and keeping a short note explaining why trend_dominant was dropped.
```

Avoid vague comments such as:

```text
This needs work.
```

Instead, explain:

* what is unclear;
* where the issue appears;
* what should be changed;
* which file should be updated.

## Decision-making

Methodological decisions should be made explicitly.

A decision is considered accepted only when:

1. it is discussed in a GitHub Issue;
2. the conclusion is written clearly in the issue;
3. the relevant document or script is updated;
4. the Pull Request is merged.

## Review expectations

Before requesting review, check that:

* the branch is up to date with `main`;
* the task matches the linked issue;
* changed scripts run, or the reason they were not run is stated;
* outputs affected by the change are listed;
* documentation is updated where needed.

Reviewers should focus on:

* scientific consistency;
* reproducibility;
* whether the change matches the issue;
* whether file names and outputs are clear;
* whether manuscript wording matches the actual workflow.

## Data and outputs

Do not commit large generated data files unless they are intentionally part of the repository.

Large outputs should remain in the shared data/output location
