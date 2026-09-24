# Segmental predictors of perceptual learning

This repository contains the reproducible analysis for *Segmental variability
as a predictor of perceptual learning of children’s speech*.

## Repository contents

- `_targets.R` defines the complete data-preparation and manuscript-analysis
  pipeline.
- `R/` contains the data-preparation functions used by the pipeline.
- `data/02_preppedData/` contains the de-identified, analysis-ready data used in
  the manuscript. Raw and interim data are intentionally excluded.
- `manuscript_analysis/manuscript.qmd` is the manuscript source.
- `manuscript_analysis/R/` contains the reported statistical analyses,
  descriptives, table helpers, and figure code.
- `manuscript_analysis/figures/` contains final paper graphics.
- `manuscript_analysis/tables/` contains publication-ready tables.
- `manuscript_analysis/results/` contains machine-readable statistical and
  descriptive outputs.
- `manuscript_analysis/models/` contains the fitted model objects.
- `manuscript_submissions/submission_1/` contains the submitted Word document.

Earlier drafts, presentations, exploratory models, and outputs not included in
the final paper are retained locally under `archive/`, which is excluded from
version control.

## Reproduce the manuscript

The committed analysis-ready data are sufficient to rerun the inferential
models and render the manuscript:

```sh
cd manuscript_analysis
Rscript R/analysis.R
quarto render manuscript.qmd
```

Project members with access to the excluded raw and interim data can rebuild
the complete data-preparation pipeline from the repository root:

```r
targets::tar_make()
```

`R/descriptives.R` depends on token-level interim data and the local `targets`
store. Its manuscript-facing CSV outputs are committed in
`manuscript_analysis/results/`, so the paper itself renders without those
excluded files.
