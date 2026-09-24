# Final manuscript analysis

This folder contains only the source files and generated outputs that support
the final manuscript.

## Source files

- `manuscript.qmd` — manuscript source.
- `R/analysis.R` — RQ1 and RQ2 models plus the reported correlations between
  segmental variability and acoustic STI.
- `R/descriptives.R` — descriptive statistics reported in the manuscript.
- `R/create_figures.R` — the two figures included in the manuscript.
- `R/model_tables.R` — formatting helpers for the regression table.
- `templates-data/custom-reference.docx` — Word reference document used by
  Quarto.

The scripts read the canonical analysis-ready files in
`../data/02_preppedData/`. The top-level `_targets.R` pipeline tracks their
inputs and outputs.

## Generated outputs used by the paper

- `figures/` — the two final paper graphics.
- `tables/` — the publication-ready RQ2 regression table and its HTML
  renderings.
- `results/` — machine-readable model coefficients, descriptive summaries,
  segmental–STI correlations, and the plain-text model summaries.
- `models/RQ1_*.RDS` and `models/RQ2_*.RDS`

From this folder, run `Rscript R/analysis.R` to regenerate the inferential
models and their result tables, then run `quarto render manuscript.qmd` to
render the paper. Regenerating the descriptive CSV files requires the excluded
token-level interim data and local `targets` store; the committed copies allow a
public clone to render the manuscript without those files.

Historical drafts, preliminary presentations, exploratory models, and analyses
that were not included in the final paper are retained locally under
`../archive/` and are not version-controlled.
