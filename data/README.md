Author: Austin Thompson

Updated September 24, 2026

This document describes the folders within `data/`. Files flow through three stages: `00_raw` (untouched source data) → `01_interim` (intermediate pipeline outputs) → `02_preppedData` (final summary tables used by the manuscript analysis).

## 00_raw — source data, never modified

-   `00_raw/audio` contains the .wav and .lab files for each speaker in the study. Each speaker has ~275 .wav and .lab files for each phrase/utterance recorded. This data is prepped to be used for the Montreal Forced Alignment protocol.
-   `00_raw/prior_data` contains `PWC_Data_Final.csv` and `kid_mean_SPS.csv`, prior-project data (perceptual learning scores and speaking rate) merged in during data prep.

## 01_interim — intermediate pipeline outputs

Produced and consumed by the pipeline in `_targets.R`, in this order. The
earlier monolithic `data_prep.Rmd` has been retained locally in `archive/`.

-   `01_interim/01_mfa_output` contains the output for the Montreal Forced Alignment protocol. Each participant's folder contains ~275 TextGrids that have two tiers: `words` and `phones`.
-   `01_interim/02_merged_mfa_output` contains the merged `.wav` and `.TextGrid` files for each speaker.
-   `01_interim/03_target_stops` contains the output from the "# Separate out the targets" block of `data_prep.Rmd`: `SPEAKER_targets.TextGrid` files that pull out the target word-initial stop consonants for manual segmentation and review. See `Instructions for Generating the Target TextGrids.md` for the RA workflow.
-   `01_interim/04_manual_vots` contains each RA's completed stop-consonant segmentations (raw, as downloaded from Praat), organized in per-RA subfolders.
-   `01_interim/05_corrected_manual_vots` contains the VOT-boundary-corrected versions of the same segmentations. This is what the "## VOT Check" block of `data_prep.Rmd` reads to build `summary_stop_measures.csv`.
-   `01_interim/06_vowel_pipeline` contains the working files for the "# Vowel Analysis" section of `data_prep.Rmd`. Sub-file/folder numbers mirror that section's own `## 01`–`## 07` step numbers:
    -   `01_target_vowels.csv` / `01_target_vowels_row_hashes.csv` — the main vowel-token table, built and updated across steps, with a hash file used for change detection so reprocessing only touches rows that changed.
    -   `02_audio_slices/` / `02_slice_hashes.csv` — per-token audio clips extracted for formant measurement, and their change-detection hashes.
    -   `03_formants/` / `03_formant_hashes.csv` — Praat formant objects for each vowel token, and their change-detection hashes.
    -   `06_outlier_formant_overrides.csv` — manual corrections applied to formant outliers identified in step 05/06 of the script.
-   `01_interim/Progress.xlsx` tracks which speakers have been assigned/completed at each stage of manual segmentation and review; read throughout `data_prep.Rmd` to filter to completed speakers.

## 02_preppedData — final summary tables

The outputs consumed by `manuscript_analysis/`:

-   `summary_stop_measures.csv` — speaker-level VOT CV and related stop measures.
-   `summary_stop_measures_by_stop.csv` — the same, broken out by stop category (P/T/K).
-   `summary_vowel_measures.csv` — speaker-level vowel MCV and related vowel measures.
-   `PWC.csv` — merged perceptual-learning and demographic data.
-   `Aggregate_Results_Table_BB.csv` / `Aggregate_Results_Table_BBAP.csv` — Tetzloff et al. spatiotemporal index (STI) values used for the segmental–suprasegmental correlations reported in the manuscript.

## Other folders

-   `Stimuli_KidPerceptualLearning.xlsx` — the stimuli list read at the top of `data_prep.Rmd`.
