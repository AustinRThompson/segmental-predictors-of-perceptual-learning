library(targets)

tar_option_set(
  packages = c(
    "tidyverse", "patchwork", "sjPlot", "here", "rio", "rPraat",
    "tuneR", "PraatR", "fs", "glue", "joeyr", "GFDmcv", "irr"
  )
)

source("R/perceptual_data_pipeline.R")
source("R/stop_pipeline.R")
source("R/vot_reliability.R")
source("R/vowel_pipeline.R")

MANUSCRIPT_DIR <- "manuscript_analysis"
stop_data_dir <- "data/01_interim/05_corrected_manual_vots"
vowel_data_dir <- "data/01_interim/04_manual_vots"
ra_names <- c("Adrienne", "Aleeza", "Araceli", "Austin", "Kaylah", "Leslie")

# Manuscript scripts assume they are run with manuscript_analysis/
# as the working directory (their own internal paths are relative to it).
# tar_make() is invoked from the repo root, so temporarily switch into
# manuscript_analysis/ for the subprocess call, then switch back.
run_r_script <- function(script, outputs) {
  old_wd <- setwd(MANUSCRIPT_DIR)
  on.exit(setwd(old_wd), add = TRUE)
  status <- system2("Rscript", sub(paste0("^", MANUSCRIPT_DIR, "/"), "", script))
  if (!identical(status, 0L)) {
    stop("R script failed: ", script)
  }
  outputs
}

load_figure_functions <- function(script) {
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  env
}

list(

  # Perceptual Data pipeline ----
  # Was (data_prep.Rmd "# Perceptual Data")
  # Produces PWC.csv (perceptual learning scores + demographics), which the
  # Stop Data pipeline below reads. Runs first even though the Perceptual
  # Data chunk sits *after* VOT Check in data_prep.Rmd -- VOT Check has
  # always assumed a pre-existing PWC.csv on disk, so this is the true
  # dependency order, not the .Rmd's chunk order.

  # Track Progress.xlsx (speaker assignment/status) for changes
  tar_target(
    progress_file,
    "data/01_interim/Progress.xlsx",
    format = "file"
  ),

  # Track kid_mean_SPS.csv (prior-project speaking rate) for changes
  tar_target(
    speech_rate_file,
    "data/00_raw/prior_data/kid_mean_SPS.csv",
    format = "file"
  ),

  # Track PWC_Data_Final.csv (prior-project perceptual learning scores)
  # for changes
  tar_target(
    pwc_raw_file,
    "data/00_raw/prior_data/PWC_Data_Final.csv",
    format = "file"
  ),

  # Merge speech rate + perceptual learning scores, restricted to speakers
  # currently tracked in Progress.xlsx
  tar_target(
    pwc_data,
    build_pwc_data(progress_file, speech_rate_file, pwc_raw_file)
  ),

  # Write PWC.csv
  tar_target(
    pwc_file,
    {
      path <- "data/02_preppedData/PWC.csv"
      rio::export(pwc_data, path)
      path
    },
    format = "file"
  ),

  # Stop Data pipeline ----
  # Was (data_prep.Rmd "## VOT Check")
  # The manual "## Separate out the targets" Praat step is NOT a target --
  # a human runs it per-speaker, and its output (the *_targets.TextGrid
  # files) is a tracked input below, same as the RA-corrected segmentations.

  # Discover corrected VOT TextGrids across all RA folders
  tar_target(
    corrected_vot_files,
    list_corrected_vot_textgrids(stop_data_dir, ra_names),
    format = "file"
  ),

  # Resolve one corrected VOT file per speaker, using Progress.xlsx's
  # "Assigned" column to pick the right copy when a speaker has files in
  # more than one RA folder (QC/spot-check overlap)
  tar_target(
    vot_file_index,
    build_vot_file_index(corrected_vot_files, progress_file)
  ),

  # Extract labeled stop tokens + containing word from each speaker's
  # TextGrid (one branch per speaker, so editing one file only reprocesses
  # that speaker)
  tar_target(
    stop_target_tokens,
    read_stop_target_tokens(
      path = vot_file_index$path,
      speaker = vot_file_index$Speaker,
      coder = vot_file_index$Assigned
    ),
    pattern = map(vot_file_index)
  ),

  # Apply exclusion rules and summarize VOT coefficient of variation,
  # both overall and broken out by stop category (P/T/K)
  tar_target(
    stop_summaries,
    compute_stop_summaries(stop_target_tokens, pwc_file)
  ),

  # ---- VOT inter-rater reliability -------------------------------------
  # A few speakers were segmented independently by two RAs. vot_file_index
  # keeps only the assigned coder's copy; here we recover both copies and
  # use them as a reliability subset.

  tar_target(
    double_coded_files,
    find_double_coded_files(corrected_vot_files)
  ),

  tar_target(
    double_coded_tokens,
    read_vot_targets(
      path = double_coded_files$path,
      speaker = double_coded_files$speaker,
      coder = double_coded_files$coder
    ),
    pattern = map(double_coded_files)
  ),

  tar_target(
    vot_reliability_tokens,
    dplyr::bind_rows(lapply(
      split(double_coded_tokens, double_coded_tokens$speaker),
      pair_double_coded_tokens
    ))
  ),

  tar_target(
    vot_reliability_file,
    {
      path <- "data/02_preppedData/vot_reliability.csv"
      rio::export(summarise_vot_reliability(vot_reliability_tokens), path)
      path
    },
    format = "file"
  ),

  # ---- VOT intra-measurer reliability ---------------------------------
  # Austin resegmented five speakers after a delay of several months. Pair
  # those repetitions with Austin's original TextGrids and apply the same
  # token matching, exclusions, and summary statistics used above.

  tar_target(
    intra_vot_files,
    list_intra_measurer_vot_textgrids(vowel_data_dir, "Austin"),
    format = "file"
  ),

  tar_target(
    intra_coded_files,
    index_intra_measurer_files(intra_vot_files)
  ),

  tar_target(
    intra_coded_tokens,
    read_vot_targets(
      path = intra_coded_files$path,
      speaker = intra_coded_files$speaker,
      coder = intra_coded_files$coder
    ),
    pattern = map(intra_coded_files)
  ),

  tar_target(
    vot_intra_reliability_tokens,
    dplyr::bind_rows(lapply(
      split(intra_coded_tokens, intra_coded_tokens$speaker),
      pair_double_coded_tokens
    ))
  ),

  tar_target(
    vot_intra_reliability_file,
    {
      path <- "data/02_preppedData/vot_intra_reliability.csv"
      rio::export(
        summarise_vot_reliability(vot_intra_reliability_tokens),
        path
      )
      path
    },
    format = "file"
  ),

  # Write speaker-level summary_stop_measures.csv
  tar_target(
    summary_stop_measures_file,
    {
      path <- "data/02_preppedData/summary_stop_measures.csv"
      rio::export(stop_summaries$summary_stop_measures, path)
      path
    },
    format = "file"
  ),

  # Write speaker-by-stop summary_stop_measures_by_stop.csv
  tar_target(
    summary_stop_measures_by_stop_file,
    {
      path <- "data/02_preppedData/summary_stop_measures_by_stop.csv"
      rio::export(stop_summaries$summary_stop_measures_by_stop, path)
      path
    },
    format = "file"
  ),

  # Vowel Analysis pipeline ----
  # Was (data_prep.Rmd "# Vowel Analysis", steps 01-07)
  # Replaces the original's hand-rolled per-row hash files (slice_hash,
  # formant_hash) entirely -- each vowel token is its own branch below, and
  # targets' own dependency hashing decides when a token needs
  # reprocessing. See R/vowel_pipeline.R for the two-pass overrides note.

  # Track the formant-count overrides currently on disk (from a previous
  # run's outlier correction -- see R/vowel_pipeline.R header comment)
  tar_target(
    overrides_input_file,
    "data/01_interim/06_vowel_pipeline/06_outlier_formant_overrides.csv",
    format = "file"
  ),

  # Resolve one raw vowel-target TextGrid per speaker (same Assigned-folder
  # resolution as the Stop Data pipeline, for the same duplicate-file reason)
  tar_target(
    manual_vot_textgrid_index,
    list_manual_vot_textgrids(vowel_data_dir, ra_names, progress_file)
  ),

  # Extract corner-vowel target tokens from each speaker's TextGrid
  # (one branch per speaker)
  tar_target(
    speaker_target_vowels,
    extract_speaker_target_vowels(
      path = manual_vot_textgrid_index$path,
      speaker = manual_vot_textgrid_index$speaker
    ),
    pattern = map(manual_vot_textgrid_index)
  ),

  # Combine all speakers' tokens, filter to the target word list, derive
  # vowel_label/vowel_id/n_formants, and apply the overrides currently on
  # disk
  tar_target(
    target_vowels_raw,
    build_target_vowels_raw(speaker_target_vowels, overrides_input_file)
  ),

  # Slice audio, extract Praat formants, and read F1/F2 at each token's
  # midpoint (one branch per token -- ~2,900 branches, the payoff for
  # per-token change detection)
  tar_target(
    measured_tokens,
    measure_vowel_token(
      speaker = target_vowels_raw$speaker,
      word = target_vowels_raw$word,
      vowel_label = target_vowels_raw$vowel_label,
      t1 = target_vowels_raw$t1,
      t2 = target_vowels_raw$t2,
      n_formants = target_vowels_raw$n_formants,
      n_formants_override = target_vowels_raw$n_formants_override,
      audio_path = target_vowels_raw$audio_path
    ),
    pattern = map(target_vowels_raw)
  ),

  # Bind the per-token measurements back onto the base token table
  tar_target(
    target_vowels,
    combine_target_vowels(target_vowels_raw, measured_tokens)
  ),

  # Write the full token-level table (not consumed downstream in this
  # pipeline, kept for manual inspection, matching the original script)
  tar_target(
    target_vowels_file,
    {
      path <- "data/01_interim/06_vowel_pipeline/01_target_vowels.csv"
      rio::export(target_vowels, path)
      path
    },
    format = "file"
  ),

  # Flag tokens more than 10 Mahalanobis units from their vowel category's
  # centroid
  tar_target(
    outliers,
    compute_vowel_outliers(target_vowels, pwc_data)
  ),

  # Derive corrected formant counts for flagged outliers
  tar_target(
    outlier_formant_overrides,
    compute_outlier_formant_overrides(outliers)
  ),

  # Write the corrected overrides back out. If these differ from
  # overrides_input_file, run tar_make() again to carry the correction
  # through target_vowels_raw on the next pass.
  tar_target(
    outlier_formant_overrides_file,
    {
      path <- "data/01_interim/06_vowel_pipeline/06_outlier_formant_overrides.csv"
      rio::export(outlier_formant_overrides, path)
      path
    },
    format = "file"
  ),

  # Speaker-level vowel variability summary (F1/F2 CoV, multivariate CV)
  tar_target(
    summary_vowel_measures,
    summarize_vowel_measures(target_vowels, pwc_data)
  ),

  # Write summary_vowel_measures.csv
  tar_target(
    summary_vowel_measures_file,
    {
      path <- "data/02_preppedData/summary_vowel_measures.csv"
      rio::export(summary_vowel_measures, path)
      path
    },
    format = "file"
  ),

  # Final manuscript analysis ----
  # (manuscript_analysis/) -- reported models, descriptive tables, and the two
  # figures embedded in manuscript.qmd

  # The manuscript scripts themselves read straight from the canonical
  # data/02_preppedData/ files (see manuscript_analysis/R/analysis.R's
  # data_dir) -- summary_stop_measures_file and summary_vowel_measures_file
  # above already track those. sti_data_file tracks the third canonical
  # input the same way.
  tar_target(
    sti_data_file,
    "data/02_preppedData/Aggregate_Results_Table_BBAP.csv",
    format = "file"
  ),
  tar_target(
    sti_bb_data_file,
    "data/02_preppedData/Aggregate_Results_Table_BB.csv",
    format = "file"
  ),

  tar_target(
    analysis_script,
    file.path(MANUSCRIPT_DIR, "R/analysis.R"),
    format = "file"
  ),
  tar_target(
    descriptives_script,
    file.path(MANUSCRIPT_DIR, "R/descriptives.R"),
    format = "file"
  ),
  tar_target(
    figure_script,
    file.path(MANUSCRIPT_DIR, "R/create_figures.R"),
    format = "file"
  ),
  tar_target(
    manuscript_analysis,
    {
      c(
        summary_stop_measures_file,
        summary_vowel_measures_file,
        sti_bb_data_file,
        sti_data_file
      )
      run_r_script(
        analysis_script,
        file.path(
          MANUSCRIPT_DIR,
          c(
            "results/model_coefficients.csv",
            "tables/RQ2_model_table.csv",
            "tables/RQ2_model_table_fragment.html",
            "tables/RQ2_model_table_gt.html",
            "results/model_summaries.txt",
            "results/segmental_sti_correlations.csv",
            "models/RQ1_model_stops.RDS",
            "models/RQ1_model_vowels.RDS",
            "models/RQ2_model_stops.RDS",
            "models/RQ2_model_vowels.RDS",
            "models/RQ2_parsimonious_model_stops.RDS",
            "models/RQ2_parsimonious_model_vowels.RDS"
          )
        )
      )
    },
    format = "file"
  ),
  tar_target(
    manuscript_descriptives,
    {
      c(
        summary_stop_measures_file,
        summary_vowel_measures_file,
        sti_bb_data_file,
        sti_data_file,
        target_vowels_file
      )
      stop_target_tokens
      run_r_script(
        descriptives_script,
        file.path(
          MANUSCRIPT_DIR,
          c(
            "results/descriptives_by_category.csv",
            "results/descriptives_speaker_level.csv",
            "results/descriptives_token_counts.csv",
            "results/descriptives_vot_shape_by_speaker.csv",
            "results/descriptives_vot_shape_summary.csv",
            "results/descriptives_correlations.csv"
          )
        )
      )
    },
    format = "file"
  ),
  tar_target(
    figure_rq1,
    {
      manuscript_analysis
      figures <- load_figure_functions(figure_script)
      output <- file.path(
        MANUSCRIPT_DIR,
        "figures/RQ1_pretest_x_segmental_variability.png"
      )
      figures$create_RQ1_figure(
        stop_model = file.path(MANUSCRIPT_DIR, "models/RQ1_model_stops.RDS"),
        vowel_model = file.path(MANUSCRIPT_DIR, "models/RQ1_model_vowels.RDS"),
        output_file = output
      )
      output
    },
    format = "file"
  ),
  tar_target(
    figure_rq2,
    {
      manuscript_analysis
      figures <- load_figure_functions(figure_script)
      output <- file.path(
        MANUSCRIPT_DIR,
        "figures/RQ2_learning_pretest_x_segmental_variability.png"
      )
      figures$create_RQ2_figure(
        stop_model = file.path(
          MANUSCRIPT_DIR,
          "models/RQ2_parsimonious_model_stops.RDS"
        ),
        vowel_model = file.path(
          MANUSCRIPT_DIR,
          "models/RQ2_parsimonious_model_vowels.RDS"
        ),
        output_file = output
      )
      output
    },
    format = "file"
  )
)
