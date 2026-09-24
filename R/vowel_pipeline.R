# Step functions for the "# Vowel Analysis" section of data_prep.Rmd
# (## 01-07), used by the root _targets.R.
#
# PraatR::praat() expects `library(PraatR)` to have run first -- it stashes
# a `SupportedCommands` lookup table in the global environment as a load-time
# side effect rather than proper namespaced package data. tar_option_set()
# already declares "PraatR", but load it explicitly here too so
# measure_vowel_token() works even when this file is sourced standalone.
suppressMessages(library(PraatR))
#
# The original script hand-rolled per-row hash files (slice_hash,
# formant_hash) to avoid re-slicing audio / re-extracting formants for
# unchanged tokens. targets replaces that entirely: each vowel token is its
# own branch (`pattern = map()`), and targets' own dependency hashing decides
# when a token needs reprocessing. There is no separate hash-file target.
#
# One genuine cycle in the original logic: step 01 applies formant-count
# overrides from 06_outlier_formant_overrides.csv, but that file is only
# produced by step 06, which depends on formants computed *after* step 01.
# This mirrors how the original notebook was actually used -- run the whole
# thing, inspect outliers, then re-run to pick up the corrected overrides.
# Targets can't express a cycle, so we keep the same two-pass shape: an
# `overrides_input_file` target tracks whatever overrides are currently on
# disk (from a previous run), while a separate `outlier_formant_overrides`
# target computes fresh ones and writes them back out. If they differ,
# running tar_make() a second time carries the correction through.

TARGET_VOWEL_WORDS <- list(
  AA = c("sock", "socks", "popped", "bobby", "bobbie", "bob", "pop", "dog", "box", "not", "hop", "hot", "body", "stop", "bottom", "boxes", "cannot"),
  AE = c("hattie", "that", "have", "can", "hat", "had", "at", "match", "has", "ladder", "happy", "basket", "apple", "matches", "bad", "sad", "cat", "max", "pat"),
  IY = c("tea", "see", "key", "means", "easy", "eating", "people", "teeth", "eat", "weeping", "pizza", "be", "being", "beep", "beeping", "beaking", "we", "beat", "feet", "peeping", "squeaked", "beef", "seems"),
  UW = c("do", "tutu", "too", "noodle", "super", "spoon", "smooth", "scooter", "tattoo", "soup", "two")
)

# --- Step 01: build the raw token table -------------------------------

# One row per (speaker, RA-folder) TextGrid, resolved the same way the stop
# pipeline resolves corrected VOT files: keep only the copy in the RA
# folder Progress.xlsx's "Assigned" column names, since a few speakers have
# a spot-check copy in a second folder too.
list_manual_vot_textgrids <- function(base_dir, ra_names, progress_file) {
  progress <- rio::import(progress_file) %>%
    dplyr::select(speaker = Speaker, assigned = Assigned)

  dirs <- file.path(base_dir, ra_names)
  paths <- unlist(lapply(dirs, list.files, full.names = TRUE), use.names = FALSE)

  data.frame(path = paths, stringsAsFactors = FALSE) %>%
    dplyr::filter(stringr::str_detect(basename(path), "_target")) %>%
    dplyr::mutate(
      file = basename(path),
      coder = basename(dirname(path)),
      speaker = stringr::str_remove(file, "_targets.*\\.TextGrid")
    ) %>%
    dplyr::inner_join(progress, by = "speaker") %>%
    dplyr::filter(coder == assigned) %>%
    dplyr::distinct(speaker, .keep_all = TRUE)
}

# Extract the four corner-vowel target tokens from one speaker's TextGrid.
# One branch per speaker.
extract_speaker_target_vowels <- function(path, speaker) {
  tg <- rPraat::tg.read(path, encoding = "auto")
  words <- tibble::as_tibble(tg$words)
  phones <- tibble::as_tibble(tg$phones)

  if (speaker == "F5_44") {
    targets <- phones %>%
      dplyr::filter(stringr::str_detect(label, "AA|AE|IY|UW") & !stringr::str_detect(label, "\\*"))
  } else {
    targets <- phones %>%
      dplyr::filter(stringr::str_detect(label, "AA1|AE1|IY1|UW1") & !stringr::str_detect(label, "\\*"))
  }

  targets %>%
    dplyr::mutate(
      midpoint = (t2 - t1) / 2 + t1,
      word = purrr::map_chr(midpoint, ~ {
        label_match <- words %>% dplyr::filter(t1 < .x, t2 > .x) %>% dplyr::pull(label)
        if (length(label_match) == 0) NA_character_ else tolower(label_match)
      }),
      speaker = speaker,
      audio_file = paste0(speaker, ".wav"),
      audio_path = file.path("data/01_interim/02_merged_mfa_output", audio_file)
    )
}

# Reduce step: combine all speakers' tokens, filter to the target word
# list, derive vowel_label/vowel_id/n_formants, and apply whatever formant
# overrides are currently on disk (see module docstring re: the two-pass
# cycle).
build_target_vowels_raw <- function(speaker_target_vowels, overrides_file) {
  all_target_vowels <- dplyr::bind_rows(speaker_target_vowels)

  formant_overrides <- rio::import(overrides_file)

  all_target_vowels %>%
    dplyr::filter(word %in% unlist(TARGET_VOWEL_WORDS)) %>%
    dplyr::filter(!(speaker == "F5_44" & stringr::str_detect(label, "IY") & !(word %in% TARGET_VOWEL_WORDS$IY))) %>%
    dplyr::rename(original_label = label) %>%
    dplyr::mutate(
      f1 = NA, f1_unit = NA, f2 = NA, f2_unit = NA,
      vowel_label = stringr::str_remove(original_label, "1"),
      vowel_label = stringr::str_remove(vowel_label, "_.*"),
      n_formants_override = ifelse(
        stringr::str_detect(original_label, "nf-"),
        stringr::str_extract(original_label, "(?<=_nf-)\\d+"),
        NA
      ),
      n_formants_override = as.numeric(n_formants_override),
      vowel_label = dplyr::case_when(
        stringr::str_detect(vowel_label, "AA") ~ "AA",
        stringr::str_detect(vowel_label, "UW") ~ "UW",
        stringr::str_detect(vowel_label, "AE") ~ "AE",
        stringr::str_detect(vowel_label, "IY") ~ "IY",
        TRUE ~ vowel_label
      ),
      vowel_id = paste(speaker, original_label, word, t1, sep = "_"),
      n_formants = dplyr::case_when(
        vowel_label == "IY" ~ 3,
        vowel_label == "AE" ~ 3,
        vowel_label == "AA" ~ 4,
        vowel_label == "UW" ~ 4,
        TRUE ~ 5
      ),
      vowel_duration = round((t2 * 1000) - (t1 * 1000), digits = 2),
      window_t1 = t1 - (vowel_duration / 2),
      window_t2 = t2 + (vowel_duration / 2)
    ) %>%
    dplyr::left_join(formant_overrides, by = "vowel_id") %>%
    dplyr::mutate(n_formants = dplyr::coalesce(n_formants.y, n_formants.x)) %>%
    dplyr::select(-n_formants.x, -n_formants.y) %>%
    dplyr::select(
      speaker, original_label, word, vowel_label, t1, t2, midpoint,
      vowel_duration, n_formants, n_formants_override, audio_file,
      audio_path, vowel_id
    )
}

# --- Steps 02-04 fused: slice audio, extract formants, read F1/F2 -----

# Praat needs an absolute path for the input/output of "To Formant (burg)".
praat_project_path <- function(file_name) {
  file.path(getwd(), file_name)
}

# One vowel token's full measurement: slice its audio, run Praat's Formant
# (burg) analysis, and read F1/F2 at the token's temporal midpoint. One
# branch per token -- targets only re-runs a token when its own inputs
# (t1/t2/n_formants/n_formants_override/...) change.
measure_vowel_token <- function(speaker, word, vowel_label, t1, t2,
                                 n_formants, n_formants_override,
                                 audio_path) {
  slice_dir <- "data/01_interim/06_vowel_pipeline/02_audio_slices"
  formant_dir <- "data/01_interim/06_vowel_pipeline/03_formants"
  dir.create(slice_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(formant_dir, recursive = TRUE, showWarnings = FALSE)

  slice_file_name <- glue::glue("{speaker}_{word}_{vowel_label}_{round(t1, 2)}s.wav")
  slice_path <- file.path(slice_dir, slice_file_name)
  formant_file_name <- fs::path_ext_set(slice_file_name, "Formant")
  formant_path <- file.path(formant_dir, formant_file_name)

  if (!file.exists(audio_path)) {
    warning(glue::glue("Audio file not found for {speaker}: {audio_path}"))
    return(tibble::tibble(
      slice_path = NA_character_, formant_path = NA_character_,
      formant_saved = FALSE, f1 = NA, f1_unit = NA, f2 = NA, f2_unit = NA
    ))
  }

  full_audio <- tuneR::readWave(audio_path)
  slice <- tuneR::extractWave(full_audio, from = t1, to = t2, xunit = "time")
  tuneR::writeWave(slice, slice_path)

  n_formant <- if (!is.na(n_formants_override)) n_formants_override else n_formants

  formant_saved <- tryCatch({
    PraatR::praat(
      "To Formant (burg)...",
      arguments = list(0, n_formant, 5500, 0.025, 50),
      input = praat_project_path(slice_path),
      output = praat_project_path(formant_path),
      overwrite = TRUE
    )
    TRUE
  }, error = function(e) {
    warning(glue::glue("Formant extraction failed for {slice_path}: {e$message}"))
    FALSE
  })

  midpoint_values <- tryCatch({
    formant_obj <- rPraat::formant.read(fileNameFormant = formant_path, encoding = "auto")
    midpoint_time <- (formant_obj$xmin + formant_obj$xmax) / 2
    mid_frame <- rPraat::formant.getPointIndexNearestTime(formant_obj, time = midpoint_time)
    freqs <- formant_obj$frame[[mid_frame]]$frequency
    list(
      f1 = if (!is.null(freqs[[1]]) && !is.na(freqs[[1]])) freqs[[1]] else 0,
      f1_unit = "Hz",
      f2 = if (!is.null(freqs[[2]]) && !is.na(freqs[[2]])) freqs[[2]] else 0,
      f2_unit = "Hz"
    )
  }, error = function(e) {
    warning(paste("Failed to extract formants from:", formant_path, "|", e$message))
    list(f1 = NA, f1_unit = NA, f2 = NA, f2_unit = NA)
  })

  tibble::tibble(
    slice_path = slice_path,
    formant_path = formant_path,
    formant_saved = formant_saved,
    f1 = midpoint_values$f1,
    f1_unit = midpoint_values$f1_unit,
    f2 = midpoint_values$f2,
    f2_unit = midpoint_values$f2_unit
  )
}

# Reduce step: bind the per-token measurement branches back onto the base
# token table, in the same row order (branch i of measured_tokens
# corresponds to row i of target_vowels_raw).
combine_target_vowels <- function(target_vowels_raw, measured_tokens) {
  dplyr::bind_cols(target_vowels_raw, dplyr::bind_rows(measured_tokens))
}

# --- Step 05: outlier detection ----------------------------------------

# Join speaker demographics (reused from the Perceptual Data pipeline's
# pwc_data, rather than re-reading a raw PWC.csv as the original script did)
# and flag tokens more than 10 Mahalanobis units from their vowel category's
# centroid.
compute_vowel_outliers <- function(target_vowels, pwc_data) {
  speaker_demo <- pwc_data %>%
    dplyr::select(speaker = Speaker, sex = Sex, age = Age)

  target_vowels %>%
    dplyr::left_join(speaker_demo, by = "speaker") %>%
    dplyr::mutate(
      f1 = tidyr::replace_na(f1, 0),
      f2 = tidyr::replace_na(f2, 0)
    ) %>%
    dplyr::group_by(vowel_label) %>%
    dplyr::mutate(
      f1_z = abs(scale(f1))[1],
      f2_z = abs(scale(f2))[1],
      mahalanobis = round(joeyr::tidy_mahalanobis(f1, f2), 2),
      outlier = mahalanobis > 10,
      f1_ref = dplyr::case_when(
        vowel_label == "UW" ~ 460,
        vowel_label == "AE" ~ 1006,
        vowel_label == "AA" ~ 1255,
        vowel_label == "IY" ~ 410,
        TRUE ~ NA_real_
      ),
      f2_ref = dplyr::case_when(
        vowel_label == "UW" ~ 1326,
        vowel_label == "AE" ~ 2692,
        vowel_label == "AA" ~ 1893,
        vowel_label == "IY" ~ 3576,
        TRUE ~ NA_real_
      ),
      f1_distance_ref = abs(f1 - f1_ref),
      f2_distance_ref = abs(f2 - f2_ref),
      eucl_dist = joeyr::eucl_dist(f1, f1_ref, f2, f2_ref)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(speaker:n_formants_override, f1:eucl_dist, audio_path, vowel_id)
}

# --- Step 06: derive corrected formant counts for outliers -------------

compute_outlier_formant_overrides <- function(outliers) {
  outliers %>%
    dplyr::mutate(n_formants = dplyr::case_when(
      outlier == TRUE & vowel_label == "IY" & is.na(n_formants_override) ~ 4,
      outlier == TRUE & vowel_label == "AE" & is.na(n_formants_override) ~ 4,
      outlier == TRUE & vowel_label == "UW" & is.na(n_formants_override) ~ 4,
      outlier == TRUE & vowel_label == "AA" & is.na(n_formants_override) ~ 5,
      TRUE ~ n_formants
    )) %>%
    dplyr::select(vowel_id, n_formants)
}

# --- Step 07: speaker-level summary measures ---------------------------

# 95% confidence ellipse area for a set of (F1, F2) points.
vowel_ellipse_area <- function(f1, f2, conf = 0.95) {
  if (length(f1) < 3) return(NA)
  cov_mat <- cov(cbind(f1, f2))
  eig <- eigen(cov_mat)
  chisq_val <- qchisq(conf, df = 2)
  a <- sqrt(eig$values[1] * chisq_val)
  b <- sqrt(eig$values[2] * chisq_val)
  pi * a * b
}

# Voinov-Nikulin multivariate CV, matching GFDmcv::mcv_est_vn (MLE
# covariance, divisor n, not n-1). Tokens missing F1 or F2 are dropped.
mcv_vn <- function(f1, f2) {
  X <- cbind(f1, f2)
  X <- X[stats::complete.cases(X), , drop = FALSE]
  n <- nrow(X)
  mu <- colMeans(X)
  S <- ((n - 1) / n) * stats::cov(X)
  invS <- solve(S)
  as.numeric((t(mu) %*% invS %*% mu) ^ (-0.5))
}

# Speaker-level vowel variability summary. `word_filter` optionally
# restricts to tokens whose word matches a regex (used for the
# beeping/bobby subset, which also skips the vowel_MCV/na.rm handling used
# for the full sample).
summarize_vowel_measures <- function(target_vowels, pwc_data, word_filter = NULL,
                                      compute_mcv = TRUE) {
  pwc <- pwc_data %>%
    dplyr::rename(
      speaker = Speaker, sex = Sex, age = Age,
      pretest = `Pre-Test`, posttest = `Post-Test`, learning = `Learning`
    )

  tokens <- target_vowels
  if (!is.null(word_filter)) {
    tokens <- tokens %>% dplyr::filter(stringr::str_detect(word, word_filter))
  }

  by_vowel <- pwc %>%
    merge(tokens) %>%
    dplyr::group_by(speaker, age, sex, pretest, posttest, learning, rate_sps, vowel_label)

  if (compute_mcv) {
    by_vowel <- by_vowel %>%
      dplyr::summarise(
        f1_M = mean(f1, na.rm = TRUE),
        f1_SD = sd(f1, na.rm = TRUE),
        f1_CoV = (f1_SD / f1_M) * 100,
        f2_M = mean(f2, na.rm = TRUE),
        f2_SD = sd(f2, na.rm = TRUE),
        f2_CoV = (f2_SD / f2_M) * 100,
        vowel_n_tokens = dplyr::n(),
        vowel_MCV = mcv_vn(f1, f2),
        .groups = "drop"
      )
  } else {
    by_vowel <- by_vowel %>%
      dplyr::summarise(
        f1_M = mean(f1),
        f1_SD = sd(f1),
        f1_CoV = (f1_SD / f1_M) * 100,
        f2_M = mean(f2),
        f2_SD = sd(f2),
        f2_CoV = (f2_SD / f2_M) * 100,
        vowel_n_tokens = NROW(f2),
        .groups = "drop"
      )
  }

  speaker_grouping <- c(
    "speaker", "age", "sex", "pretest", "posttest", "learning", "rate_sps"
  )

  if (compute_mcv) {
    by_vowel <- by_vowel %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(speaker_grouping))) %>%
      dplyr::mutate(
        f1_centroid = mean(f1_M, na.rm = TRUE),
        f2_centroid = mean(f2_M, na.rm = TRUE)
      ) %>%
      dplyr::mutate(edist_to_system = sqrt((f1_M - f1_centroid)^2 + (f2_M - f2_centroid)^2)) %>%
      dplyr::ungroup()
  }

  summary <- by_vowel %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(speaker_grouping)))

  if (compute_mcv) {
    summary <- summary %>%
      dplyr::summarise(
        f1_M = weighted.mean(f1_M, vowel_n_tokens),
        f1_SD = weighted.mean(f1_SD, vowel_n_tokens),
        f1_CoV = weighted.mean(f1_CoV, vowel_n_tokens),
        f2_M = weighted.mean(f2_M, vowel_n_tokens),
        f2_SD = weighted.mean(f2_SD, vowel_n_tokens),
        f2_CoV = weighted.mean(f2_CoV, vowel_n_tokens),
        vowel_MCV = weighted.mean(vowel_MCV, vowel_n_tokens),
        vowel_sep_MD = mean(edist_to_system),
        vowel_n_tokens = weighted.mean(vowel_n_tokens, vowel_n_tokens),
        within_variance = vowel_MCV,
        between_separation = vowel_sep_MD,
        .groups = "drop"
      )
  } else {
    summary <- summary %>%
      dplyr::summarise(
        f1_M = weighted.mean(f1_M, vowel_n_tokens),
        f1_SD = weighted.mean(f1_SD, vowel_n_tokens),
        f1_CoV = weighted.mean(f1_CoV, vowel_n_tokens),
        f2_M = weighted.mean(f2_M, vowel_n_tokens),
        f2_SD = weighted.mean(f2_SD, vowel_n_tokens),
        f2_CoV = weighted.mean(f2_CoV, vowel_n_tokens),
        vowel_n_tokens = weighted.mean(vowel_n_tokens, vowel_n_tokens),
        .groups = "drop"
      )
  }

  cluster_areas <- tokens %>%
    dplyr::group_by(speaker, vowel_label) %>%
    dplyr::summarise(
      cluster_area = vowel_ellipse_area(f1, f2),
      tokens = NROW(f1),
      .groups = "drop"
    )

  mean_cluster_size <- cluster_areas %>%
    dplyr::group_by(speaker) %>%
    dplyr::summarise(
      mean_vowel_cluster_size = weighted.mean(cluster_area, tokens, na.rm = TRUE),
      .groups = "drop"
    )

  merge(summary, mean_cluster_size)
}
