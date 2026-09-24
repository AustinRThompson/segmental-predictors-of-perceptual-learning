#!/usr/bin/env Rscript

# Re-measure the flagged vowel tokens with FastTrackPy, as a second opinion on
# the Praat measurements.
#
# Run from the repo root, after R/flag_vowel_outliers.R:
#   Rscript R/fasttrack_flagged_vowels.R
#
# WHAT THIS DOES NOT DO: it does not overwrite any formant value, and nothing
# downstream reads its output. Accepting FastTrack's value wherever it differs
# would apply a centroid-ward correction to extreme tokens only, which shrinks
# the tails of a dispersion measure (MCV) that is *about* those tails. Instead
# this produces a comparison sheet: where Praat and FastTrack agree, the token
# is extreme but probably measured correctly; where they disagree, that is the
# queue for spectrogram review.
#
# Ceilings are scaled by speaker age. The adolescent-speech project uses
# 5500-8000 Hz for speakers up to 13 years; these children are 3-8, with /i/ F2
# means near 3300 Hz and individual tokens past 4000 Hz, so the ranges here sit
# higher. If many speakers' winning max_formant lands at the edge of its range,
# the range is too narrow and should be widened.

suppressMessages({
  library(dplyr)
  library(fs)
})

FASTTRACK_BIN <- "/opt/anaconda3/bin/fasttrack"
NSTEP <- 20

vowel_dir <- path("data", "01_interim", "06_vowel_pipeline")
flagged_file <- path(vowel_dir, "04_vowel_outliers.csv")
work_root <- path(vowel_dir, "05_fasttrack_flagged")
out_file <- path(vowel_dir, "04_vowel_outliers_fasttrack.csv")

if (!file_exists(FASTTRACK_BIN)) {
  stop("FastTrackPy not found at: ", FASTTRACK_BIN)
}
if (!file_exists(flagged_file)) {
  stop("Run R/flag_vowel_outliers.R first; missing: ", flagged_file)
}

flagged <- read.csv(flagged_file)
demo <- read.csv(path("data", "02_preppedData", "summary_stop_measures.csv")) %>%
  select(speaker, age)

flagged <- flagged %>%
  left_join(demo, by = "speaker") %>%
  mutate(
    # Widened after a first pass clipped at both ends: winning ceilings spanned
    # the full 5500-9500 range, with 13 tokens landing on a boundary.
    ft_min = if_else(age <= 5, 5000, 4500),
    ft_max = if_else(age <= 5, 11000, 10000)
  )

missing_slices <- flagged$slice_path[!file_exists(flagged$slice_path)]
if (length(missing_slices) > 0) {
  stop("Missing slice files: ", paste(head(missing_slices, 3), collapse = ", "))
}

if (dir_exists(work_root)) dir_delete(work_root)
dir_create(work_root)

# One FastTrack call per speaker, since the ceiling range is speaker-specific.
results <- list()
for (sp in sort(unique(flagged$speaker))) {
  rows <- filter(flagged, speaker == sp)
  in_dir <- dir_create(path(work_root, sp, "in"))
  out_dir <- dir_create(path(work_root, sp, "out"))

  file_copy(rows$slice_path, path(in_dir, path_file(rows$slice_path)),
            overwrite = TRUE)

  status <- system2(
    FASTTRACK_BIN,
    args = c(
      "audio",
      "--dir", in_dir,
      "--dest", out_dir,
      "--min-max-formant", rows$ft_min[1],
      "--max-max-formant", rows$ft_max[1],
      "--nstep", NSTEP,
      "--which-output", "winner"
    ),
    stdout = TRUE, stderr = TRUE
  )
  code <- attr(status, "status")
  if (!is.null(code) && code != 0) {
    warning("FastTrack failed for ", sp, ": ", paste(tail(status, 3), collapse = " "))
    next
  }

  for (i in seq_len(nrow(rows))) {
    stem <- path_ext_remove(path_file(rows$slice_path[i]))
    csv <- path(out_dir, paste0(stem, ".csv"))
    if (!file_exists(csv)) next
    tr <- read.csv(csv)
    if (nrow(tr) == 0) next
    # Slices run exactly t1 to t2, so the track midpoint is the vowel midpoint,
    # matching where the Praat measurement was taken.
    mid <- tr[which.min(abs(tr$time - median(range(tr$time)))), , drop = FALSE]
    results[[length(results) + 1]] <- tibble(
      vowel_id = rows$vowel_id[i],
      ft_f1 = mid$F1_s[1],
      ft_f2 = mid$F2_s[1],
      ft_max_formant = mid$max_formant[1],
      ft_n_formant = mid$n_formant[1],
      ft_error = mid$error[1]
    )
  }
  cat(sprintf("  %s: %d tokens, ceiling %g-%g Hz\n", sp, nrow(rows),
              rows$ft_min[1], rows$ft_max[1]))
}

ft <- bind_rows(results)

# Score which measurement is more plausible. The reference is the speaker's own
# centroid for that vowel, computed leave-one-out so the flagged token does not
# drag its own reference point, and scaled by that cell's SD so F1 and F2
# contribute comparably. This is numeric triage only -- it cannot see whether a
# formant track actually follows the spectrogram, so verdicts are provisional.
all_tokens <- read.csv(path(vowel_dir, "01_target_vowels.csv"))
loo <- all_tokens %>%
  group_by(speaker, vowel_label) %>%
  mutate(
    .n = n(),
    loo_f1 = (sum(f1) - f1) / (.n - 1),
    loo_f2 = (sum(f2) - f2) / (.n - 1),
    cell_sd1 = sd(f1),
    cell_sd2 = sd(f2)
  ) %>%
  ungroup() %>%
  select(vowel_id, loo_f1, loo_f2, cell_sd1, cell_sd2)

comparison <- flagged %>%
  left_join(ft, by = "vowel_id") %>%
  left_join(loo, by = "vowel_id") %>%
  mutate(
    praat_z = sqrt(((f1 - loo_f1) / cell_sd1)^2 + ((f2 - loo_f2) / cell_sd2)^2),
    ft_z    = sqrt(((ft_f1 - loo_f1) / cell_sd1)^2 + ((ft_f2 - loo_f2) / cell_sd2)^2),
    improvement = praat_z - ft_z,
    verdict = case_when(
      is.na(ft_f1)        ~ "review (no FastTrack value)",
      improvement >  1    ~ "remeasure (FastTrack more plausible)",
      improvement < -1    ~ "keep (FastTrack failed)",
      TRUE                ~ "review (ambiguous)"
    ),
    across(c(praat_z, ft_z, improvement), ~ round(.x, 2))
  ) %>%
  mutate(
    d_f1 = round(ft_f1 - f1),
    d_f2 = round(ft_f2 - f2),
    # Euclidean displacement in the F1-F2 plane between the two measurements.
    disagreement = round(sqrt((ft_f1 - f1)^2 + (ft_f2 - f2)^2)),
    ceiling_at_edge = !is.na(ft_max_formant) &
      (abs(ft_max_formant - ft_min) < 1 | abs(ft_max_formant - ft_max) < 1),
    across(c(ft_f1, ft_f2, ft_max_formant, ft_error), ~ round(.x, 1))
  ) %>%
  arrange(desc(disagreement)) %>%
  select(
    speaker, age, vowel_label, word, vowel_id,
    mahal, disagreement,
    f1, ft_f1, d_f1,
    f2, ft_f2, d_f2,
    praat_z, ft_z, improvement, verdict,
    n_formants, ft_n_formant, ft_max_formant, ceiling_at_edge, ft_error,
    slice_path, formant_path,
    reviewed, notes
  )

write.csv(comparison, out_file, row.names = FALSE)

ok <- sum(!is.na(comparison$ft_f1))
cat(sprintf("\nFastTrack returned values for %d of %d flagged tokens\n",
            ok, nrow(comparison)))
if (ok > 0) {
  d <- comparison$disagreement[!is.na(comparison$disagreement)]
  cat(sprintf("Praat-FastTrack displacement (Hz): median %.0f, IQR %.0f-%.0f, max %.0f\n",
              median(d), quantile(d, .25), quantile(d, .75), max(d)))
  for (thr in c(100, 250, 500)) {
    cat(sprintf("  disagreement > %4d Hz: %3d tokens (%.0f%%)\n",
                thr, sum(d > thr), 100 * mean(d > thr)))
  }
  edge <- sum(comparison$ceiling_at_edge, na.rm = TRUE)
  cat(sprintf("\nWinning ceiling at the edge of its search range: %d tokens", edge))
  cat(if (edge > 0) "  <- widen the range\n" else "\n")
}
cat("\nProvisional verdicts (numeric plausibility only, not spectrogram-verified):\n")
print(as.data.frame(count(comparison, verdict, name = "n")), row.names = FALSE)
cat(sprintf("\nWrote %s\n", out_file))
cat("Sorted by disagreement. Large values are the spectrogram-review queue.\n")
