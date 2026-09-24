#!/usr/bin/env Rscript

# Flag candidate formant-tracking errors for manual review.
#
# Run from the repo root with: Rscript R/flag_vowel_outliers.R
#
# Distance is computed WITHIN each speaker-by-vowel cell, against that cell's
# own F1-F2 centroid and covariance. Computing it that way (rather than against
# a pooled vowel centroid) holds vocal-tract size constant, so a flagged token
# is unusual for that child's own realisation of that vowel rather than merely
# unusual for a child of a different size.
#
# Note on scale: Mahalanobis distance computed against a sample's own mean and
# covariance is bounded above by (n - 1) / sqrt(n). With a median of about 20
# tokens per cell the attainable maximum is roughly 4.25, so thresholds carried
# over from large-sample settings do not apply here. 2.5 is deliberately
# permissive -- it over-flags so that review can adjudicate.
#
# Output is a review sheet, not an exclusion list. Nothing downstream reads it.

suppressMessages(library(dplyr))

THRESHOLD <- 2.5
MIN_TOKENS <- 6

vowel_dir <- file.path("data", "01_interim", "06_vowel_pipeline")
tokens <- read.csv(file.path(vowel_dir, "01_target_vowels.csv"))

flagged <- tokens %>%
  group_by(speaker, vowel_label) %>%
  mutate(
    n_in_cell = n(),
    cell_f1 = mean(f1, na.rm = TRUE),
    cell_f2 = mean(f2, na.rm = TRUE),
    mahal = if (n() >= MIN_TOKENS) {
      round(sqrt(stats::mahalanobis(
        cbind(f1, f2), colMeans(cbind(f1, f2)), stats::cov(cbind(f1, f2))
      )), 2)
    } else {
      NA_real_
    }
  ) %>%
  ungroup() %>%
  filter(!is.na(mahal), mahal > THRESHOLD) %>%
  mutate(
    f1_dev = round(f1 - cell_f1),
    f2_dev = round(f2 - cell_f2),
    reviewed = "",
    verdict = "",
    notes = ""
  ) %>%
  arrange(desc(mahal)) %>%
  select(
    speaker, vowel_label, word, vowel_id,
    mahal, n_in_cell,
    f1, f2, f1_dev, f2_dev,
    n_formants, n_formants_override,
    midpoint, vowel_duration,
    slice_path, formant_path,
    reviewed, verdict, notes
  )

out <- file.path(vowel_dir, "04_vowel_outliers.csv")
write.csv(flagged, out, row.names = FALSE)

cat(sprintf(
  "Flagged %d of %d tokens (%.2f%%) at Mahalanobis > %.1f\n",
  nrow(flagged), nrow(tokens), 100 * nrow(flagged) / nrow(tokens), THRESHOLD
))
cat(sprintf("Distance range among flagged: %.2f to %.2f\n",
            min(flagged$mahal), max(flagged$mahal)))
cat("\nFlagged tokens by vowel:\n")
print(as.data.frame(count(flagged, vowel_label, name = "flagged")), row.names = FALSE)
cat(sprintf("\nSpeakers with flagged tokens: %d of %d\n",
            length(unique(flagged$speaker)), length(unique(tokens$speaker))))
cat(sprintf("Wrote %s\n", out))
cat("\nReview columns: fill in `verdict` (keep / remeasure / exclude) and `notes`.\n")
