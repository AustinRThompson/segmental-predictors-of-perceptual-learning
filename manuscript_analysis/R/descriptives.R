#!/usr/bin/env Rscript

# Descriptive statistics and the illustrative variability figure.
# Run from manuscript_analysis with: Rscript R/descriptives.R
#
# Token-level VOT is read from the root targets store (target
# `stop_target_tokens`) and passed through the same exclusion rules used in
# R/stop_pipeline.R, so token counts reconcile with summary_stop_measures.csv.
# Token-level formants are read from the interim vowel pipeline output.

suppressMessages({
  library(dplyr)
  library(ggplot2)
})

analysis_dir <- "."
repo_root <- ".."
data_dir <- file.path(repo_root, "data", "02_preppedData")
results_dir <- file.path(analysis_dir, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------- speaker data

stop_data <- read.csv(file.path(data_dir, "summary_stop_measures.csv"))
vowel_data <- read.csv(file.path(data_dir, "summary_vowel_measures.csv"))
sti_bb <- read.csv(file.path(data_dir, "Aggregate_Results_Table_BB.csv"))
sti_bbap <- read.csv(file.path(data_dir, "Aggregate_Results_Table_BBAP.csv"))

id_columns <- c(
  "speaker", "age", "sex", "pretest", "posttest", "learning", "rate_sps"
)
dat <- merge(stop_data, vowel_data, by = id_columns, all = FALSE)
stopifnot(nrow(dat) == 40L)

# Build the speaker keys and average the previously calculated, bias-corrected
# STI values across the two repeated phrases.
add_sti_speaker_key <- function(x) {
  participant_number <- sub("-.*", "", x$ID)
  participant_number <- sub("P", "", participant_number, fixed = TRUE)
  participant_number <- sprintf("%02d", as.integer(participant_number))
  sex_age <- sub(".*-", "", x$ID)
  x$speaker <- paste0(sex_age, "_", participant_number)
  x
}

sti_bb <- add_sti_speaker_key(sti_bb)
sti_bbap <- add_sti_speaker_key(sti_bbap)
sti_data <- merge(
  sti_bb[c("speaker", "STIBC")],
  sti_bbap[c("speaker", "STIBC")],
  by = "speaker",
  all = FALSE,
  suffixes = c("_BB", "_BBAP")
)
sti_data$STI <- rowMeans(sti_data[c("STIBC_BB", "STIBC_BBAP")])

dat <- merge(dat, sti_data[c("speaker", "STI")], by = "speaker", all.x = TRUE)
stopifnot(nrow(dat) == 40L, !anyNA(dat$STI))

# ------------------------------------------------------------ token-level VOT

vot_tokens <- targets::tar_read(stop_target_tokens, store = file.path(repo_root, "_targets")) %>%
  mutate(
    VOT = (t2 - t1) * 1000,
    exclude = case_when(
      grepl("[*]", label) ~ TRUE,
      substr(tolower(word), 1, 2) == "tr" ~ TRUE,
      grepl("no burst", label) ~ TRUE,
      grepl("no clear burst", label) ~ TRUE,
      TRUE ~ FALSE
    ),
    targetStop = sub("\\_.*", "", label),
    targetStop = toupper(gsub("[[:blank:]]", "", targetStop)),
    targetStop = factor(targetStop, levels = c("P", "T", "K", "B", "D", "G"))
  ) %>%
  filter(!exclude, targetStop %in% c("P", "T", "K")) %>%
  group_by(word) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  distinct()

target_words <- unique(tolower(vot_tokens$word))
target_words <- sort(target_words)
target_words <- target_words[!grepl("^(cr|ch|cl|pl|pr|qu|tr)", target_words)]
target_words <- setdiff(target_words, "to")
vot_tokens <- vot_tokens %>% filter(word %in% target_words) %>% select(-N)

message("VOT tokens after exclusions: ", nrow(vot_tokens),
        " across ", length(unique(vot_tokens$speaker)), " speakers")

# ---------------------------------------------------------- token-level vowels

vowel_tokens <- read.csv(file.path(
  repo_root, "data", "01_interim", "06_vowel_pipeline", "01_target_vowels.csv"
))


# ------------------------------------ Table 0: per-category means by speaker
# Feeds the measurement-validation sentences in the Results. Every value is a
# speaker mean summarised across speakers, NOT a token-level statistic, so the
# SDs describe between-speaker variation. The manuscript says so explicitly;
# keep the two in step if this is ever changed to token-level.

msd <- function(m, s, digits = 0) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), m, s)
}

by_stop <- vot_tokens %>%
  group_by(targetStop, speaker) %>%
  summarise(m = mean(VOT), .groups = "drop") %>%
  group_by(targetStop) %>%
  summarise(Mean = msd(mean(m), sd(m), 1), .groups = "drop") %>%
  mutate(
    Class = "Stops",
    Category = paste0("/", tolower(as.character(targetStop)), "/"),
    Measure = "VOT (ms)"
  ) %>%
  select(Class, Category, Measure, Mean)

vowel_ipa <- c(IY = "/i/", AE = "/æ/", AA = "/ɑ/", UW = "/u/")

by_vowel <- vowel_tokens %>%
  group_by(vowel_label, speaker) %>%
  summarise(f1 = mean(f1), f2 = mean(f2), .groups = "drop") %>%
  group_by(vowel_label) %>%
  summarise(
    `F1 (Hz)` = msd(mean(f1), sd(f1)),
    `F2 (Hz)` = msd(mean(f2), sd(f2)),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(c(`F1 (Hz)`, `F2 (Hz)`),
                      names_to = "Measure", values_to = "Mean") %>%
  mutate(Class = "Vowels", Category = unname(vowel_ipa[vowel_label])) %>%
  select(Class, Category, Measure, Mean)

by_category <- bind_rows(by_stop, by_vowel)
write.csv(by_category, file.path(results_dir, "descriptives_by_category.csv"),
          row.names = FALSE, na = "")


# ------------------------------------------------- Table 1: speaker-level range

describe <- function(x, digits = 2) {
  x <- x[is.finite(x)]
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f) [%.", digits, "f, %.", digits, "f]"),
    mean(x), sd(x), min(x), max(x)
  )
}

table1 <- data.frame(
  Measure = c(
    "Age (years)", "Speech rate (syll/s)",
    "Pretest intelligibility (%)", "Posttest intelligibility (%)",
    "Perceptual learning (%)",
    "VOT mean (ms)", "VOT SD (ms)", "VOT CV (%)",
    "Vowel MCV", "Acoustic STI"
  ),
  `M (SD) [min, max]` = c(
    describe(dat$age, 1), describe(dat$rate_sps),
    describe(dat$pretest), describe(dat$posttest), describe(dat$learning),
    describe(dat$vot_M, 1), describe(dat$vot_SD, 1), describe(dat$vot_CoV, 1),
    describe(dat$vowel_MCV, 3), describe(dat$STI, 1)
  ),
  check.names = FALSE
)
write.csv(table1, file.path(results_dir, "descriptives_speaker_level.csv"),
          row.names = FALSE)

# ----------------------------------------------- Table 2: token counts by class

vot_counts <- vot_tokens %>%
  count(speaker, targetStop) %>%
  group_by(targetStop) %>%
  summarise(
    speakers = n(),
    total_tokens = sum(n),
    median_per_speaker = median(n),
    min_per_speaker = min(n),
    max_per_speaker = max(n),
    .groups = "drop"
  ) %>%
  rename(category = targetStop)

vowel_counts <- vowel_tokens %>%
  count(speaker, vowel_label) %>%
  group_by(vowel_label) %>%
  summarise(
    speakers = n(),
    total_tokens = sum(n),
    median_per_speaker = median(n),
    min_per_speaker = min(n),
    max_per_speaker = max(n),
    .groups = "drop"
  ) %>%
  rename(category = vowel_label)

token_counts <- bind_rows(
  mutate(vot_counts, class = "Stop (VOT)"),
  mutate(vowel_counts, class = "Vowel (F1/F2)")
) %>%
  select(class, category, speakers, total_tokens,
         median_per_speaker, min_per_speaker, max_per_speaker)
write.csv(token_counts, file.path(results_dir, "descriptives_token_counts.csv"),
          row.names = FALSE)

# --------------------------------------- Table 3: distributional shape of VOT
# Koenig (2001) reports rightward skew in children's voiceless-stop VOT and
# cautions that mean/SD summaries can misrepresent such data. These are her
# diagnostics: skew, the mean-median gap, and Shapiro-Wilk W.

skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  sum((x - m)^3) / n / (sum((x - m)^2) / n)^(3 / 2)
}

vot_shape <- vot_tokens %>%
  group_by(speaker, targetStop) %>%
  filter(n() >= 5) %>%
  summarise(
    n = n(),
    skew = skewness(VOT),
    mean_minus_median = mean(VOT) - median(VOT),
    shapiro_p = if (n() >= 3 && n() <= 5000) shapiro.test(VOT)$p.value else NA_real_,
    .groups = "drop"
  )

vot_shape_summary <- vot_shape %>%
  group_by(targetStop) %>%
  summarise(
    n_distributions = n(),
    median_skew = median(skew, na.rm = TRUE),
    pct_positive_skew = 100 * mean(skew > 0, na.rm = TRUE),
    median_mean_minus_median_ms = median(mean_minus_median, na.rm = TRUE),
    pct_nonnormal_p05 = 100 * mean(shapiro_p < .05, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(vot_shape, file.path(results_dir, "descriptives_vot_shape_by_speaker.csv"),
          row.names = FALSE)
write.csv(vot_shape_summary, file.path(results_dir, "descriptives_vot_shape_summary.csv"),
          row.names = FALSE)

# ------------------------------------------------- Table 4: correlation matrix
# APA-style matrix: speaker-level M and SD alongside the lower-triangle
# correlations among all variables entering the models. Replaces the earlier
# by-category descriptives table.

cor_vars <- dat %>%
  transmute(
    `VOT CV (%)` = vot_CoV,
    `Vowel MCV` = vowel_MCV,
    `Acoustic STI` = STI,
    `Speech rate (syll/s)` = rate_sps,
    `Age (years)` = age,
    `Pretest intelligibility (%)` = pretest,
    `Perceptual learning (%)` = learning
  )

star <- function(p) {
  if (is.na(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}
# APA style: drop the leading zero from correlations.
fmt_r <- function(r, p) paste0(sub("^(-?)0", "\\1", sprintf("%.2f", r)), star(p))

k <- ncol(cor_vars)
# MCV is on a much smaller scale than the other variables, so digits vary.
msd_digits <- c(2, 3, 2, 2, 1, 2, 2)
# Columns are headed by the variable name rather than an index, so the matrix
# can be read without cross-referencing the row numbering. Short forms keep the
# header row narrow; the row stub carries the full name and units.
short_names <- c(
  "VOT CV", "Vowel MCV", "STI", "Speech rate", "Age", "Pretest", "Learning"
)
cor_tbl <- data.frame(
  Variable = names(cor_vars),
  M  = mapply(function(x, d) sprintf(paste0("%.", d, "f"), mean(x)),
              cor_vars, msd_digits),
  SD = mapply(function(x, d) sprintf(paste0("%.", d, "f"), sd(x)),
              cor_vars, msd_digits),
  stringsAsFactors = FALSE
)
for (j in seq_len(k - 1)) {
  col <- character(k)
  for (i in seq_len(k)) {
    if (i > j) {
      ct <- suppressWarnings(cor.test(cor_vars[[i]], cor_vars[[j]]))
      col[i] <- fmt_r(unname(ct$estimate), ct$p.value)
    } else if (i == j) {
      col[i] <- "\u2014"
    } else {
      col[i] <- ""
    }
  }
  cor_tbl[[short_names[j]]] <- col
}
rownames(cor_tbl) <- NULL
write.csv(cor_tbl, file.path(results_dir, "descriptives_correlations.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------------- report

cat("\n== Table 1: speaker-level descriptives ==\n"); print(table1, row.names = FALSE)
cat("\n== Table 2: token counts ==\n"); print(as.data.frame(token_counts), row.names = FALSE)
cat("\n== Table 3: VOT distributional shape ==\n"); print(as.data.frame(vot_shape_summary), row.names = FALSE)
cat("\n== Table 4: correlations ==\n"); print(cor_tbl, row.names = FALSE)
cat("\nWrote results/\n")
