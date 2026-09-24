#!/usr/bin/env Rscript

# Reproduce the analyses reported in the final manuscript.
# Run from manuscript_analysis with: Rscript R/analysis.R

analysis_dir <- "."
# Read directly from the targets-produced prepped data at the repo root,
# rather than a manually copied local snapshot.
data_dir <- file.path("..", "data", "02_preppedData")
results_dir <- file.path(analysis_dir, "results")
tables_dir <- file.path(analysis_dir, "tables")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# Table helpers: format_model_table(), strip_apa_leading_zeros()
source(file.path(analysis_dir, "R", "model_tables.R"))

stop_data <- read.csv(file.path(data_dir, "summary_stop_measures.csv"))
vowel_data <- read.csv(file.path(data_dir, "summary_vowel_measures.csv"))
sti_bb <- read.csv(file.path(data_dir, "Aggregate_Results_Table_BB.csv"))
sti_bbap <- read.csv(file.path(data_dir, "Aggregate_Results_Table_BBAP.csv"))

id_columns <- c(
  "speaker", "age", "sex", "pretest", "posttest", "learning", "rate_sps"
)
dat <- merge(stop_data, vowel_data, by = id_columns, all = FALSE)
stopifnot(nrow(dat) == 40L, !anyDuplicated(dat$speaker))

# The manuscript reports vowel MCV effects per one sample SD. Scaling is done once,
# before fitting, so the coefficient units are explicit and reproducible.
vowel_MCV_mean <- mean(dat$vowel_MCV)
vowel_MCV_sd <- sd(dat$vowel_MCV)
dat$vowel_MCV_z <- (dat$vowel_MCV - vowel_MCV_mean) / vowel_MCV_sd
pretest_mean <- mean(dat$pretest)
dat$pretest_c <- dat$pretest - pretest_mean
vot_CoV_mean <- mean(dat$vot_CoV)
dat$vot_CoV_c <- dat$vot_CoV - vot_CoV_mean

# RQ1: baseline intelligibility, controlling for speaking rate.
rq1_stop <- lm(pretest ~ rate_sps + vot_CoV, data = dat)
rq1_vowel <- lm(pretest ~ rate_sps + vowel_MCV_z, data = dat)
# Figure-only reparameterization: scale(vowel_MCV) in-formula carries the
# scaled:center/scale attributes create_figures.R uses to plot predictions on
# the raw MCV axis. Statistically identical to rq1_vowel (same standardization).
rq1_vowel_plot <- lm(pretest ~ rate_sps + scale(vowel_MCV), data = dat)

# RQ2: perceptual learning, including the variability-by-pretest interaction
# and controlling for speaking rate.
rq2_stop <- lm(learning ~ rate_sps + pretest_c * vot_CoV_c, data = dat)
rq2_vowel <- lm(learning ~ rate_sps + pretest_c * vowel_MCV_z, data = dat)
rq2_stop_plot <- lm(learning ~ pretest_c * vot_CoV_c, data = dat)
rq2_vowel_plot <- lm(learning ~ pretest_c * vowel_MCV_z, data = dat)

attr(rq2_stop, "pretest_mean") <- pretest_mean
attr(rq2_stop, "vot_CoV_mean") <- vot_CoV_mean
attr(rq2_vowel, "pretest_mean") <- pretest_mean
attr(rq2_stop_plot, "pretest_mean") <- pretest_mean
attr(rq2_stop_plot, "vot_CoV_mean") <- vot_CoV_mean
attr(rq2_vowel_plot, "pretest_mean") <- pretest_mean
attr(rq2_vowel_plot, "vowel_MCV_mean") <- vowel_MCV_mean
attr(rq2_vowel_plot, "vowel_MCV_sd") <- vowel_MCV_sd

saveRDS(rq1_stop, file.path("models", "RQ1_model_stops.RDS"))
# Save the scale()-in-formula version so the figure code can back-transform.
saveRDS(rq1_vowel_plot, file.path("models", "RQ1_model_vowels.RDS"))
saveRDS(rq2_stop, file.path("models", "RQ2_model_stops.RDS"))
saveRDS(rq2_vowel, file.path("models", "RQ2_model_vowels.RDS"))
saveRDS(
  rq2_stop_plot,
  file.path("models", "RQ2_parsimonious_model_stops.RDS")
)
saveRDS(
  rq2_vowel_plot,
  file.path("models", "RQ2_parsimonious_model_vowels.RDS")
)

# Average the previously calculated, bias-corrected STI values across the two
# repeated phrases to obtain one speaker-level suprasegmental measure.
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

sti_joined <- merge(
  sti_data[c("speaker", "STI")],
  dat[c("speaker", "vot_CoV", "vowel_MCV")],
  by = "speaker",
  all = FALSE
)
stopifnot(nrow(sti_joined) == 40L, !anyDuplicated(sti_joined$speaker))

posthoc_stop <- cor.test(sti_joined$STI, sti_joined$vot_CoV)
posthoc_vowel <- cor.test(sti_joined$STI, sti_joined$vowel_MCV)

tidy_lm <- function(model, analysis) {
  coefficient_table <- coef(summary(model))
  data.frame(
    analysis = analysis,
    term = rownames(coefficient_table),
    estimate = coefficient_table[, "Estimate"],
    std_error = coefficient_table[, "Std. Error"],
    statistic = coefficient_table[, "t value"],
    p_value = coefficient_table[, "Pr(>|t|)"],
    r_squared = summary(model)$r.squared,
    adjusted_r_squared = summary(model)$adj.r.squared,
    n = nobs(model),
    row.names = NULL,
    check.names = FALSE
  )
}

model_results <- rbind(
  tidy_lm(rq1_stop, "RQ1: stop variability"),
  tidy_lm(rq1_vowel, "RQ1: vowel variability"),
  tidy_lm(rq2_stop, "RQ2: stop variability"),
  tidy_lm(rq2_vowel, "RQ2: vowel variability")
)
write.csv(
  model_results,
  file.path(results_dir, "model_coefficients.csv"),
  row.names = FALSE
)

# RQ2-only table for the manuscript Results. Its knitr fragment is retained for
# quick browser inspection; the manuscript itself reads the CSV written below.
rq2_model_table <- sjPlot::tab_model(
  rq2_stop,
  rq2_vowel,
  dv.labels = c("VOT CV", "Vowel MCV"),
  pred.labels = c(
    "Intercept",
    "Speech rate",
    "Pretest intelligibility (centered)",
    "VOT CV (centered)",
    "Pretest \u00d7 VOT CV",
    "Vowel MCV (1 SD)",
    "Pretest \u00d7 vowel MCV"
  ),
  show.ci = 0.95,
  show.se = FALSE,
  show.stat = FALSE,
  show.p = TRUE,
  show.r2 = TRUE,
  show.aic = TRUE,
  show.obs = TRUE,
  digits = 3,
  digits.p = 3,
  digits.rsq = 3,
  file = file.path(tables_dir, "RQ2_model_table.html")
)
writeLines(
  rq2_model_table$knitr,
  file.path(tables_dir, "RQ2_model_table_fragment.html")
)
strip_apa_leading_zeros(
  file.path(tables_dir, "RQ2_model_table_fragment.html")
)

# Word-renderable version of the same table (see R/model_tables.R). The sjPlot
# HTML above is kept for quick inspection; the manuscript reads this CSV.
write.csv(
  format_model_table(
    models = list("VOT CV" = rq2_stop, "Vowel MCV" = rq2_vowel),
    labels = c(
      "(Intercept)" = "Intercept",
      "rate_sps" = "Speech rate",
      "pretest_c" = "Pretest intelligibility (centered)",
      "vot_CoV_c" = "VOT CV (centered)",
      "pretest_c:vot_CoV_c" = "Pretest × VOT CV",
      "vowel_MCV_z" = "Vowel MCV (1 SD)",
      "pretest_c:vowel_MCV_z" = "Pretest × vowel MCV"
    )
  ),
  file.path(tables_dir, "RQ2_model_table.csv"),
  row.names = FALSE
)

# Standalone styled table for manual insertion into the Word manuscript.
# gt cannot produce a Word-valid table on its own, so the manuscript renders
# only the caption for docx and this file is pasted in by hand.
gt::gtsave(
  style_model_table(
    read.csv(file.path(tables_dir, "RQ2_model_table.csv"), check.names = FALSE,
             colClasses = "character")
  ),
  file.path(tables_dir, "RQ2_model_table_gt.html")
)


correlation_results <- data.frame(
  analysis = c("STI x VOT CV", "STI x vowel MCV"),
  r = c(unname(posthoc_stop$estimate), unname(posthoc_vowel$estimate)),
  statistic = c(unname(posthoc_stop$statistic), unname(posthoc_vowel$statistic)),
  df = c(unname(posthoc_stop$parameter), unname(posthoc_vowel$parameter)),
  p_value = c(posthoc_stop$p.value, posthoc_vowel$p.value),
  conf_low = c(posthoc_stop$conf.int[1], posthoc_vowel$conf.int[1]),
  conf_high = c(posthoc_stop$conf.int[2], posthoc_vowel$conf.int[2]),
  n = nrow(sti_joined)
)
write.csv(
  correlation_results,
  file.path(results_dir, "segmental_sti_correlations.csv"),
  row.names = FALSE
)

sink(file.path(results_dir, "model_summaries.txt"))
cat("Final manuscript analysis reproduction\n")
cat("Generated:", format(Sys.time(), tz = "UTC"), "UTC\n\n")
models <- list(
  "RQ1: stop variability" = rq1_stop,
  "RQ1: vowel variability" = rq1_vowel,
  "RQ2: stop variability" = rq2_stop,
  "RQ2: vowel variability" = rq2_vowel
)
for (model_name in names(models)) {
  cat("\n", strrep("=", 72), "\n", sep = "")
  cat(model_name, "\n")
  print(summary(models[[model_name]]))
}
cat("\n", strrep("=", 72), "\nSegmental–suprasegmental correlations\n", sep = "")
print(posthoc_stop)
print(posthoc_vowel)
sink()

cat("Analysis complete. Results written to", results_dir, "\n")
