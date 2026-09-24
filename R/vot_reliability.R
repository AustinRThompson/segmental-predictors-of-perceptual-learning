# Inter- and intra-measurer reliability for the manual VOT measurements.
#
# A handful of speakers were segmented independently by two RAs (a QC /
# spot-check overlap). build_vot_file_index() keeps only the copy in the RA
# folder named by Progress.xlsx's "Assigned" column and discards the other;
# this module recovers the discarded copies and uses them as a reliability
# subset.
#
# The comparison is on the `targets` tier, which carries the hand-placed VOT
# boundaries. The `labels` and `words` tiers are machine-generated and are
# near-identical between coders, so comparing across all tiers would wildly
# overstate agreement.
#
# Exclusions match summarise_stop_measures(): starred labels, "no burst" /
# "no clear burst" annotations, and anything outside /p/, /t/, /k/.

# Find Austin's original and repeated TextGrids. Only speakers represented in
# intra_reliability are included, and both files must exist. The returned paths
# are tracked as files by targets so edits to either occasion invalidate the
# reliability analysis.
list_intra_measurer_vot_textgrids <- function(base_dir, coder = "Austin") {
  original_dir <- file.path(base_dir, coder)
  repeat_dir <- file.path(original_dir, "intra_reliability")
  repeat_paths <- list.files(
    repeat_dir,
    pattern = "_targets?[.]TextGrid$",
    full.names = TRUE
  )
  speakers <- sub("_targets?[.]TextGrid$", "", basename(repeat_paths))
  original_paths <- file.path(original_dir, paste0(speakers, "_targets.TextGrid"))

  if (any(!file.exists(original_paths))) {
    stop(
      "Missing original TextGrid(s) for: ",
      paste(speakers[!file.exists(original_paths)], collapse = ", ")
    )
  }

  c(original_paths, repeat_paths)
}

# Label each tracked file by speaker and measurement occasion so the existing
# token reader/pairer can be reused without treating intra_reliability as a
# second person's name.
index_intra_measurer_files <- function(vot_paths) {
  data.frame(path = vot_paths, stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      speaker = sub("_targets?[.]TextGrid$", "", basename(path)),
      coder = dplyr::if_else(
        basename(dirname(path)) == "intra_reliability",
        "repeat",
        "original"
      )
    ) %>%
    dplyr::arrange(speaker, coder)
}

# Speakers whose TextGrids appear in more than one RA folder. Returns one row
# per (speaker, coder) so the pair can be mapped over downstream.
find_double_coded_files <- function(vot_paths) {
  data.frame(path = vot_paths, stringsAsFactors = FALSE) %>%
    dplyr::filter(grepl("_target", basename(path))) %>%
    dplyr::mutate(
      coder = basename(dirname(path)),
      speaker = sub("\\..*", "", basename(path)),
      speaker = sub("_targets?$", "", speaker)
    ) %>%
    dplyr::group_by(speaker) %>%
    dplyr::filter(dplyr::n_distinct(coder) > 1) %>%
    dplyr::arrange(speaker, coder) %>%
    dplyr::ungroup()
}

# Hand-placed VOT intervals from one TextGrid, in file order, with the same
# exclusions the analysis pipeline applies.
read_vot_targets <- function(path, speaker, coder) {
  tg <- rPraat::tg.read(path, encoding = "auto")
  as.data.frame(tg$targets) %>%
    dplyr::filter(label != "", label != " ") %>%
    dplyr::mutate(
      speaker = speaker,
      coder = coder,
      index = dplyr::row_number(),
      key = tolower(gsub("[[:blank:]]", "", label)),
      vot_ms = (t2 - t1) * 1000,
      target_stop = toupper(sub("_.*", "", key)),
      exclude = grepl("[*]", key) |
        grepl("noburst", key) |
        grepl("noclearburst", key)
    ) %>%
    dplyr::select(speaker, coder, index, key, target_stop, vot_ms, exclude)
}

# Pair the two coders' tokens for one speaker. Tokens are matched on position
# within the tier AND on label text, so a token is compared only when both
# coders annotated the same interval the same way.
pair_double_coded_tokens <- function(tokens) {
  coders <- sort(unique(tokens$coder))
  stopifnot(length(coders) == 2L)

  a <- dplyr::filter(tokens, coder == coders[1])
  b <- dplyr::filter(tokens, coder == coders[2])

  dplyr::inner_join(
    dplyr::select(a, index, key, target_stop, exclude, vot_a = vot_ms),
    dplyr::select(b, index, key, vot_b = vot_ms),
    by = c("index", "key")
  ) %>%
    dplyr::filter(!exclude, target_stop %in% c("P", "T", "K")) %>%
    dplyr::mutate(
      speaker = tokens$speaker[1],
      coder_a = coders[1],
      coder_b = coders[2]
    ) %>%
    dplyr::select(speaker, coder_a, coder_b, key, target_stop, vot_a, vot_b)
}

# Token-level agreement plus the propagation into VOT CV, which is what the
# models actually use. Agreement on individual tokens matters less than whether
# the speaker-level measure is stable, so both are reported.
summarise_vot_reliability <- function(paired) {
  # ICC(1,1), one-way random effects. Different speakers were coded by
  # different pairs of RAs (Austin/Leslie for some, Adrienne/Austin for
  # others), so raters are not crossed with subjects and the two-way model
  # does not apply. The point estimate is the same either way; the one-way
  # interval is the correct one.
  agreement <- irr::icc(
    as.matrix(paired[, c("vot_a", "vot_b")]),
    model = "oneway", unit = "single"
  )

  weighted_cv <- function(vot, stop) {
    parts <- lapply(split(vot, stop), function(x) {
      c(cv = stats::sd(x) / mean(x) * 100, n = length(x))
    })
    parts <- do.call(rbind, parts)
    sum(parts[, "cv"] * parts[, "n"]) / sum(parts[, "n"])
  }

  by_speaker <- paired %>%
    dplyr::group_by(speaker) %>%
    dplyr::summarise(
      n_tokens = dplyr::n(),
      cv_a = weighted_cv(vot_a, target_stop),
      cv_b = weighted_cv(vot_b, target_stop),
      .groups = "drop"
    ) %>%
    dplyr::mutate(cv_difference = cv_a - cv_b)

  # Per-pair bias. An aggregate bias would be meaningless because the coder
  # in each column varies by speaker; reported per pair, the differences are
  # transitive (Adrienne < Austin < Leslie), i.e. a small but systematic
  # coder effect rather than noise.
  bias <- paired %>%
    dplyr::group_by(speaker, coder_a, coder_b) %>%
    dplyr::summarise(bias_ms = mean(vot_a - vot_b), .groups = "drop")

  data.frame(
    n_speakers = length(unique(paired$speaker)),
    n_tokens = nrow(paired),
    icc = agreement$value,
    icc_lower = agreement$lbound,
    icc_upper = agreement$ubound,
    mean_absolute_difference_ms = mean(abs(paired$vot_a - paired$vot_b)),
    median_absolute_difference_ms = stats::median(abs(paired$vot_a - paired$vot_b)),
    largest_pair_bias_ms = bias$bias_ms[which.max(abs(bias$bias_ms))],
    mean_absolute_cv_difference = mean(abs(by_speaker$cv_difference))
  )
}
