# Step functions for the "# Stop Data" / "## VOT Check" section of
# data_prep.Rmd, used by the root _targets.R. Converts the manually
# hash-cached VOT pipeline into a targets-tracked pipeline: each corrected
# TextGrid is its own branch, so editing one RA's segmentation only
# reprocesses that file, not the whole dataset.

# Discover the corrected VOT TextGrids across all RA folders. Returned as
# real file paths so the `format = "file"` target tracks each file's content
# and any added/removed file changes the target's value.
list_corrected_vot_textgrids <- function(base_dir, ra_names) {
  dirs <- file.path(base_dir, ra_names)
  unlist(lapply(dirs, list.files, full.names = TRUE), use.names = FALSE)
}

# Build one row per corrected VOT file, joined to Progress.xlsx for speaker
# metadata. `coder` is read from the file's actual containing folder name.
# A few speakers have a copy in more than one RA folder (spot-check/QC
# overlap, e.g. Austin plus the originally assigned RA) -- keep only the
# copy in the RA folder Progress.xlsx's "Assigned" column names, matching
# the original script's behavior (which built the path from Assigned).
build_vot_file_index <- function(vot_paths, progress_file) {
  progress <- rio::import(progress_file) %>%
    dplyr::select(Speaker, Assigned, Status)

  data.frame(path = vot_paths, stringsAsFactors = FALSE) %>%
    dplyr::filter(grepl("_target", basename(path))) %>%
    dplyr::mutate(
      file = basename(path),
      coder = basename(dirname(path)),
      Speaker = sub("\\..*", "", file),
      Speaker = gsub("_targets", "", Speaker)
    ) %>%
    dplyr::inner_join(progress, by = "Speaker") %>%
    dplyr::filter(coder == Assigned)
}

# Read the target-tier tokens from a single corrected TextGrid, and look up
# each token's containing word from the "words" tier. One branch per file.
read_stop_target_tokens <- function(path, speaker, coder) {
  tg <- rPraat::tg.read(path, encoding = "auto")
  words <- as.data.frame(tg$words)
  targets <- as.data.frame(tg$targets) %>%
    dplyr::filter(label != "", label != " ") %>%
    dplyr::mutate(
      coder = coder,
      speaker = speaker,
      file = basename(path),
      path = path,
      midpoint = ((t2 - t1) / 2) + t1
    )

  for (i in seq_len(nrow(targets))) {
    target_word <- words %>%
      dplyr::filter(t1 < targets$t2[i], t2 > targets$t2[i]) %>%
      dplyr::pull(label)
    targets$word[i] <- tolower(target_word)
  }

  targets
}

# Reduce step: apply exclusion rules, restrict to voiceless P/T/K, drop rare
# words and known clusters, and summarize to speaker-level (and
# speaker-by-stop-level) VOT coefficient of variation.
compute_stop_summaries <- function(all_target_stops, pwc_file) {
  pwc <- rio::import(pwc_file) %>%
    dplyr::rename(
      speaker = Speaker,
      sex = Sex,
      age = Age,
      pretest = `Pre-Test`,
      posttest = `Post-Test`,
      learning = `Learning`
    )

  all_target_stops <- all_target_stops %>%
    dplyr::mutate(
      VOT = (t2 - t1) * 1000,
      exclude = dplyr::case_when(
        grepl("[*]", label) ~ TRUE,
        substr(tolower(word), 1, 2) == "tr" ~ TRUE,
        grepl("no burst", label) ~ TRUE,
        grepl("no clear burst", label) ~ TRUE,
        TRUE ~ FALSE
      ),
      targetStop = sub("\\_.*", "", label),
      targetStop = toupper(targetStop),
      targetStop = gsub("[[:blank:]]", "", targetStop),
      targetStop = factor(targetStop, levels = c("P", "T", "K", "B", "D", "G")),
      age = sub("\\_.*", "", speaker),
      age = gsub("M", "", age),
      age = gsub("F", "", age),
      age = as.numeric(age)
    )

  voiceless_stops <- all_target_stops %>%
    dplyr::filter(exclude == FALSE, targetStop %in% c("P", "T", "K")) %>%
    dplyr::group_by(word) %>%
    dplyr::mutate(N = dplyr::n(), oneoffs = N <= 19) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  target_words <- unique(tolower(voiceless_stops$word)) %>%
    as.data.frame() %>%
    dplyr::rename(word = 1) %>%
    dplyr::arrange(word) %>%
    dplyr::filter(!stringr::str_starts(word, "cr|ch|cl|pl|pr|qu|tr")) %>%
    dplyr::filter(word != "to")

  voiceless_stops <- voiceless_stops %>%
    dplyr::filter(word %in% target_words$word) %>%
    dplyr::select(-oneoffs)

  summary_stop_measures <- voiceless_stops %>%
    dplyr::group_by(speaker, age, coder, targetStop) %>%
    dplyr::summarise(
      stop_n_tokens = dplyr::n(),
      vot_M = mean(VOT),
      vot_SD = sd(VOT),
      vot_CoV = (vot_SD / vot_M) * 100,
      .groups = "drop"
    ) %>%
    dplyr::group_by(speaker) %>%
    dplyr::summarise(
      vot_M = weighted.mean(vot_M, stop_n_tokens),
      vot_SD = weighted.mean(vot_SD, stop_n_tokens),
      vot_CoV = weighted.mean(vot_CoV, stop_n_tokens),
      stop_n_tokens = weighted.mean(stop_n_tokens, stop_n_tokens),
      .groups = "drop"
    ) %>%
    merge(pwc) %>%
    dplyr::select(speaker, age, sex, pretest, posttest, learning, rate_sps, vot_M:stop_n_tokens)

  summary_stop_measures_by_stop <- voiceless_stops %>%
    dplyr::group_by(speaker, age, coder, targetStop) %>%
    dplyr::summarise(
      stop_n_tokens = dplyr::n(),
      vot_M = mean(VOT),
      vot_SD = sd(VOT),
      vot_CoV = (vot_SD / vot_M) * 100,
      .groups = "drop"
    ) %>%
    merge(pwc) %>%
    dplyr::select(
      speaker, age, sex, pretest, posttest, learning, rate_sps,
      targetStop, stop_n_tokens:vot_CoV
    )

  list(
    summary_stop_measures = summary_stop_measures,
    summary_stop_measures_by_stop = summary_stop_measures_by_stop
  )
}
