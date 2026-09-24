# Step function for the "# Perceptual Data" section of data_prep.Rmd, used
# by the root _targets.R. Merges prior-project speech rate and perceptual
# learning scores (PWC = Percent Words Correct) for speakers currently
# tracked in Progress.xlsx.
build_pwc_data <- function(progress_file, speech_rate_file, pwc_raw_file) {
  progress <- rio::import(progress_file) %>%
    dplyr::select(Speaker, Assigned, Status)

  speech_rate <- rio::import(speech_rate_file) %>%
    dplyr::select(Participant = ID, rate_sps = SpeechRate_SPS) %>%
    dplyr::mutate(
      Participant = ifelse(Participant == "M51", "P01-M5", Participant),
      pnumber = sub("_.*", "", Participant),
      pnumber = gsub("P", "", pnumber),
      pnumber = sprintf("%02d", as.numeric(pnumber)),
      sexAge = sub(".*_", "", Participant),
      Sex = substr(toupper(sexAge), 1, 1),
      Age = as.numeric(substr(toupper(sexAge), 2, 2)),
      Speaker = paste0(sexAge, "_", pnumber)
    ) %>%
    dplyr::select(Speaker, Sex, Age, rate_sps)

  pwc <- rio::import(pwc_raw_file) %>%
    dplyr::mutate(
      Participant = ifelse(Participant == "M51", "P01-M5", Participant),
      pnumber = sub("-.*", "", Participant),
      pnumber = gsub("P", "", pnumber),
      pnumber = sprintf("%02d", as.numeric(pnumber)),
      sexAge = sub(".*-", "", Participant),
      Sex = substr(toupper(sexAge), 1, 1),
      Age = as.numeric(substr(toupper(sexAge), 2, 2)),
      Speaker = paste0(sexAge, "_", pnumber)
    ) %>%
    dplyr::select(Speaker, Sex, Age, Phase, PWC) %>%
    tidyr::pivot_wider(names_from = Phase, values_from = PWC) %>%
    dplyr::mutate(Learning = `Post-Test` - `Pre-Test`) %>%
    merge(speech_rate, all = TRUE)

  pwc[pwc$Speaker %in% progress$Speaker, ]
}
