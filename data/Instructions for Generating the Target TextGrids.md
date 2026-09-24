Author: Austin Thompson

Date: May 22, 2025

# Description

These are the instructions for obtaining the `SPEAKER_targets.TextGrid` in the `01_interim/03_target_stops` folder.

1.  In the audio folder, select all of the .wav files and open them in Praat.
2.  In Praat, highlight them all and select `Combine > Concatenate recoverably`.
    -   This will generate a combined sound chain and a TextGrid, which specifies the names of the original files and their boundaries.
    -   It might be a good idea to remove all of the separate audio files in the Praat Object window.
3.  Go to the speaker’s folder in the `01_interim/01_mfa_output` folder. This is the output from the forced alignment. Select all of the TextGrids and open them in Praat.
4.  With all of the TextGrids highlighted, select `Concatenate`.
    -   It might be a good idea to remove all of the separate TextGrids in the Praat Object window.
5.  Now, you should be left with a (1) Sound chain, (2) a TextGrid chain, and (3) a TextGrid chain. Select the two TextGrid chains and click `Merge`.
    -   This step merges the recoverable audio file TextGrid tier with the output from the forced alignment.
6.  Save the sound chain into the `01_interim/02_merged_mfa_output` folder as `SPEAKER.wav`.
7.  Save the TextGrid merged into the `01_interim/02_merged_mfa_output` folder as `SPEAKER.TextGrid`.
8.  Open the `data_prep.Rmd` document and located the “Separate out the targets” block within the code. Manually change the line: `speaker <- "M8_05"` to be the current Speaker ID. Then run the block.
    -   This will generate the final `SPEAKER_targets.TextGrid` object (saved to `01_interim/03_target_stops`) that will be manually assessed by lab team members.
9.  All done!
