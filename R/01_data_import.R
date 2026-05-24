# Data import and cleaning.
#
# Replace the file names below once the final dataset is confirmed.

source("R/00_setup.R")

raw_data_dir <- file.path("data", "raw")
processed_data_dir <- file.path("data", "processed")

candidate_files <- list.files(raw_data_dir, full.names = TRUE)

if (length(candidate_files) == 0) {
  message("No raw data files found in data/raw/. Add the final report dataset before running this script.")
}

# The final import code will be added here after the research question and
# dataset are confirmed.

