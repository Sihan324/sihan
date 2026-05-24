# Data import and cleaning.

source("R/00_setup.R")

raw_data_dir <- file.path("data", "raw")
processed_data_dir <- file.path("data", "processed")

dataset_id <- "d_e39eeaeadb571c0d0725ef1eec48d166"
dataset_url <- paste0(
  "https://data.gov.sg/api/action/datastore_search?resource_id=",
  dataset_id,
  "&limit=100"
)

raw_json_path <- file.path(raw_data_dir, "births_and_fertility_rates_annual.json")
processed_csv_path <- file.path(processed_data_dir, "singapore_fertility_births.csv")

if (!file.exists(raw_json_path)) {
  download.file(dataset_url, raw_json_path, mode = "wb", quiet = FALSE)
}

raw_payload <- jsonlite::fromJSON(raw_json_path)
raw_records <- raw_payload$result$records
year_columns <- grep("^[0-9]{4}$", names(raw_records), value = TRUE)

fertility_long <- raw_records |>
  mutate(data_series = trimws(DataSeries)) |>
  select(data_series, all_of(year_columns)) |>
  pivot_longer(
    cols = all_of(year_columns),
    names_to = "year",
    values_to = "value"
  ) |>
  mutate(
    year = as.integer(year),
    value = na_if(as.character(value), "na"),
    value = as.numeric(value),
    series = case_when(
      data_series == "Total Fertility Rate (TFR)" ~ "tfr",
      data_series == "Total Live-Births" ~ "total_live_births",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(series)) |>
  select(year, series, value)

fertility_births <- fertility_long |>
  pivot_wider(names_from = series, values_from = value) |>
  arrange(year) |>
  mutate(
    log_total_live_births = log(total_live_births),
    tfr_difference = difference(tfr),
    log_births_difference = difference(log_total_live_births)
  ) |>
  as_tsibble(index = year)

readr::write_csv(fertility_births, processed_csv_path)

fertility_births
