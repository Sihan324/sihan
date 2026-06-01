# Compact tables for final report writing.

source("R/00_setup.R")
source("R/05_scenario_forecasts.R")

report_table_dir <- file.path("data", "processed", "report_tables")
dir.create(report_table_dir, recursive = TRUE, showWarnings = FALSE)

tfr_model_summary <- readr::read_csv(
  "data/processed/models/tfr_model_summary.csv",
  show_col_types = FALSE
)
birth_model_summary <- readr::read_csv(
  "data/processed/models/birth_model_summary.csv",
  show_col_types = FALSE
)
tfr_accuracy <- readr::read_csv(
  "data/processed/evaluation/tfr_forecast_accuracy.csv",
  show_col_types = FALSE
)
birth_accuracy <- readr::read_csv(
  "data/processed/evaluation/birth_forecast_accuracy.csv",
  show_col_types = FALSE
)
tfr_residual_tests <- readr::read_csv(
  "data/processed/models/tfr_residual_ljung_box.csv",
  show_col_types = FALSE
)
birth_residual_tests <- readr::read_csv(
  "data/processed/models/birth_residual_ljung_box.csv",
  show_col_types = FALSE
)
tfr_scenarios <- readr::read_csv(
  "data/processed/scenarios/tfr_scenario_paths.csv",
  show_col_types = FALSE
)
birth_scenario_forecasts <- readr::read_csv(
  "data/processed/scenarios/birth_scenario_forecasts.csv",
  show_col_types = FALSE
)
fertility_births <- readr::read_csv(
  "data/processed/singapore_fertility_births.csv",
  show_col_types = FALSE
)

selected_tfr_model <- "arima_drift"
selected_birth_model <- "arima_with_tfr"

model_selection_summary <- bind_rows(
  tfr_model_summary |>
    filter(.model == selected_tfr_model) |>
    left_join(tfr_accuracy, by = ".model") |>
    left_join(tfr_residual_tests, by = ".model") |>
    transmute(
      series = "TFR",
      selected_model = .model,
      aicc = round(AICc, 3),
      holdout_rmse = round(RMSE, 3),
      ljung_box_pvalue = round(lb_pvalue, 3),
      decision_note = "Selected for forecasting because it has the lowest holdout RMSE."
    ),
  birth_model_summary |>
    filter(.model == selected_birth_model) |>
    left_join(birth_accuracy, by = ".model") |>
    left_join(birth_residual_tests, by = ".model") |>
    transmute(
      series = "Log total live-births",
      selected_model = .model,
      aicc = round(AICc, 3),
      holdout_rmse = round(RMSE, 3),
      ljung_box_pvalue = round(lb_pvalue, 3),
      decision_note = "Selected as the best conditional model, but residual autocorrelation remains."
    )
)

forecast_accuracy_rankings <- bind_rows(
  tfr_accuracy |>
    transmute(series = "TFR", model = .model, rmse = RMSE, mae = MAE, mape = MAPE),
  birth_accuracy |>
    transmute(series = "Log total live-births", model = .model, rmse = RMSE, mae = MAE, mape = MAPE)
) |>
  group_by(series) |>
  arrange(rmse, .by_group = TRUE) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  select(series, rank, model, rmse, mae, mape)

last_observed <- fertility_births |>
  filter(year == max(year)) |>
  transmute(
    last_observed_year = year,
    last_observed_tfr = tfr,
    last_observed_live_births = total_live_births
  )

scenario_end_year <- max(tfr_scenarios$year)
model_trend_births <- birth_scenario_forecasts |>
  filter(scenario == "model_trend", year == scenario_end_year) |>
  pull(projected_live_births)

scenario_endpoint_summary <- tfr_scenarios |>
  filter(year == scenario_end_year) |>
  left_join(
    birth_scenario_forecasts |>
      filter(year == scenario_end_year),
    by = c("scenario", "year")
  ) |>
  transmute(
    scenario,
    year,
    tfr_assumption = round(tfr, 3),
    projected_live_births,
    difference_from_model_trend = projected_live_births - model_trend_births
  ) |>
  arrange(projected_live_births)

data_window_summary <- fertility_births |>
  summarise(
    start_year = min(year),
    end_year = max(year),
    observations = n(),
    start_tfr = tfr[which.min(year)],
    end_tfr = tfr[which.max(year)],
    start_live_births = total_live_births[which.min(year)],
    end_live_births = total_live_births[which.max(year)]
  )

readr::write_csv(
  model_selection_summary,
  file.path(report_table_dir, "model_selection_summary.csv")
)
readr::write_csv(
  forecast_accuracy_rankings,
  file.path(report_table_dir, "forecast_accuracy_rankings.csv")
)
readr::write_csv(
  scenario_endpoint_summary,
  file.path(report_table_dir, "scenario_endpoint_summary.csv")
)
readr::write_csv(
  data_window_summary,
  file.path(report_table_dir, "data_window_summary.csv")
)

list(
  model_selection_summary = model_selection_summary,
  forecast_accuracy_rankings = forecast_accuracy_rankings,
  scenario_endpoint_summary = scenario_endpoint_summary,
  data_window_summary = data_window_summary,
  last_observed = last_observed
)
