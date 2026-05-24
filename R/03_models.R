# Candidate model fitting and diagnostics.

source("R/00_setup.R")
source("R/01_data_import.R")

model_dir <- file.path("data", "processed", "models")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# Annual data have no within-year seasonality, so the main candidate models are
# non-seasonal ARIMA models. Total live-births are modelled on the log scale.

tfr_models <- fertility_births |>
  model(
    arima_auto = ARIMA(tfr),
    arima_drift = ARIMA(tfr ~ 1 + pdq(0:3, 1, 0:3)),
    arima_no_drift = ARIMA(tfr ~ 0 + pdq(0:3, 1, 0:3))
  )

birth_models <- fertility_births |>
  model(
    arima_auto = ARIMA(log_total_live_births),
    arima_drift = ARIMA(log_total_live_births ~ 1 + pdq(0:3, 1, 0:3)),
    arima_no_drift = ARIMA(log_total_live_births ~ 0 + pdq(0:3, 1, 0:3)),
    arima_with_tfr = ARIMA(log_total_live_births ~ tfr + pdq(0:3, 0:1, 0:3))
  )

tfr_model_summary <- glance(tfr_models) |>
  arrange(AICc)

birth_model_summary <- glance(birth_models) |>
  arrange(AICc)

tfr_model_coefficients <- tidy(tfr_models)
birth_model_coefficients <- tidy(birth_models)

tfr_residual_tests <- augment(tfr_models) |>
  features(.innov, ljung_box, lag = 10)

birth_residual_tests <- augment(birth_models) |>
  features(.innov, ljung_box, lag = 10)

readr::write_csv(tfr_model_summary, file.path(model_dir, "tfr_model_summary.csv"))
readr::write_csv(birth_model_summary, file.path(model_dir, "birth_model_summary.csv"))
readr::write_csv(tfr_model_coefficients, file.path(model_dir, "tfr_model_coefficients.csv"))
readr::write_csv(birth_model_coefficients, file.path(model_dir, "birth_model_coefficients.csv"))
readr::write_csv(tfr_residual_tests, file.path(model_dir, "tfr_residual_ljung_box.csv"))
readr::write_csv(birth_residual_tests, file.path(model_dir, "birth_residual_ljung_box.csv"))

final_tfr_model_name <- tfr_model_summary$.model[[1]]
final_birth_model_name <- birth_model_summary$.model[[1]]

final_tfr_model <- tfr_models |> select(all_of(final_tfr_model_name))
final_birth_model <- birth_models |> select(all_of(final_birth_model_name))

list(
  tfr_model_summary = tfr_model_summary,
  birth_model_summary = birth_model_summary,
  final_tfr_model_name = final_tfr_model_name,
  final_birth_model_name = final_birth_model_name
)
