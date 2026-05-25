# Candidate model fitting and diagnostics.

source("R/00_setup.R")
source("R/01_data_import.R")

model_dir <- file.path("data", "processed", "models")
diagnostic_figure_dir <- file.path("figures", "model_diagnostics")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_figure_dir, recursive = TRUE, showWarnings = FALSE)

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

# The AICc-best TFR model and the forecast-best TFR model are different.
# For the report diagnostics, use the forecast-focused TFR model identified in
# the holdout evaluation, while retaining AICc tables for comparison.
final_tfr_model_name <- "arima_drift"
final_birth_model_name <- "arima_with_tfr"

final_tfr_model <- tfr_models |> select(all_of(final_tfr_model_name))
final_birth_model <- birth_models |> select(all_of(final_birth_model_name))

tfr_residuals <- augment(tfr_models) |>
  filter(.model == final_tfr_model_name)

birth_residuals <- augment(birth_models) |>
  filter(.model == final_birth_model_name)

tfr_residual_plot <- tfr_residuals |>
  ggplot(aes(x = year, y = .innov)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(colour = "#005C53", linewidth = 0.7) +
  geom_point(colour = "#005C53", size = 1.4) +
  labs(
    title = paste("Innovation residuals:", final_tfr_model_name, "for TFR"),
    x = "Year",
    y = "Innovation residual"
  ) +
  theme_minimal()

birth_residual_plot <- birth_residuals |>
  ggplot(aes(x = year, y = .innov)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(colour = "#7A3E00", linewidth = 0.7) +
  geom_point(colour = "#7A3E00", size = 1.4) +
  labs(
    title = paste("Innovation residuals:", final_birth_model_name, "for log live-births"),
    x = "Year",
    y = "Innovation residual"
  ) +
  theme_minimal()

tfr_residual_acf_plot <- tfr_residuals |>
  ACF(.innov, lag_max = 20) |>
  autoplot() +
  labs(title = paste("Residual ACF:", final_tfr_model_name, "for TFR"))

birth_residual_acf_plot <- birth_residuals |>
  ACF(.innov, lag_max = 20) |>
  autoplot() +
  labs(title = paste("Residual ACF:", final_birth_model_name, "for log live-births"))

ggsave(file.path(diagnostic_figure_dir, "tfr_final_model_residuals.png"), tfr_residual_plot, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(diagnostic_figure_dir, "birth_final_model_residuals.png"), birth_residual_plot, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(diagnostic_figure_dir, "tfr_final_model_residual_acf.png"), tfr_residual_acf_plot, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(diagnostic_figure_dir, "birth_final_model_residual_acf.png"), birth_residual_acf_plot, width = 7, height = 4.5, dpi = 300)

list(
  tfr_model_summary = tfr_model_summary,
  birth_model_summary = birth_model_summary,
  final_tfr_model_name = final_tfr_model_name,
  final_birth_model_name = final_birth_model_name
)
