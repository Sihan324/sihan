# Forecast evaluation and model comparison.

source("R/00_setup.R")
source("R/03_models.R")

evaluation_dir <- file.path("data", "processed", "evaluation")
forecast_figure_dir <- file.path("figures", "forecasts")
dir.create(evaluation_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(forecast_figure_dir, recursive = TRUE, showWarnings = FALSE)

# Hold out the most recent ten years to evaluate short-horizon forecasting.
train_data <- fertility_births |> filter(year <= max(year) - 10)
test_data <- fertility_births |> filter(year > max(train_data$year))

tfr_train_models <- train_data |>
  model(
    arima_auto = ARIMA(tfr),
    arima_drift = ARIMA(tfr ~ 1 + pdq(0:3, 1, 0:3)),
    arima_no_drift = ARIMA(tfr ~ 0 + pdq(0:3, 1, 0:3))
  )

birth_train_models <- train_data |>
  model(
    arima_auto = ARIMA(log_total_live_births),
    arima_drift = ARIMA(log_total_live_births ~ 1 + pdq(0:3, 1, 0:3)),
    arima_no_drift = ARIMA(log_total_live_births ~ 0 + pdq(0:3, 1, 0:3)),
    arima_with_tfr = ARIMA(log_total_live_births ~ tfr + pdq(0:3, 0:1, 0:3))
  )

tfr_forecasts <- tfr_train_models |> forecast(h = nrow(test_data))
birth_forecasts <- birth_train_models |>
  forecast(new_data = test_data |> select(year, tfr))

tfr_accuracy <- tfr_forecasts |>
  accuracy(fertility_births) |>
  arrange(RMSE)

birth_accuracy <- birth_forecasts |>
  accuracy(fertility_births) |>
  arrange(RMSE)

readr::write_csv(tfr_accuracy, file.path(evaluation_dir, "tfr_forecast_accuracy.csv"))
readr::write_csv(birth_accuracy, file.path(evaluation_dir, "birth_forecast_accuracy.csv"))

tfr_forecast_plot <- fertility_births |>
  autoplot(tfr) +
  autolayer(tfr_forecasts, alpha = 0.7) +
  autolayer(
    test_data,
    tfr,
    colour = "black",
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = max(train_data$year) + 0.5,
    linetype = "dashed",
    colour = "grey45"
  ) +
  scale_colour_discrete(
    name = "Model",
    labels = c(
      "Automatic ARIMA",
      "ARIMA with drift",
      "ARIMA without drift"
    )
  ) +
  scale_fill_discrete(
    name = "Model",
    labels = c(
      "Automatic ARIMA",
      "ARIMA with drift",
      "ARIMA without drift"
    )
  ) +
  labs(
    title = "TFR holdout forecasts from candidate ARIMA models",
    subtitle = "Observed 2016-2025 values are overlaid in black",
    x = "Year",
    y = "Births per woman"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

birth_forecast_plot <- fertility_births |>
  autoplot(log_total_live_births) +
  autolayer(birth_forecasts, alpha = 0.7) +
  autolayer(
    test_data,
    log_total_live_births,
    colour = "black",
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = max(train_data$year) + 0.5,
    linetype = "dashed",
    colour = "grey45"
  ) +
  scale_colour_discrete(
    name = "Model",
    labels = c(
      "Automatic ARIMA",
      "ARIMA with drift",
      "ARIMA without drift",
      "TFR regression"
    )
  ) +
  scale_fill_discrete(
    name = "Model",
    labels = c(
      "Automatic ARIMA",
      "ARIMA with drift",
      "ARIMA without drift",
      "TFR regression"
    )
  ) +
  labs(
    title = "Log live-birth holdout forecasts",
    subtitle = "Observed 2016-2025 values are overlaid in black",
    x = "Year",
    y = "Log total live births"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(file.path(forecast_figure_dir, "tfr_forecasts.png"), tfr_forecast_plot, width = 7, height = 4.5, dpi = 300)
ggsave(
  file.path(forecast_figure_dir, "log_births_forecasts.png"),
  birth_forecast_plot,
  width = 8.5,
  height = 4.5,
  dpi = 300
)

list(
  tfr_accuracy = tfr_accuracy,
  birth_accuracy = birth_accuracy
)
