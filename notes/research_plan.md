# Research Plan

## Working research question

How have Singapore's total fertility rate and total live-births evolved over time, and how well can short-horizon time series models forecast these demographic indicators?

## Rationale

The series are annual demographic time series with strong long-run movement and possible structural changes. This makes them suitable for demonstrating:

- exploratory time series analysis;
- stationarity assessment and differencing;
- ARIMA model identification and estimation;
- residual diagnostics;
- forecast evaluation using a holdout period.

## Data

Use the official `Births And Fertility Rates, Annual` dataset from data.gov.sg/SingStat.

The core variables are:

- `tfr`: Total Fertility Rate (TFR), per female.
- `total_live_births`: annual total live-births, number.
- `log_total_live_births`: log transform used for modelling births.

## Candidate models

Primary models:

- ARIMA for TFR.
- ARIMA for log total live-births.
- ARIMAX-style model for log live-births using TFR as an explanatory regressor.

Possible appendix-only alternatives:

- local-level or structural time series model if the ARIMA residuals suggest level shifts;
- intervention or segmented trend model if key historical policy periods are discussed.
- scenario-based birth forecasts using low, medium and high TFR paths if the final report needs a stronger forecasting extension.

## Forecast evaluation

Use the most recent ten years as a holdout set. Compare candidate models by RMSE and residual diagnostics. Keep viable but non-selected models in the statistical appendix.

## Remaining polish priorities

- Render the final report and appendix after Quarto is available.
- Check that tables and figures fit cleanly in the rendered output.
- Decide whether to keep scenario forecasts as appendix discussion only or implement them as additional forecasts.
