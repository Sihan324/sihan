# Research Plan

## Research question

How have Singapore's total fertility rate (TFR) and total live births evolved since 1960, and can time series models fit to 1960–2012 data accurately forecast the 2013–2024 period?

## Motivation

Both TFR and total live births (TLB) have declined steadily in Singapore since the 1960s, punctuated by policy changes and structural shifts. Annual demographic data of this kind calls for stationarity assessment, differencing and ARIMA-family models. The holdout design (train on 1960–2012, evaluate on 2013–2024) gives an objective way to compare competing models.

## Data

Official *Births And Fertility Rates, Annual* dataset from data.gov.sg / SingStat, covering 1960–2024.

Variables used:
- TFR: total fertility rate (children per woman)
- TLB: total live births (count); analysed on log scale

## Modelling approach

- Assess stationarity with KPSS tests on raw and differenced series.
- Examine ACF and PACF after first differencing (and log transformation for TLB).
- Fit ARIMA models for TFR and log-TLB; also try SARIMA given spikes at lags 11–13.
- Fit an ARIMAX model for log-TLB using TFR as a predictor.
- Compare models by AICc (in-sample) and RMSE on the 2013–2024 holdout.
- Place non-selected viable models in the statistical appendix.

## Scenario extension

Conditional live-birth forecasts for 2026–2035 under three TFR paths (model-trend continuation, stabilisation, gradual rebound). These are scenario analyses, not standalone demographic predictions.

## Outstanding questions

- Whether SARIMA seasonal order (12) improves residual diagnostics enough to justify the extra parameters.
- Whether to include a brief state-space comparison in the appendix.
