# Development Log

## 2026-05-24

- Set up repository; downloaded official Singapore births and fertility data from data.gov.sg.
- Cleaned data into annual TFR and total live-birth series for 1960–2024.
- R packages installed to `C:/Rlibs/4.4` (default path has non-ASCII characters).
- Generated initial EDA figures: time series plots, ACF/PACF, and KPSS stationarity diagnostics.

Preliminary results:
- Raw TFR and log-TLB are non-stationary by KPSS.
- First-differenced log-TLB passes KPSS; first-differenced TFR still shows persistence.
- ACF/PACF of differenced series show notable spikes at lags 11, 12, 13 — worth investigating SARIMA.

## 2026-05-28

- Fitted ARIMA candidates for TFR and log-TLB; `arima_auto` best by AICc for TFR, `arima_drift` best by holdout RMSE.
- Live-birth model with TFR as regressor best by both AICc and holdout RMSE among the candidate set.
- Added model-selection summary table and holdout comparison to the report draft.
- Clarified in the report that the TFR regressor is predictive, not causal.

## 2026-05-29

- Added Key Findings section to `report/final_report.qmd`.
- Shifted main numerical results earlier in the report structure.
- Added appendix notes on structural breaks, state-space alternatives, and two-stage forecasting.

## 2026-05-31

- Added `R/05_scenario_forecasts.R`: generates TFR scenario paths and conditional live-birth projections for 2026–2035.
- Figures written to `figures/forecasts/`; scenario CSVs to `data/processed/scenarios/`.
- Updated report and appendix with scenario interpretation.

## 2026-06-01

- Added `R/06_report_tables.R`: writes compact summary tables to `data/processed/report_tables/`.
- Updated report and appendix to reference generated tables.

## 2026-06-02

- Added `R/07_submission_audit.R`: checks required scripts, figures, and report figure links.
- Audit reports zero missing assets.

## 2026-06-05

- Expanded statistical appendix with KPSS, AICc, Ljung-Box, RMSE, and model-equation details.
- Added executive summary and conclusion to the main report.
- Installed `rmarkdown`, `knitr`, `htmltools` for Quarto rendering; added to `R/00_setup.R`.
- Rendered both `.qmd` files successfully with the RStudio bundled Quarto executable.

## 2026-06-08

- Final figures and EDA diagnostics updated (SARIMA ACF progression, heatmap, holdout forecast, QQ residuals).
- Pushed final report sources and all figures to GitHub.
