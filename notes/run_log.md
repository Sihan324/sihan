# Run Log

## 2026-05-24

Repository setup:

- Initialised local Git repository on branch `main`.
- Configured local Git identity as `Sihan Zhuang <sihan.zhuang@student.adelaide.edu.au>`.
- No Git remote has been configured yet, so nothing has been pushed.
- Course-provided material in `zhuying/` and temporary PDF renders in `tmp/` are ignored by Git.

R environment:

- R found at `C:/Program Files/R/R-4.4.1/bin/Rscript.exe`.
- Required packages were installed to `C:/Rlibs/4.4` because the default R user library path contains non-ASCII characters.
- R scripts were run with `LC_ALL=English_United States.utf8`.
- Quarto CLI was not found in the terminal PATH.

Analysis completed:

- Downloaded official Singapore births and fertility data from data.gov.sg.
- Cleaned the data into annual TFR and total live-births series for 1960-2025.
- Generated EDA figures, ACF figures, stationarity diagnostics, model summaries, residual diagnostics, and forecast evaluation tables.

Key preliminary results:

- KPSS diagnostics suggest raw TFR and log total live-births are non-stationary.
- First-differenced log total live-births passes KPSS at the reported level; first-differenced TFR remains problematic, suggesting stronger persistence or structural change.
- By AICc, the current best TFR model is `arima_auto`.
- By 10-year holdout RMSE, the current best TFR model is `arima_drift`.
- For log total live-births, the model using TFR as an explanatory variable is best by both AICc and holdout RMSE among the candidate set currently implemented.

## 2026-05-28

Git configuration check:

- Local Git author confirmed as `Sihan Zhuang <sihan.zhuang@student.adelaide.edu.au>`.
- Repository remote confirmed as the student repository `Sihan324/sihan`.

Report update:

- Added an explicit conditional forecasting caveat for the live-birth model with TFR.
- Clarified that holdout live-birth accuracy for `arima_with_tfr` uses known test-period TFR values.
- Added final-report wording to avoid interpreting the TFR coefficient as causal.
- Added appendix guidance that future live-birth forecasts should use either forecast TFR values or TFR scenarios.
- Added a model-selection summary table to the final report.
- Added an appendix note explaining the decision rule across AICc, residual diagnostics, and holdout RMSE.

## 2026-05-29

Report update:

- Added a `Key Findings` section to the final report.
- Moved the main numerical results and modelling caveats closer to the start of the report.
- Added appendix guidance on further model development, including structural breaks, state-space models, two-stage forecasts and scenario forecasts.
- Updated the research plan with remaining polish priorities before final submission.
- Added a README pre-submission checklist for rerunning scripts, rendering reports, checking figures and verifying Git identity.
- Updated assignment requirement notes with the current completion status and remaining rendering risk.
