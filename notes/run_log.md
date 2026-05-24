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

