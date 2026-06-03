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

## 2026-05-31

Scenario forecasting update:

- Added `R/05_scenario_forecasts.R` and connected it to `R/run_all.R`.
- Generated TFR scenario paths and conditional live-birth forecasts for 2026-2035.
- Added scenario forecast figures to `figures/forecasts/`.
- Added scenario forecast interpretation to the final report and statistical appendix.
- Updated README and planning notes to document the new scenario workflow and remaining final-submission checks.
- Verified the complete workflow with `R/run_all.R` after adding the scenario script.

## 2026-06-01

Report table update:

- Added `R/06_report_tables.R` and connected it to `R/run_all.R`.
- Generated compact report tables under `data/processed/report_tables/`.
- Updated the final report and statistical appendix to read generated model-selection and scenario endpoint tables.
- Added `notes/final_review_checklist.md` for final submission review.
- Updated README to document the report-table workflow and generated outputs.

## 2026-06-02

Submission audit update:

- Added `R/07_submission_audit.R` and connected it to `R/run_all.R`.
- Generated audit tables under `data/processed/report_tables/`.
- Confirmed the current audit has zero missing required assets and zero broken report figure links.
- Added audit results to the final report and statistical appendix.
- Updated README and final review checklist to include submission audit checks.

## 2026-06-03

Final polishing update:

- Added `notes/final_polishing_notes.md` to distinguish completed reproducible work from manual submission checks.
- Documented remaining presentation risks, including Quarto rendering, table widths, report length and conditional forecast wording.
- Updated README and final review checklist to point to the polishing notes.

## 2026-06-04

Interpretation caveat update:

- Added `notes/model_interpretation_caveats.md` to define safe claims and claims to avoid.
- Documented recommended wording for conditional forecasts, scenario analysis and non-causal interpretation.
- Updated README, final polishing notes and final review checklist to reference the caveat document.
