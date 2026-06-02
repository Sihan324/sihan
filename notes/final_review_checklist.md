# Final Review Checklist

Use this checklist before submitting the final report and GitHub repository.

## GitHub and Reproducibility

- Confirm the repository remote is `Sihan324/sihan`.
- Confirm the Git author is `Sihan Zhuang <sihan.zhuang@student.adelaide.edu.au>`.
- Confirm the working tree is clean after the final push.
- Rerun `R/run_all.R` from a clean R session.
- Check that generated CSV files under `data/processed/` match the latest scripts.
- Check that generated figures under `figures/` match the latest scripts.
- Check `data/processed/report_tables/submission_audit_summary.csv` and confirm all problem counts are zero.
- Confirm no passwords, tokens, private files, or course-only materials are tracked.

## Final Report

- The research question is visible near the start.
- The key findings section includes the main numbers and the main caveat.
- The data section identifies the official Singapore data source and sample period.
- The methods section explains ARIMA and the live-birth model with TFR.
- The results section reports AICc, residual diagnostics and model selection.
- The forecasting section separates holdout evaluation from scenario forecasts.
- The discussion states limitations without overstating causality.

## Statistical Appendix

- The appendix includes data-processing details.
- The appendix includes stationarity diagnostics and model summaries.
- Non-selected viable models are documented.
- Conditional forecasting and scenario forecasting are clearly labelled.
- Generated compact tables are included so the reported numbers are traceable.
- Submission audit tables are included so missing report assets can be checked.

## Rendering

- Render `report/final_report.qmd`.
- Render `report/statistical_appendix.qmd`.
- Check figure paths and table widths in the rendered output.
- Check page or word limits if the course specifies them.
- Keep scenario forecasts in the appendix only if the main report becomes too long.
