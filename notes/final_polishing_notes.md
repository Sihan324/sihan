# Final Polishing Notes

These notes separate completed reproducible work from items that still need visual or submission-level checking.

## Completed Reproducible Work

- The final-report workflow is scripted from data import through audit checks.
- `R/run_all.R` runs the full analysis sequence, including EDA, model fitting, forecast evaluation, scenario forecasts, report tables and submission audit.
- The current audit reports zero missing required assets and zero broken report figure links.
- The Git history shows staged development using `Sihan Zhuang <sihan.zhuang@student.adelaide.edu.au>`.
- The repository documents the correct student GitHub account and warns against using any other account.

## Final Manual Checks

- Render `report/final_report.qmd` and inspect the HTML or PDF output.
- Render `report/statistical_appendix.qmd` and inspect table widths and figure placement.
- If the main report is too long, move the scenario forecast detail into the appendix and keep only a short summary in the main report.
- Check whether the course requires a specific file format, filename, cover page, or declaration.
- Confirm whether the GitHub URL or commit hash must be included in the submitted report.

## Content Risks To Watch

- The live-birth model with TFR is a conditional forecast model, so do not describe it as a complete standalone future forecast.
- The TFR coefficient is not causal because the model omits population structure, policy indicators and other demographic drivers.
- The model-trend TFR scenario declines to a very low 2035 value, so it should be presented as a model-implied sensitivity path rather than a policy expectation.
- Quarto rendering has not been confirmed from the terminal environment, so visual inspection remains essential.
- Use `notes/model_interpretation_caveats.md` as the final wording guardrail before submission.

## Submission Priority

The next highest-value task is rendering both `.qmd` files and checking the final formatted output. The code, GitHub workflow, audit checks and interpretation caveats are now substantially documented; final submission quality depends mostly on presentation, length and formatting.

As of 2026-06-05, `R/run_all.R` and `scripts/final_submission_check.ps1` have both passed in the local terminal environment. The terminal still does not expose `quarto` on PATH, so rendering should be completed through RStudio/Positron or another Quarto-enabled environment.
