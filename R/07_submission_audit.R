# Submission asset audit for the final report workflow.

source("R/00_setup.R")

report_table_dir <- file.path("data", "processed", "report_tables")
dir.create(report_table_dir, recursive = TRUE, showWarnings = FALSE)

required_assets <- tibble::tibble(
  asset_type = c(
    rep("script", 8),
    rep("report", 2),
    rep("processed_table", 12),
    rep("figure", 15),
    rep("note", 5),
    "reference"
  ),
  path = c(
    "R/00_setup.R",
    "R/01_data_import.R",
    "R/02_eda.R",
    "R/03_models.R",
    "R/04_forecast_evaluation.R",
    "R/05_scenario_forecasts.R",
    "R/06_report_tables.R",
    "R/run_all.R",
    "report/final_report.qmd",
    "report/statistical_appendix.qmd",
    "data/processed/singapore_fertility_births.csv",
    "data/processed/stationarity_diagnostics.csv",
    "data/processed/models/tfr_model_summary.csv",
    "data/processed/models/birth_model_summary.csv",
    "data/processed/models/tfr_residual_ljung_box.csv",
    "data/processed/models/birth_residual_ljung_box.csv",
    "data/processed/evaluation/tfr_forecast_accuracy.csv",
    "data/processed/evaluation/birth_forecast_accuracy.csv",
    "data/processed/scenarios/tfr_scenario_paths.csv",
    "data/processed/scenarios/birth_scenario_forecasts.csv",
    "data/processed/report_tables/model_selection_summary.csv",
    "data/processed/report_tables/scenario_endpoint_summary.csv",
    "figures/eda/tfr_time_series.png",
    "figures/eda/total_live_births_time_series.png",
    "figures/eda/indexed_tfr_births.png",
    "figures/eda/tfr_acf.png",
    "figures/eda/log_births_acf.png",
    "figures/eda/differenced_tfr_acf.png",
    "figures/eda/differenced_log_births_acf.png",
    "figures/model_diagnostics/tfr_final_model_residuals.png",
    "figures/model_diagnostics/birth_final_model_residuals.png",
    "figures/model_diagnostics/tfr_final_model_residual_acf.png",
    "figures/model_diagnostics/birth_final_model_residual_acf.png",
    "figures/forecasts/tfr_forecasts.png",
    "figures/forecasts/log_births_forecasts.png",
    "figures/forecasts/tfr_scenario_paths.png",
    "figures/forecasts/birth_scenario_forecasts.png",
    "README.md",
    "notes/assignment_requirements.md",
    "notes/research_plan.md",
    "notes/run_log.md",
    "notes/final_review_checklist.md",
    "references/data_sources.md"
  )
)

submission_asset_check <- required_assets |>
  mutate(
    exists = file.exists(path),
    size_bytes = if_else(exists, as.numeric(file.info(path)$size), NA_real_),
    status = if_else(exists & size_bytes > 0, "ok", "missing_or_empty")
  )

extract_figure_links <- function(report_path) {
  lines <- readLines(report_path, warn = FALSE)
  matches <- unlist(regmatches(
    lines,
    gregexpr("!\\[[^\\]]*\\]\\(([^)]+)\\)", lines, perl = TRUE)
  ))

  if (length(matches) == 0) {
    return(tibble::tibble(report = character(), link = character()))
  }

  tibble::tibble(
    report = report_path,
    link = sub(".*\\(([^)]+)\\).*", "\\1", matches)
  )
}

report_figure_links <- bind_rows(
  extract_figure_links("report/final_report.qmd"),
  extract_figure_links("report/statistical_appendix.qmd")
) |>
  mutate(
    normalized_path = sub("^\\.\\./", "", link),
    exists = file.exists(normalized_path),
    size_bytes = if_else(exists, as.numeric(file.info(normalized_path)$size), NA_real_),
    status = if_else(exists & size_bytes > 0, "ok", "missing_or_empty")
  )

submission_audit_summary <- bind_rows(
  submission_asset_check |>
    summarise(
      check_group = "required_assets",
      total = n(),
      ok = sum(status == "ok"),
      problems = sum(status != "ok")
    ),
  report_figure_links |>
    summarise(
      check_group = "report_figure_links",
      total = n(),
      ok = sum(status == "ok"),
      problems = sum(status != "ok")
    )
)

readr::write_csv(
  submission_asset_check,
  file.path(report_table_dir, "submission_asset_check.csv")
)
readr::write_csv(
  report_figure_links,
  file.path(report_table_dir, "report_figure_links.csv")
)
readr::write_csv(
  submission_audit_summary,
  file.path(report_table_dir, "submission_audit_summary.csv")
)

list(
  submission_audit_summary = submission_audit_summary,
  submission_asset_check = submission_asset_check,
  report_figure_links = report_figure_links
)
