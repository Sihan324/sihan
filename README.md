# Singapore Fertility and Birth Forecasting

**MATH X313 Time Series Analysis — Assignment 1 (Final Report)**
University of Adelaide, Semester 1 2026

---

## Research Question

How have Singapore's Total Fertility Rate (TFR) and Total Live Births (TLB) evolved since 1960, and how well can time series models trained on 1960–2012 data forecast the 2013–2024 period?

---

## Overview

This repository contains all code, data, and report materials for the assignment. The analysis examines two annual demographic time series from Singapore's official statistics:

- **Total Fertility Rate (TFR)** — children per woman per year
- **Total Live Births (TLB)** — annual count, modelled on the log scale

The workflow is split into two stages matching the assignment structure:

| Stage | Period | Script |
|---|---|---|
| Part 1 EDA | 1960–2024 (full series) | `eda.R` |
| Part 2 Final Report | Train 1960–2012, holdout 2013–2024 | `R/` scripts |

---

## Repository Structure

```
eda.R                               Part 1 EDA script
BirthsAndFertilityRatesAnnual.csv   Raw CSV for Part 1 EDA

R/
  00_setup.R                        Package installation
  01_data_import.R                  Download data from data.gov.sg API
  02_eda.R                          EDA plots and stationarity diagnostics
  03_models.R                       ARIMA/SARIMA model fitting
  04_forecast_evaluation.R          Holdout evaluation (2013–2024)
  05_scenario_forecasts.R           Conditional birth forecasts under TFR scenarios
  06_report_tables.R                Generate compact tables for the report
  07_submission_audit.R             Check required figures and assets exist
  run_all.R                         Run all scripts in order

data/
  raw/                              Raw JSON from data.gov.sg API
  processed/
    singapore_fertility_births.csv  Main analysis dataset
    evaluation/                     Holdout accuracy (RMSE) by model
    models/                         Coefficient tables, Ljung-Box results
    report_tables/                  Compact tables referenced in the report
    scenarios/                      TFR scenario paths and conditional birth forecasts

figures/
  eda/                              Time series, ACF/PACF, stationarity overview
  model_diagnostics/                Residual plots, QQ plots
  forecasts/                        Holdout comparison, scenario forecast figures
  sarima_*.png                      SARIMA model selection diagnostics
  aicc_comparison.png               AICc comparison across candidate models

report/
  final_report.qmd                  Main report (Quarto)
  statistical_appendix.qmd          Statistical appendix (Quarto)

notes/
  research_plan.md                  Research question and modelling approach
  run_log.md                        Development notes by date
  final_polishing_notes.md          Pre-submission checklist

references/
  data_sources.md                   Dataset metadata and source URLs
```

---

## Data

The primary dataset is **Births And Fertility Rates, Annual** from the Singapore Department of Statistics, available via data.gov.sg:

- Dataset: https://data.gov.sg/datasets/d_e39eeaeadb571c0d0725ef1eec48d166/view
- SingStat table: https://tablebuilder.singstat.gov.sg/table/TS/M810091
- Coverage: 1960–2024 (annual)

`R/01_data_import.R` downloads the data automatically and saves the raw JSON to `data/raw/`. The cleaned dataset is at `data/processed/singapore_fertility_births.csv`.

---

## Reproducing the Part 1 EDA

1. Clone the repository.
2. Open in RStudio.
3. Keep `eda.R` and `BirthsAndFertilityRatesAnnual.csv` in the same folder.
4. Run `eda.R` from top to bottom.

---

## Reproducing the Final Report Analysis

**1. Install packages**

```r
source("R/00_setup.R")
```

On Windows with non-ASCII characters in the username, set an alternative R library path:

```powershell
New-Item -ItemType Directory -Force -Path C:\Rlibs\4.4
$env:R_LIBS_USER = "C:/Rlibs/4.4"
```

**2. Run the full analysis**

```r
source("R/run_all.R")
```

Or run scripts `01` through `07` individually in order.

**3. Render the report**

From RStudio, open and render `report/final_report.qmd` and `report/statistical_appendix.qmd`. Alternatively, from the terminal:

```bash
quarto render report/final_report.qmd
quarto render report/statistical_appendix.qmd
```

---

## Modelling Approach

### TFR

Raw TFR is non-stationary by KPSS. ACF/PACF of the first-differenced series show spikes at lags 11–13, motivating both ARIMA and SARIMA candidates. Models compared by AICc and holdout RMSE.

### Total Live Births (log scale)

First-differenced log-TLB is approximately stationary. Candidate models:

1. ARIMA for log-TLB
2. SARIMA for log-TLB
3. ARIMAX with TFR as a predictor

The ARIMAX model achieves the best AICc and holdout RMSE in the candidate set. It is a **conditional forecast** — it uses observed or assumed future TFR values as input. The scenario extension in `R/05_scenario_forecasts.R` produces 2026–2035 live-birth projections under three TFR paths (model-trend continuation, stabilisation, gradual rebound).

Model selection criteria: AICc (in-sample) and RMSE on the 2013–2024 holdout. All selected models are accompanied by residual diagnostics (Ljung-Box test, ACF of residuals, QQ plots). Non-selected viable models are documented in the statistical appendix.

---

## GitHub

Repository: https://github.com/Sihan324/sihan
