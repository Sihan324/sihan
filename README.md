# Time Series Assignment

This repository contains reproducible code and report materials for the MATH X313 time series assignment and final report.

## Topic

The project studies two annual time series from Singapore:

- Total Live Births (TLB)
- Total Fertility Rate (TFR)

The initial EDA used data for 1960-2024. The current final-report workflow uses the official data.gov.sg / SingStat API and includes the latest available year in the downloaded dataset.

## Current work

The repository now contains two related stages of work:

- `eda.R`: the original Assignment 1 EDA script from the GitHub repository.
- `R/`: the reproducible final-report workflow, including data import, exploratory analysis, candidate ARIMA models, residual diagnostics, and forecast evaluation.

## GitHub account

Use the student GitHub account associated with:

`sihan.zhuang@student.adelaide.edu.au`

Do not push this repository from any other GitHub account. In particular, do not use the Gatsby account for this assignment.

Passwords and access tokens must not be committed to this repository. If authentication is required, use the browser login flow, GitHub Desktop, or a GitHub personal access token stored outside the project directory.

## Project structure

```text
eda.R                         Original EDA script from Assignment 1
BirthsAndFertilityRatesAnnual.csv
                              Original raw CSV used for Assignment 1 EDA
R/                            Reproducible final-report R scripts
data/raw/                     Raw public data downloaded by scripts
data/processed/               Cleaned data and analysis summary tables
figures/                      Figures generated for the report
report/                       Final report and statistical appendix drafts
references/                   Public data-source notes
notes/                        Assignment notes, research plan, and run log
```

The local `zhuying/` folder contains course-provided practical and key concept materials. It is intentionally excluded from Git because it may contain course materials that should not be published in a public repository.

## Reproducing the Assignment 1 EDA

1. Download or clone this repository.
2. Open the folder in RStudio.
3. Make sure the required packages are installed.
4. Keep `eda.R` and `BirthsAndFertilityRatesAnnual.csv` in the same folder.
5. Run `eda.R` from top to bottom.

## Reproducing the final-report workflow

1. Install R and the required packages listed in `R/00_setup.R`.
2. The final-report workflow downloads the raw data from data.gov.sg automatically.
3. On Windows, if the username contains non-ASCII characters and package installation fails, use an ASCII R library path:

```powershell
New-Item -ItemType Directory -Force -Path C:\Rlibs\4.4
$env:R_LIBS_USER = "C:/Rlibs/4.4"
```

4. Run the scripts in order:

```r
source("R/00_setup.R")
source("R/01_data_import.R")
source("R/02_eda.R")
source("R/03_models.R")
source("R/04_forecast_evaluation.R")
```

Or run the full workflow:

```powershell
$env:R_LIBS_USER = "C:/Rlibs/4.4"
$env:LC_ALL = "English_United States.utf8"
$env:LANG = "English_United States.utf8"
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" R/run_all.R
```

5. Render the report:

```bash
quarto render report/final_report.qmd
quarto render report/statistical_appendix.qmd
```

If `quarto` is not available in the terminal, render the `.qmd` files from RStudio/Positron or install the Quarto CLI.

## Main analysis currently included

- data cleaning and reshaping
- time series plots
- moving averages in the original EDA
- ACF and PACF plots
- stationarity diagnostics
- first differencing and log transformation where appropriate
- trend and forecasting models in the original EDA
- ARIMA candidate models for TFR and log total live-births
- ARIMA model with TFR as an explanatory variable for log total live-births
- residual diagnostics and Ljung-Box tests
- holdout forecast comparison

## Git workflow

Commit work in small logical steps so the history shows regular development:

- project setup and README
- data import and cleaning
- exploratory analysis
- candidate model fitting
- residual diagnostics
- forecast evaluation
- final report and appendix polishing

Before pushing, verify:

```bash
git config user.name
git config user.email
git remote -v
```

The email must be `sihan.zhuang@student.adelaide.edu.au`, and the remote must belong to the correct student GitHub account.

## Pre-submission checks

Before submitting the final report, run through this checklist:

- rerun `R/run_all.R` so generated tables and figures match the latest scripts;
- render `report/final_report.qmd` and `report/statistical_appendix.qmd`;
- confirm the rendered report includes the research question, key findings, model choice, diagnostics, forecast evaluation and limitations;
- check that all figures referenced in the report exist under `figures/`;
- check that no passwords, tokens or unrelated course materials are committed;
- confirm the final commit author is `Sihan Zhuang <sihan.zhuang@student.adelaide.edu.au>`.
