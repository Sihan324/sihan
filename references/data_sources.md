# Data Sources

## Singapore births and fertility (primary dataset)

- **Dataset:** Births And Fertility Rates, Annual
- **Publisher:** Singapore Department of Statistics (SingStat) via data.gov.sg
- **Source agencies:** Immigration and Checkpoints Authority; Department of Statistics, Singapore
- **Coverage:** 1960–2024 (annual)
- **Dataset page:** https://data.gov.sg/datasets/d_e39eeaeadb571c0d0725ef1eec48d166/view
- **SingStat table:** https://tablebuilder.singstat.gov.sg/table/TS/M810091

Variables used:

| Variable | Description |
|---|---|
| `Total Fertility Rate` | TFR (children per woman per year) |
| `Total Live-Births` | Annual count of live births |

The raw JSON downloaded by `R/01_data_import.R` is saved to `data/raw/births_and_fertility_rates_annual.json`.
The cleaned CSV is `data/processed/singapore_fertility_births.csv`.

## Historical EDA dataset

`BirthsAndFertilityRatesAnnual.csv` is an earlier CSV download used in the Part 1 EDA script (`eda.R`). It is kept for reproducibility of the EDA submission.
