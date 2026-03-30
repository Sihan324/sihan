# Time Series Assignment 1

This repository contains my exploratory data analysis for Assignment 1.

## Topic

The project studies two annual time series from Singapore:

-   Total Live Births (TLB)
-   Total Fertility Rate (TFR)

The data cover the period from 1960 to 2024.

## Aim of the project

The aim of this project is to explore the data, look at the main patterns over time, and do some early time series analysis for the final report.

## Files in this repository

-   `eda.R` – main R script for data cleaning, visualisation, and preliminary time series analysis
-   `BirthsAndFertilityRatesAnnual.csv` – raw dataset used in the analysis
-   `README.md` – overview of the project and instructions for reproduction

## Main analysis included

The script includes:

-   data cleaning
-   selecting TLB and TFR
-   reshaping the data
-   time series plots
-   3-point and 5-point moving averages
-   ACF and PACF plots
-   first differencing
-   simple trend models
-   forecast comparison using 1960–2012 as training data and 2013–2024 as test data
-   residual checks for the ETS model

## Packages used

The analysis uses the following R packages:

-   `readr`
-   `dplyr`
-   `ggplot2`
-   `tidyr`
-   `tsibble`
-   `feasts`
-   `fable`
-   `ggtime`
-   `slider`

## Reproducibility

1.  Download or clone this repository.
2.  Open the folder in RStudio.
3.  Make sure the required packages are installed.
4.  Keep `eda.R` and `BirthsAndFertilityRatesAnnual.csv` in the same folder.
5.  Run `eda.R` from top to bottom.

All figures and results in the EDA report are produced from this script.

## Note

This repository is for Assignment 1 EDA only. More model development will be done later in the final report.
