# Assignment 1 EDA
# Exploratory Data Analysis (EDA)
# Topic: Singapore Total Live Births (TLB) and Total Fertility Rate (TFR)

# 1. Setup

rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# 2. Import data

raw_data <- read_csv("BirthsAndFertilityRatesAnnual.csv")

glimpse(raw_data)
head(raw_data)
names(raw_data)

# 3. Keep only TFR and TLB

series_data <- raw_data %>%
  mutate(DataSeries = trimws(DataSeries)) %>%
  filter(DataSeries %in% c("Total Fertility Rate (TFR)", "Total Live-Births"))

series_data
series_data$DataSeries
dim(series_data)


# 4. Reshape to long format 宽表换长表

series_data_num <- series_data %>%
  mutate(across(-DataSeries, as.numeric))

long_data <- series_data_num %>%
  pivot_longer(
    cols = -DataSeries,
    names_to = "Year",
    values_to = "Value"
  )

long_data
head(long_data)
dim(long_data)

# 5. Keep years 1960-2024

long_data <- long_data %>%
  mutate(Year = as.integer(Year)) %>%
  filter(Year >= 1960, Year <= 2024)

long_data
head(long_data)
tail(long_data)
range(long_data$Year)
dim(long_data)