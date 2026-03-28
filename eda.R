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

