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

# 6. Create final analysis table

eda_data <- long_data %>%
  pivot_wider(
    names_from = DataSeries,
    values_from = Value
  ) %>%
  rename(
    TFR = `Total Fertility Rate (TFR)`,
    TLB = `Total Live-Births`
  ) %>%
  arrange(Year)

eda_data
head(eda_data)
tail(eda_data)
dim(eda_data)
names(eda_data)

# 7. Create time series objects创建时间序列对象

tlb_ts <- ts(eda_data$TLB, start = 1960, frequency = 1)
tfr_ts <- ts(eda_data$TFR, start = 1960, frequency = 1)

tlb_ts
tfr_ts
start(tlb_ts)
end(tlb_ts)
frequency(tlb_ts)

# 8. Plot original time series

plot(tlb_ts,
     type = "o",
     pch = 16,
     main = "Singapore Total Live Births (1960-2024)",
     xlab = "Year",
     ylab = "TLB")

plot(tfr_ts,
     type = "o",
     pch = 16,
     main = "Singapore Total Fertility Rate (1960-2024)",
     xlab = "Year",
     ylab = "TFR")

# 9. First differences

diff_tlb <- diff(tlb_ts)
diff_tfr <- diff(tfr_ts)

plot(diff_tlb,
     type = "o",
     pch = 16,
     main = "First Difference of TLB",
     xlab = "Year",
     ylab = "Differenced TLB")

plot(diff_tfr,
     type = "o",
     pch = 16,
     main = "First Difference of TFR",
     xlab = "Year",
     ylab = "Differenced TFR")

# 10. ACF and PACF of original series

acf(tlb_ts, main = "ACF of TLB")
pacf(tlb_ts, main = "PACF of TLB")

acf(tfr_ts, main = "ACF of TFR")
pacf(tfr_ts, main = "PACF of TFR")

# 11. ACF and PACF of differenced series

acf(diff_tlb, main = "ACF of Differenced TLB")
pacf(diff_tlb, main = "PACF of Differenced TLB")

acf(diff_tfr, main = "ACF of Differenced TFR")
pacf(diff_tfr, main = "PACF of Differenced TFR")