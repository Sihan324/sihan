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

# 8. Plot original time series 原始

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

# 9. First differences 差分

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

# 10. ACF and PACF of original series 原序列

acf(tlb_ts, main = "ACF of TLB")
pacf(tlb_ts, main = "PACF of TLB")

acf(tfr_ts, main = "ACF of TFR")
pacf(tfr_ts, main = "PACF of TFR")

# 11. ACF and PACF of differenced series 差分序列

acf(diff_tlb, main = "ACF of Differenced TLB")
pacf(diff_tlb, main = "PACF of Differenced TLB")

acf(diff_tfr, main = "ACF of Differenced TFR")
pacf(diff_tfr, main = "PACF of Differenced TFR")

# 12. Split into training and test periods 训练集1960-2012，测试集2013-2024

tlb_train <- window(tlb_ts, end = 2012)
tlb_test  <- window(tlb_ts, start = 2013)

tfr_train <- window(tfr_ts, end = 2012)
tfr_test  <- window(tfr_ts, start = 2013)

tlb_train
tlb_test
tfr_train
tfr_test

# 13. Initial candidate models 初步模型

fit_tlb_1 <- arima(tlb_train, order = c(0,1,0))
fit_tfr_1 <- arima(tfr_train, order = c(1,1,0))

fit_tlb_1
fit_tfr_1

# 14. Residual diagnostics 残差诊断，Box检验

acf(fit_tlb_1$resid, main = "ACF of Residuals: TLB model")
pacf(fit_tlb_1$resid, main = "PACF of Residuals: TLB model")

acf(fit_tfr_1$resid, main = "ACF of Residuals: TFR model")
pacf(fit_tfr_1$resid, main = "PACF of Residuals: TFR model")
Box.test(fit_tlb_1$resid, lag = 10, type = "Ljung-Box")
Box.test(fit_tfr_1$resid, lag = 10, type = "Ljung-Box")

# 15. Preliminary forecasts 预测

pred_tlb_1 <- predict(fit_tlb_1, n.ahead = length(tlb_test))
pred_tfr_1 <- predict(fit_tfr_1, n.ahead = length(tfr_test))

pred_tlb_1
pred_tfr_1

# Predicted values

tlb_forecast <- pred_tlb_1$pred
tfr_forecast <- pred_tfr_1$pred

tlb_forecast
tfr_forecast

# 16. Forecast accuracy measures 误差

tlb_mae <- mean(abs(tlb_test - tlb_forecast))
tlb_rmse <- sqrt(mean((tlb_test - tlb_forecast)^2))

tfr_mae <- mean(abs(tfr_test - tfr_forecast))
tfr_rmse <- sqrt(mean((tfr_test - tfr_forecast)^2))

tlb_mae
tlb_rmse
tfr_mae
tfr_rmse

