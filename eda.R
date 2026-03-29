# Assignment 1 EDA
# Exploratory Data Analysis (EDA)
# Topic: Singapore Total Live Births (TLB) and Total Fertility Rate (TFR)

# 1. Setup

rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tsibble)
library(feasts)
library(fable)
library(ggtime)
library(slider)

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

# 6.5 Data checking 数据检查

sum(is.na(eda_data$TLB))
sum(is.na(eda_data$TFR))

sum(duplicated(eda_data$Year))

summary(eda_data)

range(eda_data$Year)
nrow(eda_data)

# 7. Create tsibble object 

eda_ts <- long_data %>%
  mutate(
    Series = case_when(
      DataSeries == "Total Live-Births" ~ "TLB",
      DataSeries == "Total Fertility Rate (TFR)" ~ "TFR"
    )
  ) %>%
  select(Year, Series, Value) %>%
  arrange(Series, Year) %>%
  as_tsibble(key = Series, index = Year)

eda_ts

index_var(eda_ts)
key_vars(eda_ts)
measured_vars(eda_ts)
scan_gaps(eda_ts)

# 8. Original time series plots 

eda_ts %>%
  autoplot(Value) +
  facet_wrap(vars(Series), scales = "free_y", ncol = 1) +
  labs(
    title = "Singapore TLB and TFR (1960-2024)",
    x = "Year",
    y = NULL
  )

# 8.5 Moving average smoothers 

eda_smooth <- eda_ts %>%
  group_by(Series) %>%
  mutate(
    MA3 = slider::slide_dbl(Value, mean, .before = 1, .after = 1, .complete = TRUE),
    MA5 = slider::slide_dbl(Value, mean, .before = 2, .after = 2, .complete = TRUE)
  ) %>%
  ungroup()

ggplot(eda_smooth, aes(x = Year, y = Value)) +
  geom_line() +
  geom_line(aes(y = MA3), linetype = 2, linewidth = 0.8) +
  geom_line(aes(y = MA5), linewidth = 0.8) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  labs(
    title = "Time series with moving average smoothers",
    x = "Year",
    y = NULL
  )

# 9. ACF and PACF of original series 

eda_ts %>%
  filter(Series == "TLB") %>%
  ACF(Value) %>%
  autoplot() +
  labs(title = "ACF of TLB")

eda_ts %>%
  filter(Series == "TLB") %>%
  PACF(Value) %>%
  autoplot() +
  labs(title = "PACF of TLB")

eda_ts %>%
  filter(Series == "TFR") %>%
  ACF(Value) %>%
  autoplot() +
  labs(title = "ACF of TFR")

eda_ts %>%
  filter(Series == "TFR") %>%
  PACF(Value) %>%
  autoplot() +
  labs(title = "PACF of TFR")

# 10. First differencing 

eda_diff <- eda_ts %>%
  group_by(Series) %>%
  mutate(
    D1 = difference(Value)
  ) %>%
  ungroup()

eda_diff %>%
  ggplot(aes(x = Year, y = D1)) +
  geom_line() +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  labs(
    title = "First-differenced series",
    x = "Year",
    y = NULL
  )

# 11. ACF and PACF of differenced series 

eda_diff %>%
  filter(Series == "TLB", !is.na(D1)) %>%
  ACF(D1) %>%
  autoplot() +
  labs(title = "ACF of differenced TLB")

eda_diff %>%
  filter(Series == "TLB", !is.na(D1)) %>%
  PACF(D1) %>%
  autoplot() +
  labs(title = "PACF of differenced TLB")

eda_diff %>%
  filter(Series == "TFR", !is.na(D1)) %>%
  ACF(D1) %>%
  autoplot() +
  labs(title = "ACF of differenced TFR")

eda_diff %>%
  filter(Series == "TFR", !is.na(D1)) %>%
  PACF(D1) %>%
  autoplot() +
  labs(title = "PACF of differenced TFR")

# 12. Trend model comparison 

trend_models <- eda_ts %>%
  model(
    lm_linear = TSLM(Value ~ trend()),
    lm_quad   = TSLM(Value ~ trend() + I(trend()^2)),
    lm_cubic  = TSLM(Value ~ trend() + I(trend()^2) + I(trend()^3))
  )

tidy(trend_models)
glance(trend_models)

# 12.5 Residual plots from trend models

trend_aug <- augment(trend_models)

ggplot(trend_aug, aes(x = Year, y = .resid)) +
  geom_line() +
  facet_grid(Series ~ .model, scales = "free_y") +
  labs(
    title = "Residuals from linear, quadratic and cubic trend models",
    x = "Year",
    y = "Residual"
  )

# 13. Split into training and test periods 训练集和测试集

train_ts <- eda_ts %>%
  filter(Year >= 1960, Year <= 2012)

test_ts <- eda_ts %>%
  filter(Year >= 2013, Year <= 2024)

as_tibble(train_ts) %>%
  group_by(Series) %>%
  summarise(
    start_year = min(Year),
    end_year = max(Year),
    n = n()
  )

as_tibble(test_ts) %>%
  group_by(Series) %>%
  summarise(
    start_year = min(Year),
    end_year = max(Year),
    n = n()
  )

# 14. Candidate forecasting models

fit_models <- train_ts %>%
  model(
    lm_cubic = TSLM(Value ~ trend() + I(trend()^2) + I(trend()^3)),
    arima    = ARIMA(Value),
    ets      = ETS(Value)
  )

glance(fit_models)

# 15. Forecasts for the test period

fc <- fit_models %>%
  forecast(new_data = test_ts)

fc

autoplot(fc, train_ts) +
  autolayer(test_ts, Value, colour = "black") +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  labs(
    title = "Forecasts versus actual observations (2013-2024)",
    x = "Year",
    y = NULL
  )

# 16. Forecast accuracy on the test period

fc_accuracy <- accuracy(fc, test_ts)

fc_accuracy

fc_accuracy %>%
  as_tibble() %>%
  select(Series, .model, RMSE, MAE, MAPE) %>%
  arrange(Series, RMSE)

# 17. Residual diagnostics for the ETS model 检查模型残差

fit_aug <- augment(fit_models)

fit_aug %>%
  filter(.model == "ets") %>%
  ggplot(aes(x = Year, y = .innov)) +
  geom_line() +
  facet_wrap(~Series, scales = "free_y", ncol = 1) +
  labs(
    title = "Residuals from ETS models",
    x = "Year",
    y = "Innovation residuals"
  )

fit_aug %>%
  filter(.model == "ets", Series == "TFR") %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "ACF of ETS residuals: TFR")

fit_aug %>%
  filter(.model == "ets", Series == "TLB") %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "ACF of ETS residuals: TLB")

# 18. Ljung-Box tests for ETS residuals

ets_resid <- fit_aug %>%
  filter(.model == "ets") %>%
  as_tibble()

Box.test(
  ets_resid$.innov[ets_resid$Series == "TFR"],
  lag = 10,
  type = "Ljung-Box"
)

Box.test(
  ets_resid$.innov[ets_resid$Series == "TLB"],
  lag = 10,
  type = "Ljung-Box"
)