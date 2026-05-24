# Exploratory data analysis.

source("R/00_setup.R")
source("R/01_data_import.R")

eda_dir <- file.path("figures", "eda")
dir.create(eda_dir, recursive = TRUE, showWarnings = FALSE)

fertility_births <- fertility_births |>
  mutate(
    tfr_index = tfr / first(tfr),
    births_index = total_live_births / first(total_live_births)
  )

tfr_plot <- fertility_births |>
  ggplot(aes(x = year, y = tfr)) +
  geom_line(linewidth = 0.8, colour = "#005C53") +
  geom_point(size = 1.6, colour = "#005C53") +
  labs(
    title = "Singapore total fertility rate",
    x = "Year",
    y = "TFR per female"
  ) +
  theme_minimal()

births_plot <- fertility_births |>
  ggplot(aes(x = year, y = total_live_births)) +
  geom_line(linewidth = 0.8, colour = "#7A3E00") +
  geom_point(size = 1.6, colour = "#7A3E00") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Singapore total live-births",
    x = "Year",
    y = "Total live-births"
  ) +
  theme_minimal()

indexed_plot <- fertility_births |>
  select(year, tfr_index, births_index) |>
  pivot_longer(-year, names_to = "series", values_to = "index_value") |>
  mutate(
    series = recode(
      series,
      tfr_index = "TFR",
      births_index = "Total live-births"
    )
  ) |>
  ggplot(aes(x = year, y = index_value, colour = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Indexed comparison of fertility rate and live-births",
    x = "Year",
    y = "Index, 1960 = 1",
    colour = NULL
  ) +
  theme_minimal()

tfr_acf_plot <- fertility_births |>
  ACF(tfr, lag_max = 20) |>
  autoplot() +
  labs(title = "ACF of TFR")

births_acf_plot <- fertility_births |>
  ACF(log_total_live_births, lag_max = 20) |>
  autoplot() +
  labs(title = "ACF of log total live-births")

tfr_diff_acf_plot <- fertility_births |>
  ACF(tfr_difference, lag_max = 20) |>
  autoplot() +
  labs(title = "ACF of differenced TFR")

births_diff_acf_plot <- fertility_births |>
  ACF(log_births_difference, lag_max = 20) |>
  autoplot() +
  labs(title = "ACF of differenced log total live-births")

ggsave(file.path(eda_dir, "tfr_time_series.png"), tfr_plot, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(eda_dir, "total_live_births_time_series.png"), births_plot, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(eda_dir, "indexed_tfr_births.png"), indexed_plot, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(eda_dir, "tfr_acf.png"), tfr_acf_plot, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(eda_dir, "log_births_acf.png"), births_acf_plot, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(eda_dir, "differenced_tfr_acf.png"), tfr_diff_acf_plot, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(eda_dir, "differenced_log_births_acf.png"), births_diff_acf_plot, width = 7, height = 4.5, dpi = 300)

stationarity_diagnostics <- bind_rows(
  fertility_births |> features(tfr, unitroot_kpss) |> mutate(series = "tfr"),
  fertility_births |> features(tfr_difference, unitroot_kpss) |> mutate(series = "differenced_tfr"),
  fertility_births |> features(log_total_live_births, unitroot_kpss) |> mutate(series = "log_total_live_births"),
  fertility_births |> features(log_births_difference, unitroot_kpss) |> mutate(series = "differenced_log_total_live_births")
) |>
  select(series, everything())

readr::write_csv(stationarity_diagnostics, file.path("data", "processed", "stationarity_diagnostics.csv"))

stationarity_diagnostics
