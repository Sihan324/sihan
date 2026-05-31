# Scenario forecasts for live-births using alternative future TFR paths.

source("R/00_setup.R")
source("R/03_models.R")

scenario_dir <- file.path("data", "processed", "scenarios")
forecast_figure_dir <- file.path("figures", "forecasts")
dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(forecast_figure_dir, recursive = TRUE, showWarnings = FALSE)

scenario_horizon <- 10
last_year <- max(fertility_births$year)
last_tfr <- fertility_births |>
  filter(year == last_year) |>
  pull(tfr)

tfr_model_path <- final_tfr_model |>
  forecast(h = scenario_horizon) |>
  as_tibble() |>
  transmute(year, tfr = as.numeric(.mean))

future_years <- tibble(year = last_year + seq_len(scenario_horizon))

tfr_scenarios <- bind_rows(
  tfr_model_path |>
    mutate(scenario = "model_trend"),
  future_years |>
    mutate(
      scenario = "stabilisation",
      tfr = last_tfr
    ),
  future_years |>
    mutate(
      scenario = "gradual_rebound",
      tfr = seq(
        from = last_tfr + (1.10 - last_tfr) / scenario_horizon,
        to = 1.10,
        length.out = scenario_horizon
      )
    )
) |>
  select(scenario, year, tfr) |>
  arrange(scenario, year) |>
  as_tsibble(key = scenario, index = year)

forecast_births_for_scenario <- function(scenario_name) {
  scenario_data <- tfr_scenarios |>
    filter(scenario == scenario_name) |>
    as_tibble() |>
    select(year, tfr) |>
    as_tsibble(index = year)

  final_birth_model |>
    forecast(new_data = scenario_data) |>
    as_tibble() |>
    transmute(
      scenario = scenario_name,
      year,
      log_births_mean = as.numeric(.mean),
      projected_live_births = round(exp(log_births_mean))
    )
}

birth_scenario_forecasts <- bind_rows(lapply(
  unique(tfr_scenarios$scenario),
  forecast_births_for_scenario
)) |>
  arrange(scenario, year)

readr::write_csv(
  as_tibble(tfr_scenarios),
  file.path(scenario_dir, "tfr_scenario_paths.csv")
)
readr::write_csv(
  birth_scenario_forecasts,
  file.path(scenario_dir, "birth_scenario_forecasts.csv")
)

tfr_scenario_plot <- fertility_births |>
  ggplot(aes(x = year, y = tfr)) +
  geom_line(colour = "grey35", linewidth = 0.7) +
  geom_line(
    data = as_tibble(tfr_scenarios),
    aes(colour = scenario),
    linewidth = 0.8
  ) +
  labs(
    title = "Alternative TFR scenario paths",
    x = "Year",
    y = "TFR per female",
    colour = "Scenario"
  ) +
  theme_minimal()

birth_scenario_plot <- fertility_births |>
  ggplot(aes(x = year, y = total_live_births)) +
  geom_line(colour = "grey35", linewidth = 0.7) +
  geom_line(
    data = birth_scenario_forecasts,
    aes(y = projected_live_births, colour = scenario),
    linewidth = 0.8
  ) +
  labs(
    title = "Live-birth forecasts under alternative TFR scenarios",
    x = "Year",
    y = "Projected total live-births",
    colour = "Scenario"
  ) +
  theme_minimal()

ggsave(
  file.path(forecast_figure_dir, "tfr_scenario_paths.png"),
  tfr_scenario_plot,
  width = 7,
  height = 4.5,
  dpi = 300
)
ggsave(
  file.path(forecast_figure_dir, "birth_scenario_forecasts.png"),
  birth_scenario_plot,
  width = 7,
  height = 4.5,
  dpi = 300
)

list(
  tfr_scenarios = tfr_scenarios,
  birth_scenario_forecasts = birth_scenario_forecasts
)
