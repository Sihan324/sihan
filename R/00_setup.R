# Project setup for the time series final report.

required_packages <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "tsibble",
  "feasts",
  "fable",
  "fabletools",
  "lubridate",
  "tidyr",
  "patchwork",
  "forecast",
  "tseries"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages before running the analysis: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

