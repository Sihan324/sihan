# Project setup for the time series final report.

# Windows R can fail on paths containing non-ASCII characters when the active
# locale is C. This keeps local rendering stable without affecting other systems.
if (.Platform$OS.type == "windows") {
  invisible(try(Sys.setlocale("LC_ALL", "English_United States.utf8"), silent = TRUE))
}

local_library <- "C:/Rlibs/4.4"
if (dir.exists(local_library) && !(local_library %in% .libPaths())) {
  .libPaths(c(local_library, .libPaths()))
}

required_packages <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "ggtime",
  "tsibble",
  "feasts",
  "fable",
  "fabletools",
  "lubridate",
  "tidyr",
  "patchwork",
  "forecast",
  "tseries",
  "jsonlite",
  "scales"
)

missing_packages <- required_packages[!vapply(
  required_packages,
  function(package) requireNamespace(package, quietly = TRUE),
  logical(1)
)]

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
