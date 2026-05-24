# Candidate model fitting and diagnostics.

source("R/00_setup.R")
source("R/01_data_import.R")

# Candidate models to consider depending on the selected dataset:
# - regression with trend and seasonal terms
# - ARIMA/SARIMA
# - GARCH if volatility clustering is present
# - state-space / Kalman filter if latent level or structural change is central

# Store model summaries, diagnostics, and selected equations for the report and appendix.

