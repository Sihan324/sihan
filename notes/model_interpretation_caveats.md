# Model Interpretation Caveats

These caveats should guide the final wording of the report and appendix.

## Safe Claims

- The TFR and total live-birth series show persistent long-run decline over 1960-2025.
- The raw TFR and log live-birth series are not well described as stationary series.
- The ARIMA with drift is the best TFR model among the current candidates by ten-year holdout RMSE.
- The live-birth model with TFR as a regressor is the strongest conditional model among the current candidates by AICc and holdout RMSE.
- Scenario forecasts show that live-birth projections are sensitive to future TFR assumptions.

## Claims To Avoid

- Do not claim that TFR causes total live-births in a causal inference sense.
- Do not claim that the live-birth model is a complete standalone future forecast unless future TFR is forecast or specified as a scenario.
- Do not imply that the model-trend TFR scenario is a policy expectation or demographic certainty.
- Do not hide the Ljung-Box result for the live-birth model with TFR; residual autocorrelation remains.
- Do not treat AICc and holdout RMSE as interchangeable model-selection criteria.

## Recommended Wording

- Use "conditional forecast" for live-birth forecasts that supply future TFR values.
- Use "scenario analysis" for the 2026-2035 TFR paths.
- Use "association" or "predictive information" for the TFR regressor, not "causal effect".
- Use "best among the current candidate set" rather than "best possible model".
- Use "additional demographic variables would be needed" when discussing policy-grade forecasting.

## Final Report Risk

The largest interpretation risk is overstating the live-birth model. Its AICc and conditional holdout performance are strong, but its residual Ljung-Box p-value remains low and it relies on supplied TFR values. The report should therefore present it as useful but incomplete.
