# Assignment Requirements Notes

These notes summarise the currently available assignment guidance from the course materials and discussion screenshots.

## GitHub and coding

- A GitHub repository is part of the final report rubric.
- The GitHub component is worth 2/30 according to the staff clarification.
- The repository should contain reproducible R code and a README.
- A stronger submission has a clear commit history that shows work was developed over time.
- Work does not need to be committed every day, but the history should not look like everything was done at the last minute.
- Material uploaded to GitHub is not marked as individually correct or incorrect; it is evidence of workflow and reproducibility.

## Report and appendix

- The main report should focus on the research question, selected methods, key diagnostics, final model, and conclusions.
- The statistical appendix is for extra comments, plots, model equations, and viable models that were tried but not selected.
- The appendix should not replace the main results section.
- Full mathematical descriptions of time series models, statistical tests, and estimation procedures should be included where required by the EDA/final report instructions.

## Modelling expectations

- The report must demonstrate understanding and application of time series models from the course.
- Models outside the course may be used if justified appropriately.
- Statistical inference and forecasting are both important; the research question should demonstrate one or both.
- Candidate course models include ARIMA/SARIMA, GARCH, state-space models, and Kalman filter methods.

## Current local course materials

- Practical 1: exploring time series objects, plots, smoothing, and stationarity.
- Practical 2: trend and seasonal regression for Mauna Loa CO2.
- Practical 3: differencing, AR/ARIMA modelling, residual checks, and forecasting.
- Practical 4: spectral analysis and lung disease deaths.
- Practical 5: ARIMA simulations and sunspot model selection.
- Practical 6: SARIMA, line-up plots, and cross-validation using Arctic sea ice data.
- Practical 7: GARCH simulation and S&P returns.
- Practical 9: local level state-space model and Kalman filtering.
- Practical 10: Kalman filter parameter estimation, exogenous variables, and Nile change point analysis.

## Immediate implementation plan

1. Establish GitHub-ready repository with the correct student Git identity.
2. Choose and document the final research question and dataset.
3. Create reproducible data import and cleaning scripts.
4. Produce EDA plots and stationarity diagnostics.
5. Fit multiple viable candidate models.
6. Select a final model using diagnostics, information criteria, and forecast evaluation.
7. Move non-selected viable model details to the statistical appendix.
8. Render final report and appendix.

## Current completion status

- Repository setup, README, data import, EDA, model fitting, forecast evaluation and report drafts are in place.
- The current Git history shows multiple staged commits over time using the student identity.
- The report still needs final rendering and visual inspection before submission.
- The statistical appendix now documents non-selected model considerations and future model development options.
- The main remaining risk is formatting in the rendered output, because the terminal environment has not exposed the Quarto CLI.
