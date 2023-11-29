source("R/TimeSeries.R")
source("R/Filtering.R")
source("R/Normalization.R")
source("R/Lagging.R")
source("R/BlockedCrossValidation.R")
source("R/VSVMRegression.R")
source("R/TimeSeriesSignalPrediction.R")
source("R/ScoreSignal.R")
source("R/OptimizeBrainModelWithPSO.R")
source("R/Configurations.R")
source("R/PSOHelpers.R")
source("R/PSOModelObjectives.R")
source("R/PSOModelOptimization.R")
source("R/Helpers.R")
source("R/GridSearchSVR.R")
source("R/SignalHelpers.R")

library(dplyr)

mydata <-
  read.table("data-raw/testing/2Hz/Sujeto1.txt", header = TRUE)

brain_modeler_config <-
  configure_pso_brain_modeler(vsvr_tolerance = 0.1)

grid_search <- svr_grid_search(
  config = brain_modeler_config,
  dataset = mydata,
  kernel_type = 'radial',
  is_multivariate = TRUE,
  signal_names = c("MABP", "etCO2", "CBFV.L"),
  predictor_names = c("MABP", "etCO2"),
  response_var = "CBFV.L",
  exclude_columns = c("Time", "CBFV.R"),
  lags_column = c(1, 1),
  lags_response = NULL,
  is_test_mode = TRUE,
  extra_column_name = "etCO2",
  generate_response_predictions_cv = FALSE
)
