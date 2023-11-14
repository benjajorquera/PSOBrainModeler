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
source("R/Validators.R")
source("R/GridSearchSVR.R")

library(tseries)
library(dplyr)

mydata <- read.table("data-raw/Sujeto1.txt", header = TRUE)

brain_modeler_config <-
  configure_pso_brain_modeler(vsvr_tolerance = 0.1)

start_time <- Sys.time()

grid_search <- svr_grid_search(
  config = brain_modeler_config,
  data = mydata,
  multi = FALSE,
  signal_names = c("MABP", "CBFV.L"),
  excluded_cols = c("Time", "CBFV.R", "etCO2"),
  predictors_names = c("MABP"),
  vsvr_response = "CBFV.L",
  kernel = "radial",
  test = TRUE,
  col_lags = c(1),
  response_lags = NULL,
  extra_col_name = NULL
)

print(Sys.time() - start_time)