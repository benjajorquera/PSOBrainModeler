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
library(progress)

mydata <- read.table("data-raw/Sujeto1.txt", header = TRUE)

brain_modeler_config <-
  configure_pso_brain_modeler(vsvr_tolerance = 0.1, seed = 123)

psoptim_config <- configure_psoptim_control()

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
  col_lags = c(2),
  response_lags = NULL,
  extra_col_name = NULL
)

save(grid_search, file = "results/grid_search_results.RData")

#load("results/grid_search_results.RData")

# cors_grid <- c()
#
# for (grid in grid_search$results) {
#   if (is.list(grid)) {
#     cors_grid <- c(cors_grid, grid$avg_cor)
#     if (grid$na_count != 0) {
#       return(TRUE)
#     }
#   }
# }
#
# grid_max_cors <- max(cors_grid)
# print(grid_max_cors)