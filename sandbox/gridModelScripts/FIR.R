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

library(magrittr)

brain_modeler_config <-
  configure_pso_brain_modeler(vsvr_tolerance = 0.1,
                              seed = 123,
                              svm_cache_size = 100)

for (i in seq(1:3)) {
  file_src <- paste0("data-raw/source/2Hz/", i, ".txt")
  mydata <- read.table(file_src, header = TRUE)
  
  grid_search <- svr_grid_search(
    config = brain_modeler_config,
    dataset = mydata,
    is_multivariate = FALSE,
    signal_names = c("MABP", "CBFV.L"),
    exclude_columns = c("Time", "CBFV.R", "etCO2"),
    predictor_names = c("MABP"),
    response_var = "CBFV.L",
    kernel_type = "linear",
    is_test_mode = TRUE,
    lags_column = c(3),
    lags_response = NULL,
    extra_column_name = NULL,
    is_silent_mode = TRUE
  )
  
  file_name <-
    paste0("sandbox/gridModelsScripts/results/FIR_",
           i,
           ".RData")
  
  save(grid_search, file = file_name)
}