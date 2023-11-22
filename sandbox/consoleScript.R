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

mydata <- read.table("data-raw/Sujeto1.txt", header = TRUE)

# brain_modeler_config <-
#   configure_pso_brain_modeler(vsvr_tolerance = 0.1, seed = 123)
#
# grid_search <- svr_grid_search(
#   config = brain_modeler_config,
#   dataset = mydata,
#   is_multivariate = TRUE,
#   signal_names = c("MABP", "etCO2", "CBFV.L"),
#   exclude_columns = c("Time", "CBFV.R"),
#   predictor_names = c("MABP", "etCO2", "CBFV.L"),
#   response_var = "CBFV.L",
#   kernel_type = "radial",
#   is_test_mode = TRUE,
#   lags_column = c(2, 2),
#   lags_response = c(2),
#   extra_column_name = "etCO2",
#   is_silent_mode = TRUE
# )
#
# file_name <- paste0("results/grid_search/Sujeto1", "_FIR", ".RData")
#
# save(grid_search, file = file_name)

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

models <- c("FIR", "NFIR", "ARX", "NARX")
multi_options <- c(FALSE, TRUE)

brain_modeler_config <-
  configure_pso_brain_modeler(vsvr_tolerance = 0.01, seed = 123)

for (model in models) {
  for (multi in multi_options) {
    multi_text <- if (multi)
      "_multi"
    else
      ""
    
    cat("\n", model, multi_text, "\n")
    
    is_fir_family <- model == "FIR" || model == "NFIR"
    is_arx_family <- model == "ARX" || model == "NARX"
    
    if (multi) {
      signal_names <- c("MABP", "etCO2", "CBFV.L")
      exclude_columns <- c("Time", "CBFV.R")
      predictor_names <-
        if (is_fir_family)
          c("MABP", "etCO2")
      else
        signal_names
      lags_column <- c(8, 6)
      extra_column_name <- if (multi)
        "etCO2"
      else
        NULL
      lags_response <- if (is_arx_family)
        c(6)
      else
        NULL
    } else {
      signal_names <- c("MABP", "CBFV.L")
      exclude_columns <- c("Time", "etCO2", "CBFV.R")
      predictor_names <-
        if (is_fir_family)
          c("MABP")
      else
        signal_names
      lags_column <- c(8)
      extra_column_name <- if (multi)
        "etCO2"
      lags_response <- if (is_arx_family)
        c(6)
      else
        NULL
    }
    
    kernel <-
      if (model == "FIR" || model == "ARX")
        "linear"
    else
      "radial"
    
    grid_search <- svr_grid_search(
      config = brain_modeler_config,
      dataset = mydata,
      is_multivariate = multi,
      signal_names = signal_names,
      exclude_columns = exclude_columns,
      predictor_names = predictor_names,
      response_var = "CBFV.L",
      kernel_type = kernel,
      is_test_mode = FALSE,
      lags_column = lags_column,
      lags_response = lags_response,
      extra_column_name = extra_column_name,
      is_silent_mode = TRUE
    )
    
    file_name <-
      paste0("results/grid_search/Sujeto1_",
             model,
             multi_text,
             ".RData")
    
    save(grid_search, file = file_name)
  }
}
# 
# load("results/grid_search/Sujeto1_FIR_multi.RData")
# 
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
