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

library(dplyr)
library(pso)
library(caret)
mydata <- read.table("data-raw/Sujeto1.txt", header = TRUE)

brain_modeler_config <- configure_pso_brain_modeler()
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
  # c(8)
  # c(8, 6)
  response_lags = NULL,
  # c(8)
  extra_col_name = NULL #"etCO2" NULL
)

###############################################################################
result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "FIR",
  multi = FALSE,
  signal_names = c("MABP", "CBFV.L"),
  excluded_cols = c("Time", "CBFV.R", "etCO2"),
  predictors_names = c("MABP"),
  params_lower_bounds = c(0.25, 0.1, 1),
  params_upper_bounds = c(4096, 0.9, 8),
  params_initial_values = c(NA, NA, NA),
  vsvr_response = "CBFV.L",
  initial_pressure_value = c(1)
)

###############################################################################

result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "NFIR",
  multi = FALSE,
  signal_names = c("MABP", "CBFV.L"),
  excluded_cols = c("Time", "CBFV.R", "etCO2"),
  predictors_names = c("MABP"),
  params_lower_bounds = c(0.25, 0.1, (1 / (2 * 1024 ^ 2)), 1),
  params_upper_bounds = c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)), 8),
  params_initial_values = c(NA, NA, NA, NA),
  vsvr_response = "CBFV.L",
  initial_pressure_value = c(1)
)

##############################################################################
result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "ARX",
  multi = FALSE,
  signal_names = c("MABP", "CBFV.L"),
  excluded_cols = c("Time", "CBFV.R", "etCO2"),
  predictors_names = c("MABP", "CBFV.L"),
  params_lower_bounds = c(0.25, 0.1, 1, 1),
  params_upper_bounds = c(4096, 0.9, 8, 6),
  params_initial_values = c(NA, NA, NA, NA),
  vsvr_response = "CBFV.L",
  initial_pressure_value = c(1)
)
###############################################################################
result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "NARX",
  multi = FALSE,
  signal_names = c("MABP", "CBFV.L"),
  excluded_cols = c("Time", "CBFV.R", "etCO2"),
  predictors_names = c("MABP", "CBFV.L"),
  params_lower_bounds = c(0.25, 0.1, (1 / (2 * 1024 ^ 2)), 1, 1),
  params_upper_bounds = c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)), 8, 6),
  params_initial_values = c(NA, NA, NA, NA, NA),
  vsvr_response = "CBFV.L",
  initial_pressure_value = c(1)
)
##############################################################################
result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "FIR",
  multi = TRUE,
  signal_names = c("MABP", "CBFV.L", "etCO2"),
  excluded_cols = c("Time", "CBFV.R"),
  predictors_names = c("MABP", "etCO2"),
  params_lower_bounds = c(0.25, 0.1, 1, 0),
  params_upper_bounds = c(4096, 0.9, 8, 6),
  params_initial_values = c(NA, NA, NA, NA),
  vsvr_response = "CBFV.L",
  initial_pressure_value = c(1)
)
###############################################################################
result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "NFIR",
  multi = TRUE,
  signal_names = c("MABP", "etCO2", "CBFV.L"),
  excluded_cols = c("Time", "CBFV.R"),
  predictors_names = c("MABP", "etCO2"),
  params_lower_bounds = c(0.25, 0.1, (1 / (2 * 1024 ^ 2)), 1, 0),
  params_upper_bounds = c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)), 8, 6),
  params_initial_values = c(NA, NA, NA, NA, NA),
  vsvr_response = "CBFV.L",
  initial_pressure_value = c(1)
)
################################################################################
result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "ARX",
  multi = TRUE,
  signal_names = c("MABP", "CBFV.L", "etCO2"),
  excluded_cols = c("Time", "CBFV.R"),
  predictors_names = c("MABP", "etCO2", "CBFV.L"),
  params_lower_bounds = c(0.25, 0.1, 1, 0, 1),
  params_upper_bounds = c(4096, 0.9, 8, 6, 6),
  params_initial_values = c(NA, NA, NA, NA, NA),
  vsvr_response = "CBFV.L",
  initial_pressure_value = c(1)
)

##############################################################################
result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "NARX",
  multi = TRUE,
  signal_names = c("MABP", "CBFV.L", "etCO2"),
  excluded_cols = c("Time", "CBFV.R"),
  predictors_names = c("MABP", "etCO2", "CBFV.L"),
  params_lower_bounds = c(0.25, 0.1, (1 / (2 * 1024 ^ 2)), 1, 0, 1),
  params_upper_bounds = c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)), 8, 6, 6),
  params_initial_values = c(NA, NA, NA, NA, NA, NA),
  vsvr_response = "CBFV.L",
  initial_pressure_value = c(1)
)
