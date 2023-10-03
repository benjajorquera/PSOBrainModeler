library(PSOBrainModeler)

mydata <- read.table("data-raw/Sujeto1.txt", header = TRUE)

brain_modeler_config <-
  configure_pso_brain_modeler()
psoptim_config <- configure_psoptim_control()

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
  vsvr_response = "CBFV.L"
)


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
  vsvr_response = "CBFV.L"
)

result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "ARX",
  multi = FALSE,
  signal_names = c("MABP", "CBFV.L"),
  excluded_cols = c("Time", "CBFV.R", "etCO2"),
  predictors_names = c("MABP", "CBFV.L"),
  params_lower_bounds = c(0.25, 0.1, 1, 0),
  params_upper_bounds = c(4096, 0.9, 8, 6),
  params_initial_values = c(NA, NA, NA, NA),
  vsvr_response = "CBFV.L"
)

result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  data = mydata,
  model = "NARX",
  multi = FALSE,
  signal_names = c("MABP", "CBFV.L"),
  excluded_cols = c("Time", "CBFV.R", "etCO2"),
  predictors_names = c("MABP", "CBFV.L"),
  params_lower_bounds = c(0.25, 0.1, (1 / (2 * 1024 ^ 2)), 1, 0),
  params_upper_bounds = c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)), 8, 6),
  params_initial_values = c(NA, NA, NA, NA, NA),
  vsvr_response = "CBFV.L"
)
