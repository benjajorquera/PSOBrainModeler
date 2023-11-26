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

library(foreach)
library(doParallel)

mydata <-
  read.table("data-raw/testing/2Hz/Sujeto1.txt", header = TRUE)

###############################################################################

brain_modeler_config <-
  configure_pso_brain_modeler(vsvr_tolerance = 1, seed = 123)

psoptim_config <- configure_psoptim_control()

result <- optimize_brain_model_with_PSO(
  config = brain_modeler_config,
  psoptim_config = psoptim_config,
  max_function_count = 1000,
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
  silent = TRUE,
  seed = 123,
  generate_response_predictions_cv = TRUE,
  fn_count_treshold = 5
)

###############################################################################

start_time <- Sys.time()

no_cores <- min(detectCores(), 10)
registerDoParallel(no_cores)

seeds <- 123:(123 + no_cores - 1)
resultados <-
  foreach(
    seed = seeds,
    .combine = 'c',
    .packages = c('dplyr', 'tseries')
  ) %dopar% {
    brain_modeler_config <-
      configure_pso_brain_modeler(vsvr_tolerance = 0.01, seed = 123)
    
    psoptim_config <- configure_psoptim_control()
    
    if (seed != 123) {
      set.seed(seed)
    }
    
    optimize_brain_model_with_PSO(
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
      initial_pressure_value = c(1),
      seed = seed
    )
  }

stopImplicitCluster()


print(Sys.time() - start_time)


all_max_cors <- c()
all_fitness <- c()
for (results in resultados) {
  all_max_cors <- c(all_max_cors, results$pso_env$max_cor)
  all_fitness <- c(all_fitness, results$pso_env$best_fitness)
  cat(
    "Máxima correlación: ",
    results$pso_env$max_cor,
    " Señales: ",
    length(results$pso_env$data),
    " Tiempo: ",
    results$pso_env$time,
    " Mejor Fitness: ",
    results$pso_env$best_fitness,
    "\n"
  )
}
pso_avg_cors <- mean(all_max_cors)
pso_avg_fitness <- mean(all_fitness)