#' Configure the Brain Modeler for PSO
#'
#' This function sets the configuration for the PSO brain modeler.
#'
#' @param seed Seed for random number generation. Default: 123.
#' @param bcv_folds Number of folds for blocked cross-validation. Default: 5.
#' @param bcv_validation_size Proportion of data for validation in blocked CV. Default: 0.2.
#' @param pressure_signal_start Starting point for pressure signal. Default: 3.
#' @param pressure_signal_response_size Size of the response signal for pressure. Default: 40.
#' @param butter_filter_order Order of the Butterworth filter. Default: 2.
#' @param butter_filter_fs Sampling frequency for the Butterworth filter. Default: 0.2.
#' @param max_lag_number Maximum number of lags to consider. Default: 8.
#' @param vsvr_tolerance Tolerance level for VSVR. Default: 1.
#' @return A list containing the configuration with class "PSOBrainModelerConfig".
configure_pso_brain_modeler <- function(seed = 123,
                                        bcv_folds = 5,
                                        bcv_validation_size = 0.2,
                                        pressure_signal_start = 3,
                                        pressure_signal_response_size = 40,
                                        butter_filter_order = 2,
                                        butter_filter_fs = 0.2,
                                        max_lag_number = 8,
                                        vsvr_tolerance = 1) {
  options <- list(
    seed = seed,
    bcv_folds = bcv_folds,
    bcv_validation_size = bcv_validation_size,
    pressure_signal_start = pressure_signal_start,
    pressure_signal_response_size = pressure_signal_response_size,
    butter_filter_order = butter_filter_order,
    butter_filter_fs = butter_filter_fs,
    max_lag_number = max_lag_number,
    vsvr_tolerance = vsvr_tolerance
  )
  
  attr(options, "class") <- "PSOBrainModelerConfig"
  return(options)
}

#' Configure the data environment for modeling
#'
#' This function prepares the data environment required for modeling.
#'
#' @param config A list of configuration options.
#' @param data A data frame containing the dataset to be processed.
#' @param excluded_cols Vector of column names that should be excluded from processing.
#' @param signal_names Vector of names representing signal columns.
#' @param predictors_names Vector of predictor names for the model.
#' @param vsvr_response Name of the column representing the VSVR response.
#' @return NULL. The function assigns values to global variables in the package environment.
configure_data_env <- function(config,
                               data,
                               excluded_cols,
                               signal_names,
                               predictors_names,
                               vsvr_response) {
  set.seed(config$seed)
  
  # Generate a dataframe for the smoothed negative pressure step response with a Butterworth filter
  pressure_df <- add_pressure_step(
    pressure_start = config$pressure_signal_start,
    signal_end = config$pressure_signal_response_size,
    butter_order = config$butter_filter_order,
    butter_fs = config$butter_filter_fs
  )
  
  assign("pressure_df", pressure_df, envir = .psoBrainModelerEnv)
  
  processed_data <- process_dataframe(
    df = data,
    excluded_cols = excluded_cols,
    lags = config$max_lag_number,
    signals = signal_names,
    lagged_signals = predictors_names
  )
  
  assign("processed_data", processed_data, envir = .psoBrainModelerEnv)
  
  # Perform k-folded blocked cross-validation
  data_partitions <- blocked_cv(
    data = processed_data,
    num_blocks = config$bcv_folds,
    validation_size = config$bcv_validation_size
  )
  
  assign("data_partitions", data_partitions, envir = .psoBrainModelerEnv)
  
  assign("NORM_SIGNAL_NAMES", paste0(signal_names, "_norm"), envir = .psoBrainModelerEnv)
  assign("NORM_PREDICTORS_NAMES",
         paste0(predictors_names, "_norm"),
         envir = .psoBrainModelerEnv)
  assign("NORM_VSVR_RESPONSE",
         paste0(vsvr_response, "_norm"),
         envir = .psoBrainModelerEnv)
  assign("VSVR_TOL", config$vsvr_tolerance, envir = .psoBrainModelerEnv)
}

#' Configure the control parameters for psoptim
#'
#' Sets the configuration for the psoptim optimizer.
#'
#' @param pso_swarm_size Size of the particle swarm. Default: 5.
#' @param pso_max_iterations Maximum number of iterations for PSO. Default: 100.
#' @param pso_max_fn_calls Maximum number of function calls for PSO. Default: 200.
#' @param pso_informed_swarm Proportion of swarm to inform. Default: 0.5.
#' @param pso_global_exp_const Global expansion constant for PSO. Default: 10.
#' @param pso_maxit_without_improvement Maximum iterations without improvement. Default: 50.
#' @param pso_restart_tolerance Tolerance for restarting PSO. Default: 0.5.
#' @param pso_hybrid_type Type of hybrid method for PSO. Default: "improved".
#' @param pso_type Type of PSO method. Default: "SPSO2011".
#' @param pso_vectorization Logical indicating if PSO should be vectorized. Default: TRUE.
#' @param pso_hybrid_control List of control parameters for the hybrid method.
#' @return A list containing the psoptim configuration with class "PSOBrainModelerPSOPTIMConfig".
configure_psoptim_control <- function(pso_swarm_size = 5,
                                      pso_max_iterations = 100,
                                      pso_max_fn_calls = 200,
                                      pso_informed_swarm = 0.5,
                                      pso_global_exp_const = 10,
                                      pso_maxit_without_improvement = 50,
                                      pso_restart_tolerance = 0.5,
                                      pso_hybrid_type = "improved",
                                      pso_type = "SPSO2011",
                                      pso_vectorization = TRUE,
                                      pso_hybrid_control = list(maxit = 1)) {
  options <- list(
    s = pso_swarm_size,
    maxit = pso_max_iterations,
    maxf = pso_max_fn_calls,
    p = pso_informed_swarm,
    c.g = pso_global_exp_const,
    maxit.stagnate = pso_maxit_without_improvement,
    reltol = pso_restart_tolerance,
    hybrid = pso_hybrid_type,
    type = pso_type,
    vectorize = pso_vectorization,
    hybrid.control = pso_hybrid_control,
    trace = 1,
    REPORT = 1,
    trace.stats = TRUE
  )
  
  attr(options, "class") <- "PSOBrainModelerPSOPTIMConfig"
  return(options)
}