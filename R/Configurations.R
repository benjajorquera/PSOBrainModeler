#' Configure the Brain Modeler for PSO
#'
#' This function sets the configuration for the PSO brain modeler.
#'
#' @param seed (Optional) Seed for random number generation. Defaults to 123.
#' @param bcv_folds (Optional) Number of folds for blocked cross-validation. Defaults to 5.
#' @param bcv_validation_size (Optional) Proportion of data for validation in blocked CV. Defaults to 0.2.
#' @param pressure_signal_start (Optional) Integer indicating the starting point for pressure signal. Defaults to 3L.
#' @param pressure_signal_response_size (Optional) Integer indicating the size of the response signal for pressure. Defaults to 40L.
#' @param butter_filter_order (Optional) Integer indicating the order of the Butterworth filter. Defaults to 2L.
#' @param butter_filter_fs (Optional) Sampling frequency for the Butterworth filter. Defaults to 0.2.
#' @param max_lag_number (Optional) Maximum number of lags to consider. Defaults to 8.
#' @param vsvr_tolerance (Optional) Tolerance level for VSVR. Defaults to 1.
#'
#' @return A list containing the configuration with class "PSOBrainModelerConfig".
#'
#' @examples
#' configure_pso_brain_modeler()
#'
#' @export

configure_pso_brain_modeler <- function(seed = 123,
                                        bcv_folds = 5,
                                        bcv_validation_size = 0.2,
                                        pressure_signal_start = 3L,
                                        pressure_signal_response_size = 65L,
                                        butter_filter_order = 2L,
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
#' This function prepares the data environment required for modeling. The signals in the resulting data
#' are normalized and the signal names have a "_norm" suffix to indicate this normalization.
#'
#' @param config A list of configuration options.
#' @param data A data frame containing the dataset to be processed.
#' @param excluded_cols Vector of column names that should be excluded from processing.
#' @param signal_names Vector of names representing signal columns. The resulting normalized signal names
#' in the processed data will have a "_norm" suffix.
#' @param predictors_names Vector of predictor names for the model. The resulting normalized predictor names
#' in the processed data will have a "_norm" suffix.
#' @param vsvr_response Name of the column representing the VSVR response. The resulting normalized name
#' in the processed data will have a "_norm" suffix.
#'
#' @return A list containing various data configurations including the processed data with normalized signal names.
#'
#' @details
#' This function depends on:
#' \itemize{
#'   \item Internal package functions:
#'   \itemize{
#'     \item \code{\link{add_pressure_step}}: Generate a dataframe for the smoothed negative
#'    pressure step response with a Butterworth filter.
#'     \item \code{\link{process_dataframe}}: Exclude, Normalize, and Lag Signals of Data Frame.
#' The normalization will append a "_norm" suffix to the signal names.
#'     \item \code{\link{blocked_cv}}: Perform k-folded blocked cross-validation.
#'   }
#' }
#'
#' @examples
#' config <- PSOBrainModeler:::configure_pso_brain_modeler()
#' data <- data.frame(feature1 = rnorm(10), feature2 = rnorm(10))
#' PSOBrainModeler:::configure_data_env(config = config, data = data, excluded_cols = NULL,
#'   signal_names = c("feature1", "feature2"), predictors_names = c("feature1"),
#'   vsvr_response = "feature2")
#'

configure_data_env <- function(config,
                               data,
                               excluded_cols,
                               signal_names,
                               predictors_names,
                               vsvr_response,
                               extra_col_name = NULL,
                               initial_prediction_values = c(1),
                               multi = FALSE) {
  set.seed(config$seed)
  
  pressure_df <- add_pressure_step(
    pressure_start = config$pressure_signal_start,
    signal_end = config$pressure_signal_response_size,
    butter_order = config$butter_filter_order,
    butter_fs = config$butter_filter_fs
  )
  
  processed_data <- process_dataframe(
    df = data,
    excluded_cols = excluded_cols,
    lags = config$max_lag_number,
    signals = signal_names,
    lagged_signals = predictors_names
  )
  
  data_partitions <- blocked_cv(
    data = processed_data,
    num_blocks = config$bcv_folds,
    validation_size = config$bcv_validation_size
  )
  
  # Check if 'multi' is TRUE to process additional data
  if (multi) {
    # Create the additional column name and extract its first values up to 'pressure_signal_start'
    extra_col_data <-
      processed_data[[paste0(extra_col_name, "_norm")]][1:config$pressure_signal_start]
    
    # Calculate the mean of those values
    extra_col_avg <- mean(extra_col_data)
    
    # Add this average to 'initial_prediction_values'
    initial_prediction_values <-
      c(initial_prediction_values, extra_col_avg)
  }
  
  
  return(
    list(
      pressure_df = pressure_df,
      processed_data = processed_data,
      data_partitions = data_partitions,
      NORM_SIGNAL_NAMES = paste0(signal_names, "_norm"),
      NORM_PREDICTORS_NAMES = paste0(predictors_names, "_norm"),
      NORM_VSVR_RESPONSE = paste0(vsvr_response, "_norm"),
      VSVR_TOL = config$vsvr_tolerance,
      INITIAL_PREDICTION_VALUES = initial_prediction_values
    )
  )
}

#' Configure the control parameters for psoptim
#'
#' Sets the configuration for the psoptim optimizer.
#'
#' @param pso_trace (Optional) Non-negative integer for tracing optimization progress. Defaults to 1.
#' @param pso_fnscale (Optional) Scaling for the fn value. If negative, optimization becomes a maximization problem.
#'  Defaults to 1.
#' @param pso_max_iterations (Optional) Max number of PSO iterations. Defaults to 100.
#' @param pso_max_fn_calls (Optional) Max function calls for PSO. Defaults to 200.
#' @param pso_abstol (Optional) Absolute convergence tolerance. Converges when fitness is <= abstol.
#'  Defaults to -Inf.
#' @param pso_restart_tolerance (Optional) Tolerance for PSO restarting. Defaults to 0.5.
#' @param pso_report (Optional) Frequency for reporting if trace is positive. Defaults to 1.
#' @param pso_trace_stats (Optional) Logical; if TRUE, collects statistics at each reporting step.
#'  Defaults to TRUE.
#' @param pso_swarm_size (Optional) Swarm size. Defaults to 5.
#' @param pso_informants (Optional) Exponent for calculating informants. Defaults to 3.
#' @param pso_informed_swarm (Optional) Proportion of swarm to inform. Defaults to 0.5.
#' @param pso_exploitation_const (Optional) Exploitation constant. A vector of length 1 or 2.
#'  Changes from w[1] to w[2] as iterations increase. Defaults to 1/(2*log(2)).
#' @param pso_local_exp_const (Optional) Local exploration constant. Defaults to .5+log(2).
#' @param pso_global_exp_const (Optional) Global expansion constant. Defaults to 10.
#' @param pso_rand_order (Optional) Logical; if TRUE, particles are processed randomly.
#'  Ignored if vectorize is TRUE. Defaults to TRUE.
#' @param pso_max_restart (Optional) Maximum restarts allowed. Defaults to Inf.
#' @param pso_maxit_without_improvement (Optional) Iterations allowed without improvement.
#'  Defaults to 50.
#' @param pso_hybrid_type (Optional) Hybrid method type. Defaults to "improved".
#' @param pso_type (Optional) PSO method type. Defaults to "SPSO2011".
#' @param pso_vectorization (Optional) Logical; if TRUE, PSO is vectorized. Defaults to TRUE.
#' @param pso_hybrid_control (Optional) Control parameters for hybrid method.
#'
#' @return A list containing the psoptim configuration with class "PSOBrainModelerPSOPTIMConfig".
#'
#' @examples
#' configure_psoptim_control()
#'
#' @export

configure_psoptim_control <- function(pso_trace = 1,
                                      pso_fnscale = 1,
                                      pso_max_iterations = 200,
                                      pso_max_fn_calls = 500,
                                      pso_abstol = -Inf,
                                      pso_restart_tolerance = 0.5,
                                      pso_report = 1,
                                      pso_trace_stats = TRUE,
                                      pso_swarm_size = 5,
                                      pso_informants = 3,
                                      pso_informed_swarm = 0.5,
                                      pso_exploitation_const = (1 / (2 * log(2))),
                                      pso_local_exp_const = (5 + log(2)),
                                      pso_global_exp_const = 10,
                                      pso_rand_order = TRUE,
                                      pso_max_restart = Inf,
                                      pso_maxit_without_improvement = 50,
                                      pso_hybrid_type = "improved",
                                      pso_type = "SPSO2011",
                                      pso_vectorization = TRUE,
                                      pso_hybrid_control = list(maxit = 1)) {
  options <- list(
    trace = pso_trace,
    fnscale = pso_fnscale,
    maxit = pso_max_iterations,
    maxf = pso_max_fn_calls,
    abstol = pso_abstol,
    reltol = pso_restart_tolerance,
    REPORT = pso_report,
    trace.stats = pso_trace_stats,
    s = pso_swarm_size,
    k = pso_informants,
    p = pso_informed_swarm,
    w = pso_exploitation_const,
    c.p = pso_local_exp_const,
    c.g = pso_global_exp_const,
    rand.order = pso_rand_order,
    max.restart = pso_max_restart,
    maxit.stagnate = pso_maxit_without_improvement,
    vectorize = pso_vectorization,
    hybrid = pso_hybrid_type,
    hybrid.control = pso_hybrid_control,
    type = pso_type
  )
  
  attr(options, "class") <- "PSOBrainModelerPSOPTIMConfig"
  return(options)
}
