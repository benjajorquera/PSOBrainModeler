#' Configure the Brain Modeler for PSO
#'
#' This function sets the configuration for the PSO brain modeler. It includes
#' options for seed initialization, blocked cross-validation settings,
#' Butterworth filter parameters, lag number, VSVR tolerance, and SVM cache size.
#'
#' @param seed (Optional) Seed for random number generation. Defaults to 123.
#' @param bcv_folds (Optional) Number of folds for blocked cross-validation.
#'  Defaults to 5.
#' @param pressure_signal_start (Optional) Integer indicating the starting point
#'  for pressure signal. Defaults to 3L.
#' @param pressure_signal_response_size (Optional) Integer indicating the size
#'  of the response signal for pressure. Defaults to 70L.
#' @param butter_filter_order (Optional) Integer indicating the order of the
#'  Butterworth filter. Defaults to 2L.
#' @param butter_filter_fs (Optional) Sampling frequency for the Butterworth
#'  filter. Defaults to 0.2.
#' @param max_lag_number (Optional) Maximum number of lags to consider. Defaults
#'  to 8.
#' @param vsvr_tolerance (Optional) Tolerance level for VSVR. Defaults to 1.
#' @param svm_cache_size (Optional) Size of the cache for SVM training. Defaults
#'  to 100.
#'
#' @return Returns a list of configuration settings for the PSO brain modeler.
#'  Each element of the list corresponds to a specific configuration option:
#'   - `seed`: Seed for random number generation.
#'   - `bcv_folds`: Number of folds for blocked cross-validation.
#'   - `pressure_signal_start`: Starting point for the pressure signal.
#'   - `pressure_signal_response_size`: Size of the response signal for pressure.
#'   - `butter_filter_order`: Order of the Butterworth filter.
#'   - `butter_filter_fs`: Sampling frequency for the Butterworth filter.
#'   - `max_lag_number`: Maximum number of lags to consider.
#'   - `vsvr_tolerance`: Tolerance level for VSVR.
#'   - `svm_cache_size`: Cache size for VSVR.
#'
#' @examples
#' configure_pso_brain_modeler()
#'
#' @export
#'
configure_pso_brain_modeler <- function(seed = 123,
                                        bcv_folds = 5,
                                        pressure_signal_start = 3L,
                                        pressure_signal_response_size = 70L,
                                        butter_filter_order = 2L,
                                        butter_filter_fs = 0.2,
                                        max_lag_number = 8,
                                        vsvr_tolerance = 1,
                                        svm_cache_size = 100) {
  
  bcv_validation_size <- 1/bcv_folds
  
  options <- list(
    seed = seed,
    bcv_folds = bcv_folds,
    bcv_validation_size = bcv_validation_size,
    pressure_signal_start = pressure_signal_start,
    pressure_signal_response_size = pressure_signal_response_size,
    butter_filter_order = butter_filter_order,
    butter_filter_fs = butter_filter_fs,
    max_lag_number = max_lag_number,
    vsvr_tolerance = vsvr_tolerance,
    svm_cache_size = svm_cache_size
  )
  
  return(options)
}

#' Configure Data Environment for PSO Modeling
#'
#' This function prepares the data environment for PSO modeling.
#' It processes the input data and sets up the necessary components and
#' configurations based on the provided parameters. It handles the addition of
#' a pressure step, processing of the data frame, and blocked cross-validation
#' partitioning. It also normalizes signal names and handles additional
#' configurations for multivariate models.
#'
#' @param config A list of configuration options for the brain modeler.
#' @param data A data frame containing the dataset to be optimized.
#' @param excluded_cols (Optional) Vector of column names to be excluded from
#'  the optimization. Defaults to NULL.
#' @param signal_names Vector of names representing signal columns.
#' @param predictors_names Vector of predictor names for the model.
#' @param vsvr_response Name of the column representing the VSVR response.
#' @param extra_col_name (Optional) Additional column name to be included. Only
#'  used if `multi` is TRUE. Defaults to NULL.
#' @param initial_prediction_values (Optional) Initial prediction values for the
#'  model. Defaults to c(1).
#' @param multi (Optional) Logical indicating if the model should be multivariate.
#'  If TRUE, additional processing is performed for the extra column. Defaults
#'  to FALSE.
#'
#' @return A list containing the following elements:
#'   - `pressure_df`: Data frame with pressure step added.
#'   - `processed_data`: The processed data frame.
#'   - `data_partitions`: Data partitions created by blocked cross-validation.
#'   - `NORM_SIGNAL_NAMES`: Normalized signal names.
#'   - `NORM_PREDICTORS_NAMES`: Normalized predictor names.
#'   - `NORM_VSVR_RESPONSE`: Normalized VSVR response name.
#'   - `VSVR_TOL`: VSVR tolerance from the configuration.
#'   - `INITIAL_PREDICTION_VALUES`: Initial prediction values,
#'       adjusted if `multi` is TRUE.
#'   - `svm_cache_size`: SVM cache size from the configuration.
#'
#' @examples
#' \dontrun{
#'   configure_data_env(...)
#' }
#'
#' @export
#'
configure_data_env <-
  function(config,
           data,
           excluded_cols = NULL,
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
    
    if (multi && !is.null(extra_col_name)) {
      extra_col_data <-
        head(processed_data[[paste0(extra_col_name, "_norm")]],
             config$pressure_signal_start)
      initial_prediction_values <-
        c(initial_prediction_values, mean(extra_col_data))
    }
    
    norm_names <- function(names)
      paste0(names, "_norm")
    
    return(
      list(
        pressure_df = pressure_df,
        processed_data = processed_data,
        data_partitions = data_partitions,
        NORM_SIGNAL_NAMES = norm_names(signal_names),
        NORM_PREDICTORS_NAMES = norm_names(predictors_names),
        NORM_VSVR_RESPONSE = norm_names(vsvr_response),
        VSVR_TOL = config$vsvr_tolerance,
        INITIAL_PREDICTION_VALUES = initial_prediction_values,
        svm_cache_size = config$svm_cache_size
      )
    )
  }


#' Configure Control Parameters for PSO Optimization
#'
#' This function sets up control parameters for Particle Swarm Optimization (PSO)
#' by providing default values and allowing customization through user-specified
#' arguments.
#'
#' @param ... Optional parameters to override the default settings. Some are:
#'   - `maxit`: Maximum number of iterations. Default is 125.
#'   - `s`: Swarm size. Default is 8.
#'   - `w`: Inertia weight. Default is 1.
#'   - `c.p`: Personal acceleration coefficient. Default is 2.
#'   - `c.g`: Global acceleration coefficient. Default is 5.
#'   - `vectorize`: Boolean indicating if vectorized calculations should be used.
#'       Default is TRUE.
#'   - `reltol`: Relative tolerance. Default is 0.5.
#'   - `hybrid`: Type of hybrid algorithm to use. Default is "improved".
#'   - `hybrid.control`: Control parameters for the hybrid algorithm. Defaults
#'       to `list(maxit = 10)`.
#'   - `maxit.stagnate`: Maximum iterations for stagnation. Default is 110.
#'   - `maxf`: Maximum number of function evaluations. Default is 1000.
#'
#'   The complete list of this parameters can be found in the psoptim documentation of the pso package.
#'
#' @return A list of control parameters for PSO optimization, combining the
#'  default settings with any user-specified overrides.
#'
#' @examples
#' # Use default control parameters
#' configure_psoptim_control()
#'
#' # Customize some parameters
#' configure_psoptim_control(maxit = 150, s = 10)
#'
#' @export
#'
configure_psoptim_control <- function(...) {
  # Define default values
  default_args <- list(
    maxit = 150,
    # Maximum number of iterations
    s = 8,
    # Swarm size
    w = 1,
    # Inertia weight
    c.p = 2,
    # Personal acceleration coefficient
    c.g = 5,
    # Global acceleration coefficient
    vectorize = TRUE,
    # Use vectorized calculations
    reltol = 0.5,
    # Relative tolerance
    hybrid = "improved",
    # Type of hybrid algorithm
    hybrid.control = list(maxit = 10),
    # Control parameters for the hybrid algorithm
    maxit.stagnate = 100,
    # Maximum iterations for stagnation
    maxf = 1200
    # Maximum number of function evaluations
  )
  
  # Capture additional user arguments
  user_args <- list(...)
  
  # Combine user arguments with default values
  combined_args <- modifyList(default_args, user_args)
  
  return(combined_args)
}
