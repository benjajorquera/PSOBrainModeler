#' Perform a Grid Search for Support Vector Regression
#'
#' @param config Configuration settings for the grid search
#' @param dataset Data to be used in the grid search
#' @param kernel_type Type of kernel to be used in SVR
#' @param is_multivariate Logical indicating if multivariate analysis is to be performed
#' @param signal_names Names of signal variables
#' @param predictor_names Names of predictor variables
#' @param response_var Name of the response variable for SVR
#' @param exclude_columns Columns to be excluded from the analysis
#' @param lags_column Number of lags for the columns
#' @param lags_response Number of lags for the response
#' @param initial_prediction_value Initial value for prediction
#' @param extra_column_name Name of an extra column to be included
#' @param should_plot_response Logical indicating if the response should be plotted
#' @param is_test_mode Logical indicating if the function is in test mode
#' @param is_silent_mode Logical indicating if the function should run in silent mode
#' @return Results of the grid search
#' @examples
#' \dontrun {
#'   svr_grid_search(my_config, my_data, "linear", ...)
#' }
#' @export
svr_grid_search <- function(config,
                            dataset,
                            kernel_type,
                            is_multivariate = FALSE,
                            signal_names,
                            predictor_names,
                            response_var,
                            exclude_columns = NULL,
                            lags_column = c(8),
                            lags_response = NULL,
                            initial_prediction_value = 1,
                            extra_column_name = NULL,
                            should_plot_response = FALSE,
                            is_test_mode = FALSE,
                            is_silent_mode = TRUE) {
  # Validate parameters
  stopifnot(is.list(config),
            is.data.frame(dataset),
            is.character(kernel_type))
  
  start_time <- Sys.time()
  
  # Configure data environment
  data_env <- configure_data_env(
    config = config,
    data = dataset,
    signal_names = signal_names,
    predictors_names = predictor_names,
    vsvr_response = response_var,
    excluded_cols = exclude_columns,
    multi = is_multivariate,
    extra_col_name = extra_column_name
  )
  
  # Conduct main grid search
  return(
    main_grid_search(
      data_env = data_env,
      data_partitions = data_env$data_partitions,
      bcv_folds = config$bcv_folds,
      processed_data = data_env$processed_data,
      norm_response_var = data_env$norm_response_var,
      kernel_type = kernel_type,
      lags_column = lags_column,
      initial_column_values = data_env$initial_prediction_values,
      lags_response = lags_response,
      initial_prediction_value = initial_prediction_value,
      tolerance = config$vsvr_tolerance,
      is_test_mode = is_test_mode,
      is_silent_mode = is_silent_mode,
      start_time = start_time,
      should_plot_response = should_plot_response
    )
  )
}

#' Main Grid Search Function
#'
#' This function performs a grid search over specified parameters for a given model.
#' It supports both linear and non-linear kernels.
#'
#' @param data_env Data environment containing the datasets and parameters.
#' @param bcv_folds Number of folds for cross-validation.
#' @param processed_data Processed data set for modeling.
#' @param norm_response_var Normalized response variable.
#' @param kernel_type Type of kernel to use, default is "linear".
#' @param lags_column Column lags.
#' @param initial_column_values Initial values for columns.
#' @param lags_response Response lags.
#' @param initial_prediction_value Initial value for prediction.
#' @param tolerance Tolerance level for VSVR.
#' @param is_test_mode Boolean for testing mode.
#' @param is_silent_mode Boolean for silent mode.
#' @param start_time Start time for the function.
#' @param should_plot_response Boolean to determine if response should be plotted.
#' @return A list containing the results of the grid search and the time taken.
#' @examples
#'
#' \dontrun {
#'   # Assuming 'data_env_list', 'data_partitions', 'processed_data', and 'norm_vsvr_response' are predefined
#'   # Example with default parameters
#'   results <- main_grid_search(
#'     data_env_list = data_env_list,
#'     data_partitions = data_partitions,
#'     processed_data = processed_data,
#'     norm_vsvr_response = norm_vsvr_response
#'   )
#' }
#' @export
main_grid_search <- function(data_env,
                             bcv_folds,
                             processed_data,
                             norm_response_var,
                             kernel_type = "linear",
                             lags_column = c(8),
                             initial_column_values,
                             lags_response = NULL,
                             initial_prediction_value = 1,
                             tolerance,
                             is_test_mode = FALSE,
                             is_silent_mode = TRUE,
                             start_time,
                             should_plot_response = FALSE) {
  # Simple parameter validation
  if (!is.list(data_env))
    stop("data_env must be a list")
  if (!is.list(data_env$data_partitions))
    stop("data_env$data_partitions must be a list")
  
  # Handle start time
  if (is.null(start_time)) {
    start_time <- Sys.time()
  }
  
  model_params <- list(type = "nu-regression",
                       tolerance = tolerance,
                       kernel = kernel_type)
  
  params <- params_config(kernel = kernel_type, test = is_test_mode)
  
  progress_bar <- progressbar_config(
    kernel = kernel_type,
    test = is_test_mode,
    col_lags = lags_column,
    response_lags = lags_response
  )
  
  main_loop_params <- list(
    data_env = data_env,
    processed_data = processed_data,
    data_partitions = data_env$data_partitions,
    bcv_folds = bcv_folds,
    norm_response_var = norm_response_var,
    lags_column = lags_column,
    initial_column_values = initial_column_values,
    lags_response = lags_response,
    initial_prediction_value = initial_prediction_value,
    model_params = model_params,
    nu = params$nu,
    cost = params$cost,
    training_list_name = "training",
    validation_list_name = "validation",
    is_silent_mode = is_silent_mode,
    should_plot_response = should_plot_response,
    progress_bar = progress_bar
  )
  
  results <- list()
  
  if (kernel_type == "linear") {
    results <-
      append(results, c(do.call(
        grid_search_main_loop, main_loop_params
      )))
  } else {
    for (g in params$gamma) {
      main_loop_params$model_params$gamma <- g
      results <-
        append(results, c(do.call(
          grid_search_main_loop, main_loop_params
        )))
    }
  }
  
  return(list(results = results, start_time = start_time))
}

#' Configure Parameters for SVR
#'
#' This function configures the parameters for Support Vector Regression (SVR) based on the kernel type and test mode.
#'
#' @param kernel_type The type of kernel to use in SVR. Valid options are "linear" and others (e.g., "radial").
#' @param is_test_mode Logical flag indicating whether the function is in test mode.
#' @return A list containing the parameters 'nu', 'cost', and 'gamma'.
#' @importFrom stats seq
#' @examples
#'   params_config(kernel_type = "linear", is_test_mode = FALSE)
#' @export
params_config <-
  function(kernel_type = "linear",
           is_test_mode = FALSE) {
    # Parameter validation
    stopifnot(is.character(kernel_type), is.logical(is_test_mode))
    
    if (is_test_mode) {
      nu_values <- stats::seq(0.7, 0.8, 0.1)
    } else {
      nu_values <- stats::seq(0.1, 0.9, 0.1)
    }
    
    if (kernel_type == "linear") {
      cost_values <-
        if (is_test_mode)
          c(1178.1)
      else
        c(
          0.25,
          292.8,
          585.36,
          877.91,
          1170.46,
          1463.02,
          1755.57,
          2048.13,
          2340.68,
          2633.23,
          2925.79,
          3218.34,
          3510.89,
          3803.45,
          4096
        )
      gamma_values <- NULL
    } else {
      cost_values <-
        if (is_test_mode)
          c(0.25, 4096)
      else
        c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)
      sigma_values <- 2 ^ stats::seq(-4, if (is_test_mode)
        - 3
        else
          10, 2)
      gamma_values <- 1 / (2 * sigma_values ^ 2)
    }
    
    return(list(nu = nu_values, cost = cost_values, gamma = gamma_values))
  }


#' Progress Bar Configuration
#'
#' Configures a progress bar for a process with adjustable parameters.
#' @param kernel_type A string indicating the type of kernel. Valid options are "linear" and "radial".
#' @param is_test_mode A boolean indicating whether this is a test scenario.
#' @param lags_column Numeric vector indicating the lags in color processing.
#' @param lags_response Numeric vector indicating the lags in response time.
#' @return A configured progress bar object.
#' @importFrom progress progress_bar
#'
#' @examples
#' progressbar_config("linear", TRUE, c(1), c(2))
#'
#' @export
progressbar_config <-
  function(kernel_type = "linear",
           is_test_mode = FALSE,
           lags_column = NULL,
           lags_response = NULL) {
    # Validations
    stopifnot(is.character(kernel_type), is.logical(is_test_mode))
    
    # Calculate total steps
    total_steps <- ifelse(test, 2, 9 * 15)
    total_steps <-
      ifelse(kernel == "radial", total_steps * 4, total_steps)
    
    # Adjust total steps based on lags
    if (!is.null(color_lags)) {
      total_steps <- total_steps * prod(color_lags)
    }
    if (!is.null(response_time_lags)) {
      total_steps <- total_steps * prod(response_time_lags)
    }
    
    # Create a progress bar
    progress_bar <- progress::progress_bar$new(
      format = "Progress: [:bar] :percent | Step :current/:total | Elapsed: :elapsed | Remaining: :eta | Rate :rate ops/sec",
      total = total_steps,
      clear = FALSE,
      width = 100
    )
    
    return(progress_bar)
  }

#' Grid Search Main Loop
#'
#' This function executes the main loop of the grid search process.
#' It iterates over different combinations of cost and nu parameters.
#'
#' @param data_env Data environment containing datasets and parameters.
#' @param processed_data Processed data set for modeling.
#' @param data_partitions Data partitions for cross-validation.
#' @param bcv_folds Number of folds for cross-validation.
#' @param norm_response_var Normalized response variable.
#' @param lags_column Column lags.
#' @param initial_column_values Initial values for columns.
#' @param lags_response Response lags.
#' @param initial_prediction_value Initial value for prediction.
#' @param model_params Model parameters including kernel type.
#' @param nu Nu parameter for the model.
#' @param cost Cost parameter for the model.
#' @param training_list_name Name of the training list.
#' @param validation_list_name Name of the validation list.
#' @param is_silent_mode Boolean for silent mode.
#' @param should_plot_response Boolean to determine if response should be plotted.
#' @param progress_bar Progress bar configuration.
#' @return A list containing the results of each iteration of the grid search.
#' @examples
#' \dontrun{
#' # Assuming 'data_env', 'processed_data', and 'model_params' are predefined
#' results <- grid_search_main_loop(
#'   data_env = data_env,
#'   processed_data = processed_data,
#'   data_partitions = data_env$data_partitions, ...)
#' }
#' @export
grid_search_main_loop <- function(data_env,
                                  processed_data,
                                  data_partitions,
                                  bcv_folds,
                                  norm_response_var,
                                  lags_column = c(8),
                                  initial_column_values = c(1),
                                  lags_response = NULL,
                                  initial_prediction_value = 1,
                                  model_params,
                                  nu,
                                  cost,
                                  training_list_name,
                                  validation_list_name,
                                  is_silent_mode = TRUE,
                                  should_plot_response = FALSE,
                                  progress_bar) {
  # Validation
  stopifnot(is.list(data_env),
            is.numeric(bcv_folds),
            is.list(model_params))
  
  results <- list()
  for (c in cost) {
    for (n in nu) {
      gamma <-
        if (model_params$kernel == "radial")
          model_params$gamma
      else
        NULL
      
      grid_eval_params <- list(
        data_env = data_env,
        processed_data = processed_data,
        bcv_folds = bcv_folds,
        norm_response_var = norm_response_var,
        initial_column_values = initial_column_values,
        lags_response = lags_response,
        initial_prediction_value = initial_prediction_value,
        cost = c,
        nu = n,
        gamma = gamma,
        kernel = model_params$kernel,
        should_plot_response = should_plot_response,
        is_silent_mode = is_silent_mode
      )
      
      if (length(lags_column) == 1) {
        for (i in 1:lags_column[1]) {
          grid_eval_params$grid_col_lags <- c(i)
          results <-
            append(results,
                   grid_eval(grid_eval_params,
                             progress_bar = progress_bar))
        }
      } else {
        for (i in 1:lags_column[1]) {
          for (j in 0:lags_column[2]) {
            grid_eval_params$grid_col_lags <- c(i, j)
            results <-
              append(results,
                     grid_eval(grid_eval_params,
                               progress_bar = progress_bar))
          }
        }
      }
    }
  }
  
  return(results)
}


#' Evaluate a grid of parameters
#'
#' This function evaluates a grid of parameters for signal processing. It iterates over response lags,
#' displays a message (if not silent), and calls a function to evaluate the signal on the grid.
#' It supports a progress bar update after each iteration.
#'
#' @param params A list containing the parameters for the grid evaluation.
#' @param progress_bar A progress bar object for tracking progress. Optional.
#' @importFrom somePackage someFunction (if someFunction is used within the function)
#' @export
grid_eval <-
  function(params,
           progress_bar = NULL) {
    # Validate main parameters
    stopifnot(is.list(params))
    
    # Extract and remove response_lags from params
    lags_response <- params$lags_response
    params$lags_response <- NULL
    
    # Main processing loop
    if (!is.null(lags_response)) {
      for (lag_response in 1:lags_response[1]) {
        if (!params$is_silent_mode) {
          display_grid_message(
            params$cost,
            params$nu,
            params$gamma,
            c(params$grid_col_lags, lag_response)
          )
        }
        params$lag_response <- lag_response
        results <- do.call(grid_signal_eval, params)
        if (!is.null(progress_bar))
          progress_bar$tick()
      }
    } else {
      if (!params$is_silent_mode) {
        display_grid_message(params$cost,
                             params$nu,
                             params$gamma,
                             params$grid_col_lags)
      }
      results <- do.call(grid_signal_eval, params)
      if (!is.null(progress_bar))
        progress_bar$tick()
    }
    
    return(results)
  }


#' Display Grid Message
#'
#' This function displays a message on the console, showing the values of cost, nu, gamma, and lags.
#' It's used for logging purposes during grid evaluation.
#'
#' @param cost The cost parameter value.
#' @param nu The nu parameter value.
#' @param gamma The gamma parameter value.
#' @param lags A vector of lag values.
#' @export
display_grid_message <- function(cost, nu, gamma, lags) {
  # Basic parameter validations
  stopifnot(is.numeric(cost), is.numeric(nu))
  
  # Display message
  cat("Cost: ", cost,
      " Nu: ", nu,
      " Gamma: ", gamma,
      " Lags: ", lags,
      "\n")
}


#' Grid Signal Evaluation
#'
#' This function evaluates the quality of signals on a grid of parameters. It generates signal predictions,
#' evaluates signal quality, performs cross-validation, and returns results including advanced signal scores.
#'
#' @param data_env_list List of data environments for signal processing.
#' @param processed_data Preprocessed data used for generating predictions.
#' @param bcv_folds Number of folds for cross-validation.
#' @param norm_vsvr_response Normalized response variable.
#' @param initial_column_values Initial values for columns in the dataset.
#' @param grid_col_lags Column lags for the grid.
#' @param response_lag The lag of the response. Default is NULL.
#' @param prediction_initial_value Initial value for prediction. Default is 1.
#' @param cost Cost parameter for the model.
#' @param nu Nu parameter for the model.
#' @param gamma Gamma parameter for the model. Default is NULL.
#' @param kernel Kernel type for the model.
#' @param silent Boolean to control output verbosity. Default is TRUE.
#' @param plot_response Boolean to control plotting of response signals. Default is FALSE.
#' @return A list containing evaluation results.
#' @export
grid_signal_eval <-
  function(data_env_list,
           processed_data,
           bcv_folds,
           norm_vsvr_response,
           initial_column_values,
           grid_col_lags,
           response_lag = NULL,
           prediction_initial_value = 1,
           cost,
           nu,
           gamma = NULL,
           kernel,
           silent = TRUE,
           plot_response = FALSE) {
    # Simple parameter validations
    stopifnot(is.list(data_env_list), is.numeric(cost), is.numeric(nu))
    
    # Signal prediction generation
    response_predictions <-
      generate_signal_response_predictions_helper(
        data = processed_data,
        col_lags = c(grid_col_lags),
        response_lags = response_lag,
        initial_column_values = initial_column_values,
        prediction_initial_value = prediction_initial_value,
        cost = cost,
        nu = nu,
        gamma = gamma,
        data_list = data_env_list
      )
    
    # Signal quality evaluation
    signal_score <-
      evaluate_signal_quality(response_predictions[[norm_vsvr_response]], silent = silent)
    if (signal_score != "TEST PASSED")
      return(signal_score)
    
    # Cross-validation
    cv_result <- cross_validate_partition_helper(
      cost = cost,
      nu = nu,
      gamma = gamma,
      col_lags = c(grid_col_lags, response_lag),
      data_list = data_env_list,
      bcv_folds = bcv_folds
    )
    if (is.nan(cv_result$avg_cor) ||
        is.nan(cv_result$avg_error))
      return ("CV FAILED")
    
    # Result compilation
    results <- list()
    result_list <- list(
      avg_cor = cv_result$avg_cor,
      avg_error = cv_result$avg_error,
      na_count = cv_result$na_count,
      score = advanced_filter(response_predictions[[norm_vsvr_response]], 3L, silent = silent)$score,
      advanced_score_results = advanced_filter(response_predictions[[norm_vsvr_response]], 3L, silent = silent)$results,
      response_signal = response_predictions[[norm_vsvr_response]],
      params = list(
        cost = cost,
        nu = nu,
        gamma = gamma,
        col_lags = grid_col_lags,
        response_lag = response_lag
      )
    )
    results <- append(results, list(c(result_list)))
    
    # Plot response signal if required
    if (plot_response) {
      plot_vsvr_response_signal(response_predictions[[norm_vsvr_response]])
    }
    
    return(results)
  }
