#' Perform a Grid Search for Support Vector Regression
#'
#' This function performs a grid search to find the optimal parameters for a
#' Support Vector Regression (SVR) model. It allows for various configurations
#' and settings, facilitating extensive testing and optimization of the SVR model.
#'
#' @param config List of configuration settings for the grid search.
#' @param dataset Dataframe containing the data to be used in the grid search.
#' @param kernel_type Character string specifying the type of kernel to be used
#'  in SVR.
#' @param is_multivariate Logical flag indicating if a multivariate analysis is
#'  to be performed.
#' @param signal_names Character vector naming the signal variables.
#' @param predictor_names Character vector naming the predictor variables.
#' @param response_var Character string naming the response variable for SVR.
#' @param exclude_columns Character vector specifying columns to be excluded
#'  from the analysis. Defaults to NULL.
#' @param lags_column Integer or vector specifying the number of lags for the
#'  columns. Defaults to c(8).
#' @param lags_response Integer or vector specifying the number of lags for the
#'  response. Defaults to NULL.
#' @param initial_column_value Numeric value or vector providing initial values
#'  for columns. Defaults to c(1).
#' @param initial_prediction_value Numeric value providing the initial value
#'  for prediction. Defaults to 1.
#' @param extra_column_name Character string specifying the name of an extra
#'  column to be included. Defaults to NULL.
#' @param should_plot_response Logical flag indicating if the response should
#'  be plotted. Defaults to FALSE.
#' @param is_test_mode Logical flag indicating if the function is in test mode.
#'  Defaults to FALSE.
#' @param is_silent_mode Logical flag indicating if the function should run in
#'  silent mode, minimizing output. Defaults to TRUE.
#' @param generate_response_predictions_cv Logical flag indicating if response
#'  predictions should be generated. Defaults to FALSE.
#' @param basic_filter_check Logical flag indicating if a basic filter check
#'  should be applied. Defaults to TRUE.
#'
#' @return A list containing the results of the grid search, potentially
#'  including the best model parameters, performance metrics, and any generated response predictions.
#'
#' @examples
#' \dontrun{
#'   svr_grid_search(my_config, my_data, "linear", ...)
#' }
#' @export
#'
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
                            initial_column_value = c(1),
                            initial_prediction_value = 1,
                            extra_column_name = NULL,
                            should_plot_response = FALSE,
                            is_test_mode = FALSE,
                            is_silent_mode = TRUE,
                            generate_response_predictions_cv = FALSE,
                            basic_filter_check = TRUE) {
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
    extra_col_name = extra_column_name,
    initial_prediction_values = initial_column_value
  )
  
  # Conduct main grid search
  return(
    main_grid_search(
      data_env = data_env,
      bcv_folds = config$bcv_folds,
      processed_data = data_env$processed_data,
      norm_response_var = data_env$NORM_VSVR_RESPONSE,
      kernel_type = kernel_type,
      lags_column = lags_column,
      initial_column_values = data_env$INITIAL_PREDICTION_VALUES,
      lags_response = lags_response,
      initial_prediction_value = initial_prediction_value,
      is_test_mode = is_test_mode,
      is_silent_mode = is_silent_mode,
      start_time = start_time,
      should_plot_response = should_plot_response,
      generate_response_predictions_cv = generate_response_predictions_cv,
      basic_filter_check = basic_filter_check
    )
  )
}

#' Main Grid Search Function
#'
#' This function performs a comprehensive grid search over specified parameters
#' for a given model, supporting both linear and non-linear kernels. It is
#' designed to optimize models by testing various combinations of parameters
#' and evaluating their performance.
#'
#' @param data_env Data environment object containing datasets, model
#'  parameters, and other settings for the grid search.
#' @param bcv_folds Integer specifying the number of folds for cross-validation,
#'  defaults to 5.
#' @param processed_data Dataframe of processed data set to be used in the
#'  model.
#' @param norm_response_var Normalized response variable used in the model.
#' @param kernel_type Character string indicating the type of kernel to use,
#'  default is "linear".
#' @param lags_column Integer or vector specifying the number of lags for the
#'  columns, defaults to c(8).
#' @param initial_column_values Numeric or vector providing initial values for
#'  columns, defaults to c(1).
#' @param lags_response Integer or vector specifying the number of lags for the
#'  response, defaults to NULL.
#' @param initial_prediction_value Numeric value indicating the initial value
#'  for prediction, defaults to 1.
#' @param is_test_mode Logical flag indicating if the function is running in
#'  test mode, defaults to FALSE.
#' @param is_silent_mode Logical flag indicating if the function should operate
#'  in silent mode, suppressing messages and output, defaults to TRUE.
#' @param start_time POSIXct time object indicating the start time of the
#'  function, defaults to NULL.
#' @param should_plot_response Logical flag indicating whether the response
#'  variable should be plotted, defaults to FALSE.
#' @param generate_response_predictions_cv Logical flag indicating if response
#'  predictions should be generated, defaults to FALSE.
#' @param basic_filter_check Logical flag indicating if basic filters should
#'  be applied, defaults to TRUE.
#'
#' @return A list containing the results of the grid search, time taken for the
#'  execution, and additional model parameters such as SVM tolerance and cache size.
#'
#' @examples
#' \dontrun{
#'   # Assuming 'data_env_list', 'processed_data', and 'norm_vsvr_response'
#'   # are predefined
#'   # Example with default parameters
#'   results <- main_grid_search(
#'     data_env_list = data_env_list,
#'     processed_data = processed_data,
#'     norm_vsvr_response = norm_vsvr_response
#'   )
#' }
#' @export
#'
main_grid_search <- function(data_env,
                             bcv_folds = 5,
                             processed_data,
                             norm_response_var,
                             kernel_type = "linear",
                             lags_column = c(8),
                             initial_column_values = c(1),
                             lags_response = NULL,
                             initial_prediction_value = 1,
                             is_test_mode = FALSE,
                             is_silent_mode = TRUE,
                             start_time = NULL,
                             should_plot_response = FALSE,
                             generate_response_predictions_cv = FALSE,
                             basic_filter_check = TRUE) {
  # Simple parameter validation
  stopifnot(is.list(data_env), is.list(data_env$data_partitions))
  
  # Handle start time
  if (is.null(start_time)) {
    start_time <- Sys.time()
  }
  
  model_params <- list(kernel = kernel_type)
  
  params <-
    params_config(kernel_type = kernel_type, is_test_mode = is_test_mode)
  
  progress_bar <- progressbar_config(
    lags_column = lags_column,
    lags_response = lags_response,
    cost_size = length(params$cost),
    nu_size = length(params$nu),
    gamma_size = length(params$gamma)
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
    progress_bar = progress_bar,
    generate_response_predictions_cv = generate_response_predictions_cv,
    basic_filter_check = basic_filter_check
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
  
  return(
    list(
      results = results,
      time = (Sys.time() - start_time),
      svm_tolerance = data_env$VSVR_TOL,
      svm_cache_size = data_env$svm_cache_size
    )
  )
}

#' Configure Parameters for SVR
#'
#' This function configures the parameters for Support Vector Regression (SVR)
#' based on the kernel type and test mode.
#'
#' @param kernel_type The type of kernel to use in SVR. Valid options are
#'  "linear" and others (e.g., "radial").
#' @param is_test_mode Logical flag indicating whether the function is in
#'  test mode.
#' @return A list containing the parameters 'nu', 'cost', and 'gamma'.
#' @examples
#'   params_config(kernel_type = "linear", is_test_mode = FALSE)
#' @export
#'
params_config <-
  function(kernel_type = "linear",
           is_test_mode = FALSE) {
    # Parameter validation
    stopifnot(is.character(kernel_type), is.logical(is_test_mode))
    
    if (is_test_mode) {
      nu_values <- seq(0.4, 0.7, 0.1)
      cost_values <- c(0.5, 4, 64, 128, 2048, 4096)
    } else {
      nu_values <- seq(0.1, 0.9, 0.1)
      cost_values <- 2 ^ seq(-2, 14, 2)
    }

    gamma_values <- NULL
    
    if (kernel_type == "radial") {
      sigma_values <- 2 ^ seq(-4, if (is_test_mode)
        2
        else
          12, 1)
      gamma_values <- 1 / (2 * sigma_values ^ 2)
    }
    
    return(list(nu = nu_values, cost = cost_values, gamma = gamma_values))
  }


#' Progress Bar Configuration
#'
#' Configures a progress bar for a process with adjustable parameters.
#' This function allows customization of the progress bar by specifying
#' lags in column processing, response times, and sizes for cost, nu, and gamma.
#'
#' @param lags_column Numeric vector indicating the lags in column processing.
#' @param lags_response Numeric vector indicating the lags in response time.
#' @param cost_size Numeric value for the cost size parameter.
#' @param nu_size Numeric value for the nu size parameter.
#' @param gamma_size Numeric value for the gamma size parameter.
#' @return A configured progress bar object.
#' @importFrom progress progress_bar
#'
#' @examples
#' progressbar_config(c(1), c(2), 0, 0, 0)
#'
#' @export
progressbar_config <-
  function(lags_column = NULL,
           lags_response = NULL,
           cost_size = 0,
           nu_size = 0,
           gamma_size = 0) {
    # Validations
    stopifnot(is.numeric(lags_column))
    
    total_steps <- cost_size * nu_size
    total_steps <-
      ifelse(gamma_size != 0, total_steps * gamma_size, total_steps)
    
    # Adjust total steps based on lags
    if (!is.null(lags_column)) {
      total_steps <- total_steps * lags_column[1]
      if (!is.na(lags_column[2])) {
        total_steps <- total_steps * (lags_column[2] + 1)
      }
    }
    if (!is.null(lags_response)) {
      total_steps <- total_steps * prod(lags_response)
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
#' This function executes the main loop of the grid search process for Support
#' Vector Regression.
#' It iterates over different combinations of cost and nu parameters, evaluating
#' the model's performance for each combination.
#'
#' @param data_env Data environment object containing datasets and parameters
#'  for the grid search.
#' @param processed_data Dataframe of processed data set to be used in the model.
#' @param data_partitions Data partitions object for cross-validation.
#' @param bcv_folds Integer specifying the number of folds for cross-validation,
#'  defaults to 5.
#' @param norm_response_var Normalized response variable used in the model.
#' @param lags_column Integer or vector specifying the number of lags for the
#'  columns, defaults to c(8).
#' @param initial_column_values Numeric or vector providing initial values for
#'  columns, defaults to c(1).
#' @param lags_response Integer or vector specifying the number of lags for the
#'  response, defaults to NULL.
#' @param initial_prediction_value Numeric value indicating the initial value
#'  for prediction, defaults to 1.
#' @param model_params List of model parameters including kernel type.
#' @param nu Numeric value representing the nu parameter for the SVR model.
#' @param cost Numeric value representing the cost parameter for the SVR model.
#' @param training_list_name Character string naming the list of training data,
#'  defaults to 'training'.
#' @param validation_list_name Character string naming the list of validation
#'  data, defaults to 'validation'.
#' @param is_silent_mode Logical flag indicating if the function should operate
#'  in silent mode, suppressing messages and output, defaults to TRUE.
#' @param should_plot_response Logical flag indicating whether the response
#'  variable should be plotted, defaults to FALSE.
#' @param progress_bar Progress bar object configured for the grid search
#'  process.
#' @param generate_response_predictions_cv Logical flag indicating if response
#'  predictions should be generated, defaults to FALSE.
#' @param basic_filter_check Logical flag indicating if basic filters should
#'  be applied, defaults to TRUE.
#'
#' @return A list containing the results of each iteration of the grid search,
#'  including model performance metrics and potentially the generated response
#'  predictions.
#'
#' @examples
#' \dontrun{
#' # Assuming 'data_env', 'processed_data', and 'model_params' are predefined
#' results <- grid_search_main_loop(
#'   data_env = data_env,
#'   processed_data = processed_data,
#'   data_partitions = data_env$data_partitions, ...)
#' }
#' @export
#'
grid_search_main_loop <- function(data_env,
                                  processed_data,
                                  data_partitions,
                                  bcv_folds = 5,
                                  norm_response_var,
                                  lags_column = c(8),
                                  initial_column_values = c(1),
                                  lags_response = NULL,
                                  initial_prediction_value = 1,
                                  model_params,
                                  nu,
                                  cost,
                                  training_list_name = 'training',
                                  validation_list_name = 'validation',
                                  is_silent_mode = TRUE,
                                  should_plot_response = FALSE,
                                  progress_bar,
                                  generate_response_predictions_cv = FALSE,
                                  basic_filter_check = TRUE) {
  # Validation
  stopifnot(is.list(data_env),
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
        lag_response = lags_response,
        initial_prediction_value = initial_prediction_value,
        cost = c,
        nu = n,
        gamma = gamma,
        should_plot_response = should_plot_response,
        is_silent_mode = is_silent_mode,
        generate_response_predictions_cv = generate_response_predictions_cv,
        basic_filter_check = basic_filter_check
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
#' This function evaluates a grid of parameters for signal processing. It
#' iterates over response lags,
#' displays a message (if not silent), and calls a function to evaluate the
#' signal on the grid.
#' It supports a progress bar update after each iteration.
#'
#' @param params A list containing the parameters for the grid evaluation.
#' @param progress_bar A progress bar object for tracking progress. Optional.
#' @examples
#' \dontrun{
#'   grid_eval(params, progress_bar)
#' }
#'
#' @export
#'
grid_eval <-
  function(params,
           progress_bar = NULL) {
    # Validate main parameters
    stopifnot(is.list(params))
    
    # Extract and remove response_lags from params
    lags_response <- params$lag_response
    params$lag_response <- NULL
    
    results <- list()
    
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
        results <-
          append(results, do.call(grid_signal_eval, params))
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
#' This function displays a message on the console, showing the values of cost,
#' nu, gamma, and lags.
#' It's used for logging purposes during grid evaluation.
#'
#' @param cost The cost parameter value.
#' @param nu The nu parameter value.
#' @param gamma The gamma parameter value.
#' @param lags A vector of lag values.
#' @examples
#' display_grid_message(1, 1, 1, 1)
#'
#' @export
display_grid_message <- function(cost, nu, gamma, lags) {
  # Basic parameter validations
  stopifnot(is.numeric(cost), is.numeric(nu))
  
  # Display message
  cat("\nCost: ", cost,
      " Nu: ", nu,
      " Gamma: ", gamma,
      " Lags: ", lags,
      "\n")
}


#' Grid Signal Evaluation
#'
#' This function evaluates the quality of signals on a grid of parameters.
#' It generates signal predictions,
#' evaluates signal quality, performs cross-validation, and returns results
#' including advanced signal scores.
#'
#' @param data_env Data environment containing datasets and parameters for
#'  signal processing.
#' @param processed_data Preprocessed data used for generating predictions.
#' @param bcv_folds Number of folds for cross-validation, defaults to 5.
#' @param norm_response_var Normalized response variable used in the evaluation.
#' @param initial_column_values Numeric or vector providing initial values for
#'  columns in the dataset, defaults to c(1).
#' @param grid_col_lags Integer or vector specifying column lags for the grid.
#' @param lag_response Integer or vector specifying the lag of the response,
#'  defaults to NULL.
#' @param initial_prediction_value Numeric value indicating the initial value
#'  for prediction, defaults to 1.
#' @param cost Numeric value representing the cost parameter for the SVR model.
#' @param nu Numeric value representing the nu parameter for the SVR model.
#' @param gamma Numeric value (optional) representing the gamma parameter for
#'  the SVR model, defaults to NULL.
#' @param is_silent_mode Logical flag indicating if the function should operate
#'  in silent mode, suppressing messages and output, defaults to TRUE.
#' @param should_plot_response Logical flag indicating whether the response
#'  variable should be plotted, defaults to FALSE.
#' @param generate_response_predictions_cv Logical flag indicating if response
#'  predictions should be generated, defaults to FALSE.
#' @param basic_filter_check Logical flag indicating if basic filters should
#'  be applied, defaults to TRUE.
#'
#' @return A list containing the evaluation results, including signal quality
#'  metrics and potentially generated response predictions.
#'
#' @examples
#' \dontrun{
#'   grid_signal_eval(data_env, processed_data, ...)
#' }
#' @export
#'
grid_signal_eval <-
  function(data_env,
           processed_data,
           bcv_folds = 5,
           norm_response_var,
           initial_column_values = c(1),
           grid_col_lags,
           lag_response = NULL,
           initial_prediction_value = 1,
           cost,
           nu,
           gamma = NULL,
           is_silent_mode = TRUE,
           should_plot_response = FALSE,
           generate_response_predictions_cv = FALSE,
           basic_filter_check = TRUE) {
    # Simple parameter validations
    stopifnot(is.list(data_env), is.numeric(cost), is.numeric(nu))
    
    response_signal_time <- Sys.time()
    overall_time <- Sys.time()
    
    # Signal prediction generation
    response_predictions <-
      generate_signal_response_predictions_helper(
        data = processed_data,
        col_lags = c(grid_col_lags),
        response_lags = lag_response,
        initial_column_values = initial_column_values,
        prediction_initial_value = initial_prediction_value,
        cost = cost,
        nu = nu,
        gamma = gamma,
        data_list = data_env
      )
    
    response_signal_time <- Sys.time() - response_signal_time
    
    results <- list()
    
    # Signal quality evaluation
    signal_score <-
      evaluate_signal_quality(
        response_predictions$predicted_values[[norm_response_var]],
        silent = is_silent_mode,
        max_diff_threshold = data_env$response_max_diff_threshold
      )
    
    if (signal_score$result != "TEST PASSED" &&
        basic_filter_check) {
      return(append(results,
                    list(c(
                      list(
                        test_result = signal_score$result,
                        warnings = response_predictions$warnings,
                        support_vectors = response_predictions$model$tot.nSV,
                        signal_response = response_predictions$predicted_values[[norm_response_var]],
                        response_signal_time = response_signal_time
                      )
                    ))))
    }
    
    cv_time <- Sys.time()
    
    # Cross-validation
    cv_result <- cross_validate_partition_helper(
      cost = cost,
      nu = nu,
      gamma = gamma,
      combined_col_lags = c(grid_col_lags, lag_response),
      data_list = data_env,
      bcv_folds = bcv_folds,
      col_lags = grid_col_lags,
      response_lags = lag_response,
      initial_column_values = initial_column_values,
      prediction_initial_value = initial_prediction_value,
      generate_response_predictions_cv = generate_response_predictions_cv,
      silent = is_silent_mode
    )
    
    cv_time <- Sys.time() - cv_time
    
    if (is.nan(cv_result$avg_cor) ||
        is.nan(cv_result$avg_error)) {
      return(append(results,
                    list(c(
                      list(
                        test_result = "CV FAILED",
                        prediction_warnings = response_predictions$warnings,
                        cv_warnings = cv_result$warnings,
                        support_vectors = response_predictions$model$tot.nSV,
                        signal_response = response_predictions$predicted_values[[norm_response_var]],
                        cv_time = cv_time,
                        response_signal_time = response_signal_time
                      )
                    ))))
    }
    
    overall_time <- Sys.time() - overall_time
    
    # Result compilation
    result_list <- list(
      avg_cor = cv_result$avg_cor,
      avg_error = cv_result$avg_error,
      na_count = cv_result$na_count,
      cv_warnings = cv_result$warnings,
      score = advanced_filter(response_predictions$predicted_values[[norm_response_var]], 3L, silent = is_silent_mode)$score,
      advanced_score_results = advanced_filter(response_predictions$predicted_values[[norm_response_var]], 3L, silent = is_silent_mode)$results,
      response_signal = response_predictions$predicted_values[[norm_response_var]],
      params = list(
        cost = cost,
        nu = nu,
        gamma = gamma,
        col_lags = grid_col_lags,
        response_lag = lag_response
      ),
      response_predictions_warnings = response_predictions$warnings,
      support_vectors = response_predictions$model$tot.nSV,
      cv_predictions = cv_result$cv_predictions,
      response_signal_time = response_signal_time,
      cv_time = cv_time,
      overall_time = overall_time
    )
    results <- append(results, list(c(result_list)))
    
    # Plot response signal if required
    if (should_plot_response) {
      plot_vsvr_response_signal(response_predictions$predicted_values[[norm_response_var]])
    }
    
    return(results)
  }
