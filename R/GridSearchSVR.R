svr_grid_search <- function(config,
                            data,
                            kernel,
                            multi = FALSE,
                            signal_names,
                            predictors_names,
                            vsvr_response,
                            excluded_cols = NULL,
                            col_lags = c(8),
                            initial_pressure_value = c(1),
                            response_lags = NULL,
                            extra_col_name = NULL,
                            silent = FALSE,
                            plot_response = TRUE,
                            test = FALSE) {
  data_env_list <- configure_data_env(
    config = config,
    data = data,
    signal_names = signal_names,
    predictors_names = predictors_names,
    vsvr_response = vsvr_response,
    excluded_cols = excluded_cols,
    multi = multi,
    extra_col_name = extra_col_name
  )
  
  data_partitions <- data_env_list$data_partitions
  processed_data <- data_env_list$processed_data
  
  return(
    main_grid_search(
      data_env_list = data_env_list,
      data_partitions = data_partitions,
      bcv_folds = config$bcv_folds,
      processed_data = processed_data,
      norm_vsvr_response = data_env_list$NORM_VSVR_RESPONSE,
      kernel = kernel,
      col_lags = col_lags,
      initial_column_values = data_env_list$INITIAL_PREDICTION_VALUES,
      response_lags = response_lags,
      tolerance = config$vsvr_tolerance,
      test = test
    )
  )
}


main_grid_search <-
  function(data_env_list,
           data_partitions,
           bcv_folds = 5,
           processed_data,
           norm_vsvr_response,
           kernel = "linear",
           col_lags = c(8),
           initial_column_values = c(1),
           response_lags = NULL,
           tolerance = 1,
           test = FALSE,
           validation_list_name = 'validation',
           training_list_name = 'training') {
    if (test) {
      nu <- seq(0.7, 0.8, 0.1)
    }
    else {
      nu <- seq(0.1, 0.9, 0.1)
    }
    
    model_params <- list(type = "nu-regression",
                         tolerance = tolerance,
                         kernel = kernel)
    
    main_loop_params <- list(
      data_env_list = data_env_list,
      processed_data = processed_data,
      data_partitions = data_partitions,
      bcv_folds = bcv_folds,
      norm_vsvr_response = norm_vsvr_response,
      col_lags = col_lags,
      initial_column_values = initial_column_values,
      response_lags = response_lags,
      model_params = model_params,
      nu = nu,
      training_list_name = training_list_name,
      validation_list_name = validation_list_name
    )
    
    results <- list()
    
    if (kernel == "linear") {
      if (test) {
        cost <- c(1178.1)
      }
      else {
        cost <- c(
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
      }
      
      main_loop_params$cost <- cost
      results <-
        append(results, c(do.call(
          grid_search_main_loop, main_loop_params
        )))
    }
    else {
      if (test) {
        cost <- c(0.25, 4096)
        sigma <- 2 ^ seq(-4, -3, 1)
        gamma <- 1 / (2 * sigma ^ 2)
      }
      else {
        cost <-
          c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)
        sigma <- 2 ^ seq(-4, 10, 2)
        gamma <- 1 / (2 * sigma ^ 2)
      }
      
      main_loop_params$cost <- cost
      
      for (g in gamma) {
        main_loop_params$model_params$gamma <- g
        results <-
          append(results, c(do.call(
            grid_search_main_loop, main_loop_params
          )))
      }
    }
    
    return(results)
    
  }

grid_search_main_loop <-
  function(data_env_list,
           data_partitions,
           bcv_folds,
           processed_data,
           norm_vsvr_response,
           col_lags = c(8),
           initial_column_values = c(1),
           response_lags = NULL,
           cost,
           nu,
           model_params,
           training_list_name,
           validation_list_name) {
    results <- list()
    for (c in cost) {
      for (n in nu) {
        gamma <- NULL
        
        if (model_params$kernel == "radial")
          gamma <- model_params$gamma
        
        grid_eval_params <- list(
          data_env_list = data_env_list,
          processed_data = processed_data,
          bcv_folds = bcv_folds,
          norm_vsvr_response = norm_vsvr_response,
          initial_column_values = initial_column_values,
          response_lags = response_lags,
          cost = c,
          nu = n,
          gamma = gamma,
          kernel = kernel
        )
        
        if (length(col_lags) == 1) {
          for (i in 1:col_lags[1]) {
            grid_col_lags <- c(i)
            grid_eval_params$grid_col_lags <- c(i)
            results <- append(results, grid_eval(grid_eval_params))
          }
        } else {
          for (i in 1:col_lags[1]) {
            for (j in 0:col_lags[2]) {
              grid_eval_params$grid_col_lags <- c(i, j)
              results <-
                append(results, grid_eval(grid_eval_params))
            }
          }
        }
      }
    }
    
    return(results)
  }

grid_eval <- function(params) {
  response_lags <- params$response_lags
  params$response_lags <- NULL
  if (!is.null(response_lags)) {
    for (response_lag in 1:response_lags[1]) {
      display_grid_message(params$cost,
                           params$nu,
                           params$gamma,
                           c(params$grid_col_lags, response_lag))
      params$response_lag = response_lag
      results <- do.call(grid_signal_eval, params)
    }
  }
  else {
    display_grid_message(params$cost,
                         params$nu,
                         params$gamma,
                         params$grid_col_lags)
    results <- do.call(grid_signal_eval, params)
  }
  
  return(results)
}

display_grid_message <- function(cost, nu, gamma, lags) {
  cat("Cost: ",
      cost,
      " Nu: ",
      nu,
      " Gamma: ",
      gamma,
      " Lags: ",
      lags,
      "\n")
}

grid_signal_eval <-
  function(data_env_list,
           processed_data,
           bcv_folds,
           norm_vsvr_response,
           initial_column_values,
           grid_col_lags,
           response_lag = NULL,
           cost,
           nu,
           gamma = NULL,
           kernel) {
    response_predictions <-
      generate_signal_response_predictions_helper(
        data = processed_data,
        col_lags = c(grid_col_lags),
        response_lags = response_lag,
        initial_column_values = initial_column_values,
        prediction_initial_value = 1,
        cost = cost,
        nu = nu,
        gamma = gamma,
        data_list = data_env_list
      )
    
    signal_score <-
      evaluate_signal_quality(response_predictions[[norm_vsvr_response]])
    
    if (signal_score != 1)
      return(NULL)
    
    results <- list()
    
    advanced_signal_score <-
      advanced_filter(response_predictions[[norm_vsvr_response]], 3L)
    
    cv_result <- cross_validate_partition_helper(
      cost = cost,
      nu = nu,
      gamma = gamma,
      col_lags = c(grid_col_lags, response_lag),
      data_list = data_env_list,
      bcv_folds = bcv_folds
    )
    
    result_list <- list(avg_cor = cv_result$avg_cor,
                        avg_error = cv_result$avg_error,
                        na_count = cv_result$na_count)
    
    result_list$score <- advanced_signal_score
    
    result_list$response_signal <-
      response_predictions[[norm_vsvr_response]]
    
    result_list$params <-
      list(cost = cost,
           nu = nu,
           gamma = gamma,
           col_lags = grid_col_lags,
           response_lag = response_lag)
    
    results <- append(results, list(c(result_list)))
    
    return(results)
  }