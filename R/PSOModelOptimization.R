#' Model training function using PSO optimization
#'
#' This function handles the training process for a model based on given parameters
#' using Particle Swarm Optimization (PSO). It performs cross-validation, response
#' predictions, and signal evaluation. The function returns an optimization value for
#' minimization, calculated based on the average correlation, average error, and signal score.
#'
#' @param cost A numeric value for the cost parameter.
#' @param nu A numeric value for the nu parameter.
#' @param gamma (Optional) A numeric value for the gamma parameter. Defaults to NULL.
#' @param col_lags Numeric vector indicating the lags for the columns.
#' @param response_lags (Optional) Numeric vector indicating the response lags. Defaults to NULL.
#' @param vsvr_response Name of the column representing the VSVR response.
#' @param data_list A list containing various data configurations.
#' @param silent (Optional) A logical for run the function silently (without printouts). Defaults to FALSE.
#' @param plot_response (Optional) A logical to decide whether to plot the response signal. Defaults to TRUE.
#'
#' @details
#' The function conducts the following steps:
#' 1. Cross-validation using the provided parameters.
#' 2. Training with all the provided data.
#' 3. Generate response predictions based on the training.
#' 4. Evaluate the quality of the signal response.
#'
#' This function depends on the following internal package functions:
#' - \code{\link{cross_validate_partition_helper}}: Helper function for cross-validation of partitions
#' - \code{\link{generate_time_series_data_helper}}: Helper function to generate time series data
#' - \code{\link{generate_signal_response_predictions_helper}}: Helper function to generate signal response predictions
#' - \code{\link{evaluate_signal_quality}}: Evaluate Signal Quality
#'
#' @return A numeric value that represents the optimization result for minimization.
#'
#' @export
pso_training_model <- function(cost,
                               nu,
                               gamma = NULL,
                               col_lags,
                               response_lags = NULL,
                               vsvr_response,
                               data_list,
                               silent = TRUE,
                               plot_response = FALSE,
                               initial_column_values = c(1),
                               prediction_initial_value = 1,
                               bcv_folds = 5,
                               pso_env,
                               seed = 123) {
  pso_env[["function_count"]] <- pso_env[["function_count"]] + 1
  
  # print(pso_env[["function_count"]])
  # print(pso_env[["function_count_without_improvement"]])
  
  if (pso_env[["function_count"]] > 1000)
    return(10)
  
  # Training with all data
  data_training <-
    generate_time_series_data_helper(
      lag_values = c(col_lags, response_lags),
      is_training = TRUE,
      data_list = data_list
    )
  
  # Generate response predictions
  response_predictions <-
    generate_signal_response_predictions_helper(
      data = data_training,
      col_lags = col_lags,
      response_lags = response_lags,
      initial_column_values = initial_column_values,
      prediction_initial_value = prediction_initial_value,
      cost = cost,
      nu = nu,
      gamma = gamma,
      data_list = data_list
    )
  
  # Process response signal and score it
  signal_score <-
    evaluate_signal_quality(response_predictions[[vsvr_response]], silent = silent)
  
  if (signal_score != 1) {
    return(3)
  }
  
  if (seed != 123) {
    set.seed(123)
  }
  
  # Cross-validation
  results <- cross_validate_partition_helper(
    cost = cost,
    nu = nu,
    gamma = gamma,
    col_lags = c(col_lags, response_lags),
    data_list = data_list,
    silent = silent,
    bcv_folds = bcv_folds
  )
  
  if (seed != 123) {
    set.seed(seed)
  }
  
  avg_cor <- results$avg_cor
  avg_error <- results$avg_error
  
  # Return early if any of the results is NaN
  if (is.nan(avg_cor) ||
      is.nan(avg_error))
    return (3)
  
  if (signal_score == 1) {
    optim_score <-
      advanced_filter(response_predictions[[vsvr_response]])
    
    if (avg_cor > pso_env[["max_cor"]])
      pso_env[["max_cor"]] <- avg_cor
    
    fitness <-
      (3 - avg_cor + avg_error - (optim_score * 0.01) - signal_score)
    if (fitness < pso_env[["best_fitness"]])
      pso_env[["best_fitness"]] <- fitness
    
    new_data <-
      c(
        avg_cor = avg_cor,
        avg_error = avg_error,
        na_counts = results$na_count,
        score = optim_score,
        response_signal = list(response_predictions[[vsvr_response]]),
        fitness_score = fitness,
        params = list(
          c(
            cost = cost,
            nu =  nu,
            gamma = gamma,
            col_lags = col_lags,
            response_lags = response_lags
          )
        )
      )
    pso_env[["data"]] <- c(pso_env[["data"]], list(new_data))
    
    if (plot_response) {
      plot_response_signal(response_predictions[[vsvr_response]])
    }
  }
  else {
    optim_score <- 0
  }
  
  if (!silent) {
    # Print optimization values
    cat(
      "AVG COR: ",
      avg_cor,
      "AVG MSE: ",
      avg_error,
      "Signal basic filter: ",
      signal_score,
      "Signal advance score: ",
      optim_score,
      "\n"
    )
  }
  
  #print(round(avg_cor, digits = 3))
  #print(round(pso_env[["max_global_cor"]], digits = 3))
  
  if (round(avg_cor, digits = 4) <= round(pso_env[["max_global_cor"]], digits = 4)) {
    if (pso_env[["function_count_without_improvement"]] > 50) {
      return(5)
    }
    pso_env[["function_count_without_improvement"]] <-
      pso_env[["function_count_without_improvement"]] + 1
  }
  else {
    pso_env[["function_count_without_improvement"]] <- 0
  }
  
  if (avg_cor > pso_env[["max_global_cor"]])
    pso_env[["max_global_cor"]] <- avg_cor
  
  # Return optimization value for minimization
  return(3 - avg_cor + avg_error - (optim_score * 0.01) - signal_score)
}
