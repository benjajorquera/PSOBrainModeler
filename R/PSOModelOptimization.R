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
                               silent = FALSE,
                               plot_response = TRUE) {
  # Cross-validation
  results <- cross_validate_partition_helper(
    cost = cost,
    nu = nu,
    gamma = gamma,
    col_lags = c(col_lags, response_lags),
    data_list = data_list,
    silent = silent
  )
  
  avg_cor <- results$avg_cor
  avg_error <- results$avg_error
  
  # Return early if any of the results is NA
  if (is.na(avg_cor) || is.na(avg_error))
    return (10)
  
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
      initial_column_values = c(1),
      prediction_initial_value = 0.8,
      cost = cost,
      nu = nu,
      gamma = gamma,
      data_list = data_list
    )
  
  # Process response signal and score it
  signal_score <-
    evaluate_signal_quality(response_predictions[[vsvr_response]], silent = silent)
  
  # Return early if signal score is less than or equal to zero
  if (signal_score <= 0)
    return(5)
  
  # Plot response signal if plot_response is TRUE
  if (plot_response) {
    plot(
      response_predictions[[vsvr_response]],
      type = "l",
      main = "VSVR Response Signal",
      ylab = "Response",
      xlab = "Time/Instance"
    )
  }
  
  if (!silent) {
    # Print optimization values
    cat(
      "AVG COR: ",
      round(avg_cor, digits = 2),
      "AVG MSE: ",
      round(avg_error, digits = 2),
      "Signal score: ",
      round(signal_score, digits = 2),
      "\n"
    )
  }
  
  # Return optimization value for minimization
  return(2 - avg_cor + avg_error - (signal_score * 0.1))
}
