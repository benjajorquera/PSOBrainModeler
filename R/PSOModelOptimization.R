#' Model training function using PSO optimization
#'
#' This function handles the training process for a model based on given parameters
#' using Particle Swarm Optimization (PSO). It performs cross-validation, response
#' predictions, and signal evaluation.
#'
#' @param cost A numeric value for the cost parameter.
#' @param nu A numeric value for the nu parameter.
#' @param gamma A numeric value for the gamma parameter (default: NULL).
#' @param col_lags Numeric vector indicating the lags for the columns.
#' @param response_lags Numeric vector indicating the response lags (default: NULL).
#' @param vsvr_response Name of the column representing the VSVR response. (default: NULL).
#'
#' @return A numeric value that represents the optimization result for minimization.
#'
#' @export
pso_training_model <- function(cost,
                               nu,
                               gamma = NULL,
                               col_lags,
                               response_lags = NULL,
                               vsvr_response = NULL) {
  # Cross-validation
  results <- cross_validate_partition_helper(
    cost = cost,
    nu = nu,
    gamma = gamma,
    col_lags = col_lags
  )
  
  avg_cor <- results$avg_cor
  avg_error <- results$avg_error
  
  # Return early if any of the results is NA
  if (is.na(avg_cor) || is.na(avg_error))
    return (10)
  
  # Training with all data
  data_training <-
    generate_time_series_data_helper(lag_values = col_lags,
                                     is_training = TRUE)
  
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
      gamma = gamma
    )
  
  # Process response signal and score it
  signal_score <-
    evaluate_signal_quality(response_predictions[[vsvr_response]])
  
  # Return early if signal score is less than or equal to zero
  if (signal_score <= 0)
    return(5)
  
  # Plot response signal
  plot(response_predictions[[vsvr_response]], type = "l")
  
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
  
  # Return optimization value for minimization
  return(2 - avg_cor + avg_error - (signal_score * 0.1))
}
