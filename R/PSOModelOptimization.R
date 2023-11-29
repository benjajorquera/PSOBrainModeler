#' Particle Swarm Optimization (PSO) Training Model Function
#'
#' Conducts PSO training with a given set of parameters, data, and environmental
#' settings.
#' It includes functionality for progress tracking, data processing, response
#' prediction generation,
#' signal scoring, and optimization. This function is designed to handle various
#' aspects of PSO training,
#' including parameter tuning, cross-validation, and response evaluation.
#'
#' @param max_function_count Maximum number of function evaluations, defaults
#'  to 1000.
#' @param cost Cost parameter for the model.
#' @param nu Nu parameter for the model.
#' @param gamma Gamma parameter for the model, if applicable.
#' @param col_lags Column lag values for the model.
#' @param response_lags Response lag values for the model, if applicable.
#' @param vsvr_response VSVR response name in the data.
#' @param data_list List containing the data required for the model.
#' @param silent Flag to control verbosity during the training process.
#' @param plot_response Flag to enable or disable plotting the response.
#' @param initial_column_values Initial column values for the model.
#' @param prediction_initial_value Initial prediction value for the model.
#' @param bcv_folds Number of folds for blocked cross-validation.
#' @param pso_env PSO environment settings.
#' @param seed Seed for random number generation.
#' @param progress_bar Progress bar for visualization of the optimization
#'  process.
#' @param generate_response_predictions_cv Flag to generate response predictions.
#' @param basic_filter_check Flag to enable basic filtering of the data.
#' @param fn_count_threshold Threshold for function count in optimization.
#'
#' @return Depending on the stage and result of the optimization, it can return
#'  different values, including early termination or the current optimization
#'  score.
#'
#' @examples
#' \dontrun{
#' result <- pso_training_model(cost = 0.5, nu = 0.1, col_lags = c(3),
#'                             vsvr_response = "response",
#'                             data_list = my_data_list, pso_env = my_pso_env,
#'                             progress_bar = my_progress_bar)
#' }
#'
#' @export
#'
pso_training_model <- function(max_function_count = 1000,
                               cost,
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
                               seed = 123,
                               progress_bar,
                               generate_response_predictions_cv = FALSE,
                               basic_filter_check = TRUE,
                               fn_count_threshold = 30) {
  if (pso_env[["function_count"]] >= max_function_count)
    return(10)
  
  pso_env[["function_count"]] <- pso_env[["function_count"]] + 1
  
  progress_bar$tick()
  
  time <- Sys.time()
  
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
  basic_filter <-
    evaluate_signal_quality(response_predictions$predicted_values[[vsvr_response]],
                            silent = silent, max_diff_threshold = data_list$response_max_diff_threshold)
  
  if (basic_filter$result != "TEST PASSED" &&
      basic_filter_check) {
    new_data <- c(
      test_result = basic_filter$result,
      warnings = response_predictions$warnings,
      list(support_vectors = response_predictions$model$tot.nSV),
      list(signal_response = response_predictions$predicted_values[[vsvr_response]]),
      time = Sys.time() - time
    )
    pso_env[["data"]] <- c(pso_env[["data"]], list(new_data))
    return(5)
  }
  
  if (seed != 123) {
    set.seed(123)
  }
  
  # Cross-validation
  cv_results <- cross_validate_partition_helper(
    cost = cost,
    nu = nu,
    gamma = gamma,
    combined_col_lags = c(col_lags, response_lags),
    data_list = data_list,
    silent = silent,
    bcv_folds = bcv_folds,
    col_lags = col_lags,
    response_lags = response_lags,
    initial_column_values = initial_column_values,
    prediction_initial_value = prediction_initial_value,
    generate_response_predictions_cv = generate_response_predictions_cv
  )
  
  if (seed != 123) {
    set.seed(seed)
  }
  
  avg_cor <- cv_results$avg_cor
  avg_error <- cv_results$avg_error
  
  # Return early if any of the results is NaN
  if (is.nan(avg_cor) ||
      is.nan(avg_error)) {
    new_data <- c(
      test_result = "CV FAILED",
      prediction_warnings = response_predictions$warnings,
      cv_warnings = cv_results$warnings,
      list(support_vectors = response_predictions$model$tot.nSV),
      list(signal_response = response_predictions$predicted_values[[vsvr_response]]),
      time = Sys.time() - time
    )
    return (5)
  }
  
  advanced_filter <-
    advanced_filter(response_predictions$predicted_values[[vsvr_response]])
  
  pso_env[["max_cor"]] <- max(pso_env[["max_cor"]], avg_cor)
  
  fitness <-
    3 - avg_cor + avg_error - (advanced_filter$score * 0.01) - basic_filter$score
  pso_env[["best_fitness"]] <-
    min(pso_env[["best_fitness"]], fitness)
  
  new_data <- list(
    avg_cor = avg_cor,
    avg_error = avg_error,
    na_counts = cv_results$na_count,
    cv_warnings = cv_results$warnings,
    score = advanced_filter$score,
    advanced_score_results = advanced_filter$results,
    response_signal = response_predictions$predicted_values[[vsvr_response]],
    fitness_score = fitness,
    params = list(
      cost = cost,
      nu = nu,
      gamma = gamma,
      col_lags = col_lags,
      response_lags = response_lags
    ),
    response_predictions_warnings = response_predictions$warnings,
    support_vectors = response_predictions$model$tot.nSV,
    cv_predictions = cv_results$cv_predictions,
    time = Sys.time() - time
  )
  pso_env[["data"]] <- c(pso_env[["data"]], list(new_data))
  
  if (plot_response) {
    plot_vsvr_response_signal(response_predictions$predicted_values[[vsvr_response]])
  }
  
  if (!silent)
    display_pso_message(avg_cor,
                        avg_error,
                        basic_filter$score,
                        advanced_filter$score)
  
  if (round(avg_cor, digits = 4) <= round(pso_env[["max_global_cor"]], digits = 4)) {
    pso_env[["function_count_without_improvement"]] <-
      ifelse(pso_env[["function_count_without_improvement"]] > fn_count_threshold,
             return(5), pso_env[["function_count_without_improvement"]] + 1)
  } else {
    pso_env[["function_count_without_improvement"]] <- 0
    pso_env[["max_global_cor"]] <-
      max(pso_env[["max_global_cor"]], avg_cor)
  }
  
  return(fitness)
}