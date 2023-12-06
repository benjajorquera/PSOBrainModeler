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
#' @param basic_filter_check Logical flag to enable basic filtering of the data; default is TRUE.
#' @param fn_count_threshold Integer setting the function count threshold for optimization; default is 30.
#' @param fitness_accuracy Numeric value specifying fitness evaluation accuracy; default is 3.
#' @param penalization_weight Numeric value for the weight in optimization penalization; default is 0.5.
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
                               fn_count_threshold = 30,
                               fitness_accuracy = 3,
                               penalization_weight = 0.5) {
  # TODO: MODULARIZE
  if (pso_env[["function_count"]] >= max_function_count)
    return(5)
  
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
    evaluate_signal_quality(
      response_predictions$predicted_values[[vsvr_response]],
      silent = silent,
      max_diff_threshold = data_list$response_max_diff_threshold
    )
  advanced_filter <-
    advanced_filter(response_predictions$predicted_values[[vsvr_response]],
                    silent = silent)
  
  # Calculate normalized predicted time for fitness function
  prediction_time <- Sys.time() - time
  fitness_prediction_time <- as.numeric(prediction_time)
  pso_env[["max_pred_time"]] <-
    max(pso_env[["max_pred_time"]], fitness_prediction_time)
  fitness_norm_pred_time <-
    fitness_prediction_time / pso_env[["max_pred_time"]]
  
  fitness_cor <- response_predictions$predictions_all_data_cor
  fitness_err <- response_predictions$predictions_all_data_err
  
  new_data <- c(
    test_result = basic_filter$result,
    warnings = response_predictions$warnings,
    list(support_vectors = response_predictions$model$tot.nSV),
    list(signal_response = response_predictions$predicted_values[[vsvr_response]]),
    prediction_time = prediction_time,
    predictions_cor = fitness_cor,
    predictions_err = fitness_err
  )
  
  fitness_filter <- basic_filter$score
  fitness_adv_score <- advanced_filter$score * 0.1
  
  # Handle local solution
  if (is.null(fitness_cor) || fitness_cor == "FAILED")
    return(round(3 + fitness_norm_pred_time, fitness_accuracy))
  else{
    if (round(fitness_cor, fitness_accuracy) <= pso_env[["max_global_cor"]]) {
      if (pso_env[["function_count_without_improvement"]] > fn_count_threshold) {
        fitness_stats <- fitness_err - fitness_cor
        if (fitness_stats < 0) {
          fitness_stats <- fitness_stats * (1 - penalization_weight)
        }
        else {
          fitness_stats <- fitness_stats * (1 + penalization_weight)
        }
        partial_fitness_score <-
          round(
            fitness_stats - fitness_filter - fitness_adv_score + (fitness_norm_pred_time *
                                                                    (1 + penalization_weight)),
            fitness_accuracy
          )
        new_data <- c(new_data, c(fn_count_threshold = TRUE))
        new_data <-
          c(new_data,
            c(partial_fitness_score = partial_fitness_score))
        pso_env[["data"]] <- c(pso_env[["data"]], list(new_data))
        return(partial_fitness_score)
      }
      else {
        pso_env[["function_count_without_improvement"]] <-
          pso_env[["function_count_without_improvement"]] + 1
      }
    } else {
      pso_env[["function_count_without_improvement"]] <- 0
      pso_env[["max_global_cor"]] <-
        round(fitness_cor, fitness_accuracy)
    }
  }
  
  # Evaluate predicted signal
  if (basic_filter$result != "TEST PASSED" &&
      basic_filter_check) {
    fitness_partial_result <-
      round(fitness_err - fitness_cor + fitness_norm_pred_time,
            fitness_accuracy)
    
    new_data <-
      c(new_data,
        c(fitness_partial_result = fitness_partial_result))
    pso_env[["data"]] <- c(pso_env[["data"]], list(new_data))
    
    return(fitness_partial_result)
  }
  
  cv_time <- Sys.time()
  
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
  
  # Calculate normalized prediction and cross-validation time for fitness function
  cv_time <- Sys.time() - cv_time
  pred_cv_time <- prediction_time + cv_time
  fitness_pred_cv_time <- as.numeric(pred_cv_time)
  pso_env[["max_pred_cv_time"]] <-
    max(pso_env[["max_pred_cv_time"]], fitness_pred_cv_time)
  fitness_norm_pred_cv_time <-
    fitness_pred_cv_time / pso_env[["max_pred_cv_time"]]
  
  # Return early if any of the results is NaN
  if (is.nan(avg_cor) ||
      is.nan(avg_error)) {
    fitness_partial_score <-
      round(3 + fitness_norm_pred_cv_time, fitness_accuracy)
    new_data <- c(
      test_result = "CV FAILED",
      prediction_warnings = response_predictions$warnings,
      cv_warnings = cv_results$warnings,
      list(support_vectors = response_predictions$model$tot.nSV),
      list(signal_response = response_predictions$predicted_values[[vsvr_response]]),
      cv_time = cv_time,
      prediction_time = prediction_time,
      fitness_partial_score = fitness_partial_score
    )
    return (fitness_partial_score)
  }
  
  # Calculate normalized fitness function
  fitness <-
    avg_error - avg_cor - fitness_filter - fitness_adv_score + fitness_norm_pred_cv_time
  fitness <- round(fitness, fitness_accuracy)
  
  pso_env[["max_cor"]] <- max(pso_env[["max_cor"]], avg_cor)
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
    response_predictions_cor = response_predictions$predictions_all_data_cor,
    response_predictions_err = response_predictions$predictions_all_data_err,
    cv_predictions = cv_results$cv_predictions,
    prediction_time = prediction_time,
    cv_time = cv_time,
    overall_time = Sys.time() - time
  )
  pso_env[["data"]] <- c(pso_env[["data"]], list(new_data))
  
  if (plot_response) {
    plot_vsvr_response_signal(response_predictions$predicted_values[[vsvr_response]])
  }
  
  if (!silent)
    display_pso_message(
      round(avg_cor, fitness_accuracy),
      round(avg_error, fitness_accuracy),
      basic_filter$score,
      round(advanced_filter$score, fitness_accuracy)
    )
  
  pso_env[["candidates"]] <- pso_env[["candidates"]] + 1
  
  return(fitness)
}