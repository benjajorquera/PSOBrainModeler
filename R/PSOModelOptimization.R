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
#' @param basic_filter_check Logical flag to enable basic filtering of the data.
#' @param fn_count_threshold Integer setting the function count threshold for optimization.
#' @param fitness_accuracy Numeric value specifying fitness evaluation accuracy.
#' @param show_progress_bar Disables the progress bar display.
#' @param minimum_candidates Sets the lower limit for candidate consideration.
#' @param fn_start_threshold Determines the function's starting point if there are no candidates.
#' @param cv_folds_ratio Specifies the proportion of data used for cross-validation.
#' @param time_on_fitness Apply time to the objective function.
#' @param penalization_weight Numeric value for the weight in optimization penalization.
#'
#' @return Depending on the stage and result of the optimization, it can return
#'  different values, including early termination or the current optimization
#'  score.
#'
#' @examples
#' \dontrun{
#' result <- pso_training_model(cost = 0.5, nu = 0.1, col_lags = c(3),
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
                               data_list,
                               silent = TRUE,
                               plot_response = FALSE,
                               initial_column_values = c(1),
                               prediction_initial_value = 1,
                               bcv_folds = 5,
                               pso_env,
                               seed = 123,
                               progress_bar = NULL,
                               generate_response_predictions_cv = TRUE,
                               basic_filter_check = TRUE,
                               fn_count_threshold = 30,
                               fitness_accuracy = 3,
                               show_progress_bar = FALSE,
                               minimum_candidates = 10,
                               fn_start_threshold = 100,
                               cv_folds_ratio = 0.2,
                               time_on_fitness = FALSE,
                               penalization_weight = 0.5) {
  # TODO: MODULARIZE
  
  if (pso_env[["function_count"]] >= max_function_count &&
      pso_env[["candidates"]] > minimum_candidates) {
    return(5)
  }
  
  if (pso_env[["function_count"]] > fn_start_threshold &&
      pso_env[["candidates"]] < minimum_candidates) {
    basic_filter_check <- FALSE
  }
  
  pso_env[["function_count"]] <- pso_env[["function_count"]] + 1
  
  if (show_progress_bar && !is.null(progress_bar)) {
    progress_bar$tick()
  }
  
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
  
  # Process response signal
  basic_filter <-
    evaluate_signal_quality(response_predictions$predicted_values,
                            silent = silent)
  
  prediction_time <- Sys.time() - time
  
  new_data <- c(
    test_result = basic_filter$result,
    response_predictions_results = list(response_predictions),
    prediction_time = list(prediction_time),
    basic_filter_check = basic_filter_check
  )
  
  fitness_prediction_time <- as.numeric(prediction_time)
  pso_env[["max_pred_time"]] <-
    max(pso_env[["max_pred_time"]], fitness_prediction_time)
  fitness_norm_pred_time <-
    fitness_prediction_time / pso_env[["max_pred_time"]]
  
  # Evaluate predicted signal
  if (basic_filter$result != "TEST PASSED" && basic_filter_check) {
    fitness_partial_result <- 3
    if (time_on_fitness) {
      fitness_partial_result <-
        fitness_partial_result + (fitness_norm_pred_time * (1  + penalization_weight))
    }
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
  
  fitness_avg_cor <- cv_results$avg_cor
  fitness_avg_error <- cv_results$avg_error
  fitness_filter <- basic_filter$score
  
  advanced_filter <-
    advanced_filter(response_predictions$predicted_values,
                    silent = silent)
  
  if (fitness_filter == 1) {
    fitness_adv_score <- advanced_filter$score * 0.1
  }
  else {
    fitness_adv_score <- 0
  }
  
  # Calculate normalized prediction and cross-validation time for fitness function
  cv_time <- Sys.time() - cv_time
  pred_cv_time <- prediction_time + cv_time
  fitness_pred_cv_time <- as.numeric(pred_cv_time)
  pso_env[["max_pred_cv_time"]] <-
    max(pso_env[["max_pred_cv_time"]], fitness_pred_cv_time)
  fitness_norm_pred_cv_time <-
    fitness_pred_cv_time / pso_env[["max_pred_cv_time"]]
  
  # Return early if any of the results is NaN
  if (is.nan(fitness_avg_cor) ||
      is.nan(fitness_avg_error)) {
    new_data <- c(
      test_result = "CV FAILED",
      response_predictions_results = list(response_predictions),
      cv_results = list(cv_results),
      cv_time = list(cv_time),
      prediction_time = list(prediction_time),
      basic_filter_check = basic_filter_check
    )
    fitness_partial_result <- 5
    if (time_on_fitness) {
      fitness_partial_result <-
        fitness_partial_result + fitness_norm_pred_cv_time
    }
    new_data <-
      c(new_data,
        c(fitness_partial_result = fitness_partial_result))
    pso_env[["data"]] <- c(pso_env[["data"]], list(new_data))
    return(fitness_partial_result)
    
  }
  
  # Handle local solution
  if (round(fitness_avg_cor, fitness_accuracy) <= pso_env[["max_global_cor"]]) {
    if (pso_env[["function_count_without_improvement"]] > fn_count_threshold) {
      if (!(pso_env[["candidates"]] < minimum_candidates &&
            fitness_filter == 1)) {
        new_data <- c(new_data, c(fn_count_threshold = TRUE))
        fitness_partial_result <- 3
        if (time_on_fitness) {
          fitness_partial_result <-
            fitness_partial_result + fitness_norm_pred_cv_time
        }
        new_data <-
          c(new_data,
            c(fitness_partial_result = fitness_partial_result))
        pso_env[["data"]] <- c(pso_env[["data"]], list(new_data))
        return(fitness_partial_result)
      }
      else {
        pso_env[["function_count_without_improvement"]] <- 0
      }
    }
    else {
      pso_env[["function_count_without_improvement"]] <-
        pso_env[["function_count_without_improvement"]] + 1
    }
  } else {
    pso_env[["function_count_without_improvement"]] <- 0
    pso_env[["max_global_cor"]] <-
      round(fitness_avg_cor, fitness_accuracy)
  }
  
  
  fitness_cv_filter <-
    (cv_folds_ratio * cv_results$cv_basic_filter_check)
  
  # Calculate normalized fitness function
  fitness <- (
    fitness_avg_error - fitness_avg_cor  - fitness_filter - fitness_adv_score - fitness_cv_filter + fitness_norm_pred_cv_time
  )
  
  fitness <- round(fitness, fitness_accuracy)
  
  pso_env[["best_fitness"]] <-
    min(pso_env[["best_fitness"]], fitness)
  
  new_data <- c(
    advanced_score_results = list(advanced_filter),
    response_predictions_results = list(response_predictions),
    fitness_score = fitness,
    basic_filter = list(basic_filter),
    params = list(
      c(
        cost = cost,
        nu = nu,
        gamma = gamma,
        col_lags = col_lags,
        response_lags = response_lags
      )
    ),
    cv_results = list(cv_results),
    prediction_time = list(prediction_time),
    cv_time = list(cv_time),
    overall_time = list(Sys.time() - time),
    basic_filter_check = basic_filter_check
  )
  
  pso_env[["data"]] <- c(pso_env[["data"]], list(new_data))
  
  if (plot_response) {
    plot_vsvr_response_signal(response_predictions$predicted_values)
  }
  
  if (!silent)
    display_pso_message(
      round(fitness_avg_cor, fitness_accuracy),
      round(fitness_avg_error, fitness_accuracy),
      fitness_filter,
      round(fitness_adv_score, fitness_accuracy)
    )
  
  if (fitness_filter == 1) {
    pso_env[["candidates"]] <- pso_env[["candidates"]] + 1
  }
  
  return(fitness)
}