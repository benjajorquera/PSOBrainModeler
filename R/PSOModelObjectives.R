#' Particle Swarm Optimization (PSO) Model Function
#'
#' Executes PSO modeling using provided parameters and configurations.
#' This function is tailored for both univariate and multivariate models and
#' offers options for silent operation, response plotting, and integrates
#' various model parameters and PSO environment settings.
#'
#' @param params Parameter list for the PSO model.
#' @param max_function_count Maximum number of function evaluations. Defaults
#'  to 1000.
#' @param multi Boolean flag to indicate if the model is multivariate. Defaults
#'  to FALSE.
#' @param data_list A list containing processed data relevant to the model.
#' @param model The model type to be optimized.
#' @param silent Flag to control the display of messages during execution.
#'  Defaults to TRUE.
#' @param plot_response Flag to enable or disable plotting the response.
#'  Defaults to TRUE.
#' @param initial_response_value Initial value for the response variable.
#'  Defaults to 1.
#' @param model_parameters List of specific model parameters.
#' @param bcv_folds Number of folds for blocked cross-validation. Defaults to 5.
#' @param pso_env PSO environment configuration.
#' @param seed Seed for random number generation. Defaults to 123.
#' @param progress_bar Progress bar configuration for the optimization process.
#' @param generate_response_predictions_cv Flag to generate response
#'  predictions. Defaults to FALSE.
#' @param basic_filter_check Flag to enable basic filtering of the data.
#'  Defaults to TRUE.
#' @param fn_count_threshold Threshold for function count in optimization.
#'  Defaults to 30.
#' @param fitness_accuracy Numeric value specifying fitness evaluation accuracy; default is 3.
#' @param penalization_weight Numeric value for the weight in optimization penalization; default is 0.5.
#' @param round_accuracy Numeric value for rounding off the parameters.
#'  Specifies the number of decimal places for rounding.
#' @param signif_accuracy Numeric value for significant figure accuracy.
#'  Defines the number of significant digits to retain.
#' @param show_progress_bar Disables progress bar. Defaults to FALSE.
#'
#' @return The result of the PSO training model function, including any metrics,
#'         model parameters, and performance indicators.
#'
#' @examples
#' \dontrun{
#'   pso_model(params, data_list = my_data_list, model = "my_model")
#' }
#'
#' @export
#'
pso_model <-
  function(params,
           max_function_count = 1000,
           multi = FALSE,
           data_list,
           model,
           silent = TRUE,
           plot_response = TRUE,
           initial_response_value = 1,
           model_parameters,
           bcv_folds = 5,
           pso_env,
           seed = 123,
           progress_bar = NULL,
           generate_response_predictions_cv = FALSE,
           basic_filter_check = TRUE,
           fn_count_threshold = 30,
           fitness_accuracy = 3,
           penalization_weight = 0.5,
           round_accuracy = 2,
           signif_accuracy = 3,
           show_progress_bar = FALSE) {
    params_list <-
      extract_params_list(
        params = params,
        model_parameters = model_parameters,
        multi = multi,
        round_accuracy = round_accuracy,
        signif_accuracy = signif_accuracy
      )
    
    # Display message
    if (!silent && pso_env[["function_count"]] < max_function_count)
      display_params_message(params_list)
    
    pso_training_model_result <- pso_training_model(
      max_function_count = max_function_count,
      cost = params_list$cost,
      nu = params_list$nu,
      gamma = params_list$gamma,
      col_lags = params_list$col_lags,
      response_lags = params_list$response_lags,
      vsvr_response = data_list$NORM_VSVR_RESPONSE,
      data_list = data_list,
      silent = silent,
      plot_response = plot_response,
      initial_column_values = data_list$INITIAL_PREDICTION_VALUES,
      prediction_initial_value = initial_response_value,
      bcv_folds = bcv_folds,
      pso_env = pso_env,
      seed = seed,
      progress_bar = progress_bar,
      generate_response_predictions_cv = generate_response_predictions_cv,
      basic_filter_check = basic_filter_check,
      fn_count_threshold = fn_count_threshold,
      fitness_accuracy = fitness_accuracy,
      penalization_weight = penalization_weight,
      show_progress_bar = show_progress_bar
    )
    
    return(pso_training_model_result)
  }
