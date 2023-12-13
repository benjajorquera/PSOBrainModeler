#' Optimize Brain Model Using PSO
#'
#' This function offers a comprehensive toolkit for the analysis and
#' modeling of biological signal data specific to individual patients.
#' It facilitates the training of SVR models to represent and predict
#' cerebral autoregulation phenomena.
#'
#' @param config A list of configuration options for the brain modeler.
#' @param psoptim_config A list of configuration options for psoptim.
#' @param data A data frame containing the dataset to be optimized.
#' @param model Character string specifying the type of model to be optimized.
#'  Should be one of "FIR", "NFIR", "ARX", or "NARX". Defaults to "FIR".
#' @param multi (Optional) Logical indicating if the model should be multivariate.
#'  If TRUE, optimizes with two input variables; if FALSE, optimizes with one
#'  input variable. Defaults to FALSE.
#' @param signal_names Vector of names representing signal columns.
#' @param excluded_cols (Optional) Vector of column names that should be excluded
#'  from optimization. Defaults to NULL.
#' @param predictors_names Vector of predictor names for the model.
#' @param max_function_count (Optional) Maximum number of function evaluations.
#'  Default is 1000.
#' @param params_lower_bounds Vector of lower bounds for model optimization
#'  parameters.
#' @param params_upper_bounds Vector of upper bounds for model optimization
#'  parameters.
#' @param params_initial_values (Optional) Vector of initial values for model
#'  optimization parameters.
#'  If provided, length should be equal for all parameter vectors. Defaults to
#'   NULL.
#' @param vsvr_response Name of the column representing the VSVR response.
#' @param extra_col_name (Optional) Additional column name to be included.
#'  Defaults to NULL.
#' @param silent (Optional) A logical indicating if the function should run
#'  silently. Defaults to TRUE.
#' @param plot_response (Optional) A logical to decide whether to plot the
#'  response signal. Defaults to FALSE.
#' @param initial_pressure_value (Optional) Initial pressure value for the
#'  optimization. Defaults to c(1).
#' @param initial_response_value Initial value set for the response variable in
#'  the optimization process. Defaults to 1.
#' @param seed (Optional) Seed value for random number generation. Defaults to
#'  123.
#' @param generate_response_predictions_cv Logical flag to determine if response
#'  predictions should be generated. Defaults to FALSE.
#' @param basic_filter_check Logical flag indicating whether a basic filter
#'  check is performed. Defaults to TRUE.
#' @param fn_count_threshold Integer threshold for a specific function count
#'  condition, affecting the optimization flow. Defaults to 30.
#' @param fitness_accuracy Numeric value specifying fitness evaluation accuracy; default is 3.
#' @param round_accuracy Numeric value for rounding off the parameters.
#' Specifies the number of decimal places for rounding.
#' @param signif_accuracy Numeric value for significant figure accuracy.
#' Defines the number of significant digits to retain.
#' @param show_progress_bar Disables progress bar. Defaults to FALSE.
#' @param minimum_candidates Sets the lower limit for candidate consideration.
#' @param fn_start_threshold Determines the function's starting point if there are no candidates.
#' @param time_on_fitness Apply time to the objective function.
#' @param penalization_weight Numeric value for the weight in optimization penalization.
#'
#' @return Returns a list containing two elements:
#'   - `psoptim_result`: The result from the psoptim optimization process.
#'   - `pso_env`: The environment or context in which the psoptim optimization
#'       was performed.
#'
#' @description This function implements blocked k-fold cross-validation for
#'  conducting hyperparameter optimization using Particle Swarm Optimization (PSO).
#'  It supports the creation of various model types, including FIR, NFIR, ARX,
#'  and NARX, which can be either univariate or multivariate. The function has
#'  been enhanced with the integration of a progress bar to provide visual
#'  feedback during the optimization process. Additionally, it utilizes an
#'  environment (env) for improved management and tracking of optimization
#'  parameters and results.
#'
#' @examples
#'
#' \dontrun{
#'  brain_modeler_config <- configure_pso_brain_modeler()
#'  psoptim_config <- configure_psoptim_control()
#'
#'  optimize_brain_model_with_PSO(config = brain_modeler_config,
#'    psoptim_config = psoptim_config, ... )
#' }
#'
#' @importFrom pso psoptim
#' @importFrom progress progress_bar
#'
#' @export
#'
optimize_brain_model_with_PSO <- function(config,
                                          psoptim_config,
                                          data,
                                          model = "FIR",
                                          multi = FALSE,
                                          signal_names,
                                          excluded_cols = NULL,
                                          predictors_names,
                                          max_function_count = 1000,
                                          params_lower_bounds,
                                          params_upper_bounds,
                                          params_initial_values = NULL,
                                          vsvr_response,
                                          extra_col_name = NULL,
                                          silent = TRUE,
                                          plot_response = FALSE,
                                          initial_pressure_value = c(1),
                                          initial_response_value = 1,
                                          seed = 123,
                                          generate_response_predictions_cv = FALSE,
                                          basic_filter_check = TRUE,
                                          fn_count_threshold = 30,
                                          fitness_accuracy = 3,
                                          round_accuracy = 2,
                                          signif_accuracy = 3,
                                          show_progress_bar = FALSE,
                                          minimum_candidates = 10,
                                          fn_start_threshold = 100,
                                          time_on_fitness = FALSE,
                                          penalization_weight = 0.5) {
  stopifnot(is.data.frame(data))
  # TODO: Validate vector lengths
  # TODO: Fix some function documentations
  # TODO: Add extra information messages
  # TODO: Fix static values with parameters
  
  data_env_list <- configure_data_env(
    config = config,
    data = data,
    signal_names = signal_names,
    excluded_cols = excluded_cols,
    predictors_names = predictors_names,
    vsvr_response = vsvr_response,
    multi = multi,
    extra_col_name = extra_col_name,
    initial_prediction_values = initial_pressure_value
  )
  
  if (show_progress_bar) {
    if (psoptim_config$maxf < max_function_count) {
      max_function_count <- psoptim_config$maxf
    }
    if (psoptim_config$maxit * psoptim_config$s < max_function_count) {
      max_function_count <- psoptim_config$maxit * psoptim_config$s
    }
  }
  
  if (!silent)
    message(model)
  
  # Determine validation lengths and n_lags based on model
  model_parameters <-
    get_model_parameters(model = model, multi = multi)
  
  pso_env <- new.env()
  
  pso_env[["data"]] <- list()
  pso_env[["max_global_cor"]] <- -1
  pso_env[["best_fitness"]] <- 5
  pso_env[["function_count"]] <- 0
  pso_env[["time"]] <- Sys.time()
  pso_env[["function_count_without_improvement"]] <- 0
  pso_env[["max_pred_cv_time"]] <- 0
  pso_env[["candidates"]] <- 0
  pso_env[["max_pred_time"]] <- 0
  pso_env[["tolanrace"]] <- config$vsvr_tolerance
  
  # TODO: Maximum time condition
  progress_bar <- NULL
  
  if (show_progress_bar) {
    progress_bar <- progress::progress_bar$new(
      format = "Progress: [:bar] :percent | Step :current/:total | Elapsed: :elapsed | Remaining: :eta | Rate :rate ops/sec",
      total = max_function_count,
      clear = FALSE,
      width = 100
    )
  }
  
  psoptim_result <- pso::psoptim(
    par = params_initial_values,
    fn = pso_model,
    max_function_count = max_function_count,
    model = model,
    multi = multi,
    data_list = data_env_list,
    initial_response_value = initial_response_value,
    silent = silent,
    generate_response_predictions_cv = generate_response_predictions_cv,
    basic_filter_check = basic_filter_check,
    plot_response = plot_response,
    model_parameters = model_parameters,
    bcv_folds = config$bcv_folds,
    pso_env = pso_env,
    seed = seed,
    fn_count_threshold = fn_count_threshold,
    fitness_accuracy = fitness_accuracy,
    round_accuracy = round_accuracy,
    signif_accuracy = signif_accuracy,
    progress_bar = progress_bar,
    show_progress_bar = show_progress_bar,
    minimum_candidates = minimum_candidates,
    fn_start_threshold = fn_start_threshold,
    cv_folds_ratio = config$bcv_validation_size,
    time_on_fitness = time_on_fitness,
    penalization_weight = penalization_weight,
    lower = params_lower_bounds,
    upper = params_upper_bounds,
    control = psoptim_config
  )
  
  if (!is.null(progress_bar)) {
    while (!progress_bar$finished && show_progress_bar) {
      progress_bar$tick()
    }
  }
  
  pso_env[["time"]] <- Sys.time() - pso_env[["time"]]
  
  return(c(list(psoptim_result = psoptim_result), pso_env = pso_env))
}