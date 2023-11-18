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
#'  Should be one of "FIR", "NFIR", "ARX", or "NARX".
#' @param multi (Optional) Logical indicating if the model should be multivariate.
#'  If TRUE, optimizes with two input variables; if FALSE, optimizes with one input variable.
#'  Defaults to FALSE.
#' @param signal_names Vector of names representing signal columns.
#' @param excluded_cols (Optional) Vector of column names that should be excluded from optimization.
#'  Defaults to NULL.
#' @param predictors_names Vector of predictor names for the model.
#' @param params_lower_bounds Vector of lower bounds for model optimization parameters.
#' @param params_upper_bounds Vector of upper bounds for model optimization parameters.
#' @param params_initial_values Vector of initial values for model optimization parameters.
#'  Length should be equal for all parameter vectors.
#' @param vsvr_response Name of the column representing the VSVR response.
#' @param silent (Optional) A logical indicating if the function should run silently. Default is TRUE.
#' @param plot_response (Optional) A logical to decide whether to plot the response signal. Defaults to TRUE.
#'
#' @return An object with results from the psoptim optimization.
#'
#' @description Utilizing blocked k-fold cross-validation, the package
#'  conducts hyperparameter optimization through PSO. Models generated
#'  include FIR, NFIR, ARX, and NARX. These models can be univariate or multivariate.
#'
#' @details
#' This function depends on:
#' \itemize{
#'   \item Internal package functions:
#'   \itemize{
#'     \item \code{\link{configure_pso_brain_modeler}}: Configure the Brain Modeler for PSO
#'     \item \code{\link{configure_psoptim_control}}: Configure the control parameters for psoptim
#'     \item \code{\link{configure_data_env}}: Configure the data environment for modeling
#'     \item \code{\link{validate_inputs_main}}: Validate Main Inputs
#'     \item \code{\link{pso_model}}: PSO Model Function
#'   }
#'   \item External package function:
#'   \itemize{
#'     \item \code{psoptim}
#'   }
#' }
#'
#' @examples
#'
#' \dontrun{
#'  mydata <-   data.frame(
#'   Feature1 = rnorm(100),
#'   Feature2 = rnorm(100),
#'   Feature3 = rnorm(100),
#'   Feature4 = rnorm(100)
#'   )
#'  brain_modeler_config <- configure_pso_brain_modeler()
#'  psoptim_config <- configure_psoptim_control()
#'
#'  optimize_brain_model_with_PSO(config = brain_modeler_config,
#'    psoptim_config = psoptim_config,
#'    data = mydata,
#'    model = "FIR",
#'    multi = FALSE,
#'    signal_names = c("Feature1", "Feature2"),
#'    excluded_cols = c("Feature3", "Feature4"),
#'    predictors_names = c("Feature1"),
#'    params_lower_bounds = c(1, 0.1, 1),
#'    params_upper_bounds = c(10, 0.5, 3),
#'    params_initial_values = c(NA, NA, NA),
#'    vsvr_response = "Feature2",
#'    silent = TRUE)
#' }
#'
#' @importFrom pso psoptim
#'
#' @export
#'
optimize_brain_model_with_PSO <- function(config,
                                          psoptim_config,
                                          data,
                                          model,
                                          multi = FALSE,
                                          signal_names,
                                          excluded_cols = NULL,
                                          predictors_names,
                                          params_lower_bounds,
                                          params_upper_bounds,
                                          params_initial_values = NULL,
                                          vsvr_response,
                                          silent = TRUE,
                                          plot_response = FALSE,
                                          initial_pressure_value = c(1),
                                          seed = 123) {
  # Call the validation function
  validate_inputs_main(
    data,
    model,
    signal_names,
    predictors_names,
    vsvr_response,
    excluded_cols,
    multi,
    silent,
    params_lower_bounds,
    params_upper_bounds,
    params_initial_values
  )
  
  data_env_list <- configure_data_env(
    config,
    data = data,
    signal_names = signal_names,
    excluded_cols = excluded_cols,
    predictors_names = predictors_names,
    vsvr_response = vsvr_response,
    multi = multi,
    extra_col_name = "etCO2",
    initial_prediction_values = initial_pressure_value
  )
  
  if (!silent)
    message(model)
  
  # Determine validation lengths and n_lags based on model
  model_parameters <- get_model_parameters(model, multi)
  
  pso_env <- new.env()
  
  pso_env[["data"]] <- list()
  pso_env[["max_cor"]] <- -1
  pso_env[["best_fitness"]] <- 5
  pso_env[["function_count"]] <- 0
  pso_env[["time"]] <- Sys.time()
  pso_env[["function_count_without_improvement"]] <- 0
  pso_env[["max_global_cor"]] <- -1
  
  psoptim_result <- pso::psoptim(
    par = params_initial_values,
    fn = pso_model,
    model = model,
    multi = multi,
    data_list = data_env_list,
    silent = silent,
    plot_response = plot_response,
    model_parameters = model_parameters,
    bcv_folds = config$bcv_folds,
    pso_env = pso_env,
    seed = seed,
    lower = params_lower_bounds,
    upper = params_upper_bounds,
    control = list(
      trace = 1,
      REPORT = 1,
      maxit = 125,
      s = 8,
      w = 1,
      c.p = 2,
      c.g = 5,
      vectorize = TRUE,
      reltol = 0.5,
      hybrid = "improved",
      hybrid.control = list(maxit = 10),
      maxit.stagnate = 110,
      maxf = 1000
    )
    #control = psoptim_config
  )
  
  pso_env[["time"]] <- Sys.time() - pso_env[["time"]]
  
  return(list(c(
    psoptim_result = psoptim_result, pso_env = pso_env
  )))
}

# Auxiliary function to get model parameters
get_model_parameters <- function(model, multi) {
  if (model %in% c("FIR", "NFIR")) {
    valid_lengths <- c(3, 4)
    valid_multi_lengths <- c(4, 5)
    n_lags <- 1
    n_multi_lags <- 2
  } else if (model %in% c("ARX", "NARX")) {
    valid_lengths <- c(4, 5)
    valid_multi_lengths <- c(5, 6)
    n_lags <- 2
    n_multi_lags <- 3
  }
  if (multi) {
    return(list(valid_lengths = valid_multi_lengths, n_lags = n_multi_lags))
  }
  return(list(valid_lengths = valid_lengths, n_lags = n_lags))
}