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
#' Should be one of "FIR", "NFIR", "ARX", or "NARX".
#' @param multi Logical indicating if the model should be multivariate.
#' If TRUE, optimizes with two input variables; if FALSE, optimizes with one input variable.
#' @param signal_names Vector of names representing signal columns.
#' @param excluded_cols Vector of column names that should be excluded from optimization.
#' @param predictors_names Vector of predictor names for the model.
#' @param params_lower_bounds Vector of lower bounds for model optimization parameters.
#' @param params_upper_bounds Vector of upper bounds for model optimization parameters.
#' @param params_initial_values Vector of initial values for model optimization parameters.
#' Length should be equal for all parameter vectors.
#' @param vsvr_response Name of the column representing the VSVR response.
#'
#' @return An object with results from the psoptim optimization.
#'
#' @description Utilizing blocked k-fold cross-validation, the package
#' conducts hyperparameter optimization through PSO. Models generated
#' include FIR, NFIR, ARX, and NARX. These models can be univariate or multivariate.
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
#'    model = "MODEL",
#'    multi = FALSE,
#'    signal_names = c("Feature1", "Feature2"),
#'    excluded_cols = c("Feature3", "Feature4"),
#'    predictors_names = c("Feature1"),
#'    params_lower_bounds = c(1, 0.1, 1),
#'    params_upper_bounds = c(10, 0.5, 3),
#'    params_initial_values = c(NA, NA, NA),
#'    vsvr_response = "Feature2")
#' }
#'
#' @importFrom pso psoptim
#'
#' @export
optimize_brain_model_with_PSO <- function(config,
                                          psoptim_config,
                                          data,
                                          model,
                                          multi,
                                          signal_names,
                                          excluded_cols,
                                          predictors_names,
                                          params_lower_bounds,
                                          params_upper_bounds,
                                          params_initial_values,
                                          vsvr_response) {
  if (!inherits(config, "PSOBrainModelerConfig")) {
    stop("Please provide a valid configuration.")
  }
  
  if (!inherits(psoptim_config, "PSOBrainModelerPSOPTIMConfig")) {
    stop("Please provide a valid 'psoptim' configuration.")
  }
  
  # Validate model
  allowed_models <- c("FIR", "NFIR", "ARX", "NARX")
  if (!(model %in% allowed_models)) {
    stop("Invalid model specified. Please select from 'FIR', 'NFIR', 'ARX', or 'NARX'.")
  }
  
  # Validate parameter vectors
  param_vectors <-
    list(params_lower_bounds,
         params_upper_bounds,
         params_initial_values)
  lengths <- sapply(param_vectors, length)
  if (length(unique(lengths)) != 1) {
    stop(
      "Length of 'params_lower_bounds', 'params_upper_bounds', and 'params_initial_values' must be equal."
    )
  }
  
  configure_data_env(
    config,
    data = data,
    signal_names = signal_names,
    excluded_cols = excluded_cols,
    predictors_names = predictors_names,
    vsvr_response = vsvr_response
  )
  
  main_pso <- pso_objective_selector(model, multi)
  
  return(
    pso::psoptim(
      par = params_initial_values,
      fn = main_pso,
      lower = params_lower_bounds,
      upper = params_upper_bounds,
      control = psoptim_config
    )
  )
}
