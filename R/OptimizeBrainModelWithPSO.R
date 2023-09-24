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
#' @param model Type of model to be optimized (e.g., FIR, NFIR).
#' @param multi Logical indicating if the model should be multivariate.
#' @param signal_names Vector of names representing signal columns.
#' @param excluded_cols Vector of column names that should be excluded from optimization.
#' @param predictors_names Vector of predictor names for the model.
#' @param params_lower_bounds Vector of lower bounds for model parameters.
#' @param params_upper_bounds Vector of upper bounds for model parameters.
#' @param params_initial_values Vector of initial values for model parameters.
#' @param vsvr_response Name of the column representing the VSVR response.
#' @return An object with results from the psoptim optimization.
#' @description Utilizing blocked k-fold cross-validation, the package
#' conducts hyperparameter optimization through PSO. Models generated
#' include FIR, NFIR, ARX, and NARX. These models can be univariate or multivariate.
#' @importFrom pso psoptim
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
