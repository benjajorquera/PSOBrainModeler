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
#'     \item \code{\link{validate_inputs_main}}: Validate Main Inputs
#'     \item \code{\link{configure_data_env}}: Configure the data environment for modeling
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
                                          params_initial_values,
                                          vsvr_response,
                                          silent = FALSE,
                                          plot_response = TRUE) {
  # Call the validation function
  validate_inputs_main(
    data,
    model,
    signal_names,
    predictors_names,
    vsvr_response,
    excluded_cols,
    multi,
    silent
  )
  
  data_env_list <- configure_data_env(
    config,
    data = data,
    signal_names = signal_names,
    excluded_cols = excluded_cols,
    predictors_names = predictors_names,
    vsvr_response = vsvr_response
  )
  
  message(model)
  
  return(
    pso::psoptim(
      par = params_initial_values,
      fn = pso_model,
      model = model,
      multi = multi,
      data_list = data_env_list,
      silent = silent,
      plot_response = plot_response,
      lower = params_lower_bounds,
      upper = params_upper_bounds,
      control = psoptim_config
    )
  )
}
