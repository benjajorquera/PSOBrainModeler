#' PSO Model Function
#'
#' This function performs the Particle Swarm Optimization (PSO) model based on the provided parameters and model.
#'
#' @param params A numeric vector of parameters.
#' @param model A character string specifying the model. Valid values are "FIR", "NFIR", "ARX", and "NARX".
#' @param multi A logical indicating whether multi-response is used. Defaults to FALSE.
#' @param data_list A list containing various data configurations.
#' @param silent A logical for run the function silently (without printouts). Defaults to FALSE.
#' @param plot_response (Optional) A logical to decide whether to plot the response signal. Defaults to TRUE.
#'
#' @return The result of the `pso_training_model`.
#'
#' @details
#' This function depends on the following internal package functions:
#' - \code{\link{extract_and_round_pso_params}}: Extract and Round Parameters from PSO Optimization
#' - \code{\link{pso_training_model}}: Model training function using PSO optimization
#'
#' @examples
#' \dontrun{
#'  pso_model(params = c(cost = 1, nu = 0.5, lags = c(1)),
#'    model = "FIR", multi = FALSE, data_list = list())
#' }
#'
#' @export
pso_model <-
  function(params,
           multi = FALSE,
           data_list,
           model,
           silent = FALSE,
           plot_response = TRUE,
           initial_response_value = 1,
           model_parameters,
           bcv_folds = 5,
           pso_env,
           seed = 123) {
    # Validation
    validate_params(params, model_parameters, model)
    
    # Get other parameters and extract them
    has_gamma <-
      length(params) == max(model_parameters$valid_lengths)
    params_list <- extract_and_round_pso_params(params,
                                                has_gamma = has_gamma,
                                                n_lags = model_parameters$n_lags)
    
    # More parameter extraction
    col_lags <- params_list$lags[1]
    response_lags <-
      if (model_parameters$n_lags >= 2)
        params_list$lags[2]
    else
      NULL
    
    # Additional extraction if multi is TRUE
    if (multi && model_parameters$n_lags >= 2) {
      col_lags <- c(params_list$lags[1], params_list$lags[2])
      response_lags <- if (length(params_list$lags) >= 3) {
        params_list$lags[3]
      } else {
        NULL
      }
    }
    
    # Display message
    display_message(params_list, silent)
    
    pso_training_model_result <- pso_training_model(
      cost = params_list$cost,
      nu = params_list$nu,
      gamma = params_list$gamma,
      col_lags = col_lags,
      response_lags = response_lags,
      vsvr_response = data_list$NORM_VSVR_RESPONSE,
      data_list = data_list,
      silent = silent,
      plot_response = plot_response,
      initial_column_values = data_list$INITIAL_PREDICTION_VALUES,
      prediction_initial_value = initial_response_value,
      bcv_folds = bcv_folds,
      pso_env = pso_env,
      seed = seed
    )
    
    return(pso_training_model_result)
  }


# Display message function
display_message <- function(params_list, silent) {
  if (!silent) {
    message <- paste(
      "Cost: ",
      params_list$cost,
      "Nu: ",
      params_list$nu,
      if (!is.null(params_list$gamma))
        paste("Gamma: ", params_list$gamma),
      "Lags: ",
      paste(params_list$lags, collapse = ", "),
      "\n"
    )
    cat(message)
  }
}

# Validation function
validate_params <- function(params, model_parameters, model) {
  valid_lengths <- model_parameters$valid_lengths
  
  if (!is.numeric(params)) {
    stop(sprintf("params should be a numeric vector for model %s.", model))
  }
  
  if (!(length(params) %in% valid_lengths)) {
    stop(sprintf(
      "params should be a numeric vector of length %s for model %s.",
      paste(valid_lengths, collapse = " or "),
      model
    ))
  }
}
