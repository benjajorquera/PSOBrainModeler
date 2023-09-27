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
           model,
           multi = FALSE,
           data_list,
           silent = FALSE,
           plot_response = TRUE) {
    # Determine validation lengths and n_lags based on model
    switch(
      model,
      "FIR" = {
        valid_lengths <- c(3, 4)
        n_lags <- 1
      },
      "NFIR" = {
        valid_lengths <- c(3, 4)
        n_lags <- 1
      },
      "ARX" = {
        valid_lengths <- c(4, 5)
        n_lags <- 2
      },
      "NARX" = {
        valid_lengths <- c(4, 5)
        n_lags <- 2
      }
    )
    
    # Validation
    if (!is.numeric(params) ||
        !(length(params) %in% valid_lengths)) {
      stop(sprintf(
        "params should be a numeric vector of length %s for model %s.",
        paste(valid_lengths, collapse = " or "),
        model
      ))
    }
    
    has_gamma <- length(params) == max(valid_lengths)
    params_list <-
      extract_and_round_pso_params(params, has_gamma = has_gamma, n_lags = n_lags)
    
    col_lags <- params_list$lags[1]
    response_lags <- if (n_lags == 2)
      params_list$lags[2]
    else
      NULL
    
    if (!silent) {
      # Preparing message
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
    
    return(
      pso_training_model(
        cost = params_list$cost,
        nu = params_list$nu,
        gamma = params_list$gamma,
        col_lags = col_lags,
        response_lags = response_lags,
        vsvr_response = data_list$NORM_VSVR_RESPONSE,
        data_list = data_list,
        silent = silent,
        plot_response = plot_response
      )
    )
  }
