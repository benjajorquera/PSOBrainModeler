#' Extract and Round Parameters from PSO Optimization
#'
#' This function takes a numeric vector of parameters, rounds them as necessary,
#' and structures them in a list for easier use in subsequent processes.
#'
#' @param params A numeric vector with the optimization parameters.
#' @param has_gamma (Optional) A logical indicating if the gamma parameter is included. Defaults to FALSE.
#' @param n_lags (Optional) An integer for the number of lags being optimized. Defaults to 1.
#'
#' @return A list containing the rounded parameters including cost, nu, gamma (if applicable), and lags.
#'
#' @examples
#' params <- c(1.234, 0.5678, 0.00089, 2.345)
#' PSOBrainModeler:::extract_and_round_pso_params(params, has_gamma = TRUE, n_lags = 1)
#'
extract_and_round_pso_params <-
  function(params,
           has_gamma = FALSE,
           n_lags = 1) {
    #cat("Params before rounding: \n", params, "\n")
    cost <- ifelse(params[1] >= 1,
                   round(params[1], digits = 3),
                   signif(params[1], digits = 3))
    
    nu <- round(params[2], digits = 3)
    
    if (has_gamma) {
      gamma <-
        ifelse(params[3] >= 1,
               round(params[3], digits = 3),
               signif(params[3], digits = 5))
      lags_start <- 4
    } else {
      gamma <- NULL
      lags_start <- 3
    }
    
    lags <- round(params[lags_start:(lags_start - 1 + n_lags)])
    params_list <- list(cost = cost, nu = nu, lags = lags)
    
    if (!is.null(gamma)) {
      params_list$gamma <- gamma
    }
    
    validate_pso_svr_params(params_list)
    return(params_list)
  }

#' Validate Parameters for PSO-SVR
#'
#' This function validates the parameters for the Particle Swarm Optimization
#' Support Vector Regression (PSO-SVR) model. It checks if the provided
#' parameters 'cost', 'nu', 'gamma', and 'lags' meet the expected criteria.
#'
#' @param list A list containing the parameters to be validated. The list can
#'             include 'cost', 'nu', 'gamma', and 'lags'.
#'
#' @return Invisible NULL. If any of the parameters fail validation, the function
#'         stops with an error message.
#'
#' @details
#' The function expects the following parameters in the list:
#' - 'cost': A positive numeric value.
#' - 'nu': A numeric value between 0 (exclusive) and 1 (inclusive).
#' - 'gamma': (Optional) A positive numeric value.
#' - 'lags': A numeric vector. The function validates only the first value, which
#'           should be a positive integer.
#'
#' @examples
#' parameters <- list(cost = 1.5, nu = 0.5, gamma = 0.01, lags = c(2, 3, 4))
#' PSOBrainModeler:::validate_pso_svr_params(parameters)
#'
validate_pso_svr_params <- function(list) {
  # Validate 'cost'
  if (!is.numeric(list$cost) || list$cost <= 0) {
    stop("Invalid value for 'cost'. It should be a positive number.")
  }
  
  # Validate 'nu'
  if (!is.numeric(list$nu) ||
      list$nu <= 0 || list$nu > 1) {
    stop("Invalid value for 'nu'. It should be between 0 (exclusive) and 1 (inclusive).")
  }
  
  # Validate 'gamma', if it exists
  if (!is.null(list$gamma)) {
    if (!is.numeric(list$gamma) || list$gamma <= 0) {
      stop("Invalid value for 'gamma'. It should be a positive number.")
    }
  }
  
  # Validate 'lags'
  if (!all(is.numeric(list$lags)) || list$lags[1] < 1) {
    stop("Invalid first value for lags")
  }
  
}
