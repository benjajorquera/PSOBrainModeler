#' Extract and Round Parameters from PSO Optimization
#'
#' This function takes a numeric vector of parameters, rounds them as necessary,
#' and structures them in a list for easier use in subsequent processes.
#'
#' @param params A numeric vector with the optimization parameters.
#' @param has_gamma A logical indicating if the gamma parameter is included.
#' @param n_lags An integer for the number of lags being optimized.
#' @return A list containing the rounded parameters including cost, nu, gamma (if applicable), and lags.
#' @examples
#' params <- c(1.234, 0.5678, 0.00089, 2.345)
#' extract_and_round_pso_params(params, has_gamma = TRUE, n_lags = 1)
#' @export
extract_and_round_pso_params <-
  function(params,
           has_gamma = FALSE,
           n_lags = 1) {
    # Validations
    stopifnot(is.numeric(params),
              is.logical(has_gamma),
              is.numeric(n_lags),
              n_lags >= 1)
    
    cost <- round(params[1], digits = 2)
    nu <- round(params[2], digits = 2)
    
    if (has_gamma) {
      gamma <-
        ifelse(params[3] >= 1,
               round(params[3], digits = 2),
               signif(params[3], digits = 2))
      lags <- round(params[4:(3 + n_lags)])
      return(list(
        cost = cost,
        nu = nu,
        gamma = gamma,
        lags = lags
      ))
    } else {
      lags <- round(params[3:(2 + n_lags)])
      return(list(
        cost = cost,
        nu = nu,
        gamma = NULL,
        lags = lags
      ))
    }
  }
