#' PSO Objective Function for FIR Modeling
#'
#' This function takes a numeric vector of optimization parameters and returns
#' the objective value for the FIR modeling using the specified parameters.
#'
#' @param params A numeric vector with the optimization parameters.
#'   Length should be either 3 (without gamma) or 4 (with gamma).
#'
#' @return A numeric value representing the objective value for the given parameters.
#'
#' @examples
#' \dontrun{
#'   # Assuming appropriate context:
#'   pso_fir(c(0.5, 0.7, 1))
#'   pso_fir(c(0.5, 0.7, 1, 0.2))
#' }
#' @export
pso_fir <- function(params) {
  # Validation
  if (!is.numeric(params) || !(length(params) %in% c(3, 4))) {
    stop("params should be a numeric vector of length 3 or 4.")
  }
  
  has_gamma <- length(params) == 4
  params_list <-
    extract_and_round_pso_params(params, has_gamma = has_gamma, n_lags = 1)
  
  cat(
    "Cost: ",
    params_list$cost,
    "\nNu: ",
    params_list$nu,
    "\nLags: ",
    params_list$lags,
    "\n"
  )
  
  result <- pso_training_model(
    cost = params_list$cost,
    nu = params_list$nu,
    gamma = params_list$gamma,
    col_lags = params_list$lags,
    vsvr_response = get("NORM_VSVR_RESPONSE", envir = .psoBrainModelerEnv)
  )
  
  # Ensure result is numeric
  if (!is.numeric(result) || length(result) != 1) {
    stop("The function pso_training_model should return a single numeric value.")
  }
  
  return(result)
}

#' PSO Objective Function for ARX Modeling
#'
#' This function takes a numeric vector of optimization parameters and returns
#' the objective value for ARX modeling using the specified parameters.
#'
#' @param params A numeric vector with the optimization parameters.
#'   Its length should be 4 (without gamma) or 5 (with gamma).
#'
#' @return A numeric value that represents the objective value for the given parameters.
#'
#' @examples
#' \dontrun{
#'   # Assuming appropriate context:
#'   pso_arx(c(0.5, 0.7, 1, 2))
#'   pso_arx(c(0.5, 0.7, 0.2, 1, 2))
#' }
#' @export
pso_arx <- function(params) {
  # Validation
  if (!is.numeric(params) || !(length(params) %in% c(4, 5))) {
    stop("params should be a numeric vector of length 4 or 5.")
  }
  
  has_gamma <- length(params) == 5
  
  if (has_gamma) {
    params_list <-
      extract_and_round_pso_params(params, has_gamma = TRUE, n_lags = 2)
  } else {
    params_list <-
      extract_and_round_pso_params(params, has_gamma = FALSE, n_lags = 2)
  }
  
  cat("Cost: ",
      params_list$cost,
      "Nu: ",
      params_list$nu,
      "Lags: ",
      params_list$lags,
      "\n")
  
  return(
    pso_training_model(
      cost = params_list$cost,
      nu = params_list$nu,
      gamma = params_list$gamma,
      col_lags = params_list$lags
    )
  )
}
