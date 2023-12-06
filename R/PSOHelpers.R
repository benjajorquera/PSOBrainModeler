#' Extract and Round PSO Parameters
#'
#' This function extracts parameters from a PSO optimization result, rounding
#' or applying significance as necessary.
#' It is designed to handle parameters including cost, nu, gamma (optional),
#' and lags.
#'
#' @param params Numeric vector containing the parameters to be extracted.
#' @param has_gamma Logical indicating whether the gamma parameter is present.
#' Set to TRUE to include gamma in the extraction.
#' @param n_lags Integer specifying the number of lag parameters to extract.
#' Determines the count of lagged values included.
#' @param round_accuracy Numeric value for rounding off the parameters.
#' Specifies the number of decimal places for rounding.
#' @param signif_accuracy Numeric value for significant figure accuracy.
#' Defines the number of significant digits to retain.
#'
#' @return A list containing the extracted parameters:
#'   - `cost`: The cost parameter, rounded or significant based on its value.
#'   - `nu`: The nu parameter, rounded to 3 decimal places.
#'   - `lags`: A vector of lag parameters, each rounded to the nearest integer.
#'   If `has_gamma` is TRUE, the list also includes:
#'   - `gamma`: The gamma parameter, rounded or significant based on its value.
#'
#' @examples
#' extract_and_round_pso_params(c(1.234, 2.345, 0.456, 3, 4, 5),
#'  has_gamma = TRUE, n_lags = 3)
#' extract_and_round_pso_params(c(0.234, 2.345, 3, 4), has_gamma = FALSE,
#'  n_lags = 2)
#'
#' @export
#'
extract_and_round_pso_params <-
  function(params,
           has_gamma = FALSE,
           n_lags = 1,
           round_accuracy = 2,
           signif_accuracy = 3) {
    round_or_signif <-
      function(x,
               threshold = 1,
               round_digits = round_accuracy,
               signif_digits = signif_accuracy) {
        if (x >= threshold) {
          round(x, digits = round_digits)
        } else {
          signif(x, digits = signif_digits)
        }
      }
    
    cost <-
      round_or_signif(params[1],
                      round_digits = (round_accuracy - 1),
                      signif_digits = round_accuracy)
    nu <- round(params[2], digits = round_accuracy)
    lags_start <- if (has_gamma)
      4
    else
      3
    lags <- round(params[lags_start:(lags_start + n_lags - 1)])
    
    params_list <- list(cost = cost, nu = nu, lags = lags)
    
    if (has_gamma) {
      gamma <-
        round_or_signif(params[3])
      params_list$gamma <- gamma
    }
    
    return(params_list)
  }


#' Auxiliary Function to Get Model Parameters
#'
#' This function retrieves the valid parameter lengths and number of lags for
#'  different model types.
#' It supports both univariate and multivariate cases for FIR, NFIR, ARX, and
#'  NARX models.
#'
#' @param model Character string specifying the type of model.
#'  Should be one of "FIR", "NFIR", "ARX", or "NARX".
#' @param multi (Optional) Logical indicating if the model is multivariate.
#'  Defaults to FALSE.
#'
#' @return A list containing two elements:
#'   - `valid_lengths`: Vector of valid parameter lengths for the specified model.
#'   - `n_lags`: Number of lags to be used for the model.
#'
#' @examples
#' get_model_parameters("FIR", TRUE)
#' get_model_parameters("ARX", FALSE)
#'
#' @export
#'
get_model_parameters <- function(model, multi = FALSE) {
  is_fir_nfir <- model %in% c("FIR", "NFIR")
  is_arx_narx <- model %in% c("ARX", "NARX")
  
  valid_lengths <-
    if (is_fir_nfir)
      c(3, 4)
  else if (is_arx_narx)
    c(4, 5)
  valid_multi_lengths <- valid_lengths + 1
  n_lags <- if (is_fir_nfir)
    1
  else if (is_arx_narx)
    2
  n_multi_lags <- n_lags + 1
  
  if (multi) {
    return(list(valid_lengths = valid_multi_lengths, n_lags = n_multi_lags))
  }
  return(list(valid_lengths = valid_lengths, n_lags = n_lags))
}


#' Display Formatted Message Based on Parameters List
#'
#' This function constructs and displays a message detailing the values of the
#' parameters in the provided list. It specifically formats and includes
#' information about cost, nu, gamma (if not NULL), and lags.
#'
#' @param params_list A list containing parameters including cost, nu, gamma,
#'  column lags, and response lags.
#'
#' @examples
#' params_list <- list(cost = 1.23, nu = 0.45, gamma = NULL, col_lags = 1:3,
#'  response_lags = 4:5)
#' display_params_message(params_list)
#'
#' @export
#'
display_params_message <- function(params_list) {
  gamma_message <-
    if (!is.null(params_list$gamma))
      paste("Gamma:", params_list$gamma)
  else
    ""
  lags <-
    paste(c(params_list$col_lags, params_list$response_lags),
          collapse = ", ")
  
  message <-
    paste(
      "Cost:",
      params_list$cost,
      "Nu:",
      params_list$nu,
      gamma_message,
      "Lags:",
      lags,
      sep = " "
    )
  cat(message, "\n")
}

#' Extract and Format Parameters List for PSO Modeling
#'
#' This function extracts parameters necessary for PSO modeling from a given
#' list, adjusts for the presence of a gamma parameter, and formats them
#' accordingly. It also handles adjustments for multivariate models.
#'
#' @param params A vector of parameters for PSO modeling.
#' @param model_parameters A list of model parameters, including valid lengths
#'  and number of lags.
#' @param multi Logical indicating if the model is multivariate. Defaults to
#'  FALSE.
#' @param round_accuracy Numeric value for rounding off the parameters.
#' Specifies the number of decimal places for rounding.
#' @param signif_accuracy Numeric value for significant figure accuracy.
#' Defines the number of significant digits to retain.
#'
#' @return A list of formatted parameters, including cost, nu,
#'  gamma (if applicable), response lags, and column lags.
#'
#' @examples
#' params <- c(1, 2, 3, 4, 5)
#' model_parameters <- list(valid_lengths = c(4, 5), n_lags = 2)
#' extract_params_list(params, model_parameters)
#'
#' @export
#'
extract_params_list <-
  function(params,
           model_parameters,
           multi = FALSE,
           round_accuracy = 2,
           signif_accuracy = 3) {
    has_gamma <- length(params) == max(model_parameters$valid_lengths)
    params_list <-
      extract_and_round_pso_params(
        params,
        has_gamma = has_gamma,
        n_lags = model_parameters$n_lags,
        round_accuracy = round_accuracy,
        signif_accuracy = signif_accuracy
      )
    
    col_lags <- params_list$lags[1]
    response_lags <-
      if (model_parameters$n_lags >= 2)
        params_list$lags[2]
    else
      NULL
    
    if (multi && model_parameters$n_lags >= 2) {
      col_lags <- params_list$lags[1:2]
      response_lags <-
        if (length(params_list$lags) >= 3)
          params_list$lags[3]
      else
        NULL
    }
    
    return(
      list(
        cost = params_list$cost,
        nu = params_list$nu,
        gamma = params_list$gamma,
        response_lags = response_lags,
        col_lags = col_lags
      )
    )
  }

#' Display PSO Message
#'
#' This function displays a message with various statistical measures and
#' signal filter scores.
#' It is designed to output the average correlation (AVG COR), average mean
#' squared error (AVG MSE), basic filter signal score, and advanced filter
#' signal score.
#'
#' @param cor A numeric value or vector representing the average correlation.
#'             This parameter should provide the average correlation scores
#'             calculated
#'             by your process or model.
#' @param err A numeric value or vector representing the average mean squared
#'            error (MSE).
#'            This parameter should contain the average MSE values calculated
#'            by your process or model.
#' @param basic A numeric or character value representing the basic filter
#'              signal score.
#'              This score or flag indicates the result of a basic filtering
#'              process or evaluation.
#' @param advanced A numeric or character value representing the advanced filter
#'                 signal score.
#'                 This score or flag indicates the result of an advanced
#'                 filtering process or evaluation.
#'
#' @return This function does not return a value. It prints the input parameters
#'  in a formatted manner to the R console.
#' @examples
#' display_pso_message(0.85, 0.02, "Pass", "High")
#'
#' @export
display_pso_message <- function(cor, err, basic, advanced) {
  cat(
    "\nAVG COR: ",
    cor,
    "AVG RMSE: ",
    err,
    "Signal basic filter: ",
    basic,
    "Signal advance score: ",
    advanced,
    "\n"
  )
}
