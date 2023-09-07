#' v-Support Vector Regression (v-SVR) Model
#'
#' This function fits a Nu Support Vector Regression (SVR) model to the given data
#' for regression tasks.
#'
#' @param data A data frame containing predictor variables.
#' @param response_var The name of the response variable in the data frame.
#' @param cost Cost parameter for the SVR model.
#' @param nu Nu parameter for the SVR model.
#' @param gamma Kernel parameter for the SVR model.
#' @param tolerance Tolerance for stopping criterion.
#' @param kernel Kernel function to be used (e.g., "linear", "radial").
#'
#' @return An SVR model object.
#'
#' @examples
#' # Fit an SVR model
#' model <- vsvr_model(data = my_data,
#'                     response_var = "target",
#'                     cost = 1,
#'                     nu = 0.3,
#'                     gamma = 0.01,
#'                     tolerance = 0.001,
#'                     kernel = "radial")
#'
vsvr_model <-
  function(data,
           response_var,
           cost,
           nu,
           gamma,
           tolerance,
           kernel) {
    response_data <- data[[response_var]]
    data <- data[,!names(data) %in% c(response_var)]
    svm_model <- svm(
      data,
      response_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = kernel,
      type = "nu-regression",
      tolerance = tolerance
    )
    return(svm_model)
  }
