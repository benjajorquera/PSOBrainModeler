#' v-Support Vector Regression (v-SVR) Model
#'
#' Fits a Nu Support Vector Regression (SVR) model using the `svm` function from
#' the `e1071` package. This function is designed for regression tasks using
#' Nu-SVR, a variant of SVM that uses a parameter nu to control the number of
#' support vectors.
#'
#' @param data Dataframe containing predictor variables and the response
#'  variable.
#' @param response_var Character string specifying the name of the response
#'  variable in the data frame.
#' @param cost Numeric value representing the cost parameter of the SVR model,
#'  controlling the trade-off between model complexity and error.
#' @param nu Numeric value representing the nu parameter of the SVR model,
#'  controlling the number of support vectors.
#' @param gamma Numeric value (optional) representing the kernel parameter for
#'  the SVR model. Required for radial basis function (RBF) kernels.
#' @param tolerance Numeric value representing the tolerance for the stopping
#'  criterion of the SVR algorithm.
#' @param cache_size Numeric value indicating the size of the cache for the SVM
#'  algorithm. Defaults to 100.
#'
#' @return A list containing two elements: `svm_model` - an SVR model object
#'  from `e1071::svm`, and `max_iterations_warnings` - a count of any warnings
#'  that occurred during the maximum iterations of the model fitting process.
#'
#' @examples
#' # Example of fitting an SVR model with a radial kernel
#' # model <- vsvr_model(data = my_data,
#' #                     response_var = "target",
#' #                     cost = 1,
#' #                     nu = 0.3,
#' #                     gamma = 0.01,
#' #                     tolerance = 0.001,
#' #                     cache_size = 100)
#' @importFrom e1071 svm
#' @importFrom utils capture.output
#' @export
#'
vsvr_model <-
  function(data,
           response_var,
           cost,
           nu,
           gamma = NULL,
           tolerance,
           cache_size = 100) {
    warnings_counter <- 0
    
    # Validations
    stopifnot(
      is.data.frame(data),
      is.character(response_var),
      length(response_var) == 1,
      response_var %in% names(data)
    )
    
    # Extracting response data
    response_data <- data[[response_var]]
    predictor_data <- data[,!names(data) %in% c(response_var)]
    
    # Setting up the common parameters for the SVM model
    model_params <- list(
      x = predictor_data,
      y = response_data,
      cost = cost,
      nu = nu,
      type = "nu-regression",
      tolerance = tolerance,
      cache.size = cache_size
    )
    
    # Adjusting parameters based on the kernel choice
    if (is.null(gamma)) {
      model_params$kernel <- "linear"
    } else {
      model_params$kernel <- "radial"
      model_params$gamma <- gamma
    }
    
    captured_output <- utils::capture.output({
      svm_model <- do.call(e1071::svm, model_params)
    }, type = "message")
    
    if (!identical(captured_output, character(0))) {
      warnings_counter <- warnings_counter + 1
    }
    
    return(list(svm_model = svm_model,
                max_iterations_warnings = warnings_counter))
  }