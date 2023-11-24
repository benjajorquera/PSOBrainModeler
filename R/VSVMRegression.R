#' v-Support Vector Regression (v-SVR) Model
#'
#' Fits a Nu Support Vector Regression (SVR) model using the `svm` function from the `e1071` package.
#'
#' @param data Dataframe. Contains predictor variables and the response variable.
#' @param response_var Character. Name of the response variable in the data frame.
#' @param cost Numeric. Cost parameter for the SVR model.
#' @param nu Numeric. Nu parameter for the SVR model.
#' @param gamma Numeric (optional). Kernel parameter for the SVR model. Required for radial kernels.
#' @param tolerance Numeric. Tolerance for stopping criterion.
#'
#' @return An SVR model object from `e1071::svm`.
#'
#' @examples
#' # Fit an SVR model with a radial kernel
#' # model <- vsvr_model(data = my_data,
#' #                     response_var = "target",
#' #                     cost = 1,
#' #                     nu = 0.3,
#' #                     gamma = 0.01,
#' #                     tolerance = 0.001)
#' @importFrom e1071 svm
#' @export

vsvr_model <-
  function(data,
           response_var,
           cost,
           nu,
           gamma = NULL,
           tolerance,
           cache_size = 100) {
    contador_warnings <- 0
    
    # Validations
    stopifnot(
      is.data.frame(data),
      is.character(response_var),
      length(response_var) == 1,
      response_var %in% names(data)
    )
    
    # Extracting response data
    response_data <- data[[response_var]]
    predictor_data <- data[, !names(data) %in% c(response_var)]
    
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
    
    # print("SVR params")
    # cat(cost, nu, gamma, tolerance, model_params$kernel, "\n")
    
    captured_output <- capture.output({
      svm_model <- do.call(e1071::svm, model_params)
    }, type = "message")
    
    if (!identical(captured_output, character(0))) {
      contador_warnings <- contador_warnings + 1
    }
    
    return(list(svm_model = svm_model,
                max_iterations_warnings = contador_warnings))
  }