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
           tolerance) {
    # 1. Validate data type
    if (!is.data.frame(data))
      stop("'data' must be a data.frame.")
    
    # 2. Validate response_var
    if (!is.character(response_var) ||
        length(response_var) != 1)
      stop("'response_var' must be a single character string.")
    if (!response_var %in% names(data))
      stop(sprintf(
        "The response variable '%s' is not found in the provided data.",
        response_var
      ))
    
    # 3. Validate cost, nu, and tolerance
    if (!is.numeric(cost) ||
        length(cost) != 1)
      stop("'cost' must be a single numeric value.")
    if (!is.numeric(nu) ||
        length(nu) != 1 ||
        nu <= 0 ||
        nu >= 1)
      stop("'nu' must be a single numeric value between 0 and 1.")
    if (!is.numeric(tolerance) ||
        length(tolerance) != 1)
      stop("'tolerance' must be a single numeric value.")
    
    # 4. Validate gamma
    if (!is.null(gamma) &&
        (!is.numeric(gamma) ||
         length(gamma) != 1))
      stop("'gamma' must be a single numeric value or NULL.")
    
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
      tolerance = tolerance
    )
    
    # Adjusting parameters based on the kernel choice
    if (is.null(gamma)) {
      model_params$kernel <- "linear"
    } else {
      model_params$kernel <- "radial"
      model_params$gamma <- gamma
    }
    
    # Training the SVM model using the do.call function
    svm_model <- do.call(e1071::svm, model_params)
    
    return(svm_model)
  }
