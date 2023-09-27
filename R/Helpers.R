#' Helper function for cross-validation of partitions
#'
#' This function constructs parameters for the cross_validate_partition function.
#' It sets the configuration related to the SVR model, data partitions, and normalization.
#'
#' @param cost A numeric value for the cost parameter.
#' @param nu A numeric value for the nu parameter.
#' @param gamma A numeric value for the gamma parameter.
#' @param col_lags Numeric vector indicating the lags for the columns.
#' @param data_list A list containing various data configurations
#' @param silent A logical for run the function silently (without printouts). Defaults to FALSE.
#'
#' @return Result from the cross_validate_partition function.
cross_validate_partition_helper <-
  function(cost,
           nu,
           gamma,
           col_lags,
           data_list,
           silent = FALSE) {
    params_list <- list(
      cost = cost,
      nu = nu,
      gamma = gamma,
      data_partitions = data_list$data_partitions,
      signal_norm_names = data_list$NORM_SIGNAL_NAMES,
      predictors_norm_names = data_list$NORM_PREDICTORS_NAMES,
      lagged_cols = data_list$NORM_PREDICTORS_NAMES,
      col_lags = col_lags,
      vsvr_response = data_list$NORM_VSVR_RESPONSE,
      silent = silent
    )
    return(do.call(cross_validate_partition, params_list))
  }

#' Helper function to generate time series data
#'
#' Constructs parameters for the generate_time_series_data function. It prepares the
#' data for model training and evaluation based on the provided configurations.
#'
#' @param lag_values Numeric vector indicating the lags for the columns.
#' @param is_training Logical indicating whether the data is used for training.
#' @param data_list A list containing various data configurations
#'
#' @return Result from the generate_time_series_data function.
generate_time_series_data_helper <-
  function(lag_values, is_training, data_list) {
    params_list <- list(
      input_df = data_list$processed_data,
      data_cols = data_list$NORM_SIGNAL_NAMES,
      predictor_cols = data_list$NORM_PREDICTORS_NAMES,
      lagged_cols = data_list$NORM_PREDICTORS_NAMES,
      vsvr_response = data_list$NORM_VSVR_RESPONSE,
      lag_values = lag_values,
      is_training = is_training
    )
    return(do.call(generate_time_series_data, params_list))
  }

#' Helper function to generate signal response predictions
#'
#' Constructs parameters for the generate_signal_response_predictions function. This
#' involves predicting signal responses based on the provided parameters and data.
#'
#' @param data Data frame containing the dataset to be processed.
#' @param col_lags Numeric vector indicating the lags for the columns.
#' @param response_lags Numeric vector indicating the response lags (default: NULL).
#' @param initial_column_values Numeric vector indicating initial values for columns.
#' @param prediction_initial_value Numeric value indicating the initial prediction value.
#' @param cost A numeric value for the cost parameter.
#' @param nu A numeric value for the nu parameter.
#' @param gamma A numeric value for the gamma parameter (default: NULL).
#' @param data_list A list containing various data configurations
#'
#' @return Result from the generate_signal_response_predictions function.
generate_signal_response_predictions_helper <-
  function(data,
           col_lags,
           response_lags,
           initial_column_values,
           prediction_initial_value,
           cost,
           nu,
           gamma,
           data_list) {
    initial_column_names <-
      data_list$NORM_PREDICTORS_NAMES
    prediction_col_name <-
      data_list$NORM_VSVR_RESPONSE
    if (prediction_col_name %in% initial_column_names)
      initial_column_names <-
      setdiff(initial_column_names, prediction_col_name)
    
    params_list <- list(
      data = data,
      pressure_signal_df = data_list$pressure_df,
      column_names = data_list$NORM_SIGNAL_NAMES,
      initial_columns_lags = col_lags,
      predicted_column_lags = response_lags,
      initial_column_names = initial_column_names,
      initial_column_values = initial_column_values,
      prediction_col_name = prediction_col_name,
      prediction_initial_value = prediction_initial_value,
      predictor_cols = data_list$NORM_PREDICTORS_NAMES,
      cost = cost,
      nu = nu,
      gamma = gamma,
      tolerance = data_list$VSVR_TOL
    )
    return(do.call(generate_signal_response_predictions, params_list))
  }

#' Validate Main Inputs
#'
#' This function validates the main inputs for data processing and model building.
#'
#' @param data A dataframe with the data.
#' @param model A character string specifying the model. Valid values are "FIR", "NFIR", "ARX", and "NARX".
#' @param signal_names A character vector with signal names.
#' @param predictors_names A character vector with predictor names.
#' @param vsvr_response A single character string indicating the response variable for v-SVR.
#' @param excluded_cols (Optional) A character vector with column names to be excluded. Defaults to NULL.
#' @param multi (Optional) A logical indicating whether multi-response is used. Default is FALSE.
#' @param silent (Optional) A logical indicating if the function should run silently. Default is TRUE.
#'
#' @examples
#' data <- data.frame(signal = rnorm(100), predictor = rnorm(100))
#' PSOBrainModeler:::validate_inputs_main(data = data,
#'   model = "FIR",
#'   signal_names = c("signal"),
#'   predictors_names = c("predictor"),
#'   vsvr_response = "signal")
#'

validate_inputs_main <- function(data,
                                 model,
                                 signal_names,
                                 predictors_names,
                                 vsvr_response,
                                 excluded_cols = NULL,
                                 multi = FALSE,
                                 silent = FALSE) {
  # Validate data
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Data is either not a dataframe or is empty.")
  }
  
  # Validate model
  valid_models <- c("FIR", "NFIR", "ARX", "NARX")
  if (!(model %in% valid_models)) {
    stop(sprintf(
      "Invalid model specified. Please select from %s.",
      paste(valid_models, collapse = ", ")
    ))
  }
  
  # Helper function to validate character vectors
  validate_character_vector <- function(vec, name) {
    if (is.null(vec) || length(vec) == 0 || !is.character(vec)) {
      stop(sprintf("'%s' is NULL, empty, or not a character vector.", name))
    }
  }
  validate_character_vector(signal_names, "signal_names")
  validate_character_vector(predictors_names, "predictors_names")
  
  # Validate vsvr_response
  validate_character_vector(vsvr_response, "vsvr_response")
  if (length(vsvr_response) != 1) {
    stop("'vsvr_response' should be a single character value.")
  }
  
  # Validate excluded_cols
  if (!is.null(excluded_cols)) {
    validate_character_vector(excluded_cols, "excluded_cols")
    if (!all(excluded_cols %in% colnames(data))) {
      stop("Some columns in 'excluded_cols' are not present in the data.")
    }
  }
  
  # Validate logical values
  validate_logical <- function(log_val, name) {
    if (!is.logical(log_val) || length(log_val) != 1) {
      stop(sprintf("'%s' should be a single logical value (TRUE or FALSE).", name))
    }
  }
  validate_logical(multi, "multi")
  validate_logical(silent, "silent")
  
  invisible(NULL)
}
