#' Helper Function for Cross-Validation of Partitions
#'
#' This function constructs and organizes parameters for the
#' cross_validate_partition function.
#' It sets the configuration related to the SVR model, data partitions,
#' normalization, and additional parameters for cross-validation.
#'
#' @param cost Numeric value for the cost parameter in the SVR model.
#' @param nu Numeric value for the nu parameter in the SVR model.
#' @param gamma (Optional) Numeric value for the gamma parameter in the SVR
#'  model. Defaults to NULL.
#' @param combined_col_lags Numeric vector indicating the combined lags of the
#'  predictor and response columns.
#' @param data_list A list containing various data configurations including data
#'  partitions, normalization names, and other settings.
#' @param silent Logical indicating whether the function should run silently.
#'  Defaults to TRUE.
#' @param bcv_folds (Optional) Number of cross-validation folds. Defaults to 5.
#' @param col_lags (Optional) Numeric or integer vector specifying the lags for
#'  each column.
#' @param response_lags (Optional) Numeric or integer for the response variable
#'  lags.
#' @param initial_column_values (Optional) Initial values for each column in the
#'  dataset.
#' @param prediction_initial_value (Optional) Initial value used for prediction
#'  in the response signal generation.
#' @param generate_response_predictions_cv (Optional) Logical flag to control
#'  the generation of response predictions. Defaults to TRUE.
#'
#' @return The result from the cross_validate_partition function, typically a list
#'  containing metrics like average correlation and mean squared error.
#'
#' @examples
#' \dontrun{
#'   cross_validate_partition_helper(...)
#' }
#'
#' @export
#'
cross_validate_partition_helper <-
  function(cost,
           nu,
           gamma = NULL,
           combined_col_lags,
           data_list,
           silent = TRUE,
           bcv_folds = 5,
           col_lags = NULL,
           response_lags = NULL,
           initial_column_values = NULL,
           prediction_initial_value = NULL,
           generate_response_predictions_cv = TRUE) {
    params_list <- list(
      cost = cost,
      nu = nu,
      gamma = gamma,
      data_partitions = data_list$data_partitions,
      signal_norm_names = data_list$NORM_SIGNAL_NAMES,
      predictors_norm_names = data_list$NORM_PREDICTORS_NAMES,
      lagged_cols = data_list$NORM_PREDICTORS_NAMES,
      combined_col_lags = combined_col_lags,
      vsvr_response = data_list$NORM_VSVR_RESPONSE,
      silent = silent,
      bcv_folds = bcv_folds,
      vsvr_tolerance = data_list$VSVR_TOL,
      svm_cache_size = data_list$svm_cache_size,
      col_lags = col_lags,
      response_lags = response_lags,
      initial_column_values = initial_column_values,
      prediction_initial_value = prediction_initial_value,
      data_env_list = data_list,
      generate_response_predictions_cv = generate_response_predictions_cv
    )
    return(do.call(cross_validate_partition, params_list))
  }

#' Helper Function to Generate Time Series Data
#'
#' This function prepares parameters and calls the generate_time_series_data
#' function.
#' It organizes and structures data for model training and evaluation based on
#' the provided configurations. This includes setting up data columns, predictor
#' columns, lagged columns, and response variables for time series analysis.
#'
#' @param lag_values Numeric vector indicating the lags for each column in the
#'                   dataset.
#'                   This parameter specifies how many previous time steps are
#'                   used for prediction.
#' @param is_training Logical flag indicating whether the data is being prepared
#'                    for training purposes.
#'                    If TRUE, the data is used for model training; if FALSE,
#'                    it's used for validation or testing.
#' @param data_list A list containing various data configurations including
#'                  processed data, normalized signal and predictor names,
#'                  and the response variable name.
#'
#' @return The result from the generate_time_series_data function, typically a
#'  transformed and structured data frame suitable for time series analysis in
#'  model training or evaluation.
#'
#' @examples
#' \dontrun{
#'   generate_time_series_data_helper(...)
#' }
#'
#' @export
#'
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

#' Helper Function to Generate Signal Response Predictions
#'
#' This function prepares parameters for the generate_signal_response_predictions
#' function.
#' It aims to predict signal responses based on the SVR model and the provided
#' dataset, taking into account various parameters such as lags, initial values,
#' and SVR model settings.
#'
#' @param data Data frame containing the dataset to be processed.
#' @param col_lags Numeric vector indicating the lags for the columns.
#' @param response_lags Numeric vector indicating the response lags. Defaults
#'  to NULL.
#' @param initial_column_values Numeric vector indicating initial values for
#'  columns.
#' @param prediction_initial_value Numeric value indicating the initial
#'  prediction value.
#' @param cost Numeric value for the cost parameter in the SVR model.
#' @param nu Numeric value for the nu parameter in the SVR model.
#' @param gamma Numeric value for the gamma parameter in the SVR model.
#'  Defaults to NULL.
#' @param data_list A list containing various data configurations and settings
#'  relevant to the SVR model and dataset.
#' @param included_model (Optional) The pre-trained SVR model to be used.
#'  If NULL, a new model is trained.
#'
#' @return The result from the generate_signal_response_predictions function,
#'  typically predictions of signal responses alongside any relevant model
#'  parameters and statistics.
#'
#' @examples
#' \dontrun{
#'   generate_signal_response_predictions_helper(...)
#' }
#'
#' @export
#'
generate_signal_response_predictions_helper <-
  function(data,
           col_lags,
           response_lags,
           initial_column_values,
           prediction_initial_value,
           cost,
           nu,
           gamma,
           data_list,
           included_model = NULL) {
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
      tolerance = data_list$VSVR_TOL,
      svm_cache_size = data_list$svm_cache_size,
      included_model = included_model
    )
    return(do.call(generate_signal_response_predictions, params_list))
  }