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