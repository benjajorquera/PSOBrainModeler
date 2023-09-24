#' Helper function for cross-validation of partitions
#'
#' This function constructs parameters for the cross_validate_partition function.
#' It sets the configuration related to the SVR model, data partitions, and normalization.
#'
#' @param cost A numeric value for the cost parameter.
#' @param nu A numeric value for the nu parameter.
#' @param gamma A numeric value for the gamma parameter.
#' @param col_lags Numeric vector indicating the lags for the columns.
#'
#' @return Result from the cross_validate_partition function.
#' @export
cross_validate_partition_helper <-
  function(cost, nu, gamma, col_lags) {
    params_list <- list(
      cost = cost,
      nu = nu,
      gamma = gamma,
      data_partitions = get("data_partitions", envir = .psoBrainModelerEnv),
      signal_norm_names = get("NORM_SIGNAL_NAMES", envir = .psoBrainModelerEnv),
      predictors_norm_names = get("NORM_PREDICTORS_NAMES", envir = .psoBrainModelerEnv),
      lagged_cols = get("NORM_PREDICTORS_NAMES", envir = .psoBrainModelerEnv),
      col_lags = col_lags,
      vsvr_response = get("NORM_VSVR_RESPONSE", envir = .psoBrainModelerEnv)
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
#'
#' @return Result from the generate_time_series_data function.
#' @export
generate_time_series_data_helper <-
  function(lag_values, is_training) {
    params_list <- list(
      input_df = get("processed_data", envir = .psoBrainModelerEnv),
      data_cols = get("NORM_SIGNAL_NAMES", envir = .psoBrainModelerEnv),
      predictor_cols = get("NORM_PREDICTORS_NAMES", envir = .psoBrainModelerEnv),
      lagged_cols = get("NORM_PREDICTORS_NAMES", envir = .psoBrainModelerEnv),
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
#'
#' @return Result from the generate_signal_response_predictions function.
#' @export
generate_signal_response_predictions_helper <-
  function(data,
           col_lags,
           response_lags,
           initial_column_values,
           prediction_initial_value,
           cost,
           nu,
           gamma) {
    params_list <- list(
      data = data,
      pressure_signal_df = get("pressure_df", envir = .psoBrainModelerEnv),
      column_names = get("NORM_SIGNAL_NAMES", envir = .psoBrainModelerEnv),
      initial_columns_lags = col_lags,
      predicted_column_lags = response_lags,
      initial_column_names = get("NORM_PREDICTORS_NAMES", envir = .psoBrainModelerEnv),
      initial_column_values = initial_column_values,
      prediction_col_name = get("NORM_VSVR_RESPONSE", envir = .psoBrainModelerEnv),
      prediction_initial_value = prediction_initial_value,
      cost = cost,
      nu = nu,
      gamma = gamma,
      tolerance = get("VSVR_TOL", envir = .psoBrainModelerEnv)
    )
    return(do.call(generate_signal_response_predictions, params_list))
  }