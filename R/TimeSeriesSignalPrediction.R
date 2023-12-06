#' Time Series Signal Prediction Generator
#'
#' This function trains a Support Vector Regression (SVR) model with the
#' provided data and then utilizes
#' it to generate predictions for a response signal in the context of a time
#' series setup. All character variables must end with "_norm".
#'
#' @param data A dataframe containing the training data for the SVR model.
#' @param pressure_signal_df A dataframe that encapsulates the pressure signal.
#'  The signal should align with the prediction size. Each row represents a
#'  time step and contains the signal's value.
#' @param pressure_start An integer specifying the starting point of the pressure signal
#'                       decrease. It marks the time step from which predictions should start.
#' @param prediction_size A positive integer indicating the total number of predictions to
#'                        be generated.
#' @param column_names A character vector specifying the names of all the variables
#'                     involved, both initial and predictive.
#' @param initial_columns_lags An integer vector specifying the number of lags for each of
#'                             the initial variables, aligned with `initial_column_names`.
#' @param predicted_column_lags An integer indicating the number of lags for the predictive
#'                              variable.
#' @param initial_column_names A character vector that lists the names of the initial
#'                             variables. These are the variables used to derive lagged
#'                             values, but exclude the main predictive variable.
#' @param initial_column_values A numeric vector that provides initial values corresponding
#'                              to the `initial_column_names`.
#' @param prediction_col_name A single character string specifying the name of the primary
#'                            predictive variable.
#' @param prediction_initial_value A numeric value indicating the initial value of the
#'                                 predictive variable.
#' @param predictor_cols Character vector of column names to include in the prediction set.
#' @param cost A numeric value indicating the cost parameter of the SVR model.
#' @param nu A numeric value indicating the nu parameter of the SVR model.
#' @param gamma A numeric value (optional) indicating the gamma parameter of the SVR model.
#' @param tolerance Numeric. SVM tolerance for stopping criterion.
#' @param svm_cache_size Numeric. The size of the cache for the SVM algorithm.
#' @param included_model (Optional) An existing SVR model that can be included for the prediction.
#'                        If NULL, a new model is trained. Defaults to NULL.
#'
#' @return A list containing five elements: `predicted_values` - a dataframe with the predicted signal,
#'         where each row corresponds to a time step from `pressure_start` up to `prediction_size`;
#'         `warnings` - any warnings generated during the max iterations of the SVR model;
#'         `model` - the SVR model object used for prediction;
#'         `predictions_all_data_cor` - a measure of the correlation between all predicted and actual values;
#'         and `predictions_all_data_err` - an error metric for all predicted versus actual values.
#'
#' @examples
#' pressure_signal_df <- data.frame(feature1_norm = rnorm(50))
#' prediction <- generate_signal_response_predictions(
#'                     data = data.frame(feature1_norm = rnorm(100),
#'                                       feature2_norm = rnorm(100),
#'                                       feature1_norm_1 = rnorm(100),
#'                                       feature2_norm_1 = rnorm(100)),
#'                     pressure_signal_df = pressure_signal_df,
#'                     column_names = c("feature1_norm", "feature2_norm"),
#'                     initial_columns_lags = c(1),
#'                     predicted_column_lags = NULL,
#'                     initial_column_names = c("feature1_norm"),
#'                     initial_column_values = c(1),
#'                     prediction_col_name = "feature2_norm",
#'                     prediction_initial_value = 0.5,
#'                     predictor_cols = c("feature1_norm"),
#'                     cost = 1,
#'                     nu = 0.5,
#'                     tolerance = 1)
#' @importFrom stats setNames predict
#' @importFrom utils tail
#' @export
#'
generate_signal_response_predictions <- function(data,
                                                 pressure_signal_df,
                                                 pressure_start = 3,
                                                 prediction_size = 70,
                                                 column_names,
                                                 initial_columns_lags,
                                                 predicted_column_lags,
                                                 initial_column_names,
                                                 initial_column_values,
                                                 prediction_col_name,
                                                 prediction_initial_value,
                                                 predictor_cols,
                                                 cost,
                                                 nu,
                                                 gamma = NULL,
                                                 tolerance,
                                                 svm_cache_size = 100,
                                                 included_model = NULL) {
  # Validations
  stopifnot(
    is.data.frame(data),
    is.data.frame(pressure_signal_df),
    length(initial_column_values) == length(initial_column_names)
  )
  
  predictions_all_data_cor <- NULL
  predictions_all_data_err <- NULL
  
  if (is.null(included_model)) {
    # Train model with all data
    data_training <- generate_time_series_data(
      input_df = data,
      data_cols = column_names,
      predictor_cols = NULL,
      lagged_cols = predictor_cols,
      lag_values = c(initial_columns_lags, predicted_column_lags),
      is_training = TRUE,
      vsvr_response = prediction_col_name
    )
    
    # TODO: Remove duplicated code from this two function parameters
    
    data_validation <- generate_time_series_data(
      input_df = data,
      data_cols = column_names,
      predictor_cols = predictor_cols,
      lagged_cols = predictor_cols,
      lag_values = c(initial_columns_lags, predicted_column_lags),
      is_training = FALSE,
      vsvr_response = prediction_col_name
    )
    
    SVR_model <-
      vsvr_model(
        data = data_training,
        response_var = prediction_col_name,
        cost = cost,
        nu = nu,
        gamma = gamma,
        tolerance = tolerance,
        cache_size = svm_cache_size
      )
    
    target_vals <- data[[prediction_col_name]]
    
    predictions_all_data <-
      predict(SVR_model$svm_model, data_validation)
    if (stats::sd(predictions_all_data) == 0) {
      predictions_all_data_cor <- "FAILED"
      predictions_all_data_err <- "FAILED"
    }
    else {
      predictions_all_data_cor <-
        stats::cor(predictions_all_data, target_vals)
      
      predictions_all_data_err <-
        sqrt(mean((target_vals - predictions_all_data) ^ 2))
    }
  } else {
    SVR_model <- included_model
  }
  
  # Create a dataframe for predicted values
  predicted_values <-
    data.frame(name = rep(prediction_initial_value, pressure_start))
  names(predicted_values) <- prediction_col_name
  
  # Set initial values with corresponding column names
  initial_values <-
    stats::setNames(initial_column_values, initial_column_names)
  
  pressure_df_model <-
    data.frame(name = rep(initial_column_values[1], each = pressure_start))
  
  if (length(initial_column_values) > 1) {
    for (value in 2:length(initial_column_values)) {
      # Create a dataframe with initial values replicated
      pressure_df_model <- cbind(pressure_df_model,
                                 data.frame(name = rep(initial_column_values[value],
                                                       each = pressure_start)))
    }
  }
  
  names(pressure_df_model) <- initial_column_names
  names(initial_columns_lags) <- initial_column_names
  
  # Initialize a list to hold lagged columns
  lagged_cols_list <- data.frame()
  
  # For each column, create lagged versions of the column
  for (col_name in initial_column_names) {
    if (initial_columns_lags[col_name] != 0)
    {
      new_col_values <- rep(initial_values[col_name], pressure_start)
      col_names <-
        paste0(col_name, "_", 1:initial_columns_lags[col_name == initial_column_names])
      new_data <-
        stats::setNames(rep(list(new_col_values), length(col_names)), col_names)
      
      
      if (nrow(lagged_cols_list) == 0) {
        lagged_cols_list <- data.frame(new_data)
      } else {
        lagged_cols_list <- cbind(lagged_cols_list, data.frame(new_data))
      }
    }
  }
  
  # Create lagged columns for the prediction column
  if (!is.null(predicted_column_lags) &&
      predicted_column_lags > 0) {
    col_name <-
      paste0(prediction_col_name, "_", 1:predicted_column_lags)
    new_data <-
      stats::setNames(rep(list(
        rep(prediction_initial_value, pressure_start)
      ), predicted_column_lags), col_name)
    lagged_prediction_col_values <- data.frame(new_data)
    
    pressure_df_model <-
      cbind(pressure_df_model,
            lagged_cols_list,
            lagged_prediction_col_values)
  } else {
    pressure_df_model <- cbind(pressure_df_model, lagged_cols_list)
  }
  
  # Initialize predictions data pressure
  predictions_data_pressure <-
    rep(prediction_initial_value, pressure_start)
  
  # Create a sequence of indices for pressure_count
  pressure_count_seq <-
    seq(from = pressure_start + 1, to = prediction_size)
  
  # Internal function to process each pressure_count
  process_pressure_count <- function(pressure_count) {
    # Create new_pressure dataframe with correct column name
    new_pressure <-
      data.frame(name = pressure_signal_df[pressure_count, ],
                 stringsAsFactors = FALSE)
    
    if (length(initial_column_values) > 1) {
      for (value in 2:length(initial_column_values)) {
        new_pressure <-
          cbind(
            new_pressure,
            data.frame(name = initial_column_values[value],
                       stringsAsFactors = FALSE)
          )
      }
    }
    
    names(new_pressure) <- initial_column_names
    
    # Add predicted value if applicable
    if (!is.null(predicted_column_lags) &&
        predicted_column_lags > 0) {
      new_pressure[paste0(prediction_col_name, "_1")] <-
        utils::tail(predictions_data_pressure, 1)
    }
    
    # Incorporate prior observations for additional column values
    for (col_name in initial_column_names) {
      if (initial_columns_lags[col_name] != 0) {
        new_pressure[paste0(col_name, "_1")] <-
          utils::tail(pressure_df_model[col_name], 1)
      }
    }
    
    for (col_name in seq_len(length(initial_column_names))) {
      max_lag <- initial_columns_lags[col_name]
      
      if (max_lag >= 2) {
        for (col_lag in 2:max_lag) {
          new_pressure <- cbind(new_pressure,
                                stats::setNames(
                                  data.frame(name = utils::tail(pressure_df_model[paste0(initial_column_names[col_name],
                                                                                         "_", col_lag - 1)], 1)),
                                  paste0(initial_column_names[col_name], "_", col_lag)
                                ))
        }
      }
    }
    
    
    if (!is.null(predicted_column_lags) &&
        predicted_column_lags >= 2) {
      for (col_lag in 2:predicted_column_lags) {
        lagged_col_name <- paste0(prediction_col_name, "_", col_lag - 1)
        new_pressure <- cbind(new_pressure,
                              stats::setNames(
                                data.frame(name = utils::tail(pressure_df_model[lagged_col_name],
                                                              1)),
                                paste0(prediction_col_name, "_", col_lag)
                              ))
      }
    }
    
    # Add the new row to pressure_df_model and reset row names
    pressure_df_model <<- rbind(pressure_df_model, new_pressure)
    row.names(pressure_df_model) <<- NULL
    
    # Perform prediction
    predictions_data_pressure <<-
      stats::predict(SVR_model$svm_model, pressure_df_model)
    
    # Save signal predicted value
    predicted_values <<-
      rbind(predicted_values,
            utils::tail(predictions_data_pressure, 1))
    
    # Lapply requires a return value
    return(NULL)
  }
  
  # Using lapply to iterate over pressure_count_seq and process each pressure_count
  invisible(lapply(pressure_count_seq, process_pressure_count))
  
  return(
    list(
      predicted_values = predicted_values,
      warnings = SVR_model$max_iterations_warnings,
      model = SVR_model$svm_model,
      predictions_all_data_cor = predictions_all_data_cor,
      predictions_all_data_err = predictions_all_data_err
    )
  )
}
