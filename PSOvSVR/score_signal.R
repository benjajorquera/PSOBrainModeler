#' Time Series Signal Prediction Generator
#'
#' This function generates predictions for a response signal using a pre-trained
#' Support Vector Regression (SVR) model.
#'
#' @param SVR_model The pre-trained e1071 SVR model used for making predictions.
#' @param pressure_signal_df Dataframe containing the pressure signal, aligned with the prediction size.
#' @param pressure_start The starting point of the pressure signal decrease.
#' @param prediction_size The number of predictions to generate.
#' @param column_names Character vector of variable names, including all variables.
#' @param column_lags The number of lags for the variables.
#' @param initial_column_names Character vector of variable names excluding the predictive variable.
#' @param initial_column_values Numeric vector of initial values for previous variables.
#' @param prediction_col_name The name of the predictive variable.
#' @param prediction_initial_value The initial value of the predictive variable.
#'
#' @return A dataframe containing the predicted signal.
#'
#' @examples
#' SVR_model <- svm(Sepal.Length ~ Sepal.Width, data = iris)
#' prediction <- generate_predictions(SVR_model, pressure_signal_df, 10, 100,
#'                                    c("Sepal.Width", "Sepal.Length"), 2,
#'                                    c("Sepal.Width"), c(2),
#'                                    "Sepal.Length", 5)
#'

generate_signal_response_predictions <- function(SVR_model,
                                                 pressure_signal_df,
                                                 pressure_start,
                                                 prediction_size,
                                                 column_names,
                                                 initial_columns_lags,
                                                 predicted_column_lags,
                                                 initial_column_names,
                                                 initial_column_values,
                                                 prediction_col_name,
                                                 prediction_initial_value) {
  # Validate input vectors and column sizes
  if (any(lengths(
    list(initial_column_names, initial_column_values, column_names)
  ) == 0) ||
  length(initial_column_values) != length(initial_column_names)) {
    stop("Error: Empty vectors or mismatched sizes between column names and values")
  }
  
  # Create a dataframe for predicted values
  predicted_values <-
    data.frame(name = rep(prediction_initial_value, pressure_start))
  names(predicted_values) <- prediction_col_name
  
  # Set initial values with corresponding column names
  initial_values <-
    setNames(initial_column_values, initial_column_names)
  
  # Create a dataframe with initial values and replicate them
  pressure_df_model <-
    data.frame(name = rep(initial_column_values, each = pressure_start))
  names(pressure_df_model) <- initial_column_names
  
  # Create lagged columns for each initial column
  # lagged_cols_list <-
  #   lapply(initial_column_names, function(col_name) {
  #     new_col_values <-
  #       rep(unname(initial_values[col_name]), pressure_start)
  #     col_names <- paste0(col_name, "_", 1:length(column_lags))
  #     data.frame(setNames(rep(list(new_col_values), length(column_lags)), col_names))
  #   })
  
  
  for (col_name in 1:length(initial_column_names)) {
    new_col_values <-
      rep(unname(initial_values[col_name]), pressure_start)
    col_names <-
      paste0(initial_column_names[col_name], "_", 1:initial_columns_lags[col_name])
    if (col_name == 1) {
      lagged_cols_list <-
        data.frame(setNames(rep(
          list(new_col_values), initial_columns_lags[col_name]
        ), col_names))
    }
    else {
      lagged_cols_list <-
        cbind(lagged_cols_list, data.frame(setNames(
          rep(list(new_col_values), initial_columns_lags[col_name]), col_names
        )))
      
    }
    
  }
  
  
  # Create lagged columns for the prediction column
  new_prediction_col_values <-
    rep(prediction_initial_value, pressure_start)
  col_name <- paste0(prediction_col_name, "_", 1:predicted_column_lags)
  lagged_prediction_col_values <-
    data.frame(setNames(rep(
      list(new_prediction_col_values), predicted_column_lags
    ), col_name))
  
  # Combine the lagged columns into the pressure_df_model dataframe
  pressure_df_model <- pressure_df_model %>%
    bind_cols(lagged_cols_list) %>%
    bind_cols(lagged_prediction_col_values)
  
  # Initialize predictions data pressure
  predictions_data_pressure <-
    rep(prediction_initial_value, pressure_start)
  
  # Create a sequence of indices for pressure_count
  pressure_count_seq <-
    seq(from = pressure_start + 1, to = prediction_size)
  
  # Process each pressure_count
  process_pressure_count <- function(pressure_count) {
    new_pressure <-
      data.frame(name = pressure_signal_df[pressure_count,])
    names(new_pressure) <- initial_column_names[1]
    
    # Add predicted value to the new_pressure dataframe
    new_pressure <-
      cbind(new_pressure, setNames(
        data.frame(name = tail(predictions_data_pressure, 1)),
        paste0(prediction_col_name, "_1")
      ))
    
    # Incorporate prior observations for additional column values
    new_pressure <-
      cbind(new_pressure, lapply(initial_column_names, function(col_name) {
        setNames(data.frame(name = tail(pressure_df_model[col_name], 1)), paste0(col_name, "_1"))
      }))
    
    # if (column_lags >= 2) {
    #   # Generate previous remaining values for each column and lag
    #   new_columns <-
    #     unlist(lapply(column_names, function(col_name) {
    #       lapply(2:column_lags, function(col_lag) {
    #         lagged_col_name <- paste0(col_name, "_", (col_lag - 1))
    #         setNames(data.frame(name = tail(pressure_df_model[lagged_col_name], 1)),
    #                  paste0(col_name, "_", col_lag))
    #       })
    #     }), recursive = FALSE)
    #   
    #   # Append new columns with previous values to new_pressure
    #   new_pressure <-
    #     cbind(new_pressure, do.call(cbind, new_columns))
    #   
    # }
    
    for (col_name in 1:length(initial_column_names)) {
      if (initial_columns_lags[col_name] >= 2) {
        for (col_lag in 2:initial_columns_lags[col_name]) {
          lagged_col_name <-
            paste0(initial_column_names[col_name], "_", (col_lag - 1))
          new_pressure <-
            cbind(new_pressure, setNames(
              data.frame(name = tail(pressure_df_model[lagged_col_name], 1)),
              paste0(initial_column_names[col_name], "_", col_lag)
            ))
        }
      }
    }
    
    if (predicted_column_lags >= 2) {
      for (col_lag in 2:predicted_column_lags) {
        lagged_col_name <-
          paste0(prediction_col_name, "_", (col_lag - 1))
        new_pressure <-
          cbind(new_pressure, setNames(
            data.frame(name = tail(pressure_df_model[lagged_col_name], 1)),
            paste0(prediction_col_name, "_", col_lag)
          ))
      }
    }
    
    # Add the new row to pressure_df_model and reset row names
    pressure_df_model <<- rbind(pressure_df_model, new_pressure)
    row.names(pressure_df_model) <<- NULL
    
    # Perform prediction and store the result
    predictions_data_pressure <<-
      predict(SVR_model, pressure_df_model)
    
    # Save signal predicted value
    predicted_values <<-
      rbind(predicted_values, tail(predictions_data_pressure, 1))
    
    # Lapply requires a return value
    return(NULL)
  }
  
  # Using lapply to iterate over pressure_count_seq and process each pressure_count
  invisible(lapply(pressure_count_seq, process_pressure_count))
  
  #print(pressure_df_model)
  #print(predicted_values)
  return(predicted_values)
}


#' Process a Signal
#'
#' This function processes a signal based on specified criteria.
#'
#' @param signal A numeric vector representing the signal.
#' @param pressure_start The starting point of the pressure.
#' @return A numeric score indicating the quality of the signal processing.
#'
#' @details
#' This function applies both basic and advanced filters to evaluate the quality of a signal.
#' The basic filter checks for specific conditions related to peaks, minimums, variance, and range.
#' The advanced filter performs additional checks related to stabilization and drop rates.
#'
#' @examples
#' signal <- c(0.2, 0.3, 0.5, 0.8, 1.0, 0.7, 0.4, 0.2, 0.1, -0.1, -0.3, -0.5,
#'             -0.6, -0.4, -0.2, 0.1, 0.3, 0.6, 0.9, 1.1, 0.9, 0.7, 0.5, 0.3)
#' pressure_start <- 5
#' score <- process_signal(signal, pressure_start)
#'
process_signal <- function(signal, pressure_start) {
  # Basic filter:
  ## 1. Global minimum (peak) between 3 and 9 seconds after the drop occurs
  ## 2. Minimum peak of the signal between -0.2 and 0.5
  ## 3. Variance < 0.002 between seconds 15 and 30 (stabilization tail)
  ## 4. Maximum and minimum of the entire signal between -0.2 and 1.2 (exclusive)
  
  peak_signal <- signal[(pressure_start + 3):(pressure_start + 9)]
  stable_signal <-
    signal[(pressure_start + 15):(pressure_start + 30)]
  drop_signal <- signal[(pressure_start + 9):(pressure_start+15)]
  
  min_signal <- min(signal)
  
  max_drop_signal <- max(drop_signal)
  
  if (!(min_signal %in% peak_signal) ||
      #(max_drop_signal < 0.5) ||
      !(min_signal >= -0.2 && min_signal <= 0.5) ||
      var(stable_signal) > 0.002 ||
      !(max(signal) < 1.2 && min(signal) > -0.2)) {
    return(-10)
  }
  
  score <- 10
  
  # Advanced filter:
  ## 1. Stabilization phase is not strictly increasing or decreasing,
  ## penalize with the distance between the maximum value and the last value of the signal, multiplied by 100
  peak_stable_distance <-
    abs(max(stable_signal) - tail(stable_signal, 1))
  score <- score - (peak_stable_distance * 100)
  
  ## 2. Drop before stabilization is at most 45% of the signal's rising section.
  ## If not met, penalize with a factor of 10.
  stable_peak <- max(signal[15:30]) - head(peak_signal, 1)
  drop_peak <- max(signal[15:30]) - min(peak_signal)
  
  if (stable_peak > (drop_peak * 0.45)) {
    score <- score - (drop_peak * 10)
  }
  
  ## 3. Signal stabilization occurs at the same level as the drop peak.
  ## Penalize based on the distance between these two points with a factor of 10.
  if (min(stable_signal) < min_signal) {
    score <- score - ((min_signal - min(stable_signal)) * 10)
  }
  
  return(score)
}
