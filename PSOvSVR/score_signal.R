########################## Time Series Signal Prediction Generator #############
# Description: This function generates predictions for a response signal using a pre-trained
# Support Vector Regression (SVR) model.
#
# Inputs:
## SVR_model (e1071 SVR model): A pre-trained SVR model used for making predictions. It should have been trained using the same variables as the column names.
## pressure_signal_df (Dataframe): Dataframe containing the pressure signal. It should have the same size as the desired prediction size.
## pressure_start (Numeric): The starting point of the pressure signal decrease. It should correspond to the same starting point in the pressure signal.
## prediction_size (Numeric): The number of predictions to generate.
## column_names (Character vector): Names of variables in the dataframe, including all variables.
## column_lags (Numeric): The number of lags for the variables.
## initial_column_names (Character vector): Names of variables excluding the predictive variable.
## initial_column_values (Numeric vector): Initial values of the previous variables. This vector should have the same dimensions as the initial_column_names.
## prediction_col_name (Character): The name of the predictive variable.
## prediction_initial_value (Numeric): The initial value of the predictive variable.
#
# Outputs:
## predicted_values (Dataframe): Dataframe containing the predicted signal.

generate_signal_response_predictions <- function(SVR_model,
                                                 pressure_signal_df,
                                                 pressure_start,
                                                 prediction_size,
                                                 column_names,
                                                 column_lags,
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
  lagged_cols_list <-
    lapply(initial_column_names, function(col_name) {
      new_col_values <-
        rep(unname(initial_values[col_name]), pressure_start)
      col_names <- paste0(col_name, "_", 1:column_lags)
      data.frame(setNames(rep(list(new_col_values), column_lags), col_names))
    })
  
  # Create lagged columns for the prediction column
  new_prediction_col_values <-
    rep(prediction_initial_value, pressure_start)
  col_name <- paste0(prediction_col_name, "_", 1:column_lags)
  lagged_prediction_col_values <-
    data.frame(setNames(rep(
      list(new_prediction_col_values), column_lags
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
      data.frame(name = pressure_signal_df[pressure_count, ])
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
    
    # Generate previous remaining values for each column and lag
    new_columns <- unlist(lapply(column_names, function(col_name) {
      lapply(2:column_lags, function(col_lag) {
        lagged_col_name <- paste0(col_name, "_", (col_lag - 1))
        setNames(data.frame(name = tail(pressure_df_model[lagged_col_name], 1)),
                 paste0(col_name, "_", col_lag))
      })
    }), recursive = FALSE)
    
    # Append new columns with previous values to new_pressure
    new_pressure <- cbind(new_pressure, do.call(cbind, new_columns))
    
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
  
  print(pressure_df_model)
  print(predicted_values)
  return(predicted_values)
}


process_signal <- function(signal, pressure_start) {
  response_signal <- signal[((pressure_start+1):30)]
  peak_signal <- signal[(pressure_start + 1):(pressure_start + 10)]
  stable_signal <-
    signal[(20:30)]
  min_response_signal <- min(response_signal)
  
  if (!(min_response_signal %in% peak_signal) ||
      !(min_response_signal >= -.2 &&
        min_response_signal <= .5) ||
      var(stable_signal) > .002 ||
      !(max(signal) < 1.2)) {
    return(0)
  }
  
  score <- 10
  
  max_peak_signal <-
    max(signal[(pressure_start + 18):(pressure_start + 30)])
  peak_stable_distance <-
    abs(max_peak_signal - tail(stable_signal, 1))
  score <- score - (peak_stable_distance * 100)
  
  stable_peak <-
    max(signal[(pressure_start + 18):(pressure_start + 30)]) - tail(stable_signal, 1)
  drop_peak <-
    max(signal[(pressure_start + 18):(pressure_start + 30)]) - min(peak_signal)
  
  if (stable_peak > (drop_peak * .45)) {
    score <- score - (drop_peak * 10)
  }
  
  if (min(stable_signal) < min_response_signal) {
    score <- score - ((min_response_signal - min(stable_signal)) * 10)
  }
  
  return(score)
}