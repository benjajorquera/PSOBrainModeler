#' Normalize Signals by Name in a Data Frame
#'
#' This function normalizes the specified signals in a data frame using min-max scaling.
#'
#' @param df A data frame containing the signals to be normalized.
#' @param signal_names A character vector of signal names to be normalized.
#'
#' @return A modified data frame with normalized signals added as new columns.
#'
#' @examples
#' df <- data.frame(Time = 1:10, Signal1 = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1), Signal2 = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' normalized_df <- normalize_signals(df, c("Signal1", "Signal2"))
#'
normalize_signals_by_name <- function(df, signal_names) {
  df_list <- lapply(signal_names, function(signal_name) {
    # Extract the signal to be normalized
    signal <- df[[signal_name]]
    
    # Perform min-max normalization
    signal_norm <-
      (signal - min(signal, na.rm = TRUE)) / (max(signal, na.rm = TRUE) - min(signal, na.rm = TRUE))
    
    # Add the normalized signal as a new column in the data frame
    df[[paste0(signal_name, "_norm")]] <<- signal_norm
  })
  
  return(df)
}

#' Normalize All Signals in a Data Frame
#'
#' This function normalizes all signals (columns data) in a data frame using min-max scaling.
#'
#' @param df A data frame containing the signals to be normalized.
#'
#' @return A modified data frame with normalized signals added as new columns.
#'
#' @examples
#' df <- data.frame(Time = 1:10, Signal1 = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1), Signal2 = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' normalized_df <- normalize_signals(df)
#'
normalize_all_signals <- function(df) {
  df_list <- lapply(colnames(df), function(signal) {
    # Perform min-max normalization
    signal_norm <-
      (df[signal] - min(df[signal], na.rm = TRUE)) / (max(df[signal], na.rm = TRUE) - min(df[signal], na.rm = TRUE))
    
    # Add the normalized signal as a new column in the data frame
    df[paste0(signal, "_norm")] <<- signal_norm
  })
  
  return(df)
}



#' Add Lagged Columns to a Normalized Data Frame
#'
#' This function adds lagged value columns to a data frame for specified columns.
#'
#' @param data_frame A data frame to which lagged columns will be added.
#' @param lags An integer specifying the number of lags to consider for adding lagged values.
#' @param df_col_names A character vector containing the names of normalized columns in the data frame
#'                    for which lagged columns will be added.
#'
#' @return A new data frame with lagged columns added. Rows with resulting NA values from lags
#'         are omitted from the final result.
#'
#' @examples
#' data <- data.frame(A = c(1, 2, 3, 4, 5))
#' lagged_data <- lag_signal(data, 2, "A")
#' print(lagged_data)
#'
lag_normalized_signals <-
  function(data_frame, lags, df_col_names) {
    for (col in df_col_names) {
      col_norm_name <- paste0(col, "_norm")
      col_data <- data_frame[[col_norm_name]]
      for (lag in 1:lags) {
        col_name <- paste(col, lag, sep = "_norm_")
        lagged_values <-
          c(rep(NA, lag), col_data[1:(length(col_data) - lag)])
        data_frame[[col_name]] <- lagged_values
      }
    }
    
    return(na.omit(data_frame))
  }

lag_all_signals <- function(data_frame, lags) {
  for (col in colnames(data_frame)) {
    col_data <- data_frame[col]
    if (nrow(col_data) < lags) {
      print("Number of rows less than number of lags")
      return()
    }
    for (lag in 1:lags) {
      col_name <- paste(col, lag, sep = "_")
      lagged_values <-
        c(rep(NA, lag), col_data[(1:(nrow(col_data) - lag)),])
      data_frame[col_name] <- lagged_values
    }
  }
  
  return(na.omit(data_frame))
}

#' Add Smoothed Pressure Step to Signal
#'
#' This function generates a smoothed pressure step and returns it as a data frame.
#'
#' @param pressure_start The index at which the pressure step starts.
#' @param signal_end The index at which the pressure step ends.
#' @param butter_order The order of the Butterworth filter.
#' @param butter_fs The sampling frequency for the Butterworth filter.
#'
#' @return A data frame containing the smoothed pressure step as a signal column.
#'
#' @examples
#' pressure_step <- add_pressure_step(pressure_start = 10, signal_end = 100,
#'                                    butter_order = 2, butter_fs = 50)
#' plot(pressure_step)
#'
add_pressure_step <- function(pressure_start,
                              signal_end,
                              butter_order,
                              butter_fs) {
  # Generate the pressure step
  pressure <-
    c(rep(0, pressure_start), rep(-1, signal_end - pressure_start))
  
  # Create the Butterworth filter
  butterSignal <- butter(butter_order, butter_fs, type = "low")
  
  # Apply the filter to the pressure step
  pressure_step_smooth <-
    as.numeric(signal::filter(butterSignal, pressure)) + 1
  
  # Return the result as a data frame
  data.frame(signal = pressure_step_smooth)
}


#' Generate Model Data
#'
#' This function generates a dataset suitable for training a time-series model by selecting columns of interest and adding lagged values.
#'
#' @param dataframe The input dataframe containing the data.
#' @param col_names A character vector of column names to be included in the output dataset when 'training' is TRUE.
#' @param predictors A character vector of column names to be included in the output dataset when 'training' is FALSE.
#' @param lags The number of lagged values to include in the dataset.
#' @param training A logical value indicating whether the data is for training (TRUE) or prediction (FALSE).
#'
#' @return A new dataframe containing the selected columns and lagged values.
#'
#' @examples
#' # Generate training data with 'col_names' and 3 lags
#' train_data <- generate_model_data(dataframe = my_data, col_names = c("feature1", "feature2"), predictors = NULL, lags = 3, training = TRUE)
#'
#' # Generate prediction data with 'predictors' and 2 lags
#' prediction_data <- generate_model_data(dataframe = my_data, col_names = NULL, predictors = c("feature3", "feature4"), lags = 2, training = FALSE)
#'
generate_model_data <-
  function(dataframe,
           col_names,
           predictors,
           lags,
           training) {
    if (training) {
      new_df <- dataframe %>% select(all_of(col_names))
    } else {
      new_df <- dataframe %>% select(all_of(predictors))
    }
    
    for (col_number in 1:length(col_names)) {
      lag_cols <-
        paste(rep(col_names[col_number], each = lags[col_number]),
              "_",
              rep(1:lags[col_number]))
      lag_cols <- gsub(" ", "", lag_cols)
      lags_df <- dataframe %>% select(all_of(lag_cols))
      new_df <- cbind(new_df, lags_df)
    }
    
    return(new_df)
  }


filter_signals_dataframe <- function(df, signal_names) {
  if (is.null(signal_names)) {
    cat("Nombre de columnas vacío.\n")
    return()
  }
  # Validar si los nombres de columnas en columnas_deseadas existen en el DataFrame
  if (all(signal_names %in% colnames(df))) {
    # Todos los nombres de columnas existen en el DataFrame
    return(df[, signal_names])
  } else {
    # Al menos un nombre de columna no existe en el DataFrame
    cat("Alguno de los nombres de columna no existe en el DataFrame.\n")
  }
}

exclude_signals_dataframe <- function(df, signal_names) {
  if (is.null(signal_names)) {
    cat("Nombre de columnas vacío.\n")
    return()
  }
  # Validar si los nombres de columnas en columnas_deseadas existen en el DataFrame
  if (all(signal_names %in% colnames(df))) {
    # Todos los nombres de columnas existen en el DataFrame
    return(df[, !(names(df) %in% signal_names)])
  } else {
    # Al menos un nombre de columna no existe en el DataFrame
    cat("Alguno de los nombres de columna no existe en el DataFrame.\n")
  }
}

process_dataframe <- function(df, excluded_cols, lags, signals) {
  new_df <- exclude_signals_dataframe(df, excluded_cols)
  new_df <- normalize_all_signals(new_df)
  new_df <- exclude_signals_dataframe(new_df, signals)
  new_df <- lag_all_signals(new_df, lags)
  return(new_df)
}