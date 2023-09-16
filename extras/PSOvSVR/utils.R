#' Normalize Selected Columns in a Data Frame Using Min-Max Scaling
#'
#' @param df Data frame containing the columns to be normalized.
#' @param signal_names Character vector specifying which columns should be normalized.
#'
#' @return Data frame with normalized columns added.
#'
#' @examples
#' df <- data.frame(Time = 1:10, Signal1 = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1), Signal2 = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' normalized_df <- normalize_signals_by_name(df, c("Signal1", "Signal2"))
normalize_signals_by_name <- function(df, signal_names) {
  # Validation
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    stop("Input data frame should not be empty or NULL.")
  }
  
  if (is.null(signal_names) || length(signal_names) == 0) {
    stop("Signal names should not be empty or NULL.")
  }
  
  if (!all(signal_names %in% colnames(df))) {
    stop("Some specified signal names are not in the data frame.")
  }
  
  # Normalization
  df <- df %>%
    dplyr::mutate(across(all_of(signal_names), ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)), .names = "{col}_norm"))
  
  return(df)
}

#' Normalize All Columns in a Data Frame Using Min-Max Scaling
#'
#' @param df Data frame containing the columns to be normalized.
#'
#' @return Data frame with normalized columns added.
#'
#' @examples
#' df <- data.frame(Time = 1:10, Signal1 = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1), Signal2 = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' normalized_df <- normalize_all_signals(df)
normalize_all_signals <- function(df) {
  # Validation
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    stop("Input data frame should not be empty or NULL.")
  }
  
  # Normalization
  df <- df %>%
    dplyr::mutate(across(everything(), ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)), .names = "{col}_norm"))
  
  return(df)
}


#' Add Lagged Columns to a Normalized Data Frame
#'
#' This function adds lagged columns for specified normalized columns in a data frame.
#' The columns to be lagged should be already normalized and have "_norm" at the end of their names.
#'
#' @param data_frame Data frame to which lagged columns will be added.
#' @param lags Integer specifying the number of lags for each column.
#' @param df_col_names Character vector containing names of columns to be lagged.
#'
#' @return A modified data frame with lagged columns and rows with NA values removed.
#'
#' @examples
#' df <- data.frame(A_norm = c(0, 0.2, 0.4, 0.6, 0.8, 1))
#' lagged_df <- lag_normalized_signals(df, 2, c("A_norm"))
#' print(lagged_df)
lag_normalized_signals <- function(data_frame, lags, df_col_names) {
  # Validation
  if (is.null(data_frame) ||
      nrow(data_frame) == 0 || ncol(data_frame) == 0) {
    stop("Input data frame should not be empty or NULL.")
  }
  
  if (is.null(df_col_names) || length(df_col_names) == 0) {
    stop("Column names should not be empty or NULL.")
  }
  
  if (!all(grepl("_norm$", df_col_names))) {
    stop("All column names should end with '_norm' to indicate they are normalized.")
  }
  
  if (!all(df_col_names %in% colnames(data_frame))) {
    stop("Some specified column names are not in the data frame.")
  }
  
  if (lags < 1) {
    stop("Number of lags should be greater than or equal to 1.")
  }
  
  # Add lagged columns
  for (col in df_col_names) {
    for (lag in 1:lags) {
      lagged_col_name <- paste0(col, "_", lag)
      data_frame[[lagged_col_name]] = dplyr::lag(data_frame[[col]], lag)
    }
  }
  
  return(na.omit(data_frame))
}

#' Add Lagged Columns to All Normalized Columns in a Data Frame
#'
#' This function adds lagged columns for all normalized columns in a data frame.
#' All columns to be lagged should be already normalized and have "_norm" at the end of their names.
#'
#' @param data_frame Data frame to which lagged columns will be added.
#' @param lags Integer specifying the number of lags for each column.
#'
#' @return A modified data frame with lagged columns and rows with NA values removed.
#'
#' @examples
#' df <- data.frame(A_norm = c(0, 0.2, 0.4, 0.6, 0.8, 1))
#' lagged_df <- lag_all_signals(df, 2)
#' print(lagged_df)
lag_all_signals <- function(data_frame, lags) {
  # Validation
  if (is.null(data_frame) ||
      nrow(data_frame) == 0 || ncol(data_frame) == 0) {
    stop("Input data frame should not be empty or NULL.")
  }
  
  if (lags < 1) {
    stop("Number of lags should be greater than or equal to 1.")
  }
  
  # Add lagged columns
  for (col in colnames(data_frame)) {
    if (grepl("_norm$", col)) {
      for (lag in 1:lags) {
        lagged_col_name <- paste0(col, "_", lag)
        data_frame[[lagged_col_name]] = dplyr::lag(data_frame[[col]], lag)
      }
    }
  }
  
  return(na.omit(data_frame))
}

#' Generate and Add a Smoothed Pressure Step to a Signal
#'
#' This function creates a smoothed pressure step using a Butterworth filter
#' and returns the result as a data frame. The output signal starts with a zero value
#' up to `pressure_start` and then steps down to -1, smoothed using the Butterworth filter.
#'
#' @param pressure_start Integer indicating the index at which the pressure step starts.
#' @param signal_end Integer indicating the index at which the pressure step ends.
#' @param butter_order Integer representing the order of the Butterworth filter.
#' @param butter_fs Numeric value representing the sampling frequency for the Butterworth filter.
#'
#' @return A data frame containing a single column, `signal`, with the smoothed pressure step.
#'
#' @examples
#' pressure_step <- add_pressure_step(pressure_start = 10, signal_end = 100,
#'                                    butter_order = 2, butter_fs = 50)
#' plot(pressure_step$signal)
#'
add_pressure_step <-
  function(pressure_start,
           signal_end,
           butter_order,
           butter_fs) {
    # Validation
    if (!is.numeric(pressure_start) || pressure_start < 0) {
      stop("pressure_start should be a non-negative numeric value.")
    }
    
    if (!is.numeric(signal_end) || signal_end <= pressure_start) {
      stop("signal_end should be a numeric value greater than pressure_start.")
    }
    
    if (!is.numeric(butter_order) || butter_order < 1) {
      stop("butter_order should be a positive integer.")
    }
    
    if (!is.numeric(butter_fs) || butter_fs <= 0) {
      stop("butter_fs should be a positive numeric value.")
    }
    
    # Generate the pressure step
    pressure <-
      c(rep(0, pressure_start), rep(-1, signal_end - pressure_start))
    
    # Create the Butterworth filter
    butter_filter <-
      signal::butter(butter_order, butter_fs, type = "low")
    
    # Apply the filter to the pressure step
    pressure_step_smooth <-
      as.numeric(signal::filter(butter_filter, pressure)) + 1
    
    # Return the result as a data frame
    return(data.frame(signal = pressure_step_smooth))
  }

#' Generate Time-series Model Data
#'
#' This function constructs a dataset tailored for training or predicting with a time-series model.
#' It selects specified columns and incorporates their lagged values.
#'
#' @param input_df Dataframe containing the raw data.
#' @param data_cols Character vector of column names to include in the training set.
#' @param predictor_cols Character vector of column names to include in the prediction set.
#' @param lagged_cols Character vector specifying which columns should have lagged values.
#' @param lag_values Integer vector specifying the number of lags for each lagged column. Should be in the same order as 'lagged_cols'.
#' @param is_training Boolean indicating whether the data is for training (TRUE) or prediction (FALSE).
#'
#' @return Dataframe containing the specified columns and their lagged values.
#'
#' @examples
#' # Generate training set
#' training_data <- generate_time_series_data(input_df = my_df, data_cols = c("feature1", "feature2"), predictor_cols = NULL, lagged_cols = c("feature1", "feature2"), lag_values = c(3, 3), is_training = TRUE)
#'
#' # Generate prediction set
#' prediction_data <- generate_time_series_data(input_df = my_df, data_cols = NULL, predictor_cols = c("feature3", "feature4"), lagged_cols = c("feature3", "feature4"), lag_values = c(2, 2), is_training = FALSE)

generate_time_series_data <-
  function(input_df,
           data_cols,
           predictor_cols,
           lagged_cols,
           lag_values,
           is_training) {
    # Validation checks
    if (is.null(input_df))
      stop("The input dataframe cannot be NULL.")
    if (is_training &&
        is.null(data_cols))
      stop("Please specify target columns for training.")
    if (!is_training &&
        is.null(predictor_cols))
      stop("Please specify predictor columns for prediction.")
    if (is.null(lagged_cols))
      stop("Please specify lagged columns.")
    if (length(lagged_cols) != length(lag_values))
      stop("The lengths of 'lagged_cols' and 'lag_values' must match.")
    
    # Create a new dataframe based on whether it is for training or prediction
    new_df <- if (is_training) {
      input_df %>% dplyr::select(dplyr::all_of(data_cols))
    } else {
      input_df %>% dplyr::select(dplyr::all_of(predictor_cols))
    }
    
    # Add lagged columns
    for (i in seq_along(lagged_cols)) {
      lag_cols <- paste0(lagged_cols[i], "_", seq_len(lag_values[i]))
      lag_df <- input_df %>% dplyr::select(dplyr::all_of(lag_cols))
      new_df <- dplyr::bind_cols(new_df, lag_df)
    }
    
    return(new_df)
  }



#' Filter Columns in a Data Frame by Names
#'
#' This function returns a subset of the given data frame containing only the specified columns.
#' It validates if the specified column names exist in the data frame before filtering.
#'
#' @param df A data frame to filter.
#' @param signal_names A character vector specifying the names of the columns to include in the output.
#'
#' @return A data frame containing only the specified columns. Returns NULL if any of the specified columns are not found.
#'
filter_signals_dataframe <- function(df, signal_names) {
  # Validate input arguments
  if (is.null(signal_names) || length(signal_names) == 0) {
    stop("Column names are empty.")
  }
  
  if (!all(signal_names %in% colnames(df))) {
    stop(
      "One or more specified column names do not exist in the data frame, or data frame it's empty."
    )
  }
  
  return(df[, signal_names, drop = FALSE])
}

#' Exclude Columns in a Data Frame by Names
#'
#' This function returns a subset of the given data frame excluding the specified columns.
#' It validates if the specified column names exist in the data frame before exclusion.
#'
#' @param df A data frame to filter.
#' @param signal_names A character vector specifying the names of the columns to exclude from the output.
#'
#' @return A data frame without the specified columns. Returns NULL if any of the specified columns are not found.
#'
exclude_signals_dataframe <- function(df, signal_names) {
  # Validate input arguments
  if (is.null(signal_names) || length(signal_names) == 0) {
    stop("Column names are empty.")
  }
  
  if (!all(signal_names %in% colnames(df))) {
    stop(
      "One or more specified column names do not exist in the data frame, or data frame it's empty."
    )
  }
  
  return(df[,!(colnames(df) %in% signal_names), drop = FALSE])
}


#' Process a Data Frame to Exclude, Normalize, and Lag Signals
#'
#' This function processes a given data frame to exclude specified columns,
#' normalize all columns, and add lagged versions of the signals.
#'
#' @param df A data frame containing the signals to be processed.
#' @param excluded_cols A character vector specifying the columns to be excluded.
#' @param lags An integer specifying the number of lags to add for each signal.
#' @param signals A character vector specifying the signals to be excluded after normalization.
#' @param lagged_signals A character vector specifying the signals to be lagged. If NULL, all signals will be lagged.
#'
#' @return A new data frame with specified columns excluded, all columns normalized,
#'         and lagged versions of the signals added. Rows with resulting NA values from lags are omitted.
#'
#' @examples
#' df <- data.frame(Time = 1:10, A = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1), B = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' processed_df <- process_dataframe(df, excluded_cols = c("Time"), lags = 2, signals = c("A", "B"))
#'
process_dataframe <-
  function(df,
           excluded_cols,
           lags,
           signals,
           lagged_signals = NULL) {
    # Validate input arguments
    if (!is.data.frame(df) || ncol(df) < 1) {
      stop("Input 'df' should be a non-empty data frame.")
    }
    
    if (!is.numeric(lags) || lags <= 0) {
      stop("Input 'lags' should be a positive integer.")
    }
    
    # Exclude specified columns
    new_df <- exclude_signals_dataframe(df, excluded_cols)
    
    # Normalize all columns
    new_df <- normalize_all_signals(new_df)
    
    # Exclude specified signals after normalization
    new_df <- exclude_signals_dataframe(new_df, signals)
    
    # Add lagged signals
    if (is.null(lagged_signals)) {
      new_df <- lag_all_signals(new_df, lags)
    }
    else {
      new_df <- lag_normalized_signals(new_df, lags, lagged_signals)
    }
    
    return(new_df)
  }
