#' Normalize Signals in a Data Frame
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
normalize_signals <- function(df, signal_names) {
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
#' @importFrom dplyr select
#' @importFrom stats lag
#'
#' @export
lag_normalized_signal <- function(data_frame, lags, df_col_names) {
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
#' @importFrom signal filter
#' @importFrom stats filter butter
#'
#' @export
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
