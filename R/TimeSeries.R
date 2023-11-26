#' Generate and Add a Smoothed Pressure Step to a Signal
#'
#' This function generates a smoothed pressure step using a Butterworth filter.
#' The output signal begins with a value of zero up to `pressure_start` and then
#' transitions to -1, which is then smoothed using the Butterworth filter.
#' The smoothed pressure step is adjusted to start from 1 after filtering.
#'
#' @param pressure_start (Optional) Integer indicating the index at which the
#'  pressure step starts. Defaults to 3L.
#' @param signal_end (Optional) Integer indicating the length of the signal,
#'  including the pressure step. Defaults to 40L.
#' @param butter_order (Optional) Integer representing the order of the
#'  Butterworth filter. Defaults to 2L.
#' @param butter_fs (Optional) Numeric value representing the sampling frequency
#'  for the Butterworth filter. Defaults to 0.2.
#'
#' @return A data frame containing a single column, `signal`,
#'  with the smoothed pressure step.
#'
#' @examples
#' pressure_step <- add_pressure_step(pressure_start = 10L, signal_end = 100L,
#'                                    butter_order = 2L, butter_fs = 0.2)
#' plot(pressure_step$signal, type = 'l', main = 'Smoothed Pressure Step')
#'
#' @importFrom signal butter filter
#' @export
#'
add_pressure_step <-
  function(pressure_start = 3L,
           signal_end = 40L,
           butter_order = 2L,
           butter_fs = 0.2) {
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
#' This function constructs a dataset tailored for training or predicting with
#' a time-series model.
#' It selects specified columns and incorporates their lagged values based on
#' the training or prediction requirement.
#'
#' @param input_df Dataframe containing the raw data.
#' @param data_cols Character vector of column names to include in the training
#'  set.
#' @param predictor_cols Character vector of column names to include in the
#'  prediction set.
#' @param lagged_cols Character vector specifying which columns should have
#'  lagged values.
#' @param lag_values Integer vector specifying the number of lags for each
#'  lagged column.
#'                   Should be in the same order as 'lagged_cols'.
#' @param is_training Boolean indicating whether the data is for training (TRUE)
#'  or prediction (FALSE).
#' @param vsvr_response Character representing the response column name for the
#'  SVR model.
#'
#' @return Dataframe containing the specified columns and their lagged values,
#'  adjusted for training or prediction.
#'
#' @examples
#' \dontrun{
#'   generate_time_series_data(...)
#' }
#'
#' @importFrom dplyr select bind_cols all_of
#' @importFrom magrittr %>%
#'
#' @export
#'
generate_time_series_data <-
  function(input_df,
           data_cols,
           predictor_cols,
           lagged_cols,
           lag_values,
           is_training,
           vsvr_response) {
    # Validation checks
    stopifnot(is.data.frame(input_df))
    
    # Create a new dataframe based on whether it is for training or prediction
    new_df <- if (is_training) {
      input_df %>% dplyr::select(dplyr::all_of(data_cols))
    } else {
      input_df %>% dplyr::select(dplyr::all_of(predictor_cols))
    }
    
    # Add lagged columns
    for (i in seq_along(lagged_cols)) {
      lag_cols <- paste0(lagged_cols[i], "_", seq_len(lag_values[i]))
      if (all(lag_cols %in% names(input_df))) {
        lag_df <- input_df %>% dplyr::select(dplyr::all_of(lag_cols))
        new_df <- dplyr::bind_cols(new_df, lag_df)
      } else if (lag_values[i] != 0) {
        stop(
          paste(
            "Error in dplyr::all_of(): Columns that you're trying to subset don't exist.",
            paste(lag_cols, collapse = ", ")
          )
        )
      }
    }
    
    if (!is_training && (vsvr_response %in% colnames(new_df)))
      new_df = exclude_signals_dataframe(df = new_df, signal_names = vsvr_response)
    
    return(new_df)
  }

#' Process a Data Frame to Exclude, Normalize, and Lag Signals
#'
#' This function processes a given data frame to exclude specified columns,
#' normalize selected columns, and add lagged versions of the signals. It first
#' excludes specified columns, then normalizes the signals, and finally adds
#' lagged versions. Note: The exclusion of signals post-normalization might be
#' redundant and is subject to review in future updates.
#'
#' @param df A data frame containing the signals to be processed. Providing an
#'  empty data frame is not practical for processing; hence, it's recommended
#'   to always provide this argument.
#' @param excluded_cols A character vector specifying the columns to be excluded.
#'  Defaults to NULL, meaning no columns are excluded if not specified.
#' @param lags An integer specifying the number of lags to add for each signal.
#'  Defaults to NULL, implying no lags are added unless specified.
#' @param signals A character vector specifying the signals to be normalized.
#'  This argument is mandatory.
#' @param lagged_signals A character vector specifying the signals to be lagged.
#'  If NULL, all signals will be lagged. Defaults to NULL. The function appends
#'  a '_norm' suffix to the signal names if not already present, assuming
#'  normalized signals follow this naming convention.
#'
#' @return A new data frame with specified columns excluded, selected columns
#'  normalized, and lagged versions of the signals added. Rows with resulting
#'  NA values from lags are not explicitly handled and should be managed in
#'  subsequent steps of analysis.
#'
#' @examples
#' df <- data.frame(Time = 1:10, A = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1),
#'                  B = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' processed_df <- process_dataframe(df = df,
#'                                   excluded_cols = c("Time"),
#'                                   lags = 2,
#'                                   signals = c("A", "B"))
#' @export
#'

process_dataframe <-
  function(df = data.frame(),
           excluded_cols = NULL,
           lags = NULL,
           signals,
           lagged_signals = NULL) {
    # Validate input arguments
    stopifnot(is.data.frame(df))
    
    # Exclude specified columns
    new_df <-
      exclude_signals_dataframe(df = df, signal_names = excluded_cols)
    
    # Normalize all columns
    new_df <-
      normalize_signals_by_name(df = new_df, signal_names = signals)
    
    # Exclude specified signals after normalization
    new_df <-
      exclude_signals_dataframe(df = new_df, signal_names = signals)
    
    # Add lagged signals
    if (is.null(lagged_signals)) {
      new_df <- lag_all_signals(data_frame = new_df, lags = lags)
    }
    else {
      # If lagged_signals doesn't end with "_norm", add the "_norm" suffix
      lagged_signals <-
        ifelse(
          !grepl("_norm$", lagged_signals),
          paste0(lagged_signals, "_norm"),
          lagged_signals
        )
      new_df <-
        lag_normalized_signals(data_frame = new_df,
                               lags = lags,
                               df_col_names = lagged_signals)
    }
    
    return(new_df)
  }
