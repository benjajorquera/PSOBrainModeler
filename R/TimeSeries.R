#' Generate and Add a Smoothed Pressure Step to a Signal
#'
#' This function creates a smoothed pressure step using a Butterworth filter
#' and returns the result as a data frame. The output signal starts with a zero value
#' up to `pressure_start` and then steps down to -1, smoothed using the Butterworth filter.
#'
#' @param pressure_start (Optional) Integer indicating the index at which the pressure step starts. Defaults to 3L.
#' @param signal_end (Optional) Integer indicating the index at which the pressure step ends. Defaults to 40L.
#' @param butter_order (Optional) Integer representing the order of the Butterworth filter. Defaults to 2L.
#' @param butter_fs (Optional) Numeric value representing the sampling frequency for the Butterworth filter. Defaults to 0.2.
#'
#' @return A data frame containing a single column, `signal`, with the smoothed pressure step.
#'
#' @examples
#' pressure_step <- add_pressure_step(pressure_start = 10L, signal_end = 100L,
#'                                    butter_order = 2L, butter_fs = 0.2)
#'
#' @importFrom signal butter filter
#' @export
add_pressure_step <-
  function(pressure_start = 3L,
           signal_end = 40L,
           butter_order = 2L,
           butter_fs = 0.2) {
    # Validation
    if (!is.integer(pressure_start) || pressure_start < 0) {
      stop("pressure_start should be a non-negative integer.")
    }
    
    if (!is.integer(signal_end) || signal_end <= pressure_start) {
      stop("signal_end should be an integer value greater than pressure_start.")
    }
    
    if (!is.integer(butter_order) || butter_order < 1) {
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
#' @param lag_values Integer vector specifying the number of lags for each lagged column.
#'  Should be in the same order as 'lagged_cols'.
#' @param is_training Boolean indicating whether the data is for training (TRUE) or prediction (FALSE).
#' @param vsvr_response Character. Response column name for the SVR model.
#'
#' @return Dataframe containing the specified columns and their lagged values.
#'
#' @examples
#'
#' my_df <- data.frame(feature1 = c(1:10), feature2 = c(1:10), feature3 = c(1:10),
#' feature4 = c(1:10), feature1_1 = c(2:11), feature2_1 = c(2:11),
#' feature3_1 = c(2:11), feature4_1 = c(2:11), feature1_2 = c(3:12),
#' feature2_2 = c(3:12), feature3_2 = c(3:12), feature4_2 = c(3:12))
#'
#' # Generate training set
#' training_data <- generate_time_series_data(input_df = my_df, data_cols = c("feature1", "feature2"),
#'  predictor_cols = NULL, lagged_cols = c("feature1", "feature2"), lag_values = c(2, 2),
#'  is_training = TRUE, vsvr_response = "feature2")
#'
#' # Generate prediction set
#' prediction_data <- generate_time_series_data(input_df = my_df, data_cols = NULL,
#'  predictor_cols = c("feature3", "feature4"), lagged_cols = c("feature3", "feature4"),
#'  lag_values = c(2, 2), is_training = FALSE, vsvr_response = "feature3")
#'
#' @importFrom dplyr select bind_cols all_of %>%
#'
#' @export
generate_time_series_data <-
  function(input_df,
           data_cols,
           predictor_cols,
           lagged_cols,
           lag_values,
           is_training,
           vsvr_response) {
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
#' normalize all columns, and add lagged versions of the signals.
#'
#' @param df (Optional) A data frame containing the signals to be processed. Defaults to an empty data frame.
#' @param excluded_cols (Optional) A character vector specifying the columns to be excluded. Defaults to NULL.
#' @param lags (Optional) An integer specifying the number of lags to add for each signal. Defaults to NULL.
#' @param signals A character vector specifying the signals to be excluded after normalization.
#' @param lagged_signals (Optional) A character vector specifying the signals to be lagged.
#'  If NULL, all signals will be lagged. Defaults to NULL.
#'
#' @return A new data frame with specified columns excluded, all columns normalized,
#'         and lagged versions of the signals added. Rows with resulting NA values
#'         from lags are omitted.
#'
#' @details
#' This function depends on:
#' \itemize{
#'   \item Internal package functions:
#'   \itemize{
#'     \item \code{\link{exclude_signals_dataframe}}: Exclude Columns in a Data Frame by Names
#'     \item \code{\link{normalize_signals_by_name}}: Normalize Selected Columns in a Data Frame Using Min-Max Scaling
#'     \item \code{\link{lag_all_signals}}: Add Lagged Columns to All Normalized Columns in a Data Frame
#'     \item \code{\link{lag_normalized_signals}}: Add Lagged Columns to a Normalized Data Frame
#'   }
#' }
#'
#' @examples
#' df <- data.frame(Time = 1:10, A = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1),
#'  B = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' processed_df <- process_dataframe(df = df,
#'                                   excluded_cols = c("Time"),
#'                                   lags = 2,
#'                                   signals = c("A", "B"))
#'
#' @export

process_dataframe <-
  function(df = data.frame(),
           excluded_cols = NULL,
           lags = NULL,
           signals,
           lagged_signals = NULL) {
    # Validate input arguments
    if (!is.data.frame(df) || ncol(df) < 1) {
      stop("Input 'df' should be a non-empty data frame.")
    }
    
    if (!is.null(lags) && (!is.numeric(lags) || lags <= 0)) {
      stop("Input 'lags' should be a positive integer.")
    }
    
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
