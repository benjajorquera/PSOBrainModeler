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
#'
#' @importFrom dplyr lag
#' @importFrom stats na.omit
#' @export
lag_normalized_signals <-
  function(data_frame = NULL,
           lags = 0,
           df_col_names = NULL) {
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
    
    return(stats::na.omit(data_frame))
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
#'
#' @importFrom dplyr lag
#' @importFrom stats na.omit
#' @export
lag_all_signals <- function(data_frame = NULL, lags = 0) {
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
  
  return(stats::na.omit(data_frame))
}