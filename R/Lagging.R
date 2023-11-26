#' Add Lagged Columns to a Normalized Data Frame
#'
#' This function adds lagged columns for specified normalized columns in a
#' data frame.
#' The columns to be lagged should be already normalized and have "_norm" at
#' the end of their names.
#'
#' @param data_frame Data frame to which lagged columns will be added.
#' @param lags Integer specifying the number of lags for each column.
#' @param df_col_names Character vector containing names of columns to be lagged.
#'
#' @return A modified data frame with lagged columns. Rows with NA values
#' resulting from the lagging process are removed.
#'
#' @examples
#' df <- data.frame(time = 1:6, A_norm = c(0, 0.2, 0.4, 0.6, 0.8, 1))
#' lagged_df <- lag_normalized_signals(df, 2, c("A_norm"))
#' # `lagged_df` will now include 'A_norm_1' and 'A_norm_2' as lagged columns.
#'
#' @importFrom dplyr lag
#' @importFrom stats na.omit
#' @export
lag_normalized_signals <-
  function(data_frame = NULL,
           lags = 0,
           df_col_names = NULL) {
    stopifnot(is.data.frame(data_frame))
    
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
#' All columns to be lagged should be already normalized and have "_norm" at the
#' end of their names.
#'
#' @param data_frame Data frame to which lagged columns will be added.
#' @param lags Integer specifying the number of lags for each column.
#'
#' @return A modified data frame with lagged columns. Rows with NA values
#' resulting from the lagging process are removed.
#'
#' @examples
#' df <- data.frame(time = 1:6, A_norm = c(0, 0.2, 0.4, 0.6, 0.8, 1),
#'  B_norm = c(1, 0.8, 0.6, 0.4, 0.2, 0))
#' lagged_df <- lag_all_signals(df, 2)
#' # `lagged_df` will now include 'A_norm_1', 'A_norm_2', 'B_norm_1',
#' # and 'B_norm_2' as lagged columns.
#'
#' @importFrom dplyr lag
#' @importFrom stats na.omit
#' @export
lag_all_signals <- function(data_frame = NULL, lags = 0) {
  # Validation
  stopifnot(is.data.frame(data_frame))
  
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
