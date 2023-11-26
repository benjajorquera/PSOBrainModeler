#' Normalize Selected Columns in a Data Frame Using Min-Max Scaling
#'
#' This function normalizes specified columns in a data frame using Min-Max scaling.
#' The normalized values are added as new columns with "_norm" appended to the
#' original column names.
#'
#' @param df Data frame containing the columns to be normalized.
#' @param signal_names Character vector specifying which columns should be
#'  normalized.
#'
#' @return Data frame with normalized columns added, retaining the original
#'  columns.
#'
#' @examples
#' df <- data.frame(Time = 1:10,
#'                  Signal1 = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1),
#'                  Signal2 = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' normalized_df <- normalize_signals_by_name(df, c("Signal1", "Signal2"))
#' # `normalized_df` will now include 'Signal1_norm' and 'Signal2_norm'.
#'
#' @importFrom dplyr mutate across all_of
#' @importFrom magrittr %>%
#' @export
normalize_signals_by_name <-
  function(df = NULL, signal_names = NULL) {
    # Validation
    stopifnot(is.data.frame(df))
    
    # Normalization
    df <- df %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(signal_names),
        ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)),
        .names = "{col}_norm"
      ))
    
    return(df)
  }


#' Normalize All Columns in a Data Frame Using Min-Max Scaling
#'
#' This function normalizes all columns in a data frame using Min-Max scaling.
#' The normalized values are added as new columns with "_norm" appended to the
#' original column names.
#'
#' @param df Data frame containing the columns to be normalized.
#'
#' @return Data frame with normalized columns added, retaining the original
#'  columns.
#'
#' @examples
#' df <- data.frame(Time = 1:10,
#'                  Signal1 = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1),
#'                  Signal2 = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' normalized_df <- normalize_all_signals(df)
#' # `normalized_df` will include 'Time_norm', 'Signal1_norm', and 'Signal2_norm'.
#'
#' @importFrom dplyr mutate across everything
#' @importFrom magrittr %>%
#' @export
normalize_all_signals <- function(df = NULL) {
  # Validation
  stopifnot(is.data.frame(df))
  
  # Normalization
  df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(),
                                           ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)),
                                           .names = "{col}_norm"))
  
  return(df)
}
