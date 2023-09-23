#' Normalize Selected Columns in a Data Frame Using Min-Max Scaling
#'
#' @param df Data frame containing the columns to be normalized.
#' @param signal_names Character vector specifying which columns should be normalized.
#'
#' @return Data frame with normalized columns added.
#'
#' @examples
#' df <- data.frame(Time = 1:10, Signal1 = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1),
#'  Signal2 = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' normalized_df <- normalize_signals_by_name(df, c("Signal1", "Signal2"))
#'
#' @importFrom dplyr mutate across all_of %>%
#' @export
normalize_signals_by_name <-
  function(df = NULL, signal_names = NULL) {
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
      dplyr::mutate(dplyr::across(
        dplyr::all_of(signal_names),
        ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)),
        .names = "{col}_norm"
      ))
    
    return(df)
  }

#' Normalize All Columns in a Data Frame Using Min-Max Scaling
#'
#' @param df Data frame containing the columns to be normalized.
#'
#' @return Data frame with normalized columns added.
#'
#' @examples
#' df <- data.frame(Time = 1:10, Signal1 = c(2, 4, 6, 8, 10, 5, 3, 7, 9, 1),
#'  Signal2 = c(10, 5, 8, 3, 6, 9, 2, 7, 4, 1))
#' normalized_df <- normalize_all_signals(df)
#'
#' @importFrom dplyr mutate across everything %>%
#' @export
normalize_all_signals <- function(df = NULL) {
  # Validation
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    stop("Input data frame should not be empty or NULL.")
  }
  
  # Normalization
  df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(),
                                           ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)),
                                           .names = "{col}_norm"))
  
  return(df)
}