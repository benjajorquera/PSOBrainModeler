#' Filter Columns in a Data Frame by Names
#'
#' This function returns a subset of the given data frame containing only the
#' specified columns.
#'
#' @param df A data frame to filter.
#' @param signal_names A character vector specifying the names of the columns
#'  to include in the output.
#'
#' @return A data frame containing only the specified columns.
#'
#' @examples
#' data_sample <- data.frame(a = 1:5, b = 6:10, c = 11:15)
#' filtered_data <- filter_signals_dataframe(data_sample, c("a", "c"))
#' # The `filtered_data` will contain only columns `a` and `c`.
#'
#' @export
filter_signals_dataframe <- function(df, signal_names) {
  # Validate input arguments
  stopifnot(is.data.frame(df), is.character(signal_names))
  
  return(df[, signal_names, drop = FALSE])
}


#' Exclude Columns in a Data Frame by Names
#'
#' This function returns a subset of the given data frame excluding the
#' specified columns.
#'
#' @param df A data frame to filter.
#' @param signal_names (Optional) A character vector specifying the names of
#'  the columns to exclude from the output. Defaults to NULL.
#'
#' @return A data frame without the specified columns.
#'
#' @examples
#' data_sample <- data.frame(a = 1:5, b = 6:10, c = 11:15)
#' excluded_data <- exclude_signals_dataframe(data_sample, c("b"))
#' # The `excluded_data` will contain all columns except `b`.
#'
#' @export
exclude_signals_dataframe <- function(df, signal_names = NULL) {
  # Validate input arguments
  stopifnot(is.data.frame(df), is.character(signal_names))
  
  return(df[, !(colnames(df) %in% signal_names), drop = FALSE])
}
