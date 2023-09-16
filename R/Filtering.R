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
  
  return(df[, !(colnames(df) %in% signal_names), drop = FALSE])
}