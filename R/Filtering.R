#' Validate Input Data Frame and Signal Names
#'
#' This function validates the provided data frame and the specified column (signal) names.
#'
#' @param df A data frame to validate.
#' @param signal_names A character vector specifying the names of the columns to validate.
#' @param exclude_mode Logical. If TRUE, indicates validation for exclusion, else for inclusion.
#'
#' @return Invisible NULL if validation passes. Stops with an error if there are issues.
validate_df_and_signals <-
  function(df, signal_names, exclude_mode = FALSE) {
    if (is.null(df)) {
      stop("Data frame is empty.")
    }
    
    if (is.null(signal_names) || length(signal_names) == 0) {
      if (exclude_mode) {
        message("Column names are empty, returning data frame")
      } else {
        stop("Column names are empty.")
      }
    }
    
    if (!all(signal_names %in% colnames(df))) {
      action_word <- ifelse(exclude_mode, "exclude", "include")
      stop(
        paste(
          "One or more specified column names to",
          action_word,
          "do not exist in the data frame."
        )
      )
    }
    
    invisible(NULL)
  }

#' Filter Columns in a Data Frame by Names
#'
#' This function returns a subset of the given data frame containing only the specified columns.
#'
#' @param df A data frame to filter.
#' @param signal_names A character vector specifying the names of the columns to include in the output.
#'
#' @return A data frame containing only the specified columns.
filter_signals_dataframe <- function(df, signal_names) {
  # Validate input arguments
  validate_df_and_signals(df, signal_names)
  
  return(df[, signal_names, drop = FALSE])
}

#' Exclude Columns in a Data Frame by Names
#'
#' This function returns a subset of the given data frame excluding the specified columns.
#'
#' @param df A data frame to filter.
#' @param signal_names (Optional) A character vector specifying the names of the columns to exclude from the output. Defaults to NULL.
#'
#' @return A data frame without the specified columns.
exclude_signals_dataframe <- function(df, signal_names = NULL) {
  # Validate input arguments
  validate_df_and_signals(df, signal_names, exclude_mode = TRUE)
  
  return(df[, !(colnames(df) %in% signal_names), drop = FALSE])
}
