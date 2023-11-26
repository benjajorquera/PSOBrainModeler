#' Calculate the Maximum Difference Between Consecutive Points in a Vector
#'
#' This function computes the maximum absolute difference between consecutive
#' points in a numeric vector. It is useful for identifying the largest change
#' between any two adjacent values in the vector.
#'
#' @param vec A numeric vector.
#'
#' @return The maximum absolute difference between consecutive points in the
#'  vector.
#'
#' @examples
#' vec <- c(1, 2, 4, 7, 11)
#' max_diff_value <- max_diff(vec)
#' # `max_diff_value` will be the largest difference between adjacent values
#' # in `vec`.
#'
#' @export
max_diff <- function(vec) {
  diffs <-
    diff(vec)  # Calculate the differences between consecutive points
  return(max(abs(diffs), na.rm = TRUE))  # Return the maximum difference in absolute value
}

#' Plot VSVR Response Signal
#'
#' This function plots the VSVR (Vector Support Vector Regression) response
#' signal over time or instances.
#' It creates a line plot for the given signal with specified points.
#'
#' @param signal A numeric vector representing the VSVR response signal.
#' @param points (Optional) The number of points to plot along the x-axis.
#'  Defaults to 70.
#'
#' @details
#' The function generates a line plot with 'Time/Instance' on the x-axis and
#'  'Response' on the y-axis.
#' It is designed to provide a visual representation of the VSVR response signal.
#'
#' @examples
#' response_signal <- runif(70, min = -1, max = 1)
#' plot_vsvr_response_signal(response_signal)
#'
#' @importFrom graphics plot axis
#' @export
plot_vsvr_response_signal <- function(signal, points = 70) {
  x_values <- 1:points
  plot(
    signal,
    type = "l",
    main = "VSVR Response Signal",
    ylab = "Response",
    xlab = "Time/Instance"
  )
  # Configure the x-axis with an interval of 1
  axis(1, at = x_values, labels = x_values)
}