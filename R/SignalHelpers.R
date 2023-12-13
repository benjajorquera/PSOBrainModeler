#' Calculate the Discrete First and Second Derivatives of a Numeric Vector
#'
#' This function computes the discrete first and second derivatives of a numeric vector.
#' The discrete first derivative is defined as the difference between consecutive points,
#' providing a measure of the instantaneous rate of change. The discrete second derivative
#' is the difference of the first derivative values, offering a measure of the acceleration
#' or the rate at which the rate of change itself is changing, indicative of the vector's
#' concavity or convexity.
#'
#' @param vec A numeric vector representing a discrete function over an ordered domain.
#'
#' @return A list containing the discrete first and second derivatives of the vector.
#' The first element, 'first', contains the differences representing the first derivative.
#' The second element, 'second', contains the differences of the first derivative,
#' representing the second derivative.
#'
#' @examples
#' vec <- c(1, 2, 4, 7, 11)
#' derivatives <- calculate_discrete_derivatives(vec)
#' # `derivatives$first` contains the discrete first derivative of `vec`.
#' # `derivatives$second` contains the discrete second derivative of `vec`.
#'
#' @export
calculate_discrete_derivatives <- function(vec) {
  first_derivative <-
    diff(vec)  # Calculate the discrete first derivative
  second_derivative <-
    diff(first_derivative)  # Calculate the discrete second derivative
  return(list(first = first_derivative, second = second_derivative))  # Return both derivatives
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