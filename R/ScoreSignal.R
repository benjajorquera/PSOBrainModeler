#' Evaluate Signal Quality
#'
#' This function evaluates the quality of a provided signal based on specific criteria.
#'
#' @param signal A numeric vector representing the signal.
#' @param pressure_start_point The starting point of the pressure.
#' @return A numeric score indicating the quality of the signal.
#'
#' @details
#' The function applies both basic and advanced filters to evaluate the signal's quality.
#' The basic filter checks for conditions related to peaks, minimums, variance, and range.
#' The advanced filter performs checks on stabilization and drop rates.
#'
#' @examples
#' sample_signal <- rnorm(40)
#' start_point <- 5
#' quality_score <- evaluate_signal_quality(sample_signal, start_point)
#' @importFrom stats var
#' @importFrom utils head
#' @export
evaluate_signal_quality <-
  function(signal, pressure_start_point = 3) {
    # Ensure that the signal is a numeric vector
    if (!is.numeric(signal) ||
        length(signal) < (pressure_start_point + 30)) {
      stop("Invalid signal or insufficient length given the pressure start point.")
    }
    
    # Basic filter checks:
    # 1. Global minimum (peak) between 3 and 9 seconds after the drop occurs
    # 2. Minimum peak of the signal between -0.2 and 0.5
    # 3. Variance < 0.002 between seconds 15 and 30 (stabilization tail)
    # 4. Maximum and minimum of the entire signal between -0.2 and 1.2 (exclusive)
    peak_range <-
      signal[(pressure_start_point + 3):(pressure_start_point + 9)]
    stabilization_range <-
      signal[(pressure_start_point + 15):(pressure_start_point + 30)]
    drop_range <-
      signal[(pressure_start_point + 9):(pressure_start_point + 15)]
    
    global_min <- min(signal)
    max_drop <- max(drop_range)
    
    if (!(global_min %in% peak_range) ||
        !(global_min >= -0.2 && global_min <= 0.5) ||
        stats::var(stabilization_range) > 0.002 ||
        !(max(signal) < 1.2 && min(signal) > -0.2)) {
      message("RESPONSE SIGNAL FAILED BASIC FILTER")
      return(-10)
    }
    
    # Initial score
    score <- 10
    
    # Advanced filter checks:
    # 1. Stabilization phase is not strictly increasing or decreasing,
    # penalize with the distance between the maximum value and the last value of the signal, multiplied by 100
    stability_range_diff <-
      abs(max(stabilization_range) - min(stabilization_range))
    score <- score - (stability_range_diff * 100)
    
    # 2. Drop before stabilization is at most 45% of the signal's rising section.
    # If not met, penalize with a factor of 10.
    peak_after_drop <-
      max(signal[15:30]) - utils::head(peak_range, 1)
    drop_diff <- max(signal[15:30]) - min(peak_range)
    
    if (peak_after_drop > (drop_diff * 0.45)) {
      score <- score - (drop_diff * 10)
    }
    
    # 3. Signal stabilization occurs at the same level as the drop peak.
    # Penalize based on the distance between these two points with a factor of 10.
    if (min(stabilization_range) < global_min) {
      score <- score - ((global_min - min(stabilization_range)) * 10)
    }
    
    return(score)
  }
