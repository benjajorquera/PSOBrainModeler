#' Signal Quality Evaluation
#'
#' @description
#' Evaluates the quality of a provided signal based on specific criteria,
#' including advanced tests complementing the basic filter. This function is
#' designed to work with signals at a frequency of 2 Hz.
#'
#' @param signal A numeric vector representing the signal.
#' @param pressure_start_point (Optional) The starting point of the pressure.
#'   Defaults to 3L.
#' @param silent (Optional) A logical indicating whether to run the function
#'   silently (without printouts). Defaults to TRUE.
#' @param max_diff_threshold (Optional) A numeric value representing the
#'   maximum difference threshold. Defaults to 0.55.
#'
#' @return A list with a numeric score indicating the quality of the signal and
#'   the result of the evaluation.
#'
#' @details
#' The function evaluates the signal's quality using a set of criteria,
#'   starting with advanced tests before applying the basic filter. The function
#'   returns a score of 0 for any failed test and 1 if all tests are passed.
#'
#' Advanced Criteria:
#' 1. Maximum Difference Test: Checks if the maximum distance between any two
#'  consecutive points in the signal exceeds 0.55.
#'  This test helps in identifying sudden spikes or drops in the signal,
#'  indicating potential anomalies.
#' 2. Stationarity Test: Uses the Augmented Dickey-Fuller test to determine if
#'  the signal is stationary.
#'  A non-stationary signal (p-value >= 0.05) might indicate trends or seasonal
#'  effects, which could affect the model's performance.
#'
#' Basic Filter Checks:
#' 1. Global minimum (peak) should occur between 3 and 9 seconds after the
#'  pressure start point.
#' 2. The minimum peak of the signal should be between -0.2 and 0.5.
#' 3. Variance of the signal should be less than 0.002 between seconds 15 and 30
#'  (stabilization tail).
#' 4. The maximum and minimum values of the entire signal should be between
#'  -0.2 and 1.2 (exclusive).
#'
#' The function returns a score of 0 for any failed test and 1 if all tests are
#'  passed.
#'
#' @examples
#' sample_signal <- rnorm(40)
#' start_point <- 5
#' quality_score <- evaluate_signal_quality(sample_signal, start_point)
#'
#' @importFrom stats var
#' @importFrom utils head
#' @importFrom tseries adf.test
#'
#' @export
#'
evaluate_signal_quality <-
  function(signal,
           pressure_start_point = 3L,
           silent = TRUE,
           max_diff_threshold = 0.55) {
    # Ensure that the signal and pressure_start_point are numeric
    stopifnot(is.numeric(signal))
    
    if (max_diff(signal) > max_diff_threshold) {
      if (!silent) {
        message(
          "\nRESPONSE SIGNAL FAILED BASIC FILTER: MAXIMUM DIFFERENCE GREATER THAN THRESHOLD"
        )
      }
      return(list(result = "TEST 1 FAILED", score = 0))
    }
    
    suppressWarnings({
      signal_test <- tseries::adf.test(signal, alternative = "stationary")
    })
    
    if (!is.nan(signal_test$p.value)) {
      if (signal_test$p.value >= 0.05) {
        if (!silent) {
          message("\nRESPONSE SIGNAL FAILED BASIC FILTER: STATIONARY TEST FAILED")
        }
        return(list(result = "TEST 2 FAILED", score = 0))
      }
    }
    else {
      if (!silent) {
        message("\nRESPONSE SIGNAL FAILED BASIC FILTER: STATIONARY TEST ERROR")
      }
      return(list(result = "TEST 3 FAILED", score = 0))
    }
    
    # Constants
    MIN_PEAK_VALUE <- -0.2
    MAX_PEAK_VALUE <- 0.5
    VARIANCE_THRESHOLD <- 0.002
    MAX_SIGNAL_VALUE <- 1.2
    
    peak_range <-
      signal[(pressure_start_point + 6):(pressure_start_point + 18)]
    drop_range <-
      signal[(pressure_start_point + 18):(pressure_start_point + 30)]
    stabilization_range <-
      signal[(pressure_start_point + 30):(pressure_start_point + 60)]
    
    signal_range <- range(signal)
    
    min_peak <- min(peak_range)
    
    not_peak_range <-
      signal[-((pressure_start_point + 6):(pressure_start_point + 18))]
    
    if ((min_peak > MAX_PEAK_VALUE) ||
        (min_peak < MIN_PEAK_VALUE)) {
      if (!silent) {
        message("\nRESPONSE SIGNAL FAILED BASIC FILTER: INCORRECT MIN PEAK VALUE")
      }
      return(list(result = "TEST 4 FAILED", score = 0))
    }
    
    if (signal_range[1] <= MIN_PEAK_VALUE ||
        signal_range[2] >= MAX_SIGNAL_VALUE) {
      if (!silent) {
        message("\nRESPONSE SIGNAL FAILED BASIC FILTER: INCORRECT GLOBAL MAX AND MIN VALUES")
      }
      return(list(result = "TEST 5 FAILED", score = 0))
    }
    
    if (stats::var(stabilization_range) > VARIANCE_THRESHOLD) {
      if (!silent) {
        message("\nRESPONSE SIGNAL FAILED BASIC FILTER: INCORRECT STABILIZATION VARIANCE VALUE")
      }
      return(list(result = "TEST 6 FAILED", score = 0))
    }
    
    return(list(result = "TEST PASSED", score = 1))
  }

#' Advanced Signal Quality Evaluation
#'
#' @description
#' Evaluates the quality of a provided signal using advanced criteria,
#' and assigns a score based on the outcome of these checks.
#'
#' @param signal A numeric vector representing the signal.
#' @param pressure_start_point (Optional) The starting point of the pressure in
#'  the signal. Defaults to 3L.
#' @param silent (Optional) A logical for running the function silently
#'  (without printouts). Defaults to TRUE.
#'
#' @return A list containing the calculated score and detailed results of each
#'  penalization.
#'
#' @details
#' The function performs advanced checks on the signal and penalizes specific
#'  characteristics:
#' 1. Stabilization Slope Check: If the stabilization phase slope
#'  (multiplied by 100) is greater than 0.0001, the score is penalized by this
#'  value.
#' 2. Drop vs. Peak Increment Check: If the drop before stabilization exceeds
#'  45% of the signal's peak increment, the score is penalized by ten times the
#'  excess drop.
#' 3. Stabilization Level Check: If the stabilization level is lower than the
#'  peak minimum, the score is penalized based on the distance between these
#'  points (multiplied by 10).
#'
#' @examples
#' sample_signal <- rnorm(100)
#' start_point <- 5
#' filter_results <- advanced_filter(sample_signal, start_point)
#'
#' @importFrom stats coef lm
#'
#' @export
#'
advanced_filter <-
  function(signal,
           pressure_start_point = 3L,
           silent = TRUE) {
    if (!silent) {
      message("\nENTERING ADVANCED FILTER")
    }
    
    # Define ranges for peak, drop, and stabilization
    peak_range <-
      signal[(pressure_start_point + 6):(pressure_start_point + 18)]
    drop_range <-
      signal[(pressure_start_point + 18):(pressure_start_point + 30)]
    stabilization_range <-
      signal[(pressure_start_point + 30):(pressure_start_point + 60)]
    
    peak_min <- min(peak_range)
    
    # Initial score
    score <- 10
    penalization_results <- list()
    
    # Calculate the slope of linear regression in the stabilization range
    stabilization_time <-
      seq(from = 1, to = length(stabilization_range))
    fit <- stats::lm(stabilization_range ~ stabilization_time)
    slope <- stats::coef(fit)[["stabilization_time"]]
    
    # Penalize based on the slope if it exceeds the threshold
    if ((abs(slope) * 100) > 0.0001) {
      score <- score - (abs(slope) * 100)
      if (!silent) {
        message("\nSTABILIZATION RANGE SLOPE PENALIZATION: ", (abs(slope) * 100))
      }
      penalization_results$slope <- (abs(slope) * 100)
    }
    
    # Calculate the maximum increment in the peak range
    max_peak_increment <- max(peak_range) - min(peak_range)
    
    # Calculate the maximum drop in the drop range
    max_drop <- max(drop_range) - min(drop_range)
    
    # Check if the drop exceeds 45% of the peak increment
    limit <- max_peak_increment * 0.45
    if (max_drop > limit) {
      if (!silent) {
        message("\nDROP MORE THAN 45% PEAK BEFORE STABILIZATION: PENALIZATION BY ",
                (max_drop * 10))
      }
      score <- score - (max_drop * 10)
      penalization_results$drop <- (max_drop * 10)
    }
    
    # Penalize based on the distance between stabilization level and peak minimum
    if (min(stabilization_range) < peak_min) {
      if (!silent) {
        message("\nSTABILIZATION LEVEL BELOW PEAK: PENALIZATION BY ",
                ((
                  peak_min - min(stabilization_range)
                ) * 10))
      }
      score <- score - ((peak_min - min(stabilization_range)) * 10)
      penalization_results$level <-
        ((peak_min - min(stabilization_range)) * 10)
    }
    
    if (!silent) {
      message("\nLEAVING ADVANCED FILTER")
    }
    
    if (score < -10)
      score <- -10
    
    return(list(score = score, results = penalization_results))
  }
