#' Signal Quality Evaluation
#'
#' @description
#' Evaluates the quality of a provided signal based on specific criteria.
#'
#' @param signal A numeric vector representing the signal.
#' @param pressure_start_point (Optional) The starting point of the pressure. Defaults to 3L.
#' @param silent (Optional) A logical for run the function silently (without printouts). Defaults to FALSE.
#'
#' @return A numeric score indicating the quality of the signal.
#'
#' @details
#' The function applies basic filter to evaluate the signal's quality.
#' The basic filter checks for conditions related to peaks, minimums, variance, and range.
#'
#' @examples
#'  sample_signal <- rnorm(40)
#'  start_point <- 5
#'  quality_score <- evaluate_signal_quality(sample_signal, start_point)
#'
#' @importFrom stats var
#' @importFrom utils head
#'
#' @export
evaluate_signal_quality <-
  function(signal,
           pressure_start_point = 3L,
           silent = FALSE,
           signal_size = 65L) {
    # Ensure that the signal and pressure_start_point are numeric
    if (!is.numeric(signal) ||
        !is.numeric(pressure_start_point) ||
        !is.numeric(signal_size) ||
        length(signal) < signal_size) {
      stop("Invalid signal or pressure start point.")
    }
    
    if (max_diff(signal) > 0.6) {
      message("RESPONSE SIGNAL FAILED BASIC FILTER: MAXIMUM DIFFERENCE GREATER THAN 0.6")
      return(0)
    }
    
    suppressWarnings({
      signal_test <- adf.test(signal, alternative = "stationary")
    })
    
    if (!is.nan(signal_test$p.value)) {
      if (signal_test$p.value >= 0.05) {
        message("RESPONSE SIGNAL FAILED BASIC FILTER: STATIONARY TEST FAILED")
        return(0)
      }
    }
    else {
      message("RESPONSE SIGNAL FAILED BASIC FILTER: STATIONARY TEST ERROR")
      return(0)
    }
    
    # Constants
    MIN_PEAK_VALUE <- -0.2
    MAX_PEAK_VALUE <- 0.5
    VARIANCE_THRESHOLD <- 0.002
    MAX_SIGNAL_VALUE <- 1.2
    
    # Basic filter checks:
    # 1. Global minimum (peak) between 3 and 9 seconds after the drop occurs
    # 2. Minimum peak of the signal between -0.2 and 0.5
    # 3. Variance < 0.002 between seconds 15 and 30 (stabilization tail)
    # 4. Maximum and minimum of the entire signal between -0.2 and 1.2 (exclusive)
    peak_range <-
      signal[(pressure_start_point + 6):(pressure_start_point + 18)]
    drop_range <-
      signal[(pressure_start_point + 18):(pressure_start_point + 30)]
    stabilization_range <-
      signal[(pressure_start_point + 30):(pressure_start_point + 60)]
    
    signal_range <- range(signal)
    
    not_peak_range <-
      signal[-((pressure_start_point + 6):(pressure_start_point + 18))]
    
    if ((signal_range[1] %in% not_peak_range) ||
        !(signal_range[1] %in% peak_range)) {
      message("RESPONSE SIGNAL FAILED BASIC FILTER: INCORRECT MIN VALUE POSITION")
      return(0)
    }
    
    if (signal_range[1] > MIN_PEAK_VALUE ||
        signal_range[2] > MAX_SIGNAL_VALUE) {
      message("RESPONSE SIGNAL FAILED BASIC FILTER: INCORRECT MAX AND MIN VALUES")
      return(0)
    }
    
    if (stats::var(stabilization_range) > VARIANCE_THRESHOLD) {
      message("RESPONSE SIGNAL FAILED BASIC FILTER: INCORRECT STABILIZATION VARIANCE VALUE")
      return(0)
    }
    
    return(1)
  }

# Definir una función que calcule la máxima diferencia entre puntos consecutivos de un vector
max_diff <- function(vec) {
  diffs <-
    diff(vec)  # Calcular las diferencias entre puntos consecutivos
  return(max(abs(diffs), na.rm = TRUE))  # Devolver la máxima diferencia en valor absoluto
}

advanced_filter <- function(signal, pressure_start_point) {
  peak_range <-
    signal[(pressure_start_point + 3):(pressure_start_point + 9)]
  stabilization_range <-
    signal[(pressure_start_point + 15):(pressure_start_point + 30)]
  drop_range <-
    signal[(pressure_start_point + 9):(pressure_start_point + 15)]
  
  global_min <- min(signal)
  
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
