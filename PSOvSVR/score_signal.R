process_signal <- function(signal, pressure_start) {
  response_signal <- signal[pressure_start:(pressure_start + 30)]
  peak_signal <- signal[(pressure_start + 6):(pressure_start + 18)]
  stable_signal <- signal[(pressure_start + 30):(pressure_start + 60)]
  min_response_signal <- min(response_signal)
  
  if (!(min_response_signal %in% peak_signal) ||
      !(min_response_signal >= -.2 &&
        min_response_signal <= .5) ||
      var(stable_signal) > .002 ||
      !(max(signal) < 1.2)) {
    return(0)
  }
  
  score <- 10
  
  max_peak_signal <-
    max(signal[(pressure_start + 18):(pressure_start + 30)])
  peak_stable_distance <-
    abs(max_peak_signal - tail(stable_signal, 1))
  score <- score - (peak_stable_distance * 100)
  
  stable_peak <-
    max(signal[(pressure_start + 18):(pressure_start + 30)]) - tail(stable_signal, 1)
  drop_peak <-
    max(signal[(pressure_start + 18):(pressure_start + 30)]) - min(peak_signal)
  
  if (stable_peak > (drop_peak * .45)) {
    score <- score - (drop_peak * 10)
  }
  
  if (min(stable_signal) < min_response_signal) {
    score <- score - ((min_response_signal - min(stable_signal)) * 10)
  }
  
  return(score)
}