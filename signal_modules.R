library(data.table)
library(mice)
library(signal)

set.seed(123)

lag_signal <- function(data_frame, lags, df_col_name) {
  df <- data_frame
  for (i in 1:lags) {
    col_name <- paste(df_col_name, i, sep = "-")
    df[[col_name]] <-
      shift(df[[df_col_name]],
            n = i * -1,
            type = "lag",
            fill = NA)
  }
  
  df <- na.omit(df)
  
  return(df)
}

normalize_signal <- function(df, signal_name) {
  signal_df <- data.frame(signal = data[[signal_name]])
  
  signal_df <- cbind(Index = seq_along(signal_df$signal), signal_df)
  
  signal_imputed <-
    mice(signal_df,
         method = "pmm",
         trimming = c(0.05, 0.95))
  signal_imputed <- complete(signal_imputed)
  
  signal_norm <-
    (signal_imputed$signal - min(signal_imputed$signal)) /
    (max(signal_imputed$signal) - min(signal_imputed$signal))
  
  signal_name <- paste(signal_name, "norm", sep = "_")
  
  df[[signal_name]] <- signal_norm
  
  return(df)
}

add_datetime_col <- function(df) {
  fecha_ref <- Sys.time()
  datetime_vec <-
    as.POSIXct(fecha_ref + df$Time, origin = "1970-01-01")
  df <- cbind(df, Datetime = datetime_vec)
  
  return(df)
}

add_pressure_step <- function(df, pressure_start) {
  pressure <-
    c(rep(0, pressure_start), rep(-1, nrow(df) - pressure_start))
  
  pressure_step <-
    butter(2, 0.2, type = "low", fs = 0.5)
  
  pressure_step_smooth <-
    as.numeric(signal::filter(pressure_step, pressure)) + 1
  
  df <- cbind(df, Pressure_step = pressure_step_smooth)
  
  return(df)
}

process_signal <- function(signal, pressure_start) {
  response_signal <- signal[pressure_start:pressure_start + 30]
  peak_signal <- signal[pressure_start + 6:pressure_start + 18]
  stable_signal <- signal[pressure_start + 80:pressure_start + 60]
  min_response_signal <- min(response_signal)
  
  if (!(min_response_signal %in% peak_signal) ||
      !(min_response_signal >= -.2 &&
        min_response_signal <= .5) ||
      var(stable_signal) > .002 ||
      !(max(signal) < 1.2) ||
      !(min(signal) > -.2)) {
    return(FALSE)
  }
  
  score <- 10
  
  max_peak_signal <-
    max(signal[pressure_start + 18:pressure_start + 30])
  peak_stable_distance <-
    abs(max_peak_signal - tail(stable_signal, 1))
  score <- score - (peak_stable_distance * 100)
  
  stable_peak <-
    max(signal[pressure_start + 18:pressure_start + 30]) - tail(stable_signal, 1)
  drop_peak <-
    max(signal[pressure_start + 18:pressure_start + 30]) - min(peak_signal)
  
  if (stable_peak > drop_peak * .45) {
    score <- score - drop_peak * 10
  }
  
  if (min(stable_signal) < min_response_signal) {
    score <- score - (min_response_signal - min(stable_signal)) * 10
  }
  
  return(score)
}



setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car")
data <- read.table("Sujeto1.txt", header = TRUE)

data <- normalize_signal(data, "MABP")

data <- lag_signal(data, 3, "MABP_norm")

data <- normalize_signal(data, "CBFV.L")

data <- add_datetime_col(data)

data <- add_pressure_step(data, 50)

plot(data$Datetime, data$CBFV.L_norm, type="l")


