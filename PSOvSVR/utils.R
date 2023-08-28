normalize_signal <- function(df, signal_name) {
  signal_df <- data.frame(signal = df[[signal_name]])
  
  signal_df <- cbind(Index = seq_along(signal_df$signal), signal_df)
  
  signal_imputed <-
    mice(signal_df,
         method = "pmm",
         trimming = c(0.05, 0.95))
  signal_imputed <- complete(signal_imputed)
  
  signal_norm <-
    (signal_imputed$signal - min(signal_imputed$signal)) /
    (max(signal_imputed$signal) - min(signal_imputed$signal))
  
  df[signal_name] <- NULL
  
  signal_name <- paste(signal_name, "norm", sep = "_")
  
  df[[signal_name]] <- signal_norm
  
  return(df)
}

lag_signal <- function(data_frame, lags, df_col_name, fill) {
  df <- data_frame
  for (i in 1:lags) {
    col_name <- paste(df_col_name, i, sep = "_")
    df[[col_name]] <-
      shift(df[[df_col_name]],
            n = i * -1,
            type = "lead",
            fill = NA)
  }
  
  if (fill) {
    df <- na.omit(df)
  }
  
  return(df)
}

add_pressure_step <- function(pressure_start) {
  pressure <-
    c(rep(0, pressure_start), rep(-1, 30 - pressure_start))
  
  pressure_step <-
    butter(2, 0.2, type = "low", fs = 0.5)
  
  pressure_step_smooth <-
    as.numeric(signal::filter(pressure_step, pressure)) + 1
  
  df <- data.frame(MABP_norm = pressure_step_smooth)
  
  return(df)
}
