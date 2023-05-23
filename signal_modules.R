library(data.table)
library(mice)
library(signal)
library(dplyr)
library(timetk)
library(e1071)
library(pso)

set.seed(123)

lag_signal <- function(data_frame, lags, df_col_name, fill) {
  df <- data_frame
  for (i in 1:lags) {
    col_name <- paste(df_col_name, i, sep = "_")
    if(fill) {
    df[[col_name]] <-
      shift(df[[df_col_name]],
            n = i * -1,
            type = "lag",
            fill = NA)
    }
    else {
      df[[col_name]] <-
        shift(df[[df_col_name]],
              n = i * -1,
              type = "lag")
    }
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
    return(0)
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

objetivo <- function(x) {
  
  cost <- x[1]
  nu <- x[2]
  
  n_splits <- length(resample_spec$splits)
  correlations <- numeric(n_splits)
  errors <- numeric(n_splits)
  scores <- numeric(n_splits)
  
  for (i in seq(n_splits, 1, -1)) {
    training_data <- data[resample_spec[[1]][[i]][["in_id"]], ]
    validation_data <- data[resample_spec[[1]][[i]][["out_id"]], ]
    
    validation_data <- add_pressure_step(validation_data, 10)
    
    new_validation_data <- data.frame(CBFV.L_norm = validation_data$CBFV.L_norm,
                                      CBFV.L_norm_1 = validation_data$CBFV.L_norm,
                                      MABP_norm = validation_data$Pressure_step,
                                      MABP_norm_1 = validation_data$Pressure_step)
    
    svm_model <- svm(CBFV.L_norm + CBFV.L_norm_1 ~ MABP_norm + MABP_norm_1,
                     data = training_data,
                     cost = cost, nu = nu, kernel = "radial", type = "nu-regression")
    
    predictions <- predict(svm_model, new_validation_data)
    
    # Correlación
    correlations[i] <- cor(predictions, validation_data$CBFV.L_norm)
    
    # Error MSE
    errors[i] <- mean((predictions - validation_data$CBFV.L_norm)^2)
    
    # Puntaje del filtro de señales
    scores[i] <- process_signal(predictions, 10)
  }
  
  # Pesos para cada criterio
  peso_correlacion <- 1
  peso_error <- 1
  peso_puntaje <- 2
  
  # Cálculo del puntaje total
  puntaje_total <- peso_correlacion * mean(correlations) -
    peso_error * mean(errors) +
    peso_puntaje * mean(scores)
  
  print(puntaje_total)
  
  return(-puntaje_total)  # Se busca maximizar el puntaje total
  
  # Intentar optimizar junto con el puntaje/filtro X
  # Buscar sobre error (cuadrático medio?) (intentar que vaya de 0 a 1) (normalizar) X/2
  # Experimentar un poco cual criterio tiene más peso
}


setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car")
data <- read.table("Sujeto1.txt", header = TRUE)

data <- normalize_signal(data, "MABP")

data <- lag_signal(data, 3, "MABP_norm", FALSE)

data <- normalize_signal(data, "CBFV.L")

data <- lag_signal(data, 3, "CBFV.L_norm", TRUE)

data <- add_datetime_col(data)

plot(data$Datetime, data$CBFV.L_norm, type="l")

### Sliding Window CV
# resample_spec <- time_series_cv(data = data,
#                                 initial     = "1.5 minutes",
#                                 assess      = "1 minute",
#                                 skip        = "30 seconds",
#                                 cumulative  = FALSE,
#                                 slice_limit = 5)

### Expanding Window CV
resample_spec <- time_series_cv(data = data,
                                initial     = "30 seconds",
                                assess      = "1.5 minute",
                                skip        = "30 seconds",
                                cumulative  = TRUE,
                                slice_limit = 3)

resample_spec %>% tk_time_series_cv_plan()

resample_spec %>%
  plot_time_series_cv_plan(Datetime, MABP_norm, .interactive = FALSE)


lo <- c(0.25, 0.1)
hi <- c(4096, 0.9)

resultados_pso <- psoptim(par = c(2000, 0.5), fn = objetivo, lower = lo,
                          upper = hi, control = list(maxit = 12,
                                                     trace = 1, REPORT=1,
                                                     reltol = 0.1))

# Buscar hiperparámetros de slice 1, utilizar esos hiperparámetros en las otras slices X
# Dentro de la función de optimización X
# Como se calcula la correlación o el error cuando se hace cv con señales (promedio de errores?) X





