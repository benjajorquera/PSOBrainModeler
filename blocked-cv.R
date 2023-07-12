library(data.table)
library(mice)
library(signal)
library(dplyr)
library(timetk)
library(e1071)
library(pso)
library(ppso)
#library(hydroPSO)

set.seed(123)

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

lag_signal <- function(data_frame, lags, df_col_name, fill) {
  df <- data_frame
  for (i in 1:lags) {
    col_name <- paste(df_col_name, i, sep = "_")
    df[[col_name]] <-
      shift(df[[df_col_name]],
            n = i * -1,
            type = "lag",
            fill = NA)
  }
  
  if (fill) {
    df <- na.omit(df)
  }
  
  return(df)
}

svr_model <- function(train_data, cost, nu, gamma) {
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = train_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression",
      #cachesize = 1000,
      tolerance = 1
    )
  return(svm_model)
}

objetivo <- function(x) {
  cost <- round(x[1], digits = 1)
  nu <- round(x[2], digits = 2)
  gamma <- x[3]
  if (gamma >= 1)
    gamma <- round(gamma, digits = 2)
  else
    gamma <- signif(gamma, digits = 2)
  
  cat(cost, nu, gamma, "\n")
  
  cors <- numeric(5)
  errors <- numeric(5)
  
  for (i in 1:5) {
    new_data_validation <- data_partitions[[i]]$validation[, -(3:4)]
    new_data_validation <<- new_data_validation[,-(5)]
    svr_model_pso <- svr_model(data_partitions[[i]]$training,
                               cost, nu, gamma)
    
    predictions_pso <<-
      predict(svr_model_pso, new_data_validation)
    correlation_pso <- cor(predictions_pso,
                           data_partitions[[i]]$validation$CBFV.L_norm)
    cors[i] <- correlation_pso
    errors[i] <-
      sqrt(mean((
        data_partitions[[i]]$validation$CBFV.L_norm - predictions_pso
      ) ^ 2
      ))
  }
  
  cat(mean(cors), mean(errors), (1 - mean(cors) + mean(errors)), "\n")
  return((1 - mean(cors) + mean(errors)))
}

setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car")
data <- read.table("Sujeto1.txt", header = TRUE)

# Normalizo las señales MABP y CBFV.L
data <- normalize_signal(data, "MABP")
data <- normalize_signal(data, "CBFV.L")

# Agrego los retardos a esas señales
data <- lag_signal(data, 5, "MABP_norm", FALSE)
data <- lag_signal(data, 5, "CBFV.L_norm", TRUE)

# Especifica el número de bloques y el tamaño de la validación
num_blocks <- 5
validation_size <- 0.2

# Calcula el tamaño de la validación
validation_length <- round(nrow(data) * validation_size)

# Crea una lista vacía para almacenar los conjuntos de entrenamiento y validación
data_partitions <- vector("list", num_blocks)

row_indices <- nrow(data)

# Itera sobre los bloques y crea los conjuntos de entrenamiento y validación
for (i in 1:num_blocks) {
  if (i == 1) {
    validation_data <- data[1:validation_length, ]
    training_data <- data[(validation_length + 1):row_indices, ]
  }
  else if (i == num_blocks) {
    training_data <- data[1:(((i - 1) * validation_length) + 1), ]
    validation_data <-
      data[(((i - 1) * validation_length) + 2):row_indices, ]
  }
  else {
    training_data <- data[1:((i - 1) * validation_length), ]
    validation_data <-
      data[(((i - 1) * validation_length) + 1):(validation_length * i), ]
    training_data <-
      rbind(training_data, data[((validation_length * i) + 1):row_indices, ])
  }
  
  validation_data <- add_pressure_step(validation_data, 30)
  
  # Almacena los conjuntos en la lista
  data_partitions[[i]] <-
    list(training = training_data, validation = validation_data)
}


lo <- c(0.25, 0.1, (1 / (2 * 1024 ^ 2)))
hi <- c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)))

# 512, 0.7, 0.00195 global optimo de búsqueda grid

# TODO: limpiar los dataframes. filtro escalón. escapar minimo local.

time <- Sys.time()

resultados_pso <-
  psoptim(
    par = c(NA, NA, NA),
    fn = objetivo,
    lower = lo,
    upper = hi,
    control = list(
      s = 5,
      maxit = 100,
      maxf = 200,
      trace = 1,
      REPORT = 1,
      p = 0.5,
      c.g = 10,
      trace.stats = TRUE,
      maxit.stagnate = 10,
      reltol = 0.5,
      hybrid = "improved",
      type = "SPSO2011",
      vectorize = TRUE,
      hybrid.control = list(maxit = 1)
    )
  )

time <- Sys.time() - time
print(time)
print(resultados_pso$value)