library(data.table)
library(mice)
library(signal)
library(dplyr)
library(timetk)
library(e1071)
library(pso)

set.seed(123)

# Funciones auxiliares

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

objetivo <- function(x) {
  cost <- x[1]
  nu <- x[2]
  gamma <- x[3]
  
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = training_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression"
    )
  
  predictions_pso <<- predict(svm_model, validation_data)
  
  # Correlación
  correlation_pso <<- cor(predictions_pso, validation_data$CBFV.L_norm)
  print(correlation_pso)
  
  return(-correlation_pso)
}

objetivo_2 <- function(x) {
  cost <- x[1]
  nu <- x[2]
  gamma <- x[3]
  
  training_data <- data[1:(n / 2),]
  validation_data <- data[(n / 2 + 1):n,]
  
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = training_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression"
    )
  
  predictions_pso <<- predict(svm_model, validation_data)
  
  # Correlación
  correlation_pso_1 <<- cor(predictions_pso, validation_data$CBFV.L_norm)
  
  error_1 <<- mean((predictions_pso - validation_data$CBFV.L_norm) ^ 2)
  
  validation_data <- data[1:(n / 2),]
  training_data <- data[(n / 2 + 1):n,]
  
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = training_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression"
    )
  
  predictions_pso <<- predict(svm_model, validation_data)
  
  # Correlación
  correlation_pso_2 <<- cor(predictions_pso, validation_data$CBFV.L_norm)
  
  error_2 <<- mean((predictions_pso - validation_data$CBFV.L_norm) ^ 2)
  
  print(correlation_pso_1)
  print(correlation_pso_2)
  
  return(-mean(correlation_pso_1, correlation_pso_2) + mean(error_1, error_2))
}

objetivo_3 <- function(x) {
  cost <- x[1]
  nu <- x[2]
  gamma <- x[3]
  
  training_data <- data[1:250,]
  validation_data <- data[251:450,]
  
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = training_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression"
    )
  
  predictions_pso <<- predict(svm_model, validation_data)
  
  # Correlación
  correlation_pso_1 <<- cor(predictions_pso, validation_data$CBFV.L_norm)
  
  training_data <- data[1:300,]
  validation_data <- data[301:500,]
  
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = training_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression"
    )
  
  predictions_pso <<- predict(svm_model, validation_data)
  
  # Correlación
  correlation_pso_2 <<- cor(predictions_pso, validation_data$CBFV.L_norm)
  
  training_data <- data[1:350,]
  validation_data <- data[352:596,]
  
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = training_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression"
    )
  
  predictions_pso <<- predict(svm_model, validation_data)
  
  # Correlación
  correlation_pso_3 <<- cor(predictions_pso, validation_data$CBFV.L_norm)
  
  print(correlation_pso_1)
  print(correlation_pso_2)
  print(correlation_pso_3)
  
  return(-mean(correlation_pso_1, correlation_pso_2, correlation_pso_3))
}

objetivo_4 <- function(x) {
  cost <- x[1]
  nu <- x[2]
  gamma <- x[3]
  
  training_data <- data[1:100,]
  validation_data <- data[101:200,]
  
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = training_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression"
    )
  
  predictions_pso <<- predict(svm_model, validation_data)
  
  # Correlación
  correlation_pso_1 <<- cor(predictions_pso, validation_data$CBFV.L_norm)
  
  training_data <- data[201:300,]
  validation_data <- data[301:400,]
  
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = training_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression"
    )
  
  predictions_pso <<- predict(svm_model, validation_data)
  
  # Correlación
  correlation_pso_2 <<- cor(predictions_pso, validation_data$CBFV.L_norm)
  
  training_data <- data[401:500,]
  validation_data <- data[501:596,]
  
  svm_model <-
    svm(
      CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
      + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
      data = training_data,
      cost = cost,
      nu = nu,
      gamma = gamma,
      kernel = "radial",
      type = "nu-regression"
    )
  
  predictions_pso <<- predict(svm_model, validation_data)
  
  # Correlación
  correlation_pso_3 <<- cor(predictions_pso, validation_data$CBFV.L_norm)
  
  print(correlation_pso_1)
  print(correlation_pso_2)
  print(correlation_pso_3)
  
  return(-mean(correlation_pso_1, correlation_pso_2, correlation_pso_3))
}

#MAIN##########################################################################



setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car")
data <- read.table("Sujeto1.txt", header = TRUE)

# Normalizo las señales MABP y CBFV.L
data <- normalize_signal(data, "MABP")
data <- normalize_signal(data, "CBFV.L")

# Agrego los retardos a esas señales
data <- lag_signal(data, 5, "MABP_norm", FALSE)
data <- lag_signal(data, 5, "CBFV.L_norm", TRUE)

# Separo los dataframes por la mitad
n <- nrow(data)
training_data <- data[1:(n / 2),]
validation_data <- data[(n / 2 + 1):n,]

# Agrego el escalón de presión invertido a los datos
training_data <- add_pressure_step(training_data, 20)
validation_data <- add_pressure_step(validation_data, 20)

# Entreno la v-SVR con las señales y 3 retardos cada una
svm <-
  svm(
    CBFV.L_norm
    ~ MABP_norm + MABP_norm_1 + MABP_norm_2 + MABP_norm_3
    + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
    data = training_data,
    kernel = "radial",
    type = "nu-regression"
  )

# Realizo las predicciones y calculo la correlación

predictions <- predict(svm, validation_data)
correlation <- cor(predictions, validation_data$CBFV.L_norm)

# PSO
lo <- c(0.25, 0.1, (1/(2*0.065^2)))
hi <- c(4096, 0.9, (1/(2*1024^2)))

start <- Sys.time()

resultados_pso <-
  psoptim(
    par = c(0, 0.1, 0),
    fn = objetivo,
    lower = lo,
    upper = hi,
    control = list(
      maxit=20,
      trace = 1,
      REPORT = 1
    )
  )

tiempo_pso_1_slot_1 <- Sys.time() - start

plot(validation_data$Time, validation_data$CBFV.L_norm, type="l", col="red")
lines(validation_data$Time, predictions_pso, col="blue")

validation_data <- data[1:(n / 2),]
training_data <- data[(n / 2 + 1):n,]

start <- Sys.time()

resultados_pso_2 <-
  psoptim(
    par = c(0, 0.1, 0),
    fn = objetivo,
    lower = lo,
    upper = hi,
    control = list(
      maxit=20,
      trace = 1,
      REPORT = 1
    )
  )

tiempo_pso_1_slot_2 <- Sys.time() - start

plot(validation_data$Time, validation_data$CBFV.L_norm, type="l", col="red")
lines(validation_data$Time, predictions_pso, col="blue")

start <- Sys.time()

resultados_pso_3 <-
  psoptim(
    par = c(0, 0.1, 0),
    fn = objetivo_2,
    lower = lo,
    upper = hi,
    control = list(
      maxit=20,
      trace = 1,
      REPORT = 1
    )
  )

tiempo_pso_2_slots <- Sys.time() - start

start <- Sys.time()

resultados_pso_4 <-
  psoptim(
    par = c(0, 0.1, 0),
    fn = objetivo_3,
    lower = lo,
    upper = hi,
    control = list(
      maxit=20,
      trace = 1,
      REPORT = 1
    )
  )

tiempo_pso_expanding_window_3_slots <- Sys.time() - start

start <- Sys.time()

resultados_pso_5 <-
  psoptim(
    par = c(0, 0.1, 0),
    fn = objetivo_4,
    lower = lo,
    upper = hi,
    control = list(
      maxit=20,
      trace = 1,
      REPORT = 1
    )
  )

tiempo_pso_sliding_window_3_slots <- Sys.time() - start

print(tiempo_pso_1_slot_1)
print(tiempo_pso_1_slot_2)
print(tiempo_pso_2_slots)
print(tiempo_pso_expanding_window_3_slots)

print(resultados_pso$value)
print(resultados_pso_2$value)
print(resultados_pso_3$value)
print(resultados_pso_4$value)

# Validación con respuesta escalón de presión

# new_validation_data <- data.frame(
#   Time = validation_data$Time,
#   CBFV.L_norm = validation_data$CBFV.L_norm,
#   CBFV.L_norm_1 = validation_data$CBFV.L_norm_1,
#   CBFV.L_norm_2 = validation_data$CBFV.L_norm_2,
#   CBFV.L_norm_3 = validation_data$CBFV.L_norm_3,
#   CBFV.L_norm_4 = validation_data$CBFV.L_norm_4,
#   CBFV.L_norm_5 = validation_data$CBFV.L_norm_5,
#   MABP_norm = validation_data$Pressure_step,
#   MABP_norm_1 = validation_data$Pressure_step,
#   MABP_norm_2 = validation_data$Pressure_step,
#   MABP_norm_3 = validation_data$Pressure_step,
#   MABP_norm_4 = validation_data$Pressure_step,
#   MABP_norm_5 = validation_data$Pressure_step
# )

# predictions2 <- predict(svm, new_validation_data)