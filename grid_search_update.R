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

training_data <- add_pressure_step(training_data, 20)
validation_data <- add_pressure_step(validation_data, 20)

# Entreno la v-SVR con las señales y 3 retardos cada una
svm <-
  svm(
    CBFV.L_norm + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3
    ~ MABP_norm + MABP_norm_1 + MABP_norm_2 + MABP_norm_3,
    data = training_data,
    kernel = "radial",
    type = "nu-regression"
  )

# Creo nuevo data frame de validación agregando el escalón de presión a
# las columnas de presión

new_validation_data <- data.frame(
  Time = validation_data$Time,
  CBFV.L_norm = validation_data$CBFV.L_norm,
  CBFV.L_norm_1 = validation_data$CBFV.L_norm_1,
  CBFV.L_norm_2 = validation_data$CBFV.L_norm_2,
  CBFV.L_norm_3 = validation_data$CBFV.L_norm_3,
  MABP_norm = validation_data$Pressure_step,
  MABP_norm_1 = validation_data$Pressure_step,
  MABP_norm_2 = validation_data$Pressure_step,
  MABP_norm_3 = validation_data$Pressure_step
)

# Realizo las predicciones y calculo la correlación

predictions <- predict(svm, newdata = new_validation_data)
correlation <- cor(predictions, validation_data$CBFV.L_norm)

#PLOTS#########################################################################

# Grafico las variables de cada dataframe

plot(
  new_validation_data$Time,
  predictions,
  type = "l",
  xlab = "Tiempo",
  ylab = "Supuesto CBFV.L"
)
title(main = "Predicciones")

plot(
  training_data$Time,
  training_data$MABP_norm,
  type = "l",
  xlab = "Tiempo",
  ylab = "MABP Normalizado"
)
title(main = "Training Data")

plot(
  training_data$Time,
  training_data$CBFV.L_norm,
  type = "l",
  xlab = "Tiempo",
  ylab = "CBFV.L Normalizado"
)
title(main = "Training Data")

plot(
  validation_data$Time,
  validation_data$MABP_norm,
  type = "l",
  xlab = "Tiempo",
  ylab = "MABP Normalizado"
)
title(main = "Validation Data")

plot(
  validation_data$Time,
  validation_data$CBFV.L_norm,
  type = "l",
  xlab = "Tiempo",
  ylab = "CBFV.L Normalizado"
)
title(main = "Validation Data")

plot(
  new_validation_data$Time,
  new_validation_data$MABP_norm,
  type = "l",
  xlab = "Tiempo",
  ylab = "Escalón de presión"
)
title(main = "New validation Data")



#GRID##########################################################################


# Esta es la búsqueda grid, demora mucho

# nu <- seq(0.1, 0.9, 0.1)
# cost <-
#   c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)
# sigma <- c(1 / 16, 1 / 4, 1, 4, 16, 2 ^ 6, 2 ^ 8, 2 ^ 10)
# tuneResult <-
#   tune(
#     svm,
#     CBFV.L_norm + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3
#     ~ MABP_norm + MABP_norm_1 + MABP_norm_2 + MABP_norm_3,
#     data = training_data,
#     ranges = list(cost = cost, nu = nu, sigma = sigma),
#     kernel = "radial",
#     type = "nu-regression"
#   )
# tunedVals <- tuneResult$best.model
# predictYsvm <- predict(tunedVals, validation_data)
# cor(predictYsvm, validation_data$CBFV.L_norm)
