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

setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car")
data <- read.table("Sujeto1.txt", header = TRUE)

# Normalizo las señales MABP y CBFV.L
data <- normalize_signal(data, "MABP")
data <- normalize_signal(data, "CBFV.L")

# Agrego los retardos a esas señales
data <- lag_signal(data, 5, "MABP_norm", FALSE)
data <- lag_signal(data, 5, "CBFV.L_norm", TRUE)

n <- nrow(data)
training_data <- data[1:(n / 2),]
validation_data <- data[(n / 2 + 1):n,]

nu <- seq(0.1, 0.9, 0.1)
cost <-
  c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)
sigma <- 2 ^ seq(-4, 10, 2)
gamma <- 1 / (2 * sigma ^ 2)

# cost <- c(2048, 4096)
# nu <- c(0.8, 0.9)
# gamma <- c((1 / (2 * 1024 ^ 2)), (1 / (2 * 32 ^ 2)))

start <- Sys.time()

tuneResult <-
  tune(
    svm,
    CBFV.L_norm
    ~ MABP_norm + MABP_norm_1 + MABP_norm_2 + MABP_norm_3 + CBFV.L_norm_1 +
      CBFV.L_norm_2 + CBFV.L_norm_3,
    data = training_data,
    ranges = list(cost = 4100, nu = 0.9, gamma = 0.0137),
    kernel = "radial",
    type = "nu-regression"
  )

time_simple_grid <- Sys.time() - start
tunedVals <- tuneResult$best.model
predictYsvm <- predict(tunedVals, validation_data)
cor(predictYsvm, validation_data$CBFV.L_norm)
