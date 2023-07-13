library(data.table)
library(mice)
library(signal)
library(dplyr)
library(e1071)
library(pso)

set.seed(123)

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
      tolerance = 1
    )
  return(svm_model)
}

objetivo <- function(x) {
  cost <- round(x[1], digits = 2)
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
    new_data_validation <-
      subset(data_partitions[[i]]$validation, select = -CBFV.L_norm)
    svr_model_pso <- svr_model(data_partitions[[i]]$training,
                               cost, nu, gamma)
    
    predictions_pso <-
      predict(svr_model_pso, new_data_validation)
    cors[i] <- cor(predictions_pso,
                           data_partitions[[i]]$validation$CBFV.L_norm)
    errors[i] <-
      sqrt(mean((
        data_partitions[[i]]$validation$CBFV.L_norm - predictions_pso
      ) ^ 2
      ))
  }
  
  data_model <- svm(
    CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
    + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
    data = data,
    cost = cost,
    nu = nu,
    gamma = gamma,
    kernel = "radial",
    type = "nu-regression",
    tolerance = 1
  )
  
  predictions_data <- predict(data_model, pressure_df)
  plot(predictions_data, type="l")
  
  cat(mean(cors), mean(errors), (1 - mean(cors) + mean(errors)), "\n")
  return((1 - mean(cors) + mean(errors)))
}

setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car/PSOvSVR/Data")

data_file <- read.table("Sujeto2.txt", header = TRUE)

setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car/PSOvSVR")

source("utils.R")
source("blocked_cross_validation.R")

# Normalizar las señales MABP y CBFV.L
data <- normalize_signal(data_file, "MABP")
data <- normalize_signal(data, "CBFV.L")

# Agregar los retardos a esas señales
data <- lag_signal(data, 5, "MABP_norm", FALSE)
data <- lag_signal(data, 5, "CBFV.L_norm", TRUE)

data_partitions <- blocked_cv(data)

pressure_df <- add_pressure_step(20)
#pressure_df$CBFV.L_norm <- rep(1, 60)
pressure_df <- lag_signal(pressure_df, 3, "MABP_norm", FALSE)
pressure_df <- lag_signal(pressure_df, 3, "CBFV.L_norm", TRUE)
pressure_df$CBFV.L_norm <- NULL

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