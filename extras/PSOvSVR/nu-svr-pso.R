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
  
  cors <- numeric(length(data_partitions))
  errors <- numeric(length(data_partitions))
  
  for (df_list in 1:length(data_partitions)) {
    new_data_validation <-
      subset(data_partitions[[df_list]]$validation, select = -CBFV.L_norm)
    print(new_data_validation)
    svr_model_pso <- svr_model(data_partitions[[df_list]]$training,
                               cost, nu, gamma)
    
    predictions_pso <-
      predict(svr_model_pso, new_data_validation)
    cors[df_list] <- cor(predictions_pso,
                         data_partitions[[df_list]]$validation$CBFV.L_norm)
    errors[df_list] <-
      sqrt(mean((
        data_partitions[[df_list]]$validation$CBFV.L_norm - predictions_pso
      ) ^ 2
      ))
  }
  
  #cristobal.acosta@usach.cl Acceso a cluster para correr programas CC JL
  
  ######################################################################
  
  data_model <- svr_model(data, cost, nu, gamma)
  
  CBFV_predictions <- generate_signal_response_predictions(
    data_model,
    pressure_df,
    3,
    40,
    c("MABP_norm", "CBFV.L_norm"),
    3,
    c("MABP_norm"),
    c(1),
    "CBFV.L_norm",
    0.8
  )
  
  signal_score <- process_signal(CBFV_predictions$CBFV.L_norm, 3)
  
  ############################################################
  
  if (signal_score > 0) {
    plot(CBFV_predictions$CBFV.L_norm, type = "l")
  }
  
  cat(mean(cors),
      mean(errors),
      (1 - mean(cors) + mean(errors)),
      (signal_score * .1),
      "\n")
  return((2 - mean(cors) + mean(errors) - (signal_score * .1)))
}

setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car/PSOvSVR/Data")

data_file <- read.table("Sujeto2.txt", header = TRUE)

setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car/PSOvSVR")

source("utils.R")
source("blocked_cross_validation.R")
source("score_signal.R")

# Normalizar las señales MABP y CBFV.L
data <- normalize_signals(data_file, c("MABP", "CBFV.L"))

# Agregar los retardos a esas señales
data <- lag_normalized_signal(data, 5, c("MABP", "CBFV.L"))

data_partitions <- blocked_cv(data, 5, 0.2)

#pressure_df$CBFV.L_norm <- NULL

lo <- c(0.25, 0.1, (1 / (2 * 1024 ^ 2)))
hi <- c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)))

pressure_df <- add_pressure_step(3, 40, 2, 0.2)

# 512, 0.7, 0.00195 global optimo de búsqueda grid

# TODO: limpiar los dataframes. filtro escalón. escapar minimo local.

time <- Sys.time()

# Retardos se agregan a la optimización
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
      maxit.stagnate = 30,
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

data_model <- svm(
  CBFV.L_norm ~ MABP_norm + MABP_norm_1 + MABP_norm_2
  + MABP_norm_3 + CBFV.L_norm_1 + CBFV.L_norm_2 + CBFV.L_norm_3,
  data = data,
  cost = 200,
  nu = 0.4,
  gamma = 1,
  kernel = "radial",
  type = "nu-regression",
  tolerance = 1
)


CBFV_predictions <- generate_signal_response_predictions(
  data_model,
  pressure_df,
  3,
  40,
  c("MABP_norm", "CBFV.L_norm"),
  3,
  c("MABP_norm"),
  c(1),
  "CBFV.L_norm",
  0.8
)

plot(CBFV_predictions$CBFV.L_norm)

process_signal(pressure_df_model$CBFV.L_norm_1)
print(process_signal(CBFV_predictions$CBFV.L_norm, 3))
