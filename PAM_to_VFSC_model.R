# Cargar librerías
library(e1071)
library(pso)
library(signal)
library(mice)
library(timetk)
library(data.table)

set.seed(123)

#_______________________________________________________________________________

#time_cv <- timetk::time_series_cv(data = new_data_frame,  initial = "150 seconds", 
#                          assess = "150 seconds",
#                          slice_limit = 2)
#time_cv %>% tk_time_series_cv_plan()
#time_cv %>%
#  plot_time_series_cv_plan(time_vec, mabp, .interactive = FALSE)

#_______________________________________________________________________________

data <- read.table("Sujeto1.txt", header = TRUE)

time <- data$Time
mabp <- data$MABP
cbfv_L <- data$CBFV.L

# Aplicar normalización
imp_mabp <-
  mice(data.frame(time, mabp),
       method = "pmm",
       trimming = c(0.05, 0.95))
mabp_imputed <- complete(imp_mabp)

imp_cbfvL <-
  mice(data.frame(time, cbfv_L),
       method = "pmm",
       trimming = c(0.05, 0.95))
cbfvL_imputed <- complete(imp_cbfvL)

mabp_norm_1 <-
  (mabp_imputed$mabp - min(mabp_imputed$mabp)) / (max(mabp_imputed$mabp) - min(mabp_imputed$mabp))
cbfv_L_norm <-
  (cbfvL_imputed$cbfv_L - min(cbfvL_imputed$cbfv_L)) / (max(cbfvL_imputed$cbfv_L) - min(cbfvL_imputed$cbfv_L))

mabp_norm_2 <- shift(mabp_norm_1, n = -1, type = "lag", fill = NA)
mabp_norm_3 <- shift(mabp_norm_1, n = -2, type = "lag", fill = NA)
mabp_norm_4 <- shift(mabp_norm_1, n = -3, type = "lag", fill = NA)

data <- data.frame(time = time, mabp_norm_1 = mabp_norm_1, 
                   mabp_norm_2 = mabp_norm_2, mabp_norm_3 = mabp_norm_3,
                   mabp_norm_4 = mabp_norm_4, cbfv_L_norm = cbfv_L_norm)

data <- na.omit(data)

time <- data$time
mabp_norm_1 <- data$mabp_norm_1
mabp_norm_2 <- data$mabp_norm_2
mabp_norm_3 <- data$mabp_norm_3
mabp_norm_4 <- data$mabp_norm_4
cbfvL_norm <- data$cbfv_L_norm


# Crear una fecha de referencia (por ejemplo, la fecha actual)
fecha_ref <- Sys.time()

# Convertir el vector numérico en un vector de tiempo
time_vec <- as.POSIXct(fecha_ref + time, origin = "1970-01-01")


#new_data_frame <- data.frame(time_vec, mabp_norm, cbfv_L_norm)
#new_data_frame %>%
#  plot_time_series(time_vec, cbfv_L_norm, .interactive = TRUE)

n <- length(time_vec)
n_half <- n / 2

time_vec_1 <- time_vec[1:n_half]
mabp_norm_1_1 <- mabp_norm_1[1:n_half]
mabp_norm_2_1 <- mabp_norm_2[1:n_half]
mabp_norm_3_1 <- mabp_norm_3[1:n_half]
mabp_norm_4_1 <- mabp_norm_4[1:n_half]
cbfvL_norm_1 <- cbfv_L_norm[1:n_half]

time_vec_2 <- time_vec[(n_half + 1):n]
mabp_norm_2_1 <- mabp_norm_1[(n_half + 1):n]
mabp_norm_2_2 <- mabp_norm_2[(n_half + 1):n]
mabp_norm_2_3 <- mabp_norm_3[(n_half + 1):n]
mabp_norm_2_4 <- mabp_norm_4[(n_half + 1):n]
cbfvL_norm_2 <- cbfv_L_norm[(n_half + 1):n]

training_data <-
  data.frame(mabp_norm_1_1 = mabp_norm_1_1, mabp_norm_2_1 = mabp_norm_2_1,
             mabp_norm_3_1 = mabp_norm_3_1, mabp_norm_4_1 = mabp_norm_4_1,
             cbfvL_norm_1 = cbfvL_norm_1)

pressure <-
  c(rep(0, 50), rep(-1, 244))

pressure_step <-
  butter(2, 0.2, type = "low", fs = 0.5)

pressure_step_smooth <-
  as.numeric(signal::filter(pressure_step, pressure)) + 1

# plot(
#   time_vec_2,
#   pressure_step_smooth,
#   type = "l",
#   col = "red",
#   xlab = "Time",
#   ylab = "Signal"
# )

validation_data <-
  data.frame(mabp_norm_1_1 = mabp_norm_2_1, mabp_norm_2_1 = mabp_norm_2_2,
             mabp_norm_3_1 = mabp_norm_2_3, mabp_norm_4_1 = mabp_norm_2_4,
             cbfvL_norm_1 = cbfvL_norm_2)

# Crear el modelo SVM regresivo
svm_model <-
  svm(
    cbfvL_norm_1 ~ mabp_norm_1_1 + mabp_norm_2_1 + mabp_norm_3_1 + mabp_norm_4_1,
    data = training_data,
    kernel = "linear",
    type = "nu-regression"
  )

# Predecir las etiquetas de clase de los datos de prueba
predicted_labels_1 <- predict(svm_model, newdata = validation_data)

# Calcular la precisión del modelo utilizando el coeficiente de correlación
correlation_1 <- cor(predicted_labels_1, cbfvL_norm_2)

lines(time_vec_2, predicted_labels_1)


#_______________________________________________________________________________


process_signal <- function(signal) {
  response_signal <- signal[50:80]
  peak_signal <- signal[56:68]
  stable_signal <- signal[80:110]
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
  
  max_peak_signal <- max(signal[68:80])
  peak_stable_distance <-
    abs(max_peak_signal - tail(stable_signal, 1))
  score <- score - (peak_stable_distance * 100)
  
  stable_peak <- max(signal[68:80]) - tail(stable_signal, 1)
  drop_peak <- max(signal[68:80]) - min(peak_signal)
  
  if (stable_peak > drop_peak * .45) {
    score <- score - drop_peak * 10
  }
  
  if (min(stable_signal) < min_response_signal) {
    score <- score - (min_response_signal - min(stable_signal)) * 10
  }
  
  return(score)
}

#_____________________________________________________________________________


# Leer el archivo y almacenar los datos en un objeto llamado "data"
setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car")
data <- read.table("Sujeto1.txt", header = TRUE)

# Almacenar las variables como vectores
time <- data$Time
mabp <- data$MABP
cbfv_L <- data$CBFV.L

# Aplicar normalización
imp_mabp <-
  mice(data.frame(time, mabp),
       method = "pmm",
       trimming = c(0.05, 0.95))
mabp_imputed <- complete(imp_mabp)

imp_cbfvL <-
  mice(data.frame(time, cbfv_L),
       method = "pmm",
       trimming = c(0.05, 0.95))
cbfvL_imputed <- complete(imp_cbfvL)

mabp_norm <-
  (mabp_imputed$mabp - min(mabp_imputed$mabp)) / (max(mabp_imputed$mabp) - min(mabp_imputed$mabp))
cbfv_L_norm <-
  (cbfvL_imputed$cbfv_L - min(cbfvL_imputed$cbfv_L)) / (max(cbfvL_imputed$cbfv_L) - min(cbfvL_imputed$cbfv_L))

Data <- data.frame(cbind(time, mabp_norm, cbfv_L_norm))

# Dividir los vectores en dos conjuntos deterministas

n <- length(time)
n_half <- n / 2

time_1 <- time[1:n_half]
mabp_norm_1 <- mabp_norm[1:n_half]
cbfvL_norm_1 <- cbfv_L_norm[1:n_half]

time_2 <- time[(n_half + 1):n]
mabp_norm_2 <- mabp_norm[(n_half + 1):n]
cbfvL_norm_2 <- cbfv_L_norm[(n_half + 1):n]

# Crear conjunto entrenamiento y validación con escalón de presión
training_data <-
  data.frame(mabp_norm_1 = mabp_norm_1, cbfvL_norm_1 = cbfvL_norm_1)

pressure <-
  c(rep(0, 50), rep(-1, 250))

pressure_step <-
  butter(2, 0.2, type = "low", fs = 0.5)

pressure_step_smooth <-
  as.numeric(signal::filter(pressure_step, pressure)) + 1

plot(
  time_2,
  pressure_step_smooth,
  type = "l",
  col = "red",
  xlab = "Time",
  ylab = "Signal"
)

validation_data <-
  data.frame(mabp_norm_2 = pressure_step_smooth, cbfvL_norm_2 = cbfvL_norm_2)

# Crear el modelo SVM regresivo
svm_model <-
  svm(
    cbfvL_norm_1 ~ mabp_norm_1,
    data = training_data,
    kernel = "linear",
    type = "nu-regression"
  )

# Predecir las etiquetas de clase de los datos de prueba
predicted_labels_1 <- predict(svm_model, newdata = validation_data)

# Calcular la precisión del modelo utilizando el coeficiente de correlación
correlation_1 <- cor(predicted_labels_1, cbfvL_norm_2)

ecmna_1 <- 1 - correlation_1 ^ 2

var_coef_1 <-
  (sd(predicted_labels_1) / mean(predicted_labels_1)) * 100

#______________________________________________________________________________



#_____________________________________________________________________________

training_data <-
  data.frame(mabp_norm_2 = mabp_norm_2, cbfvL_norm_2 = cbfvL_norm_2)

validation_data <-
  data.frame(mabp_norm_1 = pressure_step_smooth, cbfvL_norm_1 = cbfvL_norm_1)

svm_model <-
  svm(
    cbfvL_norm_2 ~ mabp_norm_2,
    data = training_data,
    kernel = "linear",
    type = "nu-regression"
  )

predicted_labels_2 <- predict(svm_model, newdata = validation_data)

correlation_2 <- cor(predicted_labels_2, cbfvL_norm_1)

ecmna_2 <- 1 - correlation_2 ^ 2

var_coef_2 <-
  (sd(predicted_labels_2) / mean(predicted_labels_2)) * 100

if (correlation_1 > correlation_2) {
  lines(time_2, cbfvL_norm_2)
  lines(time_2, predicted_labels_1)
  process_signal(predicted_labels_1)
} else {
  lines(time_1, cbfvL_norm_1)
  lines(time_1, predicted_labels_2)
  process_signal(predicted_labels_2)
}


#______________________________________________________________________________
