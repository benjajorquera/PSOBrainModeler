# Cargar librerías
library(e1071)
library(pso)
library(signal)
library(mice)

set.seed(123)


# Leer el archivo y almacenar los datos en un objeto llamado "data"
setwd("C:/Users/benja/Documents/USACH/Memoria/pso-svr-car")
data <- read.table("Sujeto1.txt", header = TRUE)

# Almacenar las variables Time y MABP como vectores
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
predicted_labels <- predict(svm_model, newdata = validation_data)

# Calcular la precisión del modelo utilizando el coeficiente de correlación
correlation <- cor(predicted_labels, cbfvL_norm_2)

lines(time_2, cbfvL_norm_2)
lines(time_2, predicted_labels)
