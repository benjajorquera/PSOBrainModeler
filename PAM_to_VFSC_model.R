# Cargar la biblioteca e1071
library(e1071)
library(pso)
library(signal)

setwd("C:/Users/benja/Documents/USACH/Memoria")

# Leer el archivo y almacenar los datos en un objeto llamado "data"
data <- read.table("Sujeto1.txt", header = TRUE)

# Almacenar las variables Time y MABP como vectores
time <- data$Time
mabp <- data$MABP
cbfv_L <- data$CBFV.L

# Para normalizar: Mean (truncar) parámetro TR (descartar 5% mayor y menor) 95% mayor y 5% menor
# Quantile

# Aplicar la fórmula de normalización
mabp_norm <- (mabp - min(mabp)) / (max(mabp) - min(mabp)) # Errores de lectura / outliers
cbfv_L_norm <- (cbfv_L - min(cbfv_L)) / (max(cbfv_L) - min(cbfv_L))

Data <- data.frame(cbind(time, mabp_norm, cbfv_L_norm))

# Obtener la longitud de los vectores de tiempo y MABP
n <- length(time)

# Obtener la mitad de la longitud de los vectores
n_half <- n/2

# Dividir los vectores en dos conjuntos deterministas
time_1 <- time[1:n_half]
mabp_norm_1 <- mabp_norm[1:n_half]
cbfvL_norm_1 <- cbfv_L_norm[1:n_half]

plot(time_1, mabp_norm_1, type="l", ylim=c(-0.5,1.5))

points(time_1, cbfvL_norm_1, type="l", col="purple")


training_data <- data.frame(mabp_norm_1 = mabp_norm_1, cbfvL_norm_1 = cbfvL_norm_1)

time_2 <- time[(n_half+1):n]
mabp_norm_2 <- mabp_norm[(n_half+1):n]
cbfvL_norm_2 <- cbfv_L_norm[(n_half+1):n]

validation_data <- data.frame(mabp_norm_2 = mabp_norm_2, cbfvL_norm_2 = cbfvL_norm_2)

# Crear el modelo SVM regresiva utilizando un kernel lineal
svm_model <- svm(cbfvL_norm_1 ~ mabp_norm_1, data = training_data, kernel = "radial", type = "nu-regression")

# Predecir las etiquetas de clase de los datos de prueba
predicted_labels <- predict(svm_model, newdata = validation_data)

points(time_1, predicted_labels, type="l", col="blue")

# Calcular la precisión del modelo utilizando el coeficiente de correlación
correlation <- cor(predicted_labels, cbfvL_norm_1)

pressure <- c(rep(0, 50), rep(-1, 250)) # escalón invertido de presión

pressure_step <- butter(2, 0.2, type = "low", fs = 0.5) # filtro Butterworth de segundo orden

pressure_step_smooth <- as.numeric(filter(pressure_step, pressure)) +1  # aplicar filtro al escalón de presión negativo

points(time_1, pressure_step_smooth, type="l", col="red")

points(time_1, pressure_step_smooth + mabp_norm_1, type="l", col="orange")

svm_model2 <- svm(cbfvL_norm_1 ~ pressure_step_smooth + mabp_norm_1 , kernel = "radial", type = "nu-regression")

# Alimentar la señal filtrada al modelo SVM y obtener la salida
output <- predict(svm_model2, newdata = validation_data)

correlation2 <- cor(output, cbfvL_norm_1)

points(time_1, output, type="l", col="green")



