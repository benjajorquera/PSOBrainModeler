# Cargar la biblioteca e1071
library(e1071)
library(pso)

setwd("C:/Users/benja/Documents/USACH/Memoria")

# Leer el archivo y almacenar los datos en un objeto llamado "data"
data <- read.table("Sujeto1.txt", header = TRUE)

# Almacenar las variables Time y MABP como vectores
time <- data$Time
mabp <- data$MABP

# Obtener el valor mínimo y máximo de mabp
mabp_min <- min(mabp)
mabp_max <- max(mabp)

# Aplicar la fórmula de normalización para cada valor de mabp
mabp_norm <- (mabp - mabp_min) / (mabp_max - mabp_min)

Data <- data.frame(cbind(time, mabp_norm))

# Obtener la longitud de los vectores de tiempo y MABP
n <- length(time)

# Obtener la mitad de la longitud de los vectores
n_half <- n/2

# Dividir los vectores en dos conjuntos deterministas
time_1 <- time[1:n_half]
mabp_norm_1 <- mabp_norm[1:n_half]

training_data <- data.frame(time_1 = time_1, mabp_norm_1 = mabp_norm_1)

# Crear el modelo SVM regresiva utilizando un kernel lineal
svm_model <- svm(mabp_norm_1 ~ time_1, kernel = "radial", type = "nu-regression")

# pressure_step <- c(rep(0, 50), rep(-0.5, 100)) # escalón invertido de presión

pressure_step <- butter(2, 0.2, type = "low", fs = 2) # filtro Butterworth de segundo orden

pressure_step_smooth <- filter(pressure_step, -mabp_norm_1) # aplicar filtro al escalón de presión negativo

# Alimentar la señal filtrada al modelo SVM y obtener la salida
output <- predict(svm_model, t(-pressure_step_smooth))

correlation <- cor(output, mabp_norm_1)

plot(time_1, output)


