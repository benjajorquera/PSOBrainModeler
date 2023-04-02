# Cargar la biblioteca e1071
library(e1071)
library(pso)

setwd("C:/Users/benja/Documents/USACH/Memoria")

# Leer el archivo y almacenar los datos en un objeto llamado "data"
data <- read.table("Sujeto1.txt", header = TRUE)

# Almacenar las variables Time y MABP como vectores
time <- data$Time
mabp <- data$MABP

# Crear un gráfico de dispersión
plot(time, mabp, type = "l", xlab = "Tiempo", ylab = "MABP")

# Agregar un título al gráfico
title(main = "Gráfico de MABP vs. Tiempo")

# Obtener el valor mínimo y máximo de mabp
mabp_min <- min(mabp)
mabp_max <- max(mabp)

# Aplicar la fórmula de normalización para cada valor de mabp
mabp_norm <- (mabp - mabp_min) / (mabp_max - mabp_min)

Data <- data.frame(cbind(time, mabp_norm))

# Crear un gráfico de dispersión
plot(time, mabp_norm, type = "l", xlab = "Tiempo", ylab = "MABP Normalizado")

# Agregar un título al gráfico
title(main = "Gráfico de MABP Normalizado vs. Tiempo")

# Obtener la longitud de los vectores de tiempo y MABP
n <- length(time)

# Obtener la mitad de la longitud de los vectores
n_half <- n/2

# Dividir los vectores en dos conjuntos deterministas
time_1 <- time[1:n_half]
mabp_norm_1 <- mabp_norm[1:n_half]

training_data <- data.frame(time_1 = time_1, mabp_norm_1 = mabp_norm_1)

time_2 <- time[(n_half+1):n]
mabp_norm_2 <- mabp_norm[(n_half+1):n]

validation_data <- data.frame(time_2 = time_2, mabp_norm_2 = mabp_norm_2)

# Crear el modelo SVM regresiva utilizando un kernel lineal
svm_model <- svm(mabp_norm_1 ~ time_1, kernel = "radial", type = "nu-regression")

# Predecir las etiquetas de clase de los datos de prueba
predicted_labels <- predict(svm_model, newdata = validation_data)

# Calcular la precisión del modelo utilizando el coeficiente de correlación
correlation <- cor(predicted_labels, mabp_norm_1)

points(time_1, predicted_labels, col = "blue", pch=4)

# Definir el rango de los hiperparámetros de la SVM
nu <- seq(0.1, 0.9, 0.1)
cost <- c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)

# Realizar una búsqueda grid de los hiperparámetros

start <- Sys.time()

tuneResult1 <- tune(svm, mabp_norm_1 ~ time_1, data = training_data,
                    ranges = list(cost = cost, nu = nu),
                    kernel = "radial", type = "nu-regression")

# Búsqueda grid más fina

costo_fino <- seq(tuneResult1$best.model$cost/2, 
                  tuneResult1$best.model$cost*2, 
                  length=5)

nu_fino <- seq(tuneResult1$best.model$nu-0.05, 
               tuneResult1$best.model$nu+0.05, 
               length=5)

tuneResult <- tune(svm, mabp_norm_1 ~ time_1, data = training_data,
                   ranges = list(cost = costo_fino, nu = nu_fino), 
                   kernel = "radial", type = "nu-regression")

tunedVals <-tuneResult$best.model
predictYsvm <- predict(tunedVals, newdata = validation_data)

correlation2 <- cor(predictYsvm, mabp_norm_1)

grid_time <- Sys.time() - start
print(grid_time)

points(time_1, predictYsvm, col = "green", pch=5)

# Realizar una PSO para la búsqueda de hiperparámetros

objetivo <- function(x) {
  
  cost <- x[1]
  nu <- x[2]
  
  # Ajustar la SVM con los hiperparámetros actuales
  
  svm_model <- svm(mabp_norm_1 ~ time_1, cost = cost, nu = nu, kernel="radial",
                   type = "nu-regression")
  
  # Calcular predicciones
  predictions <- predict(svm_model, newdata = validation_data)
  
  # Calcular el coeficiente de correlación
  return(-cor(predictions, mabp_norm_1))
}

start <- Sys.time()

lo <- c(0.25, 0.1)
hi <- c(4096, 0.9)

resultados_pso <- psoptim(par=c(2000, 0.5), fn = objetivo, lower = lo, upper = hi, 
                          control = list(s = 10, maxit = 5))

best_model <- svm(mabp_norm_1 ~ time_1, cost = resultados_pso$par[1], nu = resultados_pso$par[2], kernel="radial",
                  type = "nu-regression")

lo <- c(best_model$cost/2, best_model$nu-0.05)
hi <- c(best_model$cost*2, best_model$nu+0.05)

resultados_pso <- psoptim(par=c(best_model$cost, best_model$nu), fn = objetivo, lower = lo, upper = hi, 
                          control = list(s = 10, maxit = 5))

best_model <- svm(mabp_norm_1 ~ time_1, cost = resultados_pso$par[1], nu = resultados_pso$par[2], kernel="radial",
                  type = "nu-regression")

predictYsvm2 <- predict(best_model, newdata = validation_data)

correlation3 <- cor(predictYsvm2, mabp_norm_1)

pso_time <- Sys.time() - start
print(pso_time)

points(time_1, predictYsvm2, col = "purple", pch=4)
