library(e1071)
library(pso)
library(caret)

# Función que genera datos aleatorios de distribución normal.
# El parámetro de entrada n indica la cantidad de datos.
# La salida es una lista de vectores.

get_random_norm_data <- function(n) {
  x = 1:n
  y = cumsum((rnorm(length(x))))
  return(list(x,y))
}

# Función que genera un gráfico dado dos vectores de datos

makePlot <- function(x,y) {
  plot(x, y, col="black", pch=5, lwd=1)
  lines(x, y, lty=2, lwd=2)
  grid()
}

# Función que calcula el Error cuadrático medio

rmse <- function(errval){
  val = sqrt(mean(errval^2))
  return(val)
}

# Función objetivo para optimizar una SVR Lineal con hiperparámetros nu y costo.
# Minimizando la suma de los errores cuadráticos

objetivo <- function(x) {
  
  cost <- x[1]
  nu <- x[2]
  
  # Ajustar la SVM con los hiperparámetros actuales
  
  svm_model <- svm(y ~ x, data = Data, cost = cost, nu = nu, kernel="linear")
  
  # # Calcular predicciones
  predictions <- predict(svm_model, newdata = Data)
  
  # Calcular el coeficiente de correlación
  return(-cor(predictions, Data$y))
  
  # Calcular el coeficiente de variación
  #mean_prediction <- mean(predictions)
  #return(-sd(predictions)/mean_prediction)
}


#__________________________Generar datos aleatorios_____________________________

# Definir una semilla

set.seed(123)

# Generar datos aleatorios y se desempaquetan en las variables x e y

datos_rnorm <- get_random_norm_data(75)
x = unlist(datos_rnorm[1])
y = unlist(datos_rnorm[2])


#________________________Linear Regression______________________________________

# Crear dataframe llamado 'Data'

#Data <- data.frame(cbind(x,y))
Data <- data.frame(x = x, y = y)

# Crear modelo de regresión lineal

linregress_model <- lm(y ~ x, data=Data)

# Realizar predicciones para el modelo de regresión para cada valor de x

predictYlinregress <- predict(linregress_model, Data)

# Error en el  modelo de regresión lineal

errval <- linregress_model$residuals  # Lo mismo que data$Y - predictedY
linregress_RMSE <- rmse(errval)

#___________________________Parámetros__________________________________________

costo_exp <- c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)
costo_lineal <- c(0.25, 292.8, 585.36, 877.91, 1170.46, 1463.02, 1755.57, 2048.13, 2340.68,
                  2633.23, 2925.79, 3218.34, 3510.89, 3803.45, 4096)
nu <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
sigma <- c(1/16, 1/4, 1, 4, 16, 2^6, 2^8, 2^10)


#_______________________Support Vector Regress__________________________________

# Modelo SVM

svm_model <- svm(y ~ x , Data)

# Predecir valores para todo X

predictYsvm <- predict(svm_model, Data)

# Error de SVR

errval <- Data$y - predictYsvm 
svr_RMSE <- rmse(errval)

#_______________________SVR Validación cruzada__________________________________

start <- Sys.time()

# Realizar la validación cruzada de 5 iteraciones
tuneResult1 <- tune.svm(y ~ x, kernel = "linear", data = Data, cost = costo_lineal, nu = nu, cross = 5)

# Imprime los resultados de la búsqueda de parámetros
print(tuneResult1)

#________________________SVR-CV tuned___________________________________________

costo_lineal_fino <- seq(tuneResult1$best.model$cost-100, 
                         tuneResult1$best.model$cost+100, 
                         length=6)
nu_fino <- seq(tuneResult1$best.model$nu-0.05, 
               tuneResult1$best.model$nu+0.05, 
               length=6)

# Realizar la validación cruzada de 5 iteraciones
tuneResult1 <- tune.svm(y ~ x, kernel = "linear", data = Data, cost = costo_lineal, nu = nu, cross = 5)

# Imprime los resultados de la búsqueda de parámetros
print(tuneResult1)

SVR_CV_GRID_TIME <- Sys.time() - start

# Imprime los resultados de la búsqueda de parámetros
print(tuneResult1)
print(SVR_CV_GRID_TIME)

tunedVals <-tuneResult1$best.model
predictYsvm2 <- predict(tunedVals, Data)

errval <- Data$y - predictYsvm2
svr_CV_RMSE <- rmse(errval)

#__________________________Comparación visual___________________________________

makePlot(x,y)
abline(linregress_model, col="red")
points(Data$x, predictYsvm, col = "blue", pch=4)
points(Data$x, predictYsvm, col = "blue", type="l")
points(Data$x, predictYsvm2, col = "green", pch=5)
points(Data$x, predictYsvm2, col = "green", type="l")
