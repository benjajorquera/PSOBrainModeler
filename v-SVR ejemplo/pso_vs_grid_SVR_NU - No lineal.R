library(e1071)
library(pso)

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
  sigma <- x[3]
  
  # Ajustar la SVM con los hiperparámetros actuales
  
  svm_model <- svm(y ~ x, data = Data, cost = cost, nu = nu, sigma = sigma, kernel="radial")
  
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

Data <- data.frame(cbind(x,y))

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
nu <- seq(0.1, 0.9, 0.1)
sigma <- c(1/16, 1/4, 1, 4, 16, 2^6, 2^8, 2^10)


#_______________________Support Vector Regress__________________________________

# Modelo SVM

svm_model <- svm(y ~ x , Data, kernel="radial")

# Predecir valores para todo X

predictYsvm <- predict(svm_model, Data)

# Error de SVR

errval <- Data$y - predictYsvm 
svr_RMSE <- rmse(errval)


#________________________Modelo regresivo no lineal SVM afinado_________________


# Identificando los "mejores" parámetros + tiempo de ejecución

# Realizar búsqueda grid, ajustar afinidad del grid si toma mucho tiempo

start <- Sys.time()

tuneResult1 <- tune(svm, y ~ x,  data = Data,
                    ranges = list(cost = costo_exp, nu = nu, sigma = sigma),
                    kernel="radial")

# Grid más afinado

costo_exp_fino <- seq(tuneResult1$best.parameters$cost/2, tuneResult1$best.parameters$cost*2, length=6)
sigma_fino <- seq(tuneResult1$best.parameters$sigma/2, tuneResult1$best.parameters$sigma*2, length=6)

nu_fino <- seq(tuneResult1$best.parameters$nu-0.05, 
               tuneResult1$best.parameters$nu+0.05, 
               length=6)

tuneResult <- tune(svm, y ~ x, data = Data,
                   ranges = list(cost = costo_exp_fino, 
                                 nu = nu_fino, sigma = sigma_fino),
                   kernel="radial")


tunedSVR_time <- Sys.time() - start

# Resultados de los parámetros optimizados/afinados

print(tuneResult)

# Predicción de todo X afinado

tunedVals <-tuneResult$best.model
predictYsvm2 <- predict(tunedVals, Data)

# Error de SVR afinada

errval <- Data$y - predictYsvm2 
svr_RMSE2 <- rmse(errval)   


#______________________________________PSO SVR__________________________________

# Identificar los mejores parámetros

lo <- c(0.25, 0.1, 1/16)
hi <- c(4096, 0.9, 2^10)

# Ejecutar la optimización de PSO + tiempo de ejecución, ajustar los parámetros
# de control de la optimización para analizar el rendimiento y la precisión.

start <- Sys.time()

resultados_pso <- psoptim(par=c(2000, 0.5, 1), fn = objetivo, lower = lo, upper = hi, 
                          control = list(s = 10, maxit = 10))

best_model <- svm(y ~ x, data = Data, cost = resultados_pso$par[1], nu = resultados_pso$par[2], sigma = resultadps_pso$par[3], kernel = "radial")

# Definir el rango de búsqueda para los hiperparámetros para la segunda iteración de PSO

lo <- c(best_model$cost/2, best_model$nu-0.05, best_model$sigma/2)
hi <- c(best_model$cost*2, best_model$nu+0.05, best_model$sigma*2)

resultados_pso <- psoptim(par=c(2000, 0.5, 1), fn = objetivo, lower = lo, upper = hi, 
                          control = list(s = 10, maxit = 10))

PSO_svr_time <- Sys.time() - start

# Imprimir los resultados óptimos

print(resultados_pso$par)
best_model <- svm(y ~ x, data = Data, cost = resultados_pso$par[1], nu = resultados_pso$par[2], sigma = resultados_pso$par[3], kernel = "radial")
predictYsvm3 <- predict(best_model, Data)

errval <- Data$y - predictYsvm3
svrPSO_RMSE <- rmse(errval)

#_________________________Comparar errores de los modelos_______________________

vals <- matrix(c(linregress_RMSE, svr_RMSE, svr_RMSE2, svrPSO_RMSE),ncol=4,byrow=TRUE)
colnames(vals) <- c("Lin regress  ","SVM model  ","Tuned SVM model ", "PSO SVR model")
rownames(vals) <- c("RMSE of model")
as.table(vals)

#_______________________Comparar tiempo de ejecución de los modelos_____________

vals <- matrix(c(tunedSVR_time, PSO_svr_time), ncol=2,byrow=TRUE)
colnames(vals) <- c("Tuned SVM model ", "PSO SVR model ")
rownames(vals) <- c("Execution Time")
as.table(vals)

#__________________________Comparación visual___________________________________

makePlot(x,y)
title("Original data + Linear regression + SVR + Tuned SVR + Tuned PSO SVR")
abline(linregress_model, col="red")
points(Data$x, predictYsvm, col = "blue", pch=4)
points(Data$x, predictYsvm, col = "blue", type="l")
points(Data$x, predictYsvm2, col = "green", pch=5)
points(Data$x, predictYsvm2, col = "green", type="l")
points(Data$x, predictYsvm3, col = "purple", pch=6)
points(Data$x, predictYsvm3, col = "purple", type="l")
