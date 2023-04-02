
library(e1071)
library(pso)

# Definir una semilla

set.seed(123)

# Generar datos de señales de tiempo reales
x <- 1:75
y <- cumsum(sin(x * 2 * pi / 25))

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

# Función objetivo para optimizar una SVR con hiperparámetros epsilon y costo.
# Minimizando la suma de los errores cuadráticos

objetivo <- function(x) {
  
  epsilon <- x[1]
  cost <- x[2]
  
  # Ajustar la SVM con los hiperparámetros actuales
  
  svm_model <- svm(y ~ x, data = Data, epsilon = epsilon, cost = cost)
  
  # Calcular la suma de errores cuadráticos (también puede ser precisión del modelo, u otro tipo de error)
  
  sq_error <- sum((predict(svm_model, newdata = Data) - Data$y)^2)
  
  # Devolver la suma de errores cuadráticos como valor de la función objetivo
  
  return(sq_error)
}


#________________________Linear Regression______________________________________

# Crear dataframe llamado 'Data'

Data <- data.frame(cbind(x,y))
makePlot(x,y)

# Crear modelo de regresión lineal

linregress_model <- lm(y ~ x, data=Data)

# Realizar predicciones para el modelo de regresión para cada valor de x

predictYlinregress <- predict(linregress_model, Data)

# Error en el  modelo de regresión lineal

errval <- linregress_model$residuals  # Lo mismo que data$Y - predictedY
linregress_RMSE <- rmse(errval)

#_______________________Support Vector Regress__________________________________

# Modelo SVM

svm_model <- svm(y ~ x , Data)

# Predecir valores para todo X

predictYsvm <- predict(svm_model, Data)

# Error de SVR

errval <- Data$y - predictYsvm 
svr_RMSE <- rmse(errval)

#________________________Modelo regresivo SVM afinado___________________________


# Identificando los "mejores" parámetros + tiempo de ejecución

# Realizar búsqueda grid, ajustar afinidad del grid si toma mucho tiempo

start <- Sys.time()

tuneResult1 <- tune(svm, y ~ x,  data = Data,
                    ranges = list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5)))
)

# Mapear resultados afinados

plot(tuneResult1)

# Grid más afinado

tuneResult <- tune(svm, y ~ x,  data = Data,
                   ranges = list(epsilon = seq(tuneResult1$best.model$epsilon-.15,
                                               tuneResult1$best.model$epsilon+.15,
                                               0.01), 
                                 cost = seq(2^(log2(tuneResult1$best.model$cost)-1),
                                            2^(log2(tuneResult1$best.model$cost)+1),
                                            length=6))
)

tunedSVR_time <- Sys.time() - start

# Mapeo de resultados de grid más afinado

plot(tuneResult)

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

lo <- c(0, 2^0.5)
hi <- c(1, 2^8)

# Ejecutar la optimización de PSO + tiempo de ejecución, ajustar los parámetros
# de control de la optimización para analizar el rendimiento y la precisión.

start <- Sys.time()

resultados_pso <- psoptim(par=c(1,1), fn = objetivo, lower = lo, upper = hi, 
                          control = list(s = 10, maxit = 50, w=0.7, c.p = 0.5, c.g = 0.5))

best_model <- svm(y ~ x, data = Data, epsilon = resultados_pso$par[1], cost = resultados_pso$par[2])

# Definir el rango de búsqueda para los hiperparámetros para la segunda iteración de PSO

lo <- c(best_model$epsilon-.15, 2^(log2(best_model$cost)-1))
hi <- c(best_model$epsilon+.15, 2^(log2(best_model$cost)+1))

resultados_pso <- psoptim(par=c(1,1), fn = objetivo, lower = lo, upper = hi, 
                          control = list(s = 10, maxit = 50, w=0.7, c.p = 0.5, c.g = 0.5))

PSO_svr_time <- Sys.time() - start

# Imprimir los resultados óptimos

print(resultados_pso$par)
best_model <- svm(y ~ x, data = Data, epsilon = resultados_pso$par[1], cost = resultados_pso$par[2])
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