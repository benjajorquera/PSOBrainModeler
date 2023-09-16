library(e1071)
library(pso)
library(cvTools)

set.seed(123)

x = 1:75
y = cumsum((rnorm(length(x))))
Data <- data.frame(cbind(x,y))

makePlot <- function(x,y) {
  plot(x, y, col="black", pch=5, lwd=1)
  lines(x, y, lty=2, lwd=2)
  grid()
}

rmse <- function(errval){
  val = sqrt(mean(errval^2))
  return(val)
}

objetivo <- function(x, Data) {
  
  cost <- x[1]
  nu <- x[2]
  
  fit <- svm(y ~ x, data = Data, cost = cost, nu = nu, kernel="linear")
  
 predictions <- predict(svm_model, newdata = Data)
  
 return(-cor(predictions, Data$y))
}

lo <- c(0.25, 0.1)
hi <- c(4096, 0.9)

folds <- cvFolds(nrow(Data), K = 5)

psoptim_cv <- function(Data, folds) {
  fold_resultados <- list()
  for (i in 1:folds$K) {
    Data_train <- Data[-folds$subsets[folds$which[i],],]
    Data_test <- Data[folds$subsets[folds$which[i],],]
    objetivo_fold <- function(x) objetivo(x, Data_train)
    resultados_pso_fold <- psoptim(par=c(1, 1), fn = objetivo_fold, lower = lo, upper = hi, 
                                   control = list(s = 5, maxit = 10))
    fold_resultados[[i]] <- resultados_pso_fold$par
  }
  return(fold_resultados)
}

start <- Sys.time()

resultados_pso_cv <- psoptim_cv(Data, folds)

resultados_pso_mean <- apply(do.call(rbind, resultados_pso_cv), 2, mean)

best_model <- svm(y ~ x, data = Data, kernel="linear", cost = resultados_pso_mean[1], nu = resultados_pso_mean[2])

lo <- c(resultados_pso_mean[1]-100, resultados_pso_mean[2]-0.05)
hi <- c(resultados_pso_mean[1]+100, resultados_pso_mean[2]+0.05)

resultados_pso_cv <- psoptim_cv(Data, folds)

resultados_pso_mean <- apply(do.call(rbind, resultados_pso_cv), 2, mean)

best_model2 <- svm(y ~ x, data = Data, kernel="linear", cost = resultados_pso_mean[1], nu = resultados_pso_mean[2])

PSO_svr_time <- Sys.time() - start
print(PSO_svr_time)

# Imprimir los resultados óptimos

predictYsvm2 <- predict(best_model, Data)
predictYsvm3 <- predict(best_model2, Data)

errval <- Data$y - predictYsvm2
svrPSO_RMSE <- rmse(errval)

errval <- Data$y - predictYsvm3
svrPSO_RMSE2 <- rmse(errval)

makePlot(x,y)
points(Data$x, predictYsvm3, col = "blue", pch=4)
points(Data$x, predictYsvm3, col = "blue", type="l")

