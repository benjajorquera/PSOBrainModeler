library(foreach)

.original <- function()
{
  n <- 10000
  s <- seq(length=n)
  a <- lapply(s, function(i) matrix(rnorm(16), 4))
  b <- lapply(s, function(i) matrix(rnorm(16), 4))
  d <- lapply(s, function(i) matrix(rnorm(16), 4))
  e <- lapply(s, function(i) matrix(rnorm(16), 4))
  
  expected <- list(a=a, b=b, d=d, e=e)
  
  comb <- function(accum, ...) {
    s <- seq(along=accum)
    names(s) <- names(accum)
    lapply(s, function(i) c(accum[[i]], lapply(list(...), function(a) a[[i]])))
  }
  
  init <- list(a=list(), b=list(), d=list(), e=list())
  
  actual <- foreach(i=icount(length(a)), .combine=comb, .init=init,
                    .multicombine=TRUE) %dopar% {
    list(a=a[[i]], b=b[[i]], d=d[[i]], e=e[[i]])
  }
  
  print(identical(actual, expected))
}

.test1 <- function()
{
  lags <- 1:60
  params <- 1:100
  folds <- 1:10
  n <- 100

  lag.names <- paste("lag", lags)
  param.names <- paste("param", params)
  fold.names <- paste("fold", folds)
  
    # Start time
  start.time <- Sys.time()
  
  answer <- list()
  for(l in 1:length(lags))
  {
    subanswer <- list()
    for(p in 1:length(params))
    {
      subsubanswer <- list()
      for(f in 1:length(folds))
      {
        fold.answer <- list()
        fold.answer[["fold"]] <- f
        fold.answer[["data"]] <- rnorm(n)
        
        other.folds <- list()
        for(o in (1:length(folds))[-f])
        {
          other.fold.answer <- list()
          other.fold.answer[["fold"]] <- f
          other.fold.answer[["data"]] <- rnorm(n)
          
          other.folds[[fold.names[o]]] <- other.fold.answer
        }
        
        fold.answer[["others"]] <- other.folds
        
        subsubanswer[[fold.names[f]]] <- fold.answer
      }
      
      subanswer[[param.names[p]]] <- subsubanswer
    }
    
    answer[[lag.names[l]]] <- subanswer
  }
  
  # End time
  end.time <-Sys.time()
  
  answer[["start.time"]] <- start.time
  answer[["end.time"]] <- end.time
  answer[["time"]] <- end.time - start.time
  
  invisible(answer)
}

.test2 <- function()
{
  lags <- 60
  params <- 100
  folds <- 10
  n <- 100

  lag.names <- paste("lag", 1:lags)
  param.names <- paste("param", 1:params)
  fold.names <- paste("fold", 1:folds)
  
    # Start time
  start.time <- Sys.time()
  
  answer <- list()
  for(l in 1:lags)
  {
    subanswer <- list()
    for(p in 1:params)
    {
      subsubanswer <- foreach(f = 1:folds) %dopar%
      {
        fold.answer <- list()
        fold.answer[["fold"]] <- f
        fold.answer[["data"]] <- rnorm(n)
        
        others <- (1:folds)[-f]
        other.folds <- foreach(o = others) %dopar%
        {
          other.fold.answer <- list()
          other.fold.answer[["fold"]] <- f
          other.fold.answer[["data"]] <- rnorm(n)
          
          other.fold.answer
        }
        
        names(other.folds) <- fold.names[others]
        fold.answer[["others"]] <- other.folds
        
        fold.answer
      }
      
      names(subsubanswer) <- fold.names
      subanswer[[param.names[p]]] <- subsubanswer
    }
    
    answer[[lag.names[l]]] <- subanswer
  }
  
  # End time
  end.time <-Sys.time()
  
  answer[["start.time"]] <- start.time
  answer[["end.time"]] <- end.time
  answer[["time"]] <- end.time - start.time
  
  invisible(answer)
}

.test3 <- function()
{
  lags <- 60
  params <- 1:100
  folds <- 1:10
  n <- 100

  lag.names <- paste("lag", 1:lags)
  param.names <- paste("param", params)
  fold.names <- paste("fold", folds)
  
    # Start time
  start.time <- Sys.time()
  
  answer <- foreach(l = 1:lags) %dopar%
  {
    subanswer <- list()
    for(p in 1:length(params))
    {
      subsubanswer <- list()
      for(f in 1:length(folds))
      {
        fold.answer <- list()
        fold.answer[["fold"]] <- f
        fold.answer[["data"]] <- rnorm(n)
        
        other.folds <- list()
        for(o in (1:length(folds))[-f])
        {
          other.fold.answer <- list()
          other.fold.answer[["fold"]] <- f
          other.fold.answer[["data"]] <- rnorm(n)
          
          other.folds[[fold.names[o]]] <- other.fold.answer
        }
        
        fold.answer[["others"]] <- other.folds
        
        subsubanswer[[fold.names[f]]] <- fold.answer
      }
      
      subanswer[[param.names[p]]] <- subsubanswer
    }
    
    subanswer
  }
  names(answer) <- lag.names
  
  # End time
  end.time <-Sys.time()
  
  answer[["start.time"]] <- start.time
  answer[["end.time"]] <- end.time
  answer[["time"]] <- end.time - start.time
  
  invisible(answer)
}

.test4 <- function()
{
  lags <- 60
  params <- 100
  folds <- 1:10
  n <- 100

  lag.names <- paste("lag", 1:lags)
  param.names <- paste("param", 1:params)
  fold.names <- paste("fold", folds)
  
    # Start time
  start.time <- Sys.time()
  
  answer <- foreach(l = 1:lags) %dopar%
  {
    subanswer <- foreach(p = 1:params) %dopar%
    {
      subsubanswer <- list()
      for(f in 1:length(folds))
      {
        fold.answer <- list()
        fold.answer[["fold"]] <- f
        fold.answer[["data"]] <- rnorm(n)
        
        other.folds <- list()
        for(o in (1:length(folds))[-f])
        {
          other.fold.answer <- list()
          other.fold.answer[["fold"]] <- f
          other.fold.answer[["data"]] <- rnorm(n)
          
          other.folds[[fold.names[o]]] <- other.fold.answer
        }
        
        fold.answer[["others"]] <- other.folds
        
        subsubanswer[[fold.names[f]]] <- fold.answer
      }
      
      subsubanswer
    }
    names(subanswer) <- param.names
    
    subanswer
  }
  names(answer) <- lag.names
  
  # End time
  end.time <-Sys.time()
  
  answer[["start.time"]] <- start.time
  answer[["end.time"]] <- end.time
  answer[["time"]] <- end.time - start.time
  
  invisible(answer)
}


# Benchmark for sequential running
# .test1()   210 s x 5 replications => 42.0 s 
# .test2()  5676 m x 5 replications => 18.9 minutes
# .test3()   174 s x 5 replications => 34.0 s
# .test4()   210 s x 5 replications => 42.0 s

