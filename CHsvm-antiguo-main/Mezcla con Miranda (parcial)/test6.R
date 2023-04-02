
# TOP.DIR <- "/media/jljara/7443-AD5A/Research-backup"
TOP.DIR <- file.path("", "research")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")

SIGRID.SCRIPT.BASENAME <- paste("Signal", "grid", sep = "-")
SIGRID.SCRIPT.BASENAME <- paste(SIGRID.SCRIPT.BASENAME, "R", sep = ".")
SIGRID.SCRIPT.NAME <- file.path(SVR.DIR, SIGRID.SCRIPT.BASENAME)
source(SIGRID.SCRIPT.NAME)

SIGLAG.SCRIPT.BASENAME <- paste("Signal", "lagging", sep = "-")
SIGLAG.SCRIPT.BASENAME <- paste(SIGLAG.SCRIPT.BASENAME, "R", sep = ".")
SIGLAG.SCRIPT.NAME <- file.path(SVR.DIR, SIGLAG.SCRIPT.BASENAME)
source(SIGLAG.SCRIPT.NAME)

SIGSIM.SCRIPT.BASENAME <- paste("Signal", "simulation", sep = "-")
SIGSIM.SCRIPT.BASENAME <- paste(SIGSIM.SCRIPT.BASENAME, "R", sep = ".")
SIGSIM.SCRIPT.NAME <- file.path(SVR.DIR, SIGSIM.SCRIPT.BASENAME)
source(SIGSIM.SCRIPT.NAME)

SVR.SCRIPT.BASENAME <- paste("Signal", "SVR", "v2", sep = "-")
SVR.SCRIPT.BASENAME <- paste(SVR.SCRIPT.BASENAME, "R", sep = ".")
SVR.SCRIPT.NAME <- file.path(SVR.DIR, SVR.SCRIPT.BASENAME)
source(SVR.SCRIPT.NAME)


  
test <- function(n.cardiac.cycles = 375,
                 sampling.time = 0.6, n.folds = 4, seed = 2,
                 parallel.lagging = "seq", parallel.param.search = "seq")
{
  # Gajardo (2014) used ABP = 2:10
  lags <- list(ABP = 0:3, CO2 = 0:2, CBFV = 0:3)
  lags <- list(ABP = 0:1, CO2 = 0:1, CBFV = 0:1)
  lags <- list(ABP = 3:4, CO2 = 1:2, CBFV = 1:2)
  
  # From Gajardo (2014)
  sigma <- 2^(-1:5)
  gamma <- 1 / (2 * sigma^2)
  cost <- 2^(-2:10)
  nu <- seq(0.1, 0.9, 0.1)
  
  sigma <- 2^(-1:1)
  gamma <- 1 / (2 * sigma^2)
  cost <- 2^(-2:2)
  nu <- seq(0.1, 0.9, 0.3)
    
  parameters <- list(gamma = gamma,
                     cost = cost,
                     nu = nu)
  
  abp <- getSyntheticABPSignal(n.cardiac.cycles = n.cardiac.cycles,
                               seed = seed)
  raw.high <- getSyntheticABPAndCBFVSignals(abp, at.ari = 6)
  raw.low <- getSyntheticABPAndCBFVSignals(abp, at.ari = 1)
  
  mean.high <- getMeanSyntheticSignals(raw.high)
  mean.low <- getMeanSyntheticSignals(raw.low)
  mean.abp <- mean(mean.high[[2]])
  co2 <- rev(abs(mean.high[[2]] - mean.abp))
  co2p <- (co2 - min(co2))/(max(co2) - min(co2))
  cbfv <- (1 - co2p) * mean.high[[3]] + co2p * mean.low[[3]]
  
  time <- mean.high[["Time"]]
  t.ini <- time[1]
  t.end <- time[length(time)]
  t <- seq(t.ini, t.end, sampling.time)

  co2.interpolation <- spline(time, co2, xout = t)
  co2 <- co2.interpolation[["y"]]

  cbfvh.interpolation <- spline(time, mean.high[[3]], xout = t)
  cbfvh <- cbfvh.interpolation[["y"]]

  cbfvl.interpolation <- spline(time, mean.low[[3]], xout = t)
  cbfvl <- cbfvl.interpolation[["y"]]
  
  mean.high[[3]] <- cbfv
  signals <- resampleMeanSyntheticSignal(mean.high, t)
  
  d <- data.frame(Time = signals[[1]], ABP = signals[[2]], CO2 = co2,
                  CBFVH = cbfvh, CBFVL = cbfvl, CBFV = signals[[3]])
  i <- sample(t)
  d <- d[i, ]
  rownames(d) <- NULL
  
  # Start time
  start.time <- Sys.time()
  
  ans <- grid.lags.and.parameters(
           signal = d, time.colname = "Time", # default fold.colname,
           output.colname = "CBFV", input.colnames = c("ABP", "CO2"),
           process.fold.function = nu.svr.process.fold.v2,
           combine.folds.function = nu.svr.combine.folds.v2,
           lags = lags,
           combine.lags.function = nu.svr.combine.lags.v2,
           # default lagging.if.folded,
           parallel.lagging = parallel.lagging,
           parameters = parameters,
           combine.params.function = nu.svr.combine.params.v2,
           n.folds = n.folds,
           parallel.param.search = parallel.param.search,
           output.var.name = "CBFV",
           n.keep = 100)
  
  # End time
  end.time <-Sys.time()
  
  ans[["start.time"]] <- start.time
  ans[["end.time"]] <- end.time
  ans[["lagging.time"]] <- end.time - start.time
  
  invisible(ans)
}
