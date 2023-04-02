
TOP.DIR <- file.path("", "research")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")

SIGSIM.SCRIPT.BASENAME <- paste("Signal", "simulation", sep = "-")
SIGSIM.SCRIPT.BASENAME <- paste(SIGSIM.SCRIPT.BASENAME, "R", sep = ".")
SIGSIM.SCRIPT.NAME <- file.path(SVR.DIR, SIGSIM.SCRIPT.BASENAME)
source(SIGSIM.SCRIPT.NAME)

LAGSIG.SCRIPT.BASENAME <- paste("Signal", "lagging", sep = "-")
LAGSIG.SCRIPT.BASENAME <- paste(LAGSIG.SCRIPT.BASENAME, "R", sep = ".")
LAGSIG.SCRIPT.NAME <- file.path(SVR.DIR, LAGSIG.SCRIPT.BASENAME)
source(LAGSIG.SCRIPT.NAME)

SVR.SCRIPT.BASENAME <- paste("Signal", "SVR", "v1", sep = "-")
SVR.SCRIPT.BASENAME <- paste(SVR.SCRIPT.BASENAME, "R", sep = ".")
SVR.SCRIPT.NAME <- file.path(SVR.DIR, SVR.SCRIPT.BASENAME)
source(SVR.SCRIPT.NAME)

PARAM.LIST <- list(type = "nu-regression", kernel = "radial",
                   gamma = 1 / (2 * 2 ^ 2), cost = 2, nu = 0.5)



test <- function(n.cardiac.cycles = 375,
                 sampling.time = 0.6, n.folds = 4, seed = 2)
{
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
  
  dd <- separateSignalIntoFolds(signal = d,
                                n.folds = n.folds,
                                time.colname = "Time")

  ddd <- lagSignalForCrossValidationAndRefold(dd, list(ABP = 1, CO2 = 2))

  input <- c("ABP.lag1", "ABP", "CO2.lag2", "CO2.lag1", "CO2")
  
  dddd <- crossValidate(signal = ddd,
                        output.colname = "CBFV",
                        input.colnames = input,
                        process.fold.function = nu.svr.process.fold.v1,
                        combine.folds.function = nu.svr.combine.folds.v1,
                        model.param.list = PARAM.LIST,
                        keep.model = FALSE)
  invisible(dddd)
}
