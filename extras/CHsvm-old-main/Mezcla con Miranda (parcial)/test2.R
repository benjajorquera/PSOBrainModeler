
require(e1071)
require(ggplot2)
require(gridExtra)

TOP.DIR <- "/media/jljara/7443-AD5A/Research-2015-01-21"
#~ TOP.DIR <- file.path("", "research")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")

SIGCV.SCRIPT.BASENAME <- paste("Signal", "cross", "validation", sep = "-")
SIGCV.SCRIPT.BASENAME <- paste(SIGCV.SCRIPT.BASENAME, "R", sep = ".")
SIGCV.SCRIPT.NAME <- file.path(SVR.DIR, SIGCV.SCRIPT.BASENAME)
source(SIGCV.SCRIPT.NAME)

SIGLAG.SCRIPT.BASENAME <- paste("Signal", "lagging", sep = "-")
SIGLAG.SCRIPT.BASENAME <- paste(SIGLAG.SCRIPT.BASENAME, "R", sep = ".")
SIGLAG.SCRIPT.NAME <- file.path(SVR.DIR, SIGLAG.SCRIPT.BASENAME)
source(SIGLAG.SCRIPT.NAME)

SIGSIM.SCRIPT.BASENAME <- paste("Signal", "simulation", sep = "-")
SIGSIM.SCRIPT.BASENAME <- paste(SIGSIM.SCRIPT.BASENAME, "R", sep = ".")
SIGSIM.SCRIPT.NAME <- file.path(SVR.DIR, SIGSIM.SCRIPT.BASENAME)
source(SIGSIM.SCRIPT.NAME)

test1 <- function(n.cardiac.cycles = 15, ari.values = 5,
                  sampling.time = 0.6, seed = 2)
{
  abp <- getSyntheticABPSignal(n.cardiac.cycles = n.cardiac.cycles,
                               variability.range = -2:2, noise.sd = 1.5,
                               seed = seed)
  
  raw.signals <- getSyntheticABPAndCBFVSignals(abp, at.ari = ari.values)
  p2 <- getSyntheticABPAndCBFVSignalPlot(raw.signals)
  print(p2)
  
  mean.signals <- getMeanSyntheticSignals(raw.signals)
  p3 <- getMeanSyntheticSignalPlot(mean.signals)
  print(p3)
  
  t.ini <- mean.signals[1, "Time"]
  t.end <- mean.signals[nrow(mean.signals), "Time"]
  t <- seq(t.ini, t.end, sampling.time)
  sampled.signals <- resampleMeanSyntheticSignal(mean.signals, t)
  p4 <- getMeanSyntheticSignalPlot(sampled.signals)
  print(p4)
  
  norm.signals <- normaliseMeanSyntheticSignal(sampled.signals)
  p5 <- getMeanSyntheticSignalPlot(norm.signals)
  print(p5)
  
  invisible(norm.signals)
}

test2 <- function(n.cardiac.cycles = 15, ari.values = 5,
                  sampling.time = 0.6, seed = 2)
{
  abp <- getSyntheticABPSignal(n.cardiac.cycles = n.cardiac.cycles,
                               variability.range = -2:2, noise.sd = 1.5,
                               seed = seed)
  
  # Try min ABP = CCP = 12
  raw.signals <- getSyntheticABPAndCBFVSignals(abp, abp.min.value = 12, at.ari = ari.values)
  p2 <- getSyntheticABPAndCBFVSignalPlot(raw.signals)
  print(p2)
  
  mean.signals <- getMeanSyntheticSignals(raw.signals)
  p3 <- getMeanSyntheticSignalPlot(mean.signals)
  print(p3)
  
  t.ini <- mean.signals[1, "Time"]
  t.end <- mean.signals[nrow(mean.signals), "Time"]
  t <- seq(t.ini, t.end, sampling.time)
  sampled.signals <- resampleMeanSyntheticSignal(mean.signals, t)
  p4 <- getMeanSyntheticSignalPlot(sampled.signals)
  print(p4)
  
  norm.signals <- normaliseMeanSyntheticSignal(sampled.signals)
  p5 <- getMeanSyntheticSignalPlot(norm.signals)
  print(p5)
  
  invisible(norm.signals)
}

test3 <- function(n.cardiac.cycles = 375, ari.values = 5,
                  sampling.time = 0.6, n.folds = 10, seed = 2)
{
  abp <- getSyntheticABPSignal(n.cardiac.cycles = n.cardiac.cycles,
                               seed = seed)
  
  raw.signals <- getSyntheticABPAndCBFVSignals(abp, at.ari = ari.values)
  
  mean.signals <- getMeanSyntheticSignals(raw.signals)
  
  t.ini <- mean.signals[1, "Time"]
  t.end <- mean.signals[nrow(mean.signals), "Time"]
  t <- seq(t.ini, t.end, sampling.time)
  sampled.signals <- resampleMeanSyntheticSignal(mean.signals, t)
    
  norm.signals <- normaliseMeanSyntheticSignal(sampled.signals)
  names(norm.signals) <- c("TIME", "ABP", "CBFV")
  
  lags <- list(ABP = 4, CBFV = 2)
  lagged <- lagSignal(norm.signals, lags, "TIME")
  
  folded <- separateSignalIntoFolds(signal = lagged, n.folds = n.folds,
                                    fold.colname = "FOLD")
  
	# Fit SVR model
  i.train <- folded[["FOLD"]] == 1
  
  x.train <- folded[i.train, ]
  print(head(x.train))
  y.train <- x.train[["CBFV"]]
  x.train[["FOLD"]] <- NULL
  x.train[["TIME"]] <- NULL
  x.train[["CBFV"]] <- NULL
  x.train <- as.matrix(x.train)
  
  print(head(x.train))
  print(head(y.train))
  
  model.svr = svm(x.train, y.train, type = "nu-regression",
                  kernel = "radial", nu = 0.5, gamma = 2)
  
  # Evaluate predictions
  i.valid <- folded[["FOLD"]] == 2
  
  x.valid <- folded[i.valid, ]
  print(head(x.valid))
  y.valid <- x.valid[["CBFV"]]
  x.valid[["FOLD"]] <- NULL
  x.valid[["TIME"]] <- NULL
  x.valid[["CBFV"]] <- NULL
  x.valid <- as.matrix(x.valid)
  
  print(head(x.valid))
  predictions = predict(model.svr, x.valid)
  print(head(y.valid))
  print(head(predictions))
  print(head(abs(y.valid-predictions)))

  invisible(model.svr)
}

