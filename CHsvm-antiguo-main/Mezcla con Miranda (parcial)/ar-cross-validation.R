
require(ggplot2)
require(gridExtra)

#~ Based on:
#~ A Novel Statistical Model for Simulation of Arterial and Intracranial Pressure
#~ M. Aboy, J. McNames, R. Hornero, T. Thong, D. Cuesta, D. Novak, B. Goldstein
getSyntheticABPSignal <- function(mean.abp = 90, pulse.amplitude = 10,
                                  target.sample.time = 0.1,
                                  cardiac.cycle.duration = 8 * target.sample.time,
                                  n.cardiac.cycles = 1000,
                                  variability.range = -1:1,
                                  noise.sd = 0.5,
                                  seed = 2)
{
  set.seed(seed);
  
  cardiac.freq <- 1 / cardiac.cycle.duration
  cardiac.cycle.length <- cardiac.cycle.duration / target.sample.time
  T <- cardiac.cycle.duration * (n.cardiac.cycles + 1);
  
  t <- seq(target.sample.time, T, target.sample.time)
  abp <- mean.abp + pulse.amplitude * 
         (cos(2 * pi * cardiac.freq * t) + cos(4 * pi * cardiac.freq * t + pi / 2))
  
  # Find minima
  i.all.minima <- which(diff(sign(diff(abp)))==+2)+1
  i.actual.minima <- i.all.minima[abp[i.all.minima] < mean.abp - 0.9 * pulse.amplitude]
  
  # Trim signal
  abp <- abp[i.actual.minima[1]:i.actual.minima[length(i.actual.minima)]]
  t <- t[1:length(abp)] - target.sample.time
  i.actual.minima <- i.actual.minima - i.actual.minima[1] + 1
  minima <- rep(FALSE, length(abp))
  minima[i.actual.minima] <- TRUE
  
  # Create variability signals
  changes <-sample(variability.range, n.cardiac.cycles + 1, replace = TRUE)
  changes <- cumsum(changes)
  changes <- as.vector(sapply(changes, rep, cardiac.cycle.length))
  changes <- changes[1:length(abp)]
  
  noise <- rnorm(length(abp), 0, noise.sd)
  
  # Return answer
  invisible(data.frame(Time = t, IsCycleBoundary = minima,
                       Signal_ABP = abp + changes + noise,
                       Signal_Harmonic = abp,
                       Signal_Cycle.Change = changes + mean.abp,
                       Signal_Noise = noise + mean.abp))
}

getSyntheticABPSignalPlot <- function(synthetic.abp.signal,
                                      mark.cycle.ends = TRUE,
                                      plot.harmonic = FALSE,
                                      plot.cycle.change = TRUE,
                                      plot.noise = FALSE)
{
  varying <- "Signal_ABP"
  if(! plot.harmonic)
    synthetic.abp.signal[["Signal_Harmonic"]] <- NULL
  else
    varying <- c(varying, "Signal_Harmonic")
  if(! plot.cycle.change)
    synthetic.abp.signal[["Signal_Cycle.Change"]] <- NULL
  else
    varying <- c(varying, "Signal_Cycle.Change")
  if(! plot.noise)
    synthetic.abp.signal[["Signal_Noise"]] <- NULL
  else
    varying <- c(varying, "Signal_Noise")
  
  signals <- reshape(synthetic.abp.signal, varying = varying,
                       idvar = "Time", timevar = "Type",
                       direction = "long", sep ="_")
  signals[["Type"]] <- factor(signals[["Type"]])
  min.y <- min(signals[["Signal"]]) - 15
  max.y <- max(signals[["Signal"]]) + 15
  
  p <- ggplot(signals, aes(x = Time, y = Signal, colour = Type)) +
       geom_line() + 
       xlab("Time") +
       ylab("Signal") + ylim(min.y, max.y)
  if(mark.cycle.ends)
  {
    d <- signals[signals[["IsCycleBoundary"]] & signals[["Type"]] == "ABP", ]
    p <- p + geom_point(data = d, aes(x = Time, y = Signal, colour = Type))
  }
  
  invisible(p)
}

getATParameters <- function(ari.values = NULL)
{
  ARI <- seq(0, 9, 0.1)
  T <- c(2.000000, 1.999821, 1.999698, 1.999626, 1.999597,
         1.999607, 1.999648, 1.999714, 1.999799, 1.999896,
         2.000000, 2.000104, 2.000201, 2.000286, 2.000352,
         2.000393, 2.000403, 2.000374, 2.000302, 2.000179,
         2.000000, 1.999764, 1.999497, 1.999229, 1.998994,
         1.998821, 1.998742, 1.998789, 1.998994, 1.999387,
         2.000000, 2.000840, 2.001812, 2.002796, 2.003673,
         2.004324, 2.004629, 2.004469, 2.003724, 2.002274,
         2.000000, 1.996777, 1.992457, 1.986887, 1.979913,
         1.971382, 1.961140, 1.949035, 1.934912, 1.918618,
         1.900000, 1.878952, 1.855560, 1.829956, 1.802274,
         1.772648, 1.741209, 1.708092, 1.673429, 1.637354,
         1.600000, 1.561515, 1.522104, 1.481989, 1.441390,
         1.400528, 1.359624, 1.318898, 1.278572, 1.238866,
         1.200000, 1.162160, 1.125385, 1.089679, 1.055046,
         1.021491, 0.989016, 0.957625, 0.927323, 0.898114,
         0.870000, 0.842986, 0.817077, 0.792275, 0.768584,
         0.746009, 0.724554, 0.704221, 0.685015, 0.666940,
         0.650000)
  D <- c(1.600000, 1.595712, 1.593199, 1.592153, 1.592266,
         1.593228, 1.594732, 1.596470, 1.598133, 1.599412,
         1.600000, 1.599588, 1.597867, 1.594530, 1.589268,
         1.581772, 1.571734, 1.558847, 1.542801, 1.523288,
         1.500000, 1.472787, 1.442132, 1.408677, 1.373064,
         1.335934, 1.297930, 1.259692, 1.221864, 1.185086,
         1.150000, 1.117115, 1.086405, 1.057712, 1.030877,
         1.005741, 0.982146, 0.959933, 0.938944, 0.919019,
         0.900000, 0.881754, 0.864248, 0.847475, 0.831428,
         0.816100, 0.801485, 0.787574, 0.774361, 0.761839,
         0.750000, 0.738820, 0.728204, 0.718039, 0.708211,
         0.698607, 0.689115, 0.679621, 0.670013, 0.660177,
         0.650000, 0.639415, 0.628535, 0.617520, 0.606529,
         0.595721, 0.585255, 0.575291, 0.565988, 0.557504,
         0.550000, 0.543590, 0.538215, 0.533771, 0.530154,
         0.527260, 0.524985, 0.523225, 0.521877, 0.520837,
         0.520000, 0.519263, 0.518523, 0.517675, 0.516615,
         0.515240, 0.513446, 0.511129, 0.508185, 0.504510,
         0.500000)
  K <- c(0.000000, 0.019720, 0.039528, 0.059415, 0.079371,
         0.099386, 0.119449, 0.139553, 0.159685, 0.179838,
         0.200000, 0.220162, 0.240315, 0.260447, 0.280551,
         0.300614, 0.320629, 0.340585, 0.360472, 0.380280,
         0.400000, 0.419631, 0.439213, 0.458796, 0.478427,
         0.498157, 0.518034, 0.538107, 0.558427, 0.579041,
         0.600000, 0.621313, 0.642832, 0.664370, 0.685742,
         0.706759, 0.727236, 0.746985, 0.765820, 0.783554,
         0.800000, 0.815019, 0.828660, 0.841024, 0.852206,
         0.862306, 0.871422, 0.879651, 0.887092, 0.893842,
         0.900000, 0.905653, 0.910847, 0.915615, 0.919993,
         0.924015, 0.927716, 0.931131, 0.934293, 0.937238,
         0.940000, 0.942609, 0.945073, 0.947396, 0.949581,
         0.951632, 0.953553, 0.955346, 0.957016, 0.958566,
         0.960000, 0.961322, 0.962542, 0.963672, 0.964723,
         0.965706, 0.966633, 0.967514, 0.968361, 0.969186,
         0.970000, 0.970814, 0.971639, 0.972486, 0.973367,
         0.974294, 0.975277, 0.976328, 0.977458, 0.978678,
         0.980000)
  
  params <- data.frame(ARI, T, D, K)
  if(! is.null(ari.values))
  {
    i <- which(ARI %in% ari.values)
    params <- params[i, ]
  }
  
  invisible(params)  
}

getTheoricalCBFVResponse <- function(T, D, K, abp,
                                     abp.baseline.value = mean(abp, na.rm = TRUE),
                                     abp.min.value = min(abp, na.rm = TRUE),
                                     abp.sampling.time = 0.1)
{
  nabp = (abp - abp.min.value) / abs(abp.baseline.value - abp.min.value);
  dP = nabp - 1;
  n = length(dP);
  
  freq <- 1 / abp.sampling.time
  denom <- freq * T;
  x1 <- 0;
  x2 <- 0;
  ncbfv = 1;
  for(t in 2:n)
  {
    x1 <- c(x1, x1[t-1] + (dP[t] - x2[t-1]) / denom)
    x2 <- c(x2, x2[t-1] + (x1[t] - 2 * D * x2[t-1]) / denom)
    ncbfv <- c(ncbfv, 1 + dP[t] - K * x2[t])
  }
  
  invisible(ncbfv)
}

getSyntheticABPAndCBFVSignals <- function(synthetic.abp.signal,
                                          at.ari = 5,
                                          cbfv.baseline.value = rep(68, length(at.ari)),
                                          cbfv.min.value = rep(40, length(at.ari)))
{
  params <- getATParameters(at.ari)
  
  for(i in 1:nrow(params))
  {
    ncbfv <- getTheoricalCBFVResponse(params[i, "T"], params[i, "D"], params[i, "K"],
                                      synthetic.abp.signal[["Signal_ABP"]])
    cbfv <- cbfv.min.value + abs(cbfv.baseline.value[i] - cbfv.min.value[i]) * ncbfv
    column.name <- paste("Signal_CBFV.ARI", sprintf("%.1f", params[i, "ARI"]), sep = "=")
    synthetic.abp.signal[[column.name]] <- cbfv
  }
  
  invisible(synthetic.abp.signal)
}

getSyntheticABPAndCBFVSignalPlot <- function(synthetic.signals,
                                            mark.cycle.ends = TRUE)
{
  id.names <- c("Time", "IsCycleBoundary")
  abp.name <- "Signal_ABP"
  cbfv.names <- grep("Signal_CBFV.ARI=", colnames(synthetic.signals), value = TRUE, fixed = TRUE)
  col.varying <- c(abp.name, cbfv.names)
  col.names <- c(id.names, col.varying)
  
  signals.wide <- synthetic.signals[, col.names]
  
  signals <- reshape(signals.wide, varying = col.varying,
                    idvar = id.names, timevar = "Type",
                    direction = "long", sep = "_")
  signals[["Type"]] <- factor(signals[["Type"]])
  min.y <- min(signals[["Signal"]]) - 5
  max.y <- max(signals[["Signal"]]) + 5
  
  p <- ggplot(signals, aes(x = Time, y = Signal, colour = Type)) +
       geom_line() + 
       xlab("Time") +
       ylab("Signal") + ylim(min.y, max.y)
  if(mark.cycle.ends)
  {
    d <- signals[signals[["IsCycleBoundary"]] & signals[["Type"]] == "ABP", ]
    p <- p + geom_point(data = d, aes(x = Time, y = Signal, colour = Type))
  }
  
  invisible(p)
}

.getMeanByBoundaries <- function(signal, boundaries)
{
  which.boundaries <- which(boundaries)
  new.signal <- c()
  for (i in 2:length(which.boundaries))
  {
    j <- which.boundaries[i - 1]:which.boundaries[i]
    mean.s <- mean(signal[j], rm.na = TRUE)
    new.signal <- c(new.signal, rep(mean.s, length(j) - 1))
  }
  new.signal <- c(new.signal, mean.s)
  
  invisible(new.signal)
}

getMeanSyntheticSignals <- function(synthetic.signals)
{
  boundaries <- synthetic.signals[["IsCycleBoundary"]]
  
  abp.name <- "Signal_ABP"
  cbfv.names <- grep("Signal_CBFV.ARI=", colnames(synthetic.signals), value = TRUE, fixed = TRUE)
  col.varying <- c(abp.name, cbfv.names)
  
  mean.synthetic.signals <- data.frame(Time = synthetic.signals[["Time"]])
  for(colname in col.varying)
  {
    mean.signal <- .getMeanByBoundaries(synthetic.signals[[colname]], boundaries)
    new.colname <- sub("Signal", "Mean", colname, fixed = TRUE)
    mean.synthetic.signals[[new.colname]] <- mean.signal
  }
  
  invisible(mean.synthetic.signals)
}

getMeanSyntheticSignalPlot <- function(synthetic.signals)
{
  id.names <- "Time"
  abp.name <- "Mean_ABP"
  cbfv.names <- grep("Mean_CBFV.ARI=", colnames(synthetic.signals), value = TRUE, fixed = TRUE)
  col.varying <- c(abp.name, cbfv.names)
  col.names <- c(id.names, col.varying)
  
  signals.wide <- synthetic.signals[, col.names]
  
  signals <- reshape(signals.wide, varying = col.varying,
                    idvar = id.names, timevar = "Type",
                    direction = "long", sep = "_")
  signals[["Type"]] <- factor(signals[["Type"]])
  
  
  min.y <- min(signals[["Mean"]])
  max.y <- max(signals[["Mean"]])
  margin <- (max.y - min.y) * 0.1
  min.y <- min.y - margin
  max.y <- max.y + margin
  
  p <- ggplot(signals, aes(x = Time, y = Mean, colour = Type)) +
       geom_line() + 
       xlab("Time") +
       ylab("Mean Signal") + ylim(min.y, max.y)
  
  invisible(p)
}

resampleMeanSyntheticSignal <- function(synthetic.mean.signals, new.time)
{
  id.names <- "Time"
  abp.name <- "Mean_ABP"
  cbfv.names <- grep("Mean_CBFV.ARI=", colnames(synthetic.mean.signals), value = TRUE, fixed = TRUE)
  col.names <- c(id.names, abp.name, cbfv.names)
  
  new.mean.synthetic.signals <- data.frame(Time = new.time)
  for(colname in col.names)
  {
    interpolation <- spline(synthetic.mean.signals[["Time"]],
                            synthetic.mean.signals[[colname]],
                            xout = new.time)
    new.mean.synthetic.signals[[colname]] <- interpolation[["y"]]
  }
  
  invisible(new.mean.synthetic.signals)
}

.trim <- function(x, prop = 0, na.rm = FALSE)
{
  if(na.rm)
    x <- x[!is.na(x)]
  
  p <- min(prop, 0.5) / 2
  low <- quantile(x, probs = p, na.rm = na.rm, names = FALSE)
  high <- quantile(x, probs = 1 - p, na.rm = na.rm, names = FALSE)
  i <- x >= low & x <= high
  return(x[i])
}

normaliseMeanSyntheticSignal <- function(synthetic.mean.signals,
                                         trim = 0, na.rm = FALSE)
{
  abp.name <- "Mean_ABP"
  cbfv.names <- grep("Mean_CBFV.ARI=", colnames(synthetic.mean.signals), value = TRUE, fixed = TRUE)
  col.names <- c(abp.name, cbfv.names)
  
  new.mean.synthetic.signals <- data.frame(Time = synthetic.mean.signals[["Time"]])
  for(colname in col.names)
  {
    s <- synthetic.mean.signals[[colname]]
    s.trim <- .trim(s, prop = trim, na.rm = na.rm)
    s.baseline <- mean(s.trim)
    s.min <- min(s.trim)
    ns <- (s - s.min) / abs(s.baseline - s.min)
    new.mean.synthetic.signals[[colname]] <- ns
  }
  
  invisible(new.mean.synthetic.signals)
}

.test1 <- function(n.cardiac.cycles = 100, ari.values = c(3.5, 5, 8), seed = 2)
{
  graphics.off()
  
  abp <- getSyntheticABPSignal(n.cardiac.cycles = n.cardiac.cycles, seed = seed)
  p1 <- getSyntheticABPSignalPlot(abp)
  print(p1)
  
  raw.signals <- getSyntheticABPAndCBFVSignals(abp, at.ari = ari.values)
  p2 <- getSyntheticABPAndCBFVSignalPlot(raw.signals)
  print(p2)
  
  mean.signals <- getMeanSyntheticSignals(raw.signals)
  p3 <- getMeanSyntheticSignalPlot(mean.signals)
  print(p3)
  
  t.ini <- mean.signals[1, "Time"]
  t.end <- mean.signals[nrow(mean.signals), "Time"]
  t <- seq(t.ini, t.end, 0.6)
  mean.signals <- resampleMeanSyntheticSignal(mean.signals, t)
  p4 <- getMeanSyntheticSignalPlot(mean.signals)
  print(p4)
  
  invisible(arrangeGrob(p1, p2, p3, p4, ncol=1, nrow=4))
}

.test2 <- function(n.cardiac.cycles = 100, ari.values = c(3.5, 5, 8),
                   sampling.time = 0.2, seed = 2, trim = 0)
{
  graphics.off()
  
  abp <- getSyntheticABPSignal(n.cardiac.cycles = n.cardiac.cycles, seed = seed)
  raw.signals <- getSyntheticABPAndCBFVSignals(abp, at.ari = ari.values)
  mean.signals <- getMeanSyntheticSignals(raw.signals)
  
  t.ini <- mean.signals[1, "Time"]
  t.end <- mean.signals[nrow(mean.signals), "Time"]
  t <- seq(t.ini, t.end, sampling.time)
  sampled.signals <- resampleMeanSyntheticSignal(mean.signals, t)
  
  norm.signals <- normaliseMeanSyntheticSignal(sampled.signals)
  
  p <- getMeanSyntheticSignalPlot(norm.signals)
  print(p)
  
  invisible(norm.signals)
}
