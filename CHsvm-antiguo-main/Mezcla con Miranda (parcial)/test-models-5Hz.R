require(e1071)
require(pracma)

TOP.DIR <- file.path("", "research")
ARI.DIR <- file.path(TOP.DIR, "ARI-R")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")
TEST.DIR <- file.path(SVR.DIR, "Tests")

DATA.NAME <- "2nd-CARNet-bootstrap"
DATA.VERSION <- "5Hz"
DATA.DIR <- file.path(TOP.DIR, "Data", DATA.NAME, DATA.VERSION)

# [side effect: atARI, carSignal, Metric-utils, Rounding-utils]
MFARI.SCRIPT.BASENAME <- paste("mfARIdev", sep = "-")
MFARI.SCRIPT.BASENAME <- paste(MFARI.SCRIPT.BASENAME, "R", sep = ".")
MFARI.SCRIPT.NAME <- file.path(ARI.DIR, MFARI.SCRIPT.BASENAME)
source(MFARI.SCRIPT.NAME)

SIGLAG.SCRIPT.BASENAME <- paste("Signal", "lagging", sep = "-")
SIGLAG.SCRIPT.BASENAME <- paste(SIGLAG.SCRIPT.BASENAME, "R", sep = ".")
SIGLAG.SCRIPT.NAME <- file.path(SVR.DIR, SIGLAG.SCRIPT.BASENAME)
source(SIGLAG.SCRIPT.NAME)



test <- function()
{
  instances <- get.instances()
  
  abp.step <- get.abp.step()
  
  plots <- NULL
  for(i in 1:nrow(instances))
  {
    cat("\n\n")
    cat("----- ", i, ": ", sep = "")
    cat("Subject ", instances[i, "Instance"], ", ", sep = "")
    cat(instances[i, "Signal"])
    cat(" -----\n")
    
    instance <- instances[i, ]
    signals <- get.instance.signals(instance)
    lags <- get.lags(instance)
    hyper.params <- get.hyper.params(instance)
    
    lagged.signal <- lagSignal(
      signal = signals,
      lags = lags,
      time.colname = "Time"
    )
    folded.signal <- separateSignalIntoFolds(
      signal = lagged.signal,
      n.folds = 2,
      fold.colname = "Fold",
      time.colname = "Time"
    )
    
    no.input.var.names <- c("Time", instance[["Signal"]])
    input.var.names <- colnames(lagged.signal)
    input.var.names <- input.var.names[! input.var.names %in% no.input.var.names]
    svm.model <- get.svm.model(
      folded.signal = folded.signal,
      fold = instance[["fold"]],
      model.params = hyper.params,
      input.var.names = input.var.names,
      cbf.var.name = instance[["Signal"]],
      svm.type = "nu-regression",
      svm.kernel = "radial",
      svm.params = hyper.params
    )
    
    svm.model.cors <- get.model.cors(
      folded.signal = folded.signal,
      fold = instance[["fold"]],
      model = svm.model,
      input.var.names = input.var.names,
      cbf.var.name = instance[["Signal"]],
      cbf.rounding.digits = 3,
      train.cor.rounding.digits = 3,
      test.cor.rounding.digits = 3
    )
    
    abp.data.frame <- data.frame(
      Time = abp.step[["time.instants"]],
      MABP = abp.step[["ABP.normalised"]],
      etCO2 = mean(signals[["etCO2"]])
    )
    lagged.abp.step <- lagSignal(
      signal = abp.data.frame,
      lags = lags,
      time.colname = "Time"
    )
    
    whole.response <- get.whole.response(
      svm.model = svm.model,
      lagged.abp.step = lagged.abp.step,
      time.var.name = "Time",
      abp.var.name = "MABP",
      cbf.var.name = instance[["Signal"]],
      cbf.rounding.digits = 3
    )
    
    p1 <- get.response.plot(
      response = whole.response,
      time.var.name = "Time",
      abp.var.name = "MABP",
      cbf.var.name = instance[["Signal"]]
    )
    
    response <- get.response(
      whole.response = whole.response,
      time.var.name = "Time",
      abp.var.name = "MABP",
      cbf.var.name = instance[["Signal"]],
      time.release = abp.step[["time.release"]],
      time.until.baseline = 5,
      baseline.duration = 5,
      time.after.release = 20,
      time.tol = abp.step[["sampling.time"]] / 100
    )
    
#     show.response.info(
#       response = response,
#       time.var.name = "Time",
#       abp.var.name = "MABP",
#       cbf.var.name = instance[["Signal"]],
#       min.abp.threshold = 1/10^3,
#       min.cbf.threshold = 1/10^3
#     )
    
    error <- eval.step.response(
      response = response,
      time.var.name = "Time",
      abp.var.name = "MABP",
      cbf.var.name = instance[["Signal"]],
      time.release = abp.step[["time.release"]],
      time.tol = abp.step[["sampling.time"]] / 100,
      min.abp.threshold = 2/10^3,    # 3 is number of rounding digits for ABP
      min.abp.max.delta.time = 5 * 0.8, # Seconds from time of release
      min.cbf.threshold = 2/10^3,    # 3 is number of rounding digits for CBF
      min.cbf.max.value = 0.5,       # Normalised value, based at 0.8
      min.cbf.min.value = -0.25,     # Normalised value, based at 0.8
      max.cbf.max.value = 1,         # Normalised value, based at 0.8
      stable.cbf.initial.time = 10,  # Time instant
      stable.cbf.final.time = 20,    # Time instant
      stable.cbf.max.variance = 0.0005,
      peak.min.relative.height = 10,  # Percentage of min CBF height
      cbf.max.minima = 2,
      cbf.max.maxima = 1
    )
    
    p2 <- get.response.plot(
      response = response,
      time.var.name = "Time",
      abp.var.name = "MABP",
      cbf.var.name = instance[["Signal"]]
    )
    if(is.null(error))
      main <- "Accept"
    else
      main <- paste("Reject:", error)
    p2 <- p2 + ggtitle(main)
    p2 <- p2 + theme(plot.title = element_text(size = 10))
    
    test.cor <- paste("test correlation", "=", svm.model.cors[["test.cor"]])
    main <- paste("Subject", instance[["Instance"]])
    main <- paste(main, test.cor, sep = ", ")
    p <- arrangeGrob(p1, p2, nrow = 2, main = main)
    
    plots <- c(plots, list(p))    
  }
  
  plot.params <- c(plots, list(nrow = 1, ncol = 1, top = NULL))
  p <- do.call(marrangeGrob, plot.params)
  
  tgt.plot.basename <- paste("test", "models", "5Hz", sep = "-")
  tgt.plot.basename <- paste(tgt.plot.basename, "pdf", sep = ".")
  tgt.plot.name <- file.path(TEST.DIR, tgt.plot.basename)
  ggsave(filename = tgt.plot.name, plot = p, width = 11, height = 8, units = "in")
}


get.response <- function(
      whole.response,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      time.release = 0,
      time.until.baseline = 5,
      baseline.duration = 5,
      time.after.release = 20,
      time.tol = 0.2 / 100
      )
{
  time.until.release <- time.until.baseline + baseline.duration
  time.ini <- time.release - time.until.release - time.tol
  time.fin <- time.release + time.after.release + time.tol
  i <- whole.response[[time.var.name]] > time.ini
  j <- whole.response[[time.var.name]] < time.fin
  samples <- which(i & j)
  
  col.names <- c(time.var.name, abp.var.name, cbf.var.name)
  response <- whole.response[samples, col.names]
  
  time.ini <- time.release - time.until.baseline - time.tol
  time.fin <- time.release + time.tol
  i <- response[[time.var.name]] > time.ini
  j <- response[[time.var.name]] < time.fin
  samples <- which(i & j)
  vshift <- 0.8 - mean(response[[cbf.var.name]][samples])
  response[[cbf.var.name]] <- response[[cbf.var.name]] + vshift
  
  response
}


get.whole.response <- function(
      svm.model,
      lagged.abp.step,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      cbf.rounding.digits
      )
{
  step.response <- predict(svm.model, lagged.abp.step)
  step.response <- round(step.response, cbf.rounding.digits)
  lagged.abp.step[[cbf.var.name]] <- step.response
  
  lagged.abp.step
}


get.response.plot <- function(
      response,
      time.var.name,
      abp.var.name,
      cbf.var.name
      )
{
  abp.colour = brewer.pal(n = 9, name = "Reds")[5]
  cbf.colour = brewer.pal(n = 9, name = "Blues")[5]
  colours <- c(abp.colour, cbf.colour)
  names(colours) <- c(abp.var.name, cbf.var.name)
  
  t <- rep(response[[time.var.name]], 2)
  v <- c(response[[abp.var.name]], response[[cbf.var.name]])
  s <- rep(c(abp.var.name, cbf.var.name), each = length(response[[time.var.name]]))
  data <- data.frame(Time = t, Value = v, Signal = s)
  
  p <- ggplot(data, aes(x = Time, y = Value, colour = Signal))
  p <- p + geom_line()
  p <- p + scale_colour_manual(values = colours)
  p <- p + ylab("Normalised signals")
  p <- p + scale_y_continuous(breaks = seq(0, 1, 0.2))
  
  p
}


get.model.cors <- function(
      folded.signal,
      fold,
      model,
      input.var.names,
      cbf.var.name,
      cbf.rounding.digits,
      train.cor.rounding.digits,
      test.cor.rounding.digits
     )
{
  fold.colname <- attr(folded.signal, FOLD.COLUMN.NAME.ATTR)
  nfolds <- attr(folded.signal, NUMBER.OF.FOLDS.ATTR)
  
  i <- folded.signal[[fold.colname]] == fold
  train.signal <- folded.signal[i, ]
  fitted <- round(model[["fitted"]], cbf.rounding.digits)
  train.cor <- cor(fitted, train.signal[[cbf.var.name]])
  train.cor <- round(train.cor, train.cor.rounding.digits)
  
  test.fold <- fold + 1
  if(test.fold > nfolds)
    test.fold <- test.fold - 2
  
  i <- folded.signal[[fold.colname]] == test.fold
  test.signal <- folded.signal[i, ]
  
  x <- test.signal[, input.var.names]
  y <- test.signal[[cbf.var.name]]
  pred <- predict(model, x)
  pred <- round(pred, cbf.rounding.digits)
  test.cor <- cor(pred, y)
  test.cor <- round(test.cor, test.cor.rounding.digits)
  
  list(train.cor = train.cor, test.cor = test.cor)
}


get.svm.model <- function(
      folded.signal,
      fold,
      model.params,
      input.var.names,
      cbf.var.name,
      svm.type,
      svm.kernel,
      svm.params
      )
{
  fold.colname <- attr(folded.signal, FOLD.COLUMN.NAME.ATTR)
  i <- folded.signal[[fold.colname]] == fold
  folded.signal <- folded.signal[i, ]
  
  fmla.str <- paste(input.var.names, collapse = " + ")
  fmla.str <- paste(cbf.var.name, "~", fmla.str)
  fmla <- formula(fmla.str)
  
  params <- list(formula = fmla, data = folded.signal)
  params <- c(params, list(type = svm.type, kernel = svm.kernel))
  params <- c(params, svm.params)
  
  start.time <- Sys.time()
  model <- do.call(svm, params)
  end.time <-Sys.time()
  model[["train.time"]] <- end.time - start.time
  
  model
}


get.instances <- function()
{
  src.filename <- paste("models", "csv", sep = ".")
  src.file <- file.path(TEST.DIR, src.filename)
  instances <- read.csv(file = src.file, stringsAsFactors = FALSE)
  instances
}


get.instance.signals <- function(ins)
{
  src.filename <- paste(ins[["Instance"]], "txt", sep = ".")
  src.file <- file.path(DATA.DIR, src.filename)
  signals <- read.table(file = src.file, header = TRUE, check.names = FALSE)
  col.names <- names(signals)
  col.names <- gsub("-", "", col.names, fixed = TRUE)
  names(signals) <- col.names
  col.names <- c("Time", "MABP", "etCO2", ins[["Signal"]])
  signals <- signals[, col.names]
  signals[["MABP"]] <- normalise(signals[["MABP"]])
  signals[["etCO2"]] <- normalise(signals[["etCO2"]])
  signals[[ins[["Signal"]]]] <- normalise(signals[[ins[["Signal"]]]])
  
  signals
}


get.abp.step <- function(
      sampling.time = 0.2,
      time.until.release = 40,
      time.after.release = 30,
      smooth.step.stimulus = TRUE,
      filter.order = 2,
      cutoff.frequency = 0.20,
      left.stabilisation.time = 0,
      time.rounding.digits =  1,
      abp.rounding.digits = 3
      )
{  
  abp.step <- get.normalised.ABP.stimulus(
    sampling.time = sampling.time,
    time.until.release = time.until.release,
    time.after.release = time.after.release,
    smooth.step.stimulus = smooth.step.stimulus,
    filter.order = filter.order,
    cutoff.frequency = cutoff.frequency,
    left.stabilisation.time = left.stabilisation.time,
    time.rounding.digits =  time.rounding.digits
  )
  abp.step[["ABP.normalised"]] <- round(
    abp.step[["ABP.normalised"]], abp.rounding.digits
  )
  abp.step
}


normalise <- function(x)
{
  # Feature scaling
  nx <- (x - min(x)) / (max(x) - min(x))
  if(any(!is.finite(nx)))
    stop("signal contains NAs, NaNs or only zeroes or infinite values")
  nx
}


get.lags <- function(ins)
{
  list(MABP = ins[["MABP"]], etCO2 = ins[["etCO2"]])
}

get.hyper.params <- function(ins)
{
  sigma <- ins[["gamma"]]
  gamma <- 1 / (2 * sigma ^ 2)
  cost <- 2 ^ ins[["cost"]]
  nu <- ins[["nu"]]
  
  list(gamma = gamma, cost = cost, nu = nu)
}


eval.step.response <- function(
      response,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      time.release,
      time.tol = 1 / 100,
      min.abp.threshold = 2/10^3,    # 3 is number of rounding digits for ABP
      min.abp.max.delta.time = 5 * 0.8, # Seconds from time of release
      min.cbf.threshold = 2/10^3,    # 3 is number of rounding digits for CBF
      min.cbf.max.value = 0.5,       # Normalised value, based at 0.8
      min.cbf.min.value = -0.25,     # Normalised value, based at 0.8
      max.cbf.max.value = 1,         # Normalised value, based at 0.8
      stable.cbf.initial.time = 10,  # Time instant
      stable.cbf.final.time = 20,    # Time instant
      stable.cbf.max.variance = 0.0005,
      peak.min.relative.height = 10,  # Percentage of min CBF height
      cbf.max.minima = 2,
      cbf.max.maxima = 1
      )
{
  response <- response[, c(time.var.name, abp.var.name, cbf.var.name)]
  
  # Checks minimum ABP
  min.abp.info <- findpeaks(
    -response[[abp.var.name]],
    zero = "-",
    threshold = min.abp.threshold
  )
  if(is.null(min.abp.info) || nrow(min.abp.info) != 1)
    stop("ABP step does not have a unique global minimum")
  min.abp.time <- response[[time.var.name]][min.abp.info[1, 2]]
  if(min.abp.time < time.release + time.tol)
    stop("ABP step has a global minimum before release time")
  if(min.abp.time > time.release + min.abp.max.delta.time + time.tol)
    stop("ABP step has a global minimum after the specified maximum delta time")
  if(min(response[[abp.var.name]]) != -min.abp.info[1, 1])
    stop("ABP step has values below its global minimum")
  
  # Checks minimum CBF
  min.cbf.info <- findpeaks(
    -response[[cbf.var.name]],
    zero = "-",
    threshold = min.cbf.threshold,
    sortstr = TRUE
  )
  if(is.null(min.cbf.info))
    return("CBF does not have a local minimum")
  min.cbf.info[, 1] <- -min.cbf.info[, 1]
  llm <- min.cbf.info[1, ]
  
  r <- llm[1] >= min.cbf.min.value && llm[1] <= min.cbf.max.value
  if(!r)
  {
    s1 <- sprintf("value of CBF minimum (%.3f)", llm[1])
    s2 <- sprintf("[%.3f, %.3f]", min.cbf.min.value, min.cbf.max.value)
    return(paste(s1, "is not in the valid range", s2))
  }
  
  min.cbf.time <- response[[time.var.name]][llm[2]]
  r <- min.cbf.time > time.release + time.tol
  if(!r)
  {
    s1 <- sprintf("time of CBF minimum (%.1f)", min.cbf.time)
    s2 <- sprintf("(%.1f)", time.release)
    return(paste(s1, "is lower than allowed", s2))
  }
  
  cbf.min.sample <- which.min(response[[cbf.var.name]])
  r <- llm[2] == cbf.min.sample
  if(!r)
  {
    s <- sprintf("CBF minimum (%.1f)", min.cbf.value)
    return(paste("CBF has values below", s))
  }
  
  cbf.max <- max(response[[cbf.var.name]])
  r <- cbf.max <= max.cbf.max.value
  if(!r)
  {
    s1 <- sprintf("maximum CBF value (%.3f)", cbf.max)
    s2 <- sprintf("(%.3f)", max.cbf.max.value)
    return(paste(s1, "is higher than allowed", s2))
  }
  
  i <- response[[time.var.name]] > stable.cbf.initial.time - 0.2/100
  j <- response[[time.var.name]] < stable.cbf.final.time + 0.2/100
  stable.cbf <- response[[cbf.var.name]][i & j]
  stable.cbf.var <- var(stable.cbf)
  r <- stable.cbf.var <= stable.cbf.max.variance
  if(!r)
  {
    s1 <- sprintf("stable CBF variance (%.3f)", stable.cbf.var)
    s2 <- sprintf("(%.3f)", stable.cbf.max.variance)
    return(paste(s1, "is higher than allowed", s2))
  }
  
  heights <- apply(min.cbf.info, 1, get.minimum.height, response[[cbf.var.name]])
  min.cbf.info <- cbind(min.cbf.info, heights)
  percents <- heights / heights[1] * 100
  min.cbf.info <- cbind(min.cbf.info, percents)
  i <- percents > peak.min.relative.height
  min.cbf.info <- matrix(min.cbf.info[i, ], ncol = 6)
  r <- nrow(min.cbf.info) <= cbf.max.minima
  if(!r)
  {
    s1 <- sprintf("number of CBF minima (%d)", nrow(min.cbf.info))
    s2 <- sprintf("(%d)", cbf.max.minima)
    return(paste(s1, "is higher than allowed", s2))
  }
  
  n.maxima <- 0
  max.cbf.info <- findpeaks(
    response[[cbf.var.name]],
    zero = "+",
    threshold = min.cbf.threshold
  )
  if(!is.null(max.cbf.info))
  {
    
    max.heights <- apply(max.cbf.info, 1, get.maximum.height, response[[cbf.var.name]])
    max.cbf.info <- cbind(max.cbf.info, max.heights)
    percents <- max.heights / heights[1] * 100
    max.cbf.info <- cbind(max.cbf.info, percents)
    i <- percents > peak.min.relative.height
    max.cbf.info <- matrix(max.cbf.info[i, ], ncol = 6)
    n.maxima <- nrow(max.cbf.info)
  }
  r <- n.maxima <= cbf.max.maxima
  if(!r)
  {
    s1 <- sprintf("number of CBF maxima (%d)", nrow(max.cbf.info))
    s2 <- sprintf("(%d)", cbf.max.maxima)
    return(paste(s1, "is higher than allowed", s2))
  }
  
  return(NULL)
}


get.maximum.height <- function(peak, signal)
{
  samples <- peak[3]:peak[4]
  signal <- signal[samples]
  line <- seq(signal[1], tail(signal, 1), length.out = length(signal))
  i <- which(samples == peak[2])
  ref <- line[i]
  peak[1] - ref
}


get.minimum.height <- function(peak, signal)
{
  samples <- peak[3]:peak[4]
  signal <- signal[samples]
  line <- seq(signal[1], tail(signal, 1), length.out = length(signal))
  i <- which(samples == peak[2])
  ref <- line[i]
  ref - peak[1]
}


show.response.info <- function(
      response,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      min.abp.threshold = 2/10^3, # 3 is number of rounding digits for ABP
      min.cbf.threshold = 2/10^3  # 3 is number of rounding digits for CBF
      )
{
  time.release <- 0.0
  sampling.time <- 0.2
  min.cbf.max.value <- 0.5
  min.cbf.min.value <- -0.25
  min.cbf.min.recovery.time <- 1
  min.cbf.max.delta.time <- 2
  max.cbf.max.value <- 1
  stable.cbf.initial.time <- 10
  stable.cbf.final.time <- 20
  stable.cbf.max.variance <- 0.0005
  peak.min.relative.height <- 10 # %
  
  min.abp.info <- findpeaks(
    -response[[abp.var.name]],
    zero = "-",
    threshold = min.abp.threshold
  )
  cat("abp minima found: ", nrow(min.abp.info), "\n")
  cat("min.abp.sample: ", min.abp.info[1, 2], "\n")
  min.abp.time <- response[[time.var.name]][min.abp.info[1, 2]]
  cat("min.abp.time: ", min.abp.time, "\n")
  cat("min.abp.value: ", -min.abp.info[1, 1], "\n")
  cat("\n")
  
  min.cbf.info <- findpeaks(
    -response[[cbf.var.name]],
    zero = "-",
    threshold = min.cbf.threshold,
    sortstr = TRUE
  )
  if(is.null(min.cbf.info))
    cat("no cbf minima found\n")
  min.cbf.info[, 1] <- -min.cbf.info[, 1]
  
  cat("--- larger local minimum ---\n")
  llm <- min.cbf.info[1, ]
  
  cat("min.cbf.value: ", llm[1], "\n", sep = "")
  s <- sprintf("[%.2f, %.2f]", min.cbf.min.value, min.cbf.max.value)
  r <- llm[1] >= min.cbf.min.value && llm[1] <= min.cbf.max.value
  cat("in ", s, "?: ", r, "\n", sep = "")
  cat("\n")
  
  recovery.time <- response[[time.var.name]][llm[4]] - 
                   response[[time.var.name]][llm[2]]
  cat("recovery time: ", recovery.time, "\n", sep = "")
  s <- sprintf(">= %.1f s", min.cbf.min.recovery.time)
  r <- recovery.time >= min.cbf.min.recovery.time
  cat(s, "?: ", r, "\n", sep = "")
  cat("\n")
  
  min.cbf.time <- response[[time.var.name]][llm[2]]
  cat("min.cbf.time: ", min.cbf.time, "\n", sep = "")
  min.cbf.min.time <- time.release + sampling.time
  min.cbf.max.time <- min.abp.time + min.cbf.max.delta.time
  s <- sprintf("[%.1f, %.1f]", min.cbf.min.time, min.cbf.max.time)
  r <- min.cbf.time >= min.cbf.min.time && min.cbf.time <= min.cbf.max.time
  cat("in ", s, "?: ", r, "\n", sep = "")
  cat("\n")
  
  cat("min.cbf.sample: ", llm[2], "\n")
  cbf.min.sample <- which.min(response[[cbf.var.name]])
  cat("sample with minimum signal: ", cbf.min.sample, "\n", sep = "")
  r <- llm[2] == cbf.min.sample
  cat("same sample?: ", r, "\n", sep = "")
  cat("\n")
  
  cbf.max <- max(response[[cbf.var.name]])
  cat("maximum signal value: ", cbf.max, "\n", sep = "")
  s <- sprintf("<= %.2f", max.cbf.max.value)
  r <- cbf.max <= max.cbf.max.value
  cat(s, "?: ", r, "\n", sep = "")
  cat("\n")
  
  s <- sprintf("[%.1f, %.1f]", stable.cbf.initial.time, stable.cbf.final.time)
  cat("stable cbf in ", s, "\n", sep = "")
  i <- response[[time.var.name]] > stable.cbf.initial.time - 0.2/100
  j <- response[[time.var.name]] < stable.cbf.final.time + 0.2/100
  stable.cbf <- response[[cbf.var.name]][i & j]
  stable.cbf.var <- var(stable.cbf)
  cat("stable cbf variance: ", sprintf("%.4f", stable.cbf.var), "\n", sep = "")
  s <- sprintf("<= %.4f", stable.cbf.max.variance)
  r <- stable.cbf.var <= stable.cbf.max.variance
  cat(s, "?: ", r, "\n", sep = "")
  cat("\n")
  
  .internal.gminh.fun <- function(peak) {
    samples <- peak[3]:peak[4]
    signal <- response[[cbf.var.name]][samples]
    line <- seq(signal[1], tail(signal, 1), length.out = length(signal))
    i <- which(samples == peak[2])
    ref <- line[i]
    ref - peak[1]
  }
  
  heights <- apply(min.cbf.info, 1, .internal.gminh.fun)
  min.cbf.info <- cbind(min.cbf.info, heights)
  percents <- heights / heights[1] * 100
  min.cbf.info <- cbind(min.cbf.info, percents)
  i <- percents > peak.min.relative.height
  min.cbf.info <- matrix(min.cbf.info[i, ], ncol = 6)
  cat("cbf minima:\n")
  print(min.cbf.info)
  n.minima <- nrow(min.cbf.info)
  cat("number of cbf minima found:", n.minima, "\n", sep = "")
  cat("\n")
  
  n.maxima <- 0
  max.cbf.info <- findpeaks(
    response[[cbf.var.name]],
    zero = "+",
    threshold = min.cbf.threshold
  )
  if(!is.null(max.cbf.info))
  {
    .internal.gmaxh.fun <- function(peak) {
      samples <- peak[3]:peak[4]
      signal <- response[[cbf.var.name]][samples]
      line <- seq(signal[1], tail(signal, 1), length.out = length(signal))
      i <- which(samples == peak[2])
      ref <- line[i]
      peak[1] - ref
    }
    
    max.heights <- apply(max.cbf.info, 1, .internal.gmaxh.fun)
    max.cbf.info <- cbind(max.cbf.info, max.heights)
    percents <- max.heights / heights[1] * 100
    max.cbf.info <- cbind(max.cbf.info, percents)
    i <- percents > peak.min.relative.height
    max.cbf.info <- matrix(max.cbf.info[i, ], ncol = 6)
    cat("cbf maxima:\n")
    print(max.cbf.info)
    n.maxima <- nrow(max.cbf.info)
  }
  cat("number of cbf maxima found:", n.maxima, "\n", sep = "")
  cat("\n")
}








########


show.response.info.old <- function(
      response,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      min.abp.threshold = 1/10^3, # 3 is number of rounding digits for ABP
      min.cbf.threshold = 1/10^3  # 3 is number of rounding digits for CBF
      )
{
  min.abp.info <- findpeaks(
    -response[[abp.var.name]],
    zero = "-",
    threshold = min.abp.threshold
  )
  cat("abp minima found: ", nrow(min.abp.info), "\n")
  cat("min.abp.sample: ", min.abp.info[1, 2], "\n")
  min.abp.time <- response[[time.var.name]][min.abp.info[1, 2]]
  cat("min.abp.time: ", min.abp.time, "\n")
  cat("min.abp.value: ", -min.abp.info[1, 1], "\n")
  
  min.cbf.info <- findpeaks(
    -response[[cbf.var.name]],
    zero = "-",
    threshold = min.cbf.threshold,
    sortstr = TRUE
  )
  cat("cbf minima found: ", nrow(min.cbf.info), "\n")
  if(nrow(min.cbf.info) == 0)
    return()
  
  min.cbf.info[, 1] <- -min.cbf.info[, 1]
  llm <- min.cbf.info[1, ]
  cat("--- larger local minimum ---\n")
  cat("min.cbf.sample: ", llm[2], "\n")
  cat("min.cbf.time: ", response[[time.var.name]][llm[2]], "\n")
  cat("min.cbf.value: ", llm[1], "\n")
  if(llm[2] != llm[3])
    samples.left <- llm[3]:(llm[2]-1)
  else
    samples.left <- llm[2]
  if(llm[2] != llm[4])
    samples.right <- (llm[2]+1):llm[4]
  else
    samples.right <- llm[2]
  cat("peak.left: ", response[[time.var.name]][samples.left], "\n")
  llm.left <- response[[cbf.var.name]][samples.left]
  cat("first value on the left: ", llm.left[1], "\n")
  llm.left.max <- max(llm.left)
  cat("max value on the left: ", llm.left.max, "\n")
  cat("peak.right: ", response[[time.var.name]][samples.right], "\n")
  llm.right <- response[[cbf.var.name]][samples.right]
  cat("last value on the right: ", tail(llm.right, 1), "\n")
  llm.right.max <- max(llm.right)
  cat("max value on the right: ", llm.right.max, "\n")
  llm.base <- min(llm.left.max, llm.right.max)
  cat("peak baseline: ", llm.base, "\n")
  llm.height <- llm.base - llm[1]
  cat("height: ", llm.height, "\n")
  samples <- llm[3]:llm[4]
  llm.peak <- response[[cbf.var.name]][samples]
  llm.line <- seq(llm.peak[1], tail(llm.peak, 1), length.out = length(llm.peak))
  cat("line: ", llm.line, "\n")
  i <- which(samples == llm[2])
  llm.ref <- llm.line[i]
  cat("ref: ", llm.ref, "\n")
  llm.height2 <- llm.ref - llm[1]
  cat("height2: ", llm.height2, "\n")
  
  if(nrow(min.cbf.info) > 1)
    for(i in 2:nrow(min.cbf.info))
    {
      cat("--- local minimum", i, " ---\n")
      cat("min.cbf.sample: ", min.cbf.info[i, 2], "\n")
      cat("min.cbf.time: ", response[[time.var.name]][min.cbf.info[i, 2]], "\n")
      cat("min.cbf.value: ", min.cbf.info[i, 1], "\n")
      if(min.cbf.info[i, 2] != min.cbf.info[i, 3])
        samples.left <- min.cbf.info[i, 3]:(min.cbf.info[i, 2]-1)
      else
        samples.left <- min.cbf.info[i, 2]
      if(min.cbf.info[i, 2] != min.cbf.info[i, 4])
        samples.right <- (min.cbf.info[i, 2]+1):min.cbf.info[i, 4]
      else
        samples.right <- min.cbf.info[i, 2]
      cat("peak.left: ", response[[time.var.name]][samples.left], "\n")
      left <- response[[cbf.var.name]][samples.left]
      cat("first value on the left: ", left[1], "\n")
      left.max <- max(left)
      cat("max value on the left: ", left.max, "\n")
      cat("peak.right: ", response[[time.var.name]][samples.right], "\n")
      right <- response[[cbf.var.name]][samples.right]
      cat("last value on the right: ", tail(right, 1), "\n")
      right.max <- max(right)
      cat("max value on the right: ", right.max, "\n")
      base <- min(left.max, right.max)
      cat("peak baseline: ", base, "\n")
      height <- base - min.cbf.info[i, 1]
      cat("height: ", height, "\n")
      cat("relative height: ", round(height / llm.height * 100, 1), "% \n", sep = "")
      samples <- min.cbf.info[i, 3]:min.cbf.info[i, 4]
      peak <- response[[cbf.var.name]][samples]
      line <- seq(peak[1], tail(peak, 1), length.out = length(peak))
      cat("line: ", line, "\n")
      j <- which(samples == min.cbf.info[i, 2])
      ref <- line[j]
      cat("ref: ", ref, "\n")
      height2 <- ref - min.cbf.info[i, 1]
      cat("height2: ", height2, "\n")
      cat("relative height2: ", round(height2 / llm.height2 * 100, 1), "% \n", sep = "")
    }
    
    max.cbf.info <- findpeaks(
      response[[cbf.var.name]],
      zero = "+",
      threshold = min.cbf.threshold
    )
    if(nrow(max.cbf.info) > 1)
      for(i in 1:nrow(max.cbf.info))
      {
        cat("--- local maximum", i, " ---\n")
        cat("max.cbf.sample: ", max.cbf.info[i, 2], "\n")
        cat("max.cbf.time: ", response[[time.var.name]][max.cbf.info[i, 2]], "\n")
        cat("max.cbf.value: ", max.cbf.info[i, 1], "\n")
        if(max.cbf.info[i, 2] != max.cbf.info[i, 3])
          samples.left <- max.cbf.info[i, 3]:(max.cbf.info[i, 2]-1)
        else
          samples.left <- max.cbf.info[i, 2]
        if(max.cbf.info[i, 2] != max.cbf.info[i, 4])
          samples.right <- (max.cbf.info[i, 2]+1):max.cbf.info[i, 4]
        else
          samples.right <- max.cbf.info[i, 2]
        cat("peak.left: ", response[[time.var.name]][samples.left], "\n")
        left <- response[[cbf.var.name]][samples.left]
        cat("first value on the left: ", left[1], "\n")
        left.min <- min(left)
        cat("min value on the left: ", left.min, "\n")
        cat("peak.right: ", response[[time.var.name]][samples.right], "\n")
        right <- response[[cbf.var.name]][samples.right]
        cat("last value on the right: ", tail(right, 1), "\n")
        right.min <- min(right)
        cat("min value on the right: ", right.min, "\n")
        base <- max(left.min, right.min)
        cat("peak baseline: ", base, "\n")
        height <- max.cbf.info[i, 1] - base
        cat("height: ", height, "\n")
        cat("relative height: ", round(height / llm.height * 100, 1), "% \n", sep = "")
        samples <- max.cbf.info[i, 3]:max.cbf.info[i, 4]
        peak <- response[[cbf.var.name]][samples]
        line <- seq(peak[1], tail(peak, 1), length.out = length(peak))
        cat("line: ", line, "\n")
        j <- which(samples == max.cbf.info[i, 2])
        ref <- line[j]
        cat("ref: ", ref, "\n")
        height2 <- max.cbf.info[i, 1] - ref
        cat("height2: ", height2, "\n")
        cat("relative height2: ", round(height2 / llm.height2 * 100, 1), "% \n", sep = "")
      }
  
  cat("vecinity: ", response[[cbf.var.name]][min.cbf.info[1, 2]:(min.cbf.info[1, 2]+2)], "\n")
  cat("vecinity diff: ", abs(diff(response[[cbf.var.name]][min.cbf.info[1, 2]:(min.cbf.info[1, 2]+2)])), "\n")
  cat("max vecinity diff: ", max(abs(diff(response[[cbf.var.name]][min.cbf.info[1, 2]:(min.cbf.info[1, 2]+2)]))), "\n")
  cat("max cbf: ", max(response[[cbf.var.name]]), "\n")
  stable.initial.time <- 10
  stable.final.time <- 20
  cat("stable cbf from t=", stable.initial.time, ", ")
  cat("to t=", stable.final.time, "\n")
  i <- response[[time.var.name]] > stable.initial.time - 0.2/100
  j <- response[[time.var.name]] < stable.final.time + 0.2/100
  stable.cbf <- response[[cbf.var.name]][i & j]
  cat("stable cbf variance =", sprintf("%f", var(stable.cbf)), "\n")
}

eval.step.response.old <- function(
      response,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      sampling.time,
      time.release,
      time.tol = sampling.time / 100,
      min.abp.threshold = 2/10^3,    # 3 is number of rounding digits for ABP
      min.abp.max.delta.time = 5 * 0.8, # Seconds from time of release
      min.cbf.threshold = 2/10^3,    # 3 is number of rounding digits for CBF
      min.cbf.max.value = 0.5,       # Normalised value, based at 0.8
      min.cbf.min.value = -0.25,     # Normalised value, based at 0.8
      min.cbf.min.recovery.time = 2, # Seconds from min CBF
      min.cbf.max.delta.time = 2,    # Seconds from min ABP
      max.cbf.max.value = 1,         # Normalised value, based at 0.8
      stable.cbf.initial.time = 10,  # Time instant
      stable.cbf.final.time = 20,    # Time instant
      stable.cbf.max.variance = 0.0005,
      peak.min.relative.height = 10,  # Percentage of min CBF height
      cbf.max.minima = 2,
      cbf.max.maxima = 1
      )
{
  response <- response[, c(time.var.name, abp.var.name, cbf.var.name)]
  
  # Checks minimum ABP
  min.abp.info <- findpeaks(
    -response[[abp.var.name]],
    zero = "-",
    threshold = min.abp.threshold
  )
  if(is.null(min.abp.info) || nrow(min.abp.info) != 1)
    stop("ABP step does not have a unique global minimum")
  min.abp.time <- response[[time.var.name]][min.abp.info[1, 2]]
  if(min.abp.time < time.release + time.tol)
    stop("ABP step has a global minimum before release time")
  if(min.abp.time > time.release + min.abp.max.delta.time + time.tol)
    stop("ABP step has a global minimum after the specified maximum delta time")
  if(min(response[[abp.var.name]]) != -min.abp.info[1, 1])
    stop("ABP step has values below its global minimum")
  
  # Checks minimum CBF
  min.cbf.info <- findpeaks(
    -response[[cbf.var.name]],
    zero = "-",
    threshold = min.cbf.threshold,
    sortstr = TRUE
  )
  if(is.null(min.cbf.info))
    return("CBF does not have a local minimum")
  min.cbf.info[, 1] <- -min.cbf.info[, 1]
  llm <- min.cbf.info[1, ]
  
  r <- llm[1] >= min.cbf.min.value && llm[1] <= min.cbf.max.value
  if(!r)
  {
    s1 <- sprintf("value of CBF minimum (%.3f)", llm[1])
    s2 <- sprintf("[%.3f, %.3f]", min.cbf.min.value, min.cbf.max.value)
    return(paste(s1, "is not in the valid range", s2))
  }
  
  recovery.time <- response[[time.var.name]][llm[4]] - 
                   response[[time.var.name]][llm[2]]
  r <- recovery.time >= min.cbf.min.recovery.time
  if(!r)
  {
    s1 <- sprintf("CBF recovery duration (%.1f s)", recovery.time)
    s2 <- sprintf("(%.1f s)", min.cbf.min.recovery.time)
    return(paste(s1, "is lower than allowed", s2))
  }
  
  min.cbf.time <- response[[time.var.name]][llm[2]]
  min.cbf.min.time <- time.release + sampling.time
  min.cbf.max.time <- min.abp.time + min.cbf.max.delta.time
  r <- min.cbf.time >= min.cbf.min.time && min.cbf.time <= min.cbf.max.time
  if(!r)
  {
    s1 <- sprintf("time of CBF minimum (%.1f)", min.cbf.time)
    s2 <- sprintf("[%.1f, %.1f]", min.cbf.min.time, min.cbf.max.time)
    return(paste(s1, "is not in the valid range", s2))
  }
  
  cbf.min.sample <- which.min(response[[cbf.var.name]])
  r <- llm[2] == cbf.min.sample
  if(!r)
  {
    s <- sprintf("CBF minimum (%.1f)", min.cbf.value)
    return(paste("CBF has values below", s))
  }
  
  cbf.max <- max(response[[cbf.var.name]])
  r <- cbf.max <= max.cbf.max.value
  if(!r)
  {
    s1 <- sprintf("maximum CBF value (%.3f)", cbf.max)
    s2 <- sprintf("(%.3f)", max.cbf.max.value)
    return(paste(s1, "is higher than allowed", s2))
  }
  
  i <- response[[time.var.name]] > stable.cbf.initial.time - 0.2/100
  j <- response[[time.var.name]] < stable.cbf.final.time + 0.2/100
  stable.cbf <- response[[cbf.var.name]][i & j]
  stable.cbf.var <- var(stable.cbf)
  r <- stable.cbf.var <= stable.cbf.max.variance
  if(!r)
  {
    s1 <- sprintf("stable CBF variance (%.3f)", stable.cbf.var)
    s2 <- sprintf("(%.3f)", stable.cbf.max.variance)
    return(paste(s1, "is higher than allowed", s2))
  }
  
  heights <- apply(min.cbf.info, 1, get.minimum.height, response[[cbf.var.name]])
  min.cbf.info <- cbind(min.cbf.info, heights)
  percents <- heights / heights[1] * 100
  min.cbf.info <- cbind(min.cbf.info, percents)
  i <- percents > peak.min.relative.height
  min.cbf.info <- matrix(min.cbf.info[i, ], ncol = 6)
  r <- nrow(min.cbf.info) <= cbf.max.minima
  if(!r)
  {
    s1 <- sprintf("number of CBF minima (%d)", nrow(min.cbf.info))
    s2 <- sprintf("(%d)", cbf.max.minima)
    return(paste(s1, "is higher than allowed", s2))
  }
  
  n.maxima <- 0
  max.cbf.info <- findpeaks(
    response[[cbf.var.name]],
    zero = "+",
    threshold = min.cbf.threshold
  )
  if(!is.null(max.cbf.info))
  {
    
    max.heights <- apply(max.cbf.info, 1, get.maximum.height, response[[cbf.var.name]])
    max.cbf.info <- cbind(max.cbf.info, max.heights)
    percents <- max.heights / heights[1] * 100
    max.cbf.info <- cbind(max.cbf.info, percents)
    i <- percents > peak.min.relative.height
    max.cbf.info <- matrix(max.cbf.info[i, ], ncol = 6)
    n.maxima <- nrow(max.cbf.info)
  }
  r <- n.maxima <= cbf.max.maxima
  if(!r)
  {
    s1 <- sprintf("number of CBF maxima (%d)", nrow(max.cbf.info))
    s2 <- sprintf("(%d)", cbf.max.maxima)
    return(paste(s1, "is higher than allowed", s2))
  }
  
  return(NULL)
}

eval.step.response.old.old <- function(
      response,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      time.release = 0,
      time.tol = min(diff(response[[time.var.name]])) / 100,
      min.abp.max.delta.time = 5 * 0.8, # Seconds from time of release
      min.cbf.max.delta.time = 2,  # Seconds from min ABP
      min.abp.threshold = 1/10^3, # 3 is number of rounding digits for ABP
      min.cbf.threshold = 1/10^3,  # 3 is number of rounding digits for CBF
      min.cbf.min.value = 0.5,
      min.cbf.max.value = -0.25,
      max.cbf.value = 1,
      max.cbf.minima = 3,
      cbf.local.drop.width = 2,   # Samples around min CBF
      cbf.max.local.drop = 0.04,   # Absolute drop
      cbf.stable.initial.time = 10,
      cbf.stable.final.time = 20,
      cbf.stable.max.variance = 0.0005
      )
{
  response <- response[, c(time.var.name, abp.var.name, cbf.var.name)]
  
  # Checks minimum ABP
  min.abp.info <- findpeaks(
    -response[[abp.var.name]],
    zero = "-",
    threshold = min.abp.threshold
  )
  if(is.null(min.abp.info) || nrow(min.abp.info) != 1)
    stop("ABP step does not have a unique global minimum")
  time.min.abp <- response[[time.var.name]][min.abp.info[1, 2]]
  if(time.min.abp <= time.release)
    stop("ABP step has a global minimum before release time")
  if(time.min.abp > time.release + min.abp.max.delta.time)
    stop("ABP step has a global minimum after the specified maximum delta time")
  if(min(response[[abp.var.name]]) != -min.abp.info[1, 1])
    stop("ABP step has values below its global minimum")
  
  #
  # Checks minimum CBF
  #
  
  min.cbf.info <- findpeaks(
    -response[[cbf.var.name]],
    zero = "-",
    threshold = min.cbf.threshold,
    sortstr = TRUE
  )
  
  # Discard if there too many local minima 
  if(nrow(min.cbf.info) > max.cbf.minima)
    return(paste0(
      "CBF has too many local minima ",
      "(",
      nrow(min.cbf.info),
      ")")
    )
  
  # Discard if deepest local minimum is not around the minimum ABP
  min.cbf.sample <- min.cbf.info[1, 2]
  time.min.cbf <- response[[time.var.name]][min.cbf.sample]
  if(abs(time.min.cbf - time.min.abp) > min.cbf.max.delta.time)
    return(paste0(
      "CBF local minimum is not around min.ABP ",
      "(",
      "min.abp at t=", time.min.abp, "; ",
      "min.cbf at t=", time.min.cbf, "; ",
      "delta.time = ", abs(time.min.cbf - time.min.abp), " s",
      ")")
    )
  
  # Discard if deepest local minimum is in the expected range
  min.cbf.value <- -min.cbf.info[1, 1]
  if(min.cbf.value > min.cbf.min.value || min.cbf.value < min.cbf.max.value)
    return(paste0(
      "CBF local minimum is not in specified range ",
      "(",
      "min.cbf.value=", min.cbf.value, " ",
      "not in ",
      "[",
      min.cbf.max.value,
      ", ",
      min.cbf.min.value,
      "]",
      ")")
    )
  
  # Discard if deepest local minimum is not global minimum
  if(min(response[[cbf.var.name]]) != -min.cbf.info[1, 1])
    return(paste0(
      "CBF has values below its deepest local minimum ",
      "(",
      "=", -min.cbf.info[1, 1], " ",
      "at t=", time.min.cbf,
      ")")
    )
  
  # Discard if local minimum is too steep
  local.drop <- response[[cbf.var.name]][(min.cbf.sample - cbf.local.drop.width):(min.cbf.sample + cbf.local.drop.width)]
  locall.drop.diff <- abs(diff(local.drop))
  if(any(locall.drop.diff > cbf.max.local.drop))
    return(paste0(
      "CBF local drop is too steep ",
      "(",
      "diffs:", local.drop.diff,
      ")")
    )
  
  # Discard if signal peaks two high
  if(max(response[[cbf.var.name]]) > max.cbf.value)
    return(paste0(
      "CBF has values higher than specified maximum value ",
      "(",
      "max=", max(response[[cbf.var.name]]), " ",
      "> ",
      max.cbf.value,
      ")")
    )
  
  # Discard if signal peaks two high
  i <- response[[time.var.name]] > cbf.stable.initial.time - time.tol
  j <- response[[time.var.name]] < cbf.stable.final.time + time.tol
  stable.cbf <- response[[cbf.var.name]][i & j]
  if(var(stable.cbf) > cbf.stable.max.variance)
    return(paste0(
      "variance of stable CBF exceeds allowed maximum ",
      "(",
      "var(t=", cbf.stable.initial.time, ", ",
      "t=", cbf.stable.final.time, ")",
       "=", var(stable.cbf), " ",
      "> ",
      cbf.stable.max.variance,
      ")")
    )
  
  return(NULL)
}





