
require(e1071)

get.lagged.instance <- function(mgr, ...) UseMethod("get.lagged.instance")
get.folded.instance <- function(mgr, ...) UseMethod("get.folded.instance")
get.fold.results <- function(mgr, ...) UseMethod("get.fold.results")
join.folds.results <-function(mgr, ...) UseMethod("join.folds.results")
join.params.results <-function(mgr, ...) UseMethod("join.params.results")
join.lags.results <-function(mgr, ...) UseMethod("join.lags.results")
get.input.var.names <- function(mgr, ...) UseMethod("get.input.var.names")
get.output.var.name <- function(mgr, ...) UseMethod("get.output.var.name")
get.time.var.name <- function(mgr, ...) UseMethod("get.time.var.name")


get.signal.experiment <- function(
      signal,
      time.var.name,
      abp.var.name,
      co2.var.name,
      cbf.var.name,
      sampling.time = unique(diff(signal[[time.var.name]])),
      abp.step.time.until.release = 180,
      abp.step.time.after.release =  90,
      ...)
{
  signal <- signal[, c(time.var.name, abp.var.name, co2.var.name, cbf.var.name)]
  signal[[abp.var.name]] <- .sigexp.internal.normalise(signal[[abp.var.name]])
  signal[[co2.var.name]] <- .sigexp.internal.normalise(signal[[co2.var.name]])
  signal[[cbf.var.name]] <- .sigexp.internal.normalise(signal[[cbf.var.name]])
  
  if(length(sampling.time) != 1)
    stop("sampling time could not be determined")
  
  abp.step <- .sigexp.internal.get.abp.step(
    sampling.time,
    time.var.name,
    abp.var.name,
    co2.var.name,
    signal[[co2.var.name]],
    abp.step.time.until.release,
    abp.step.time.after.release
  )
  
  mgr <- list(
    signal = signal,
    time.var.name = time.var.name,
    abp.var.name = abp.var.name,
    co2.var.name = co2.var.name,
    cbf.var.name = cbf.var.name,
    abp.step = abp.step,
    sampling.time = sampling.time,
    version = .sigexp.internal.version
  )
  class(mgr) <- c(.sigexp.internal.mgr.class, class(mgr))
  
  mgr
}


get.lagged.instance.SIGNAL.EXPERIMENT <- function(mgr, lags, ...)
{
  .sigexp.internal.is.valid.sigexp(mgr)
  
  lagged.signal <- lagSignal(
    signal = mgr[["signal"]],
    lags = lags,
    time.colname = mgr[["time.var.name"]]
  )
  
  input.var.names <- colnames(lagged.signal)
  no.input.var.names <- c(mgr[["time.var.name"]], mgr[["cbf.var.name"]])
  input.var.names <- input.var.names[! input.var.names %in% no.input.var.names]
  
  i <- names(lags) == mgr[["cbf.var.name"]]
  input.lags <- lags[!i]
  lagged.abp.step <- lagSignal(
    signal = mgr[["abp.step"]],
    lags = input.lags,
    time.colname = mgr[["time.var.name"]]
  )
  
  ins.abp.memory <- 0
  if(!is.null(input.lags[[mgr[["abp.var.name"]]]]))
    ins.abp.memory <- input.lags[[mgr[["abp.var.name"]]]]
  ins.memory <- sum(unlist(input.lags))
  ins.order <- 0
  if(any(i))
    ins.order <- lags[[which(i)]]
  
  list(lagged.signal = lagged.signal,
       lagged.abp.step = lagged.abp.step,
       lag.list = lags,
       input.var.names = input.var.names,
       abp.memory = ins.abp.memory,
       total.memory = ins.memory,
       order = ins.order,
       version = mgr[["version"]]
      )
}


get.folded.instance.SIGNAL.EXPERIMENT <- function(
      mgr,
      lagged.ins,
      nfolds,
      fold.colname = "Fold",
      ...)
{
  .sigexp.internal.is.valid.sigexp(mgr)
  .sigexp.internal.is.valid.lagged.signal(lagged.ins)
  
  lagged.signal <- lagged.ins[["lagged.signal"]]
  
  folded.signal <- separateSignalIntoFolds(
    signal = lagged.signal,
    n.folds = nfolds,
    fold.colname = fold.colname,
    time.colname = mgr[["time.var.name"]]
  )
  
  list(folded.signal = folded.signal,
       lagged.abp.step = lagged.ins[["lagged.abp.step"]],
       lag.list = lagged.ins[["lag.list"]],
       input.var.names = lagged.ins[["input.var.names"]],
       abp.memory = lagged.ins[["abp.memory"]],
       total.memory = lagged.ins[["total.memory"]],
       order = lagged.ins[["order"]],
       version = mgr[["version"]]
      )
}


get.fold.results.SIGNAL.EXPERIMENT <- function(
      mgr,
      folded.ins,
      fold,
      model.params,
      ...)
{
  folded.signal <- folded.ins[["folded.signal"]]
  fold.colname <- attr(folded.signal, FOLD.COLUMN.NAME.ATTR)
  i <- folded.signal[[fold.colname]] == fold
  folded.signal <- folded.signal[i, ]
  
  fmla.str <- paste(folded.ins[["input.var.names"]], collapse = " + ")
  fmla.str <- paste(mgr[["cbf.var.name"]], "~", fmla.str)
  fmla <- formula(fmla.str)
  
  params <- list(formula = fmla, data = folded.signal)
  params <- c(params, list(type = "nu-regression", kernel = "radial"))
  params <- c(params, model.params)
  
  # Gets SVM model
  start.time <- Sys.time()
  model <- do.call(svm, params)
  end.time <-Sys.time()
  
  # Gets Stats
  stats <- .sigexp.internal.eval.model(mgr, folded.ins, fold, model, ...)
  train.time <- end.time - start.time
  stats <- cbind(fold = fold, train.time, stats)
  
  results <- list(models = list(model), stats = stats) 
  results[["version"]] <- mgr[["version"]]
  class(results) <- c(.sigexp.internal.result.class, class(results))
  
  results
}


join.folds.results.SIGNAL.EXPERIMENT <- function(
      mgr,
      model.params,
      folds.results,
      ...)
{
  results <- .sigexp.internal.join.results(mgr, folds.results)
  stats <- cbind(model.params, results[[2]])
  results[["stats"]] <- stats
  
  results
}


join.params.results.SIGNAL.EXPERIMENT <- function(
      mgr,
      lags,
      params.results,
      ...)
{
  results <- .sigexp.internal.join.results(mgr, params.results)
  stats <- cbind(lags, results[[2]])
  results[["stats"]] <- stats
  
  results
}

join.lags.results.SIGNAL.EXPERIMENT <- function(
      mgr,
      lags.results,
      ...)
{
  .sigexp.internal.join.results(mgr, lags.results)
}


get.input.var.names.MODEL.MANAGER <- function(mgr, ...)
{
  c(mgr[["abp.var.name"]], mgr[["co2.var.name"]])
}

get.output.var.name.MODEL.MANAGER <- function(mgr, ...)
{
  mgr[["cbf.var.name"]]
}

get.time.var.name.MODEL.MANAGER <- function(mgr, ...)
{
  mgr[["time.var.name"]]
}



### Internals

.sigexp.internal.mgr.class <- "SIGNAL.EXPERIMENT"
.sigexp.internal.result.class <- "SIGNAL.EXPERIMENT.RESULT"
.sigexp.internal.version <- "15.4.1"

.sigexp.internal.is.valid.sigexp <- function(mgr)
{
  if(!any(class(mgr) != .sigexp.internal.mgr.class))
    stop("object is not an experiment manager")
  
  if(mgr[["version"]] != .sigexp.internal.version)
    stop("experiment manager of unsupported version")
}
.sigexp.internal.is.valid.lagged.signal <- function(s)
{
  if(!any(class(s) != FOLDED.DATA.CLASS.NAME))
    stop("object is not a folded signal")
  
  if(s[["version"]] != .sigexp.internal.version)
    stop("folded signal of unsupported version")
}
.sigexp.internal.is.valid.folded.signal <- function(s)
{
  if(!any(class(s) != LAGGED.DATA.CLASS.NAME))
    stop("object is not a lagged signal")
  
  if(s[["version"]] != .sigexp.internal.version)
    stop("lagged signal of unsupported version")
}
.sigexp.internal.is.valid.sigexp.result <- function(r)
{
  if(!any(class(r) != .sigexp.internal.result.class))
    stop("object is not the results of an signal experiment")
  
  if(r[["version"]] != .sigexp.internal.version)
    stop("signal experiment results of unsupported version")
}

.sigexp.internal.normalise <- function(x)
{
  # Feature scaling
  nx <- (x - min(x)) / (max(x) - min(x))
  if(any(!is.finite(nx)))
    stop("signal contains NAs, NaNs or only zeroes or infinite values")
  nx
}

# Check with Max, abp step normalisation
.sigexp.internal.get.abp.step <- function(
      sampling.time,
      time.var.name,
      abp.var.name,
      co2.var.name,
      norm.co2.signal,
      abp.step.time.until.release,
      abp.step.time.after.release
      )
{
  smooth.step.stimulus <- TRUE
  filter.order <- 2
  cutoff.frequency <- 0.20
  left.stabilisation.time <- 0
  time.rounding.digits <-  1
  
  step <- get.normalised.ABP.stimulus(
    sampling.time = sampling.time,
    time.until.release = abp.step.time.until.release,
    time.after.release = abp.step.time.after.release,
    smooth.step.stimulus = smooth.step.stimulus,
    filter.order = filter.order,
    cutoff.frequency = cutoff.frequency,
    left.stabilisation.time = 1,
    time.rounding.digits =  time.rounding.digits
  )
  
  time <- step[["time.instants"]]
  abp <-  .sigexp.internal.normalise(step[["ABP.normalised"]])
  co2 <- mean(norm.co2.signal)
  
  abp.step <- data.frame(time, abp, co2)
  colnames(abp.step) <- c(time.var.name, abp.var.name, co2.var.name)
  
  abp.step
}

.sigexp.internal.join.results <-function(mgr, results.list)
{
  sapply(results.list, .sigexp.internal.is.valid.sigexp.result)
  
  models <- sapply(results.list, function(l) l[[1]])
  stats.list <- lapply(results.list, function(l) l[[2]])
  stats <- do.call(rbind, stats.list)
  
  results <- list(models = models, stats = stats) 
  results[["version"]] <- mgr[["version"]]
  class(results) <- c(.sigexp.internal.result.class, class(results))
  
  results
}

.sigexp.internal.eval.model <- function(
      mgr,
      folded.ins,
      fold,
      model,
      time.rounding.digits = 4,
      train.cor.rounding.digits = 3,
      test.cor.rounding.digits = 1,
      ...
     )
{
  folded.signal <- folded.ins[["folded.signal"]]
  fold.colname <- attr(folded.signal, FOLD.COLUMN.NAME.ATTR)
  nfolds <- attr(folded.signal, NUMBER.OF.FOLDS.ATTR)
  
  i <- folded.signal[[fold.colname]] == fold
  train.signal <- folded.signal[i, ]
  train.cor <- cor(model[["fitted"]], train.signal[[mgr[["cbf.var.name"]]]])
  train.cor <- round(train.cor, train.cor.rounding.digits)
  
  test.fold <- fold + 1
  if(test.fold > nfolds)
    test.fold <- test.fold - 2
  
  i <- folded.signal[[fold.colname]] == test.fold
  test.signal <- folded.signal[i, ]
  
  x <- test.signal[, folded.ins[["input.var.names"]]]
  y <- test.signal[[mgr[["cbf.var.name"]]]]
  pred <- predict(model, x, ...)
  test.cor <- cor(pred, y)
  test.cor <- round(test.cor, test.cor.rounding.digits)
  
  if(folded.ins[["order"]] > 0)
    step.eval <- .sigexp.internal.eval.arx.model(folded.ins, model)
  else
    step.eval <- .sigexp.internal.eval.fir.model(
      folded.ins,
      mgr[["time.var.name"]],
      mgr[["abp.var.name"]],
      model,
      mgr[["sampling.time"]]
    )
  step.score <- round(step.eval[["score"]], test.cor.rounding.digits)
  
  perf <- round(step.score * test.cor, test.cor.rounding.digits)
  
  data.frame(
    train.cor = train.cor,
    test.cor = test.cor,
    step.score = step.score,
    perf = perf,
    model.mem = folded.ins[["total.memory"]],
    model.ord = folded.ins[["order"]]
  )
}

.sigexp.internal.eval.arx.model <- function(folded.ins, model)
{
  stop("step response for ARX models is not implemented yet")
}

.sigexp.internal.eval.fir.model <- function(
    folded.ins,
    time.var.name,
    abp.var.name,
    model,
    sampling.time,
    time.release = 0,
    time.until.release = 10,
    time.after.release = 20,
    time.tol = sampling.time / 100
    )
{
  step.response <- predict(model, folded.ins[["lagged.abp.step"]])
  
  time.ini <- time.release - time.until.release - time.tol
  time.fin <- time.release + time.after.release + time.tol
  i <- folded.ins[["lagged.abp.step"]][[time.var.name]] > time.ini
  j <- folded.ins[["lagged.abp.step"]][[time.var.name]] < time.fin
  samples <- which(i & j)
  
  segment <- folded.ins[["lagged.abp.step"]][samples, ]
  samples <- samples + folded.ins[["abp.memory"]]
  segment[["Response"]] <- step.response[samples]
  
  i <- segment[[time.var.name]] < time.release + time.tol
  vshift <- 0.8 - mean(segment[["Response"]][i])
  segment[["Response"]] <- segment[["Response"]] + vshift
  
  #.sigexp.internal.plot.segment(segment, time.var.name, abp.var.name)
  .sigexp.internal.eval.step.response(
    segment,
    sampling.time,
    time.var.name,
    abp.var.name
  )
}

.sigexp.internal.eval.step.response <- function(
    segment,
    sampling.time,
    time.var.name,
    abp.var.name,
    cbf.var.name = "Response",
    time.release = 0,
    min.abp.max.delta.time = 5,  # From release time
    min.cbf.max.delta.time = 3,  # From min ABP
    cbf.drop.max.delta.time = sampling.time,
    cbf.drop.reference = 0.4,
    cbf.lower.bound = -0.2,
    cbf.upper.bound = 1,
    steady.cbf.delta.time = 5,   # From the end of the signal
    steady.cbf.max.variance = 0.002,
    time.tol = sampling.time / 100,
    num.tol = 1 / 10000
    )
{
  segment <- segment[, c(time.var.name, abp.var.name, cbf.var.name)]
  upto.release <- segment[[time.var.name]] < time.release + time.tol
  samples.upto.release <- which(upto.release)
  after.release <- segment[[time.var.name]] > time.release + time.tol
  samples.after.release <- which(after.release)
  
  min.abp <- diff(diff(segment[[abp.var.name]]) > num.tol) > 0
  if(sum(min.abp) != 1)
    stop("ABP step does not have a unique global minimum")
  sample.min.abp <- which(min.abp) + 1
  time.min.abp <- segment[[time.var.name]][sample.min.abp]
  
  if(sample.min.abp < samples.after.release[1])
    stop("ABP step has a global minimum before release time")
  
  if(time.min.abp > time.release + min.abp.max.delta.time)
    stop("ABP step has a global minimum after the specified maximum delta time")
  
  # There is one (and only one) local minimum of CBF around
  # the time of min ABP
  i <- segment[[time.var.name]] > time.min.abp - min.cbf.max.delta.time - time.tol
  j <- segment[[time.var.name]] < time.min.abp + min.cbf.max.delta.time + time.tol
  samples.min.cbf <- which(i & j)
  min.cbf <- diff(diff(segment[[cbf.var.name]][samples.min.cbf]) > num.tol) > num.tol
  if(sum(min.cbf) != 1)
    # CBF does not have a unique local minimum in the specified range
    return(list(segment = segment, score = 0))
  sample.min.cbf <- samples.min.cbf[which(min.cbf)[1] + 1]
  time.min.cbf <- segment[[time.var.name]][sample.min.cbf]
  
  # The mean CBF signal around its local minimum exhibits a drop 
  cbf.baseline.value <- mean(segment[[cbf.var.name]][samples.upto.release])
  i <- segment[[time.var.name]] > time.min.cbf - cbf.drop.max.delta.time - time.tol
  j <- segment[[time.var.name]] < time.min.cbf + cbf.drop.max.delta.time + time.tol
  samples.cbf.drop <- which(i & j)
  mean.cbf.drop <- mean(segment[[cbf.var.name]][samples.cbf.drop])
  drop.score <- max(0, mean.cbf.drop - cbf.drop.reference)
  drop.score <- 1 - (min(drop.score, cbf.baseline.value) / cbf.baseline.value)
  
  # The end of the signal is stable
  last.time <- tail(segment[[time.var.name]], 1)
  i <- segment[[time.var.name]] > last.time - steady.cbf.delta.time - time.tol
  samples.cbf.steady <- which(i)
  cbf.steady.var <- var(segment[[cbf.var.name]][samples.cbf.steady])
  steady.score <- 1 / max(1, min(10, cbf.steady.var / steady.cbf.max.variance))
  
  # The signal is lower bounded
  # Assumes bound is negative and > -1
  cbf.minimum <- min(segment[[cbf.var.name]])
  bound <- 2 * cbf.lower.bound
  lower.bound.score <- 1 - max(bound, min(0, cbf.minimum - cbf.lower.bound)) / bound
  
  # The signal is upper bounded
  cbf.maximum <- max(segment[[cbf.var.name]])
  bound <- 1.5 * cbf.upper.bound # assumes positive, >= 1
  upper.bound.score <- 1 - min(bound, max(0, cbf.maximum - cbf.upper.bound)) / bound
  
  # The signal does not oscillates much
  # At this point, there is at least one local minimum
  local.mins <- diff(diff(segment[[cbf.var.name]]) >= num.tol) > 0
  local.maxs <- diff(diff(segment[[cbf.var.name]]) >= num.tol) < 0
  oscillation.score <- 1 / max(1, sum(local.mins) + sum(local.maxs) - 3)

  score <- min(drop.score, steady.score, lower.bound.score,
               upper.bound.score, oscillation.score)
  
  list(segment = segment, score = score)
}

.sigexp.internal.plot.segment <- function(
    segment,
    time.var.name,
    abp.var.name
    )
{
  t <- segment[[time.var.name]]
  abp <- segment[[abp.var.name]]
  cbf <- segment[["Response"]]
  
  d1 <- data.frame(Time = t, Signal = abp, Type = abp.var.name)
  d2 <- data.frame(Time = t, Signal = cbf, Type = "Response")
  d <- rbind(d1, d2)
  
  p <- ggplot(d, aes(x = Time, y = Signal, colour = Type))
  p <- p + geom_line() + geom_point()
  print(p)
}
