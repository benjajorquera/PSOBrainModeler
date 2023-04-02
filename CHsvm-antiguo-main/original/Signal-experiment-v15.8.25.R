
require(e1071)
require(pracma)

get.lagged.instance <- function(sigexp, ...) UseMethod("get.lagged.instance")
get.folded.instance <- function(sigexp, ...) UseMethod("get.folded.instance")
get.fold.results <- function(sigexp, ...) UseMethod("get.fold.results")
join.folds.results <-function(sigexp, ...) UseMethod("join.folds.results")
join.params.results <-function(sigexp, ...) UseMethod("join.params.results")
join.lags.results <-function(sigexp, ...) UseMethod("join.lags.results")
get.input.var.names <- function(sigexp, ...) UseMethod("get.input.var.names")
get.output.var.name <- function(sigexp, ...) UseMethod("get.output.var.name")
get.time.var.name <- function(sigexp, ...) UseMethod("get.time.var.name")


get.signal.experiment <- function(
      signal,
      signal.sampling.time,
      time.var.name,
      abp.var.name,
      co2.var.name,
      cbf.var.name,
      abp.step.time.until.release = 40,
      abp.step.time.after.release =  30,
      time.rounding.digits =  1,
      abp.rounding.digits = 4,
      co2.rounding.digits = 3,
      cbf.rounding.digits = 4,
      train.cor.rounding.digits = 3,
      test.cor.rounding.digits = 3
      )
{
  signal <- signal[, c(time.var.name, abp.var.name, co2.var.name, cbf.var.name)]
  signal[[time.var.name]] <- round(signal[[time.var.name]], time.rounding.digits)
  signal[[abp.var.name]] <- .sigexp.internal.normalise(signal[[abp.var.name]])
  signal[[abp.var.name]] <- round(signal[[abp.var.name]], abp.rounding.digits)
  signal[[co2.var.name]] <- .sigexp.internal.normalise(signal[[co2.var.name]])
  signal[[co2.var.name]] <- round(signal[[co2.var.name]], co2.rounding.digits)
  signal[[cbf.var.name]] <- .sigexp.internal.normalise(signal[[cbf.var.name]])
  signal[[cbf.var.name]] <- round(signal[[cbf.var.name]], cbf.rounding.digits)
  
  abp.step <- .sigexp.internal.get.abp.step(
    signal.sampling.time,
    time.var.name,
    abp.var.name,
    co2.var.name,
    signal[[co2.var.name]],
    abp.step.time.until.release,
    abp.step.time.after.release,
    time.rounding.digits,
    abp.rounding.digits,
    co2.rounding.digits
  )
  
  sigexp <- list(
    signal = signal,
    time.var.name = time.var.name,
    abp.var.name = abp.var.name,
    co2.var.name = co2.var.name,
    cbf.var.name = cbf.var.name,
    time.rounding.digits = time.rounding.digits,
    abp.rounding.digits = abp.rounding.digits,
    co2.rounding.digits = co2.rounding.digits,
    cbf.rounding.digits = cbf.rounding.digits,
    train.cor.rounding.digits = train.cor.rounding.digits,
    test.cor.rounding.digits = test.cor.rounding.digits,
    abp.step = abp.step,
    version = .sigexp.internal.version
  )
  class(sigexp) <- c(.sigexp.internal.sigexp.class, class(sigexp))
  
  sigexp
}


get.lagged.instance.SIGNAL.EXPERIMENT <- function(sigexp, lags, ...)
{
  .sigexp.internal.is.valid.sigexp(sigexp)
  
  lagged.signal <- lagSignal(
    signal = sigexp[["signal"]],
    lags = lags,
    time.colname = sigexp[["time.var.name"]]
  )
  
  input.var.names <- colnames(lagged.signal)
  no.input.var.names <- c(sigexp[["time.var.name"]], sigexp[["cbf.var.name"]])
  input.var.names <- input.var.names[! input.var.names %in% no.input.var.names]
  
  i <- names(lags) == sigexp[["cbf.var.name"]]
  input.lags <- lags[!i]
  lagged.abp.step <- lagSignal(
    signal = sigexp[["abp.step"]],
    lags = input.lags,
    time.colname = sigexp[["time.var.name"]]
  )
  
  ins.abp.memory <- 0
  if(!is.null(input.lags[[sigexp[["abp.var.name"]]]]))
    ins.abp.memory <- input.lags[[sigexp[["abp.var.name"]]]]
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
       version = sigexp[["version"]]
      )
}


get.folded.instance.SIGNAL.EXPERIMENT <- function(
      sigexp,
      lagged.ins,
      nfolds,
      fold.colname = "Fold",
      ...)
{
  .sigexp.internal.is.valid.sigexp(sigexp)
  .sigexp.internal.is.valid.lagged.signal(lagged.ins)
  
  lagged.signal <- lagged.ins[["lagged.signal"]]
  
  folded.signal <- separateSignalIntoFolds(
    signal = lagged.signal,
    n.folds = nfolds,
    fold.colname = fold.colname,
    time.colname = sigexp[["time.var.name"]]
  )
  
  list(folded.signal = folded.signal,
       lagged.abp.step = lagged.ins[["lagged.abp.step"]],
       lag.list = lagged.ins[["lag.list"]],
       input.var.names = lagged.ins[["input.var.names"]],
       abp.memory = lagged.ins[["abp.memory"]],
       total.memory = lagged.ins[["total.memory"]],
       order = lagged.ins[["order"]],
       version = sigexp[["version"]]
      )
}


get.fold.results.SIGNAL.EXPERIMENT <- function(
      sigexp,
      folded.ins,
      fold,
      model.params,
      svm.type = "nu-regression",
      svm.kernel = "radial",
      ...)
{
  .sigexp.internal.is.valid.sigexp(sigexp)
  .sigexp.internal.is.valid.folded.signal(folded.ins)
  
  folded.signal <- folded.ins[["folded.signal"]]
  fold.colname <- attr(folded.signal, FOLD.COLUMN.NAME.ATTR)
  i <- folded.signal[[fold.colname]] == fold
  folded.signal <- folded.signal[i, ]
  
  fmla.str <- paste(folded.ins[["input.var.names"]], collapse = " + ")
  fmla.str <- paste(sigexp[["cbf.var.name"]], "~", fmla.str)
  fmla <- formula(fmla.str)
  
  params <- list(formula = fmla, data = folded.signal)
  params <- c(params, list(type = svm.type, kernel = svm.kernel))
  params <- c(params, model.params)
  
  # Gets SVM model
  start.time <- Sys.time()
  model <- do.call(svm, params)
  end.time <-Sys.time()
  
  # Gets Stats
  stats <- .sigexp.internal.eval.model(sigexp, folded.ins, fold, model, ...)
  train.time <- end.time - start.time
  stats <- cbind(fold = fold, train.time, stats)
  
  results <- list(models = list(model), stats = stats) 
  results[["version"]] <- sigexp[["version"]]
  class(results) <- c(.sigexp.internal.result.class, class(results))
  
  results
}


join.folds.results.SIGNAL.EXPERIMENT <- function(
      sigexp,
      model.params,
      folds.results,
      ...)
{
  results <- .sigexp.internal.join.results(sigexp, folds.results)
  stats <- cbind(model.params, results[[2]])
  results[["stats"]] <- stats
  
  results
}


join.params.results.SIGNAL.EXPERIMENT <- function(
      sigexp,
      lags,
      params.results,
      ...)
{
  results <- .sigexp.internal.join.results(sigexp, params.results)
  stats <- cbind(lags, results[[2]])
  results[["stats"]] <- stats
  
  results
}


join.lags.results.SIGNAL.EXPERIMENT <- function(
      sigexp,
      lags.results,
      ...)
{
  .sigexp.internal.join.results(sigexp, lags.results)
}


get.input.var.names.SIGNAL.EXPERIMENT <- function(sigexp, ...)
{
  c(sigexp[["abp.var.name"]], sigexp[["co2.var.name"]])
}

get.output.var.name.SIGNAL.EXPERIMENT <- function(sigexp, ...)
{
  sigexp[["cbf.var.name"]]
}

get.time.var.name.SIGNAL.EXPERIMENT <- function(sigexp, ...)
{
  sigexp[["time.var.name"]]
}




### Internals

.sigexp.internal.sigexp.class <- "SIGNAL.EXPERIMENT"
.sigexp.internal.result.class <- "SIGNAL.EXPERIMENT.RESULT"
.sigexp.internal.version <- "15.8.25"
.sigexp.internal.stattr <- "sampling.time"
.sigexp.internal.trattr <- "time.release"

.sigexp.internal.is.valid.sigexp <- function(sigexp)
{
  if(!any(class(sigexp) != .sigexp.internal.sigexp.class))
    stop("object is not an experiment manager")
  
  if(sigexp[["version"]] != .sigexp.internal.version)
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
      abp.step.time.after.release,
      time.rounding.digits,
      abp.rounding.digits,
      co2.rounding.digits
      )
{
  smooth.step.stimulus <- TRUE
  filter.order <- 2
  cutoff.frequency <- 0.20
  left.stabilisation.time <- 0
  
  step <- get.normalised.ABP.stimulus(
    sampling.time = sampling.time,
    time.until.release = abp.step.time.until.release,
    time.after.release = abp.step.time.after.release,
    smooth.step.stimulus = smooth.step.stimulus,
    filter.order = filter.order,
    cutoff.frequency = cutoff.frequency,
    left.stabilisation.time = 0,
    time.rounding.digits =  time.rounding.digits
  )
  
  abp <- round(step[["ABP.normalised"]], abp.rounding.digits)
  co2 <- round(mean(norm.co2.signal), co2.rounding.digits)
  
  abp.step <- data.frame(step[["time.instants"]], abp, co2)
  colnames(abp.step) <- c(time.var.name, abp.var.name, co2.var.name)
  attr(abp.step, .sigexp.internal.stattr) <- step[["sampling.time"]]
  attr(abp.step, .sigexp.internal.trattr) <- step[["time.release"]]
  
  abp.step
}

.sigexp.internal.join.results <-function(sigexp, results.list)
{
  sapply(results.list, .sigexp.internal.is.valid.sigexp.result)
  
  models <- sapply(results.list, function(l) l[[1]])
  stats.list <- lapply(results.list, function(l) l[[2]])
  stats <- do.call(rbind, stats.list)
  
  results <- list(models = models, stats = stats) 
  results[["version"]] <- sigexp[["version"]]
  class(results) <- c(.sigexp.internal.result.class, class(results))
  
  results
}

.sigexp.internal.eval.model <- function(
      sigexp,
      folded.ins,
      fold,
      model,
      ...
     )
{
  folded.signal <- folded.ins[["folded.signal"]]
  fold.colname <- attr(folded.signal, FOLD.COLUMN.NAME.ATTR)
  nfolds <- attr(folded.signal, NUMBER.OF.FOLDS.ATTR)
  
  i <- folded.signal[[fold.colname]] == fold
  train.signal <- folded.signal[i, ]
  fitted.signal <- round(model[["fitted"]], sigexp[["cbf.rounding.digits"]])
  train.cor <- cor(fitted.signal, train.signal[[sigexp[["cbf.var.name"]]]])
  train.cor <- round(train.cor, sigexp[["train.cor.rounding.digits"]])
  
  test.fold <- fold + 1
  if(test.fold > nfolds)
    test.fold <- test.fold - 2
  
  i <- folded.signal[[fold.colname]] == test.fold
  test.signal <- folded.signal[i, ]
  
  x <- test.signal[, folded.ins[["input.var.names"]]]
  y <- test.signal[[sigexp[["cbf.var.name"]]]]
  pred <- predict(model, x, ...)
  pred <- round(pred, sigexp[["cbf.rounding.digits"]])
  test.cor <- cor(pred, y)
  test.cor <- round(test.cor, sigexp[["test.cor.rounding.digits"]])
  
  data.frame(
    train.cor = train.cor,
    test.cor = test.cor,
    model.mem = folded.ins[["total.memory"]],
    model.ord = folded.ins[["order"]]
  )
}


.sigexp.internal.get.export.names <- function()
{
  c("get.folded.instance",
    "get.fold.results", 
    "get.lagged.instance", 
    "join.folds.results", 
    "join.lags.results", 
    "join.params.results", 
    "get.input.var.names", 
    "get.output.var.name", 
    "get.time.var.name", 
    ".sigexp.internal.sigexp.class", 
    ".sigexp.internal.result.class", 
    ".sigexp.internal.version", 
    ".sigexp.internal.stattr",
    ".sigexp.internal.trattr",
    ".sigexp.internal.is.valid.sigexp", 
    ".sigexp.internal.is.valid.lagged.signal", 
    ".sigexp.internal.is.valid.folded.signal", 
    ".sigexp.internal.is.valid.sigexp.result", 
    ".sigexp.internal.join.results", 
    ".sigexp.internal.eval.model",
    "get.lagged.instance.SIGNAL.EXPERIMENT", 
    "get.folded.instance.SIGNAL.EXPERIMENT", 
    "get.fold.results.SIGNAL.EXPERIMENT", 
    "join.folds.results.SIGNAL.EXPERIMENT", 
    "join.params.results.SIGNAL.EXPERIMENT", 
    "join.lags.results.SIGNAL.EXPERIMENT", 
    "get.input.var.names.SIGNAL.EXPERIMENT", 
    "get.output.var.name.SIGNAL.EXPERIMENT", 
    "get.time.var.name.SIGNAL.EXPERIMENT"
  )
}
