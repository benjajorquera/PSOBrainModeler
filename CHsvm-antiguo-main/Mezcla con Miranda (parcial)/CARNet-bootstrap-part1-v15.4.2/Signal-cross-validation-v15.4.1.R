
FOLDED.DATA.CLASS.NAME <- "SEPARATED.IN.FOLDS"
NUMBER.OF.FOLDS.ATTR <- "N.FOLDS"
FOLD.COLUMN.NAME.ATTR <- "FOLD.COLNAME"
TIME.COLUMN.NAME.ATTR <- "TIME.COLNAME"

crossValidate <- function(
      exp.mgr,
      folded.ins,
      model.params,
      ...)
{
  folds <- 1:attr(folded.ins[["folded.signal"]], NUMBER.OF.FOLDS.ATTR)
  
  cv.internal.tmp.fun <- function(f) get.fold.results(
    mgr = exp.mgr,
    folded.ins = folded.ins,
    fold = f,
    model.params = model.params,
    ...
  )
  cv.results <- lapply(folds, cv.internal.tmp.fun)
  
  join.folds.results(
    mgr = exp.mgr,
    model.params = model.params,
    folds.results = cv.results,
    ...
  )
}

# Divides "signal" in "n.folds" consecutive folds.
# When the data is not exactly divisible, first folds get an extra datum.
# If the data (rows) are not sorted by time, the name of a "time column"
# can be specified.
#
separateSignalIntoFolds <- function(
      signal,
      n.folds = 10,
      fold.colname = "FOLD",
      time.colname = attr(signal, TIME.COLUMN.NAME.ATTR))
{
  nrows <- nrow(signal)
  nbase <- as.integer(nrows / n.folds)
  nextra <- nrows - nbase * n.folds
  n.per.fold <- rep(nbase, n.folds)
  extra.per.fold <- c(rep(1, nextra), rep(0, n.folds - nextra))
  n.per.fold <- n.per.fold + extra.per.fold
  
  fold <- rep(1:n.folds, n.per.fold)

  if(!is.null(time.colname))
  {
    i <- order(order(signal[[time.colname]]))
    fold <- fold[i]
  }
  
  signal[[fold.colname]] <- factor(fold)
  class(signal) <- c(class(signal), FOLDED.DATA.CLASS.NAME)
  attr(signal, FOLD.COLUMN.NAME.ATTR) <- fold.colname
  attr(signal, NUMBER.OF.FOLDS.ATTR) <- n.folds
  if(!is.null(time.colname))
    attr(signal, TIME.COLUMN.NAME.ATTR) <- time.colname
  
  invisible(signal)
}

