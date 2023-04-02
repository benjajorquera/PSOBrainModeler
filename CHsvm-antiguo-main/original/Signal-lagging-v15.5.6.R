
LAGGED.DATA.CLASS.NAME <- "LAGGED.SIGNAL"
LAG.ORDER.NAME.ATTR <- "LAG.ORDER.LIST"
LAGGED.COLUMN.NAMES.ATTR <- "LAGGED.COLUMN.NAMES"

# Adds lags to the columns of the data frame df.
# "lags" is a named list of positive integers:
#                list(col_1 = p_1, col_2 = p_2, ..., col_n = p_n),
# in which p_1, p_2, ..., p_n are the lag orders to be respectively
# introduced for the n variables c_1, c_2, ..., c_n. Each c_i should be
# a column in the data frame.
# If "sorting.colname" is not NULL, that column in the data frame is used
# to sort the data before the lags are applied.
# No registration of the operation performed is left in the data frame
# attributes.
#
# Returns a list of two elements:
#   new.df: with the new data frame with lagged columns
#   lagged.columns.names: a vector with the name of the lagged columns
#
lagDataFrameColumns <- function(df, lags, sorting.colname = NULL)
{
  if(!inherits(df, "data.frame"))
    stop("signal must be a data frame")
  if(!inherits(lags, "list"))
    stop("lags must be specified as named list [1]")
  
  lag.vars <- names(lags)
  if(is.null(lag.vars))
    stop("lags must be specified as named list [2]")
  if(any(lag.vars == ""))
    stop("lags must be specified as named list [3]")
  
  if(!is.null(sorting.colname))
  {
    if(length(sorting.colname) != 1)
      stop("exactly one sorting column can be given")
    if(!inherits(sorting.colname, "character"))
      stop("the name of the sorting column must be given")
    indices <- order(df[[sorting.colname]])
  }
  else
  {
    nrows <- nrow(df)
    indices <- 1:nrows
  }
  
  max.lag <- max(unlist(lags)) + 1
  lag.mat <- embed(indices, max.lag)
  
  col.names <- colnames(df)
  columns <- NULL
  lagged.columns.names <- c()
  for(colname in col.names)
  {
    lag.order <- lags[[colname]]
    if(!is.null(lag.order) && lag.order > 0)
      for(i in lag.order:1)
      {
        new.colname <- paste(colname, paste0("lag", i), sep = ".")
        lagged.columns.names <- c(lagged.columns.names, new.colname)
        columns[[new.colname]] <- df[lag.mat[, i + 1], colname]
      }
    columns[[colname]] <- df[lag.mat[, 1], colname]
  }
  
  new.df <- data.frame(columns)
  sorting <- order(lag.mat[, 1])
  new.df <- new.df[sorting, ]
  
  invisible(list(new.df = new.df, lagged.columns.names = lagged.columns.names))
}

lagSignalForCrossValidationWithinEachFold <- function(signal, lags,
                                                      time.colname = attr(signal, TIME.COLUMN.NAME.ATTR))
{
  # If signal is not folded
  if(!inherits(signal, FOLDED.DATA.CLASS.NAME))
    stop("signal is not folded")

  nrows <- nrow(signal)
  signal[["tmp.index.lag.for.learning"]] <- 1:nrows
  
  # For each fold
  fold.colname <- attr(signal, FOLD.COLUMN.NAME.ATTR)
  folds <- unique(signal[[fold.colname]])
  new.signal <- NULL
  for(fold in folds)
  {
    i <- signal[[fold.colname]] == fold
    df <- signal[i, ]
    ans <- lagDataFrameColumns(df = df, lags = lags,
                               sorting.colname = time.colname)
    new.df <- ans[["new.df"]]
    new.signal <- rbind(new.signal, new.df)
  }
  
  sorting <- order(new.signal[["tmp.index.lag.for.learning"]])
  new.signal <- new.signal[sorting, ]
  new.signal[["tmp.index.lag.for.learning"]] <- NULL

  attrs <- attributes(signal)
  attrs[["names"]] <- colnames(new.signal)
  attrs[["row.names"]] <- rownames(new.signal)
  attributes(new.signal) <- attrs
  class(new.signal) <- c(class(new.signal), LAGGED.DATA.CLASS.NAME)
  attr(new.signal, LAG.ORDER.NAME.ATTR) <- lags
  attr(new.signal, LAGGED.COLUMN.NAMES.ATTR) <- ans[["lagged.columns.names"]]
  if(!is.null(time.colname))
    attr(new.signal, TIME.COLUMN.NAME.ATTR) <- time.colname
  
  invisible(new.signal)
}

lagSignalForCrossValidationAndRefold <- function(signal, lags,
                                                 time.colname = attr(signal, TIME.COLUMN.NAME.ATTR))
{
  # If signal is not folded
  if(!inherits(signal, FOLDED.DATA.CLASS.NAME))
    stop("signal is not folded")
  
  ans <- lagDataFrameColumns(df = signal, lags = lags,
                             sorting.colname = time.colname)
  
  new.signal <- ans[["new.df"]]
  
  fold.colname <- attr(signal, FOLD.COLUMN.NAME.ATTR)
  n.folds <- attr(signal, NUMBER.OF.FOLDS.ATTR)
  
  if(is.null(time.colname))
    tmp.df <- data.frame(TIME = 1:nrow(new.signal))
  else
    tmp.df <- data.frame(TIME = new.signal[[time.colname]])
  
  tmp.fold <- separateSignalIntoFolds(signal = tmp.df,
                                      n.folds = n.folds,
                                      fold.colname = "FOLD",
                                      time.colname = "TIME")
  
  new.signal[[fold.colname]] <- tmp.fold[["FOLD"]]
  
  attrs <- attributes(signal)
  attrs[["names"]] <- colnames(new.signal)
  attrs[["row.names"]] <- rownames(new.signal)
  attributes(new.signal) <- attrs
  class(new.signal) <- c(class(new.signal), LAGGED.DATA.CLASS.NAME)
  attr(new.signal, LAG.ORDER.NAME.ATTR) <- lags
  attr(new.signal, LAGGED.COLUMN.NAMES.ATTR) <- ans[["lagged.columns.names"]]
  if(!is.null(time.colname))
    attr(new.signal, TIME.COLUMN.NAME.ATTR) <- time.colname
  
  invisible(new.signal)
}

# Adds lags to a signal (data frame).
# "lags" is a named list of positive integers:
#                list(col_1 = p_1, col_2 = p_2, ..., col_n = p_n),
# in which p_1, p_2, ..., p_n are the lag orders to be respectively
# introduced for the n variables c_1, c_2, ..., c_n. Each c_i should be
# a column in the signal data frame.
# If the data (rows) are not sorted by time, the name of a "time column"
# can be specified.
# 
lagSignal <- function(signal, lags, time.colname = attr(signal, TIME.COLUMN.NAME.ATTR))
{
  # If signal is folded
  if(inherits(signal, FOLDED.DATA.CLASS.NAME))
    stop("signal is folded; there are special functions for that")
  
  ans <- lagDataFrameColumns(df = signal, lags = lags,
                             sorting.colname = time.colname)
  
  new.signal <- ans[["new.df"]]
  
  attrs <- attributes(signal)
  attrs[["names"]] <- colnames(new.signal)
  attrs[["row.names"]] <- rownames(new.signal)
  attributes(new.signal) <- attrs
  class(new.signal) <- c(class(new.signal), LAGGED.DATA.CLASS.NAME)
  attr(new.signal, LAG.ORDER.NAME.ATTR) <- lags
  attr(new.signal, LAGGED.COLUMN.NAMES.ATTR) <- ans[["lagged.columns.names"]]
  if(!is.null(time.colname))
    attr(new.signal, TIME.COLUMN.NAME.ATTR) <- time.colname
  
  invisible(new.signal)
}





.test1 <- function(n = 20)
{
  stopifnot(n >= 1)
  stopifnot(n <= 100)
  
  x1 <- 1:n
  x2 <- 101:(100 + n)
  y  <- 201:(200 + n)
  
  a <- data.frame(ABP = x1, CO2 = x2, CBFV = y)
  lags <- list(ABP = 2, CO2 = 0, CBFV = 1)
  return(lagSignal(a, lags))
}

.test2 <- function(n = 20)
{
  stopifnot(n >= 1)
  stopifnot(n <= 100)
  
  t  <- seq(1/n, 1, length.out = n)
  x1 <- 1:n
  x2 <- 101:(100 + n)
  y  <- 201:(200 + n)
  
  a <- data.frame(TIME = t, ABP = x1, CO2 = x2, CBFV = y)
  lags <- list(ABP = 3, CO2 = 2, CBFV = 1)
  return(lagSignal(a, lags, "TIME"))
}

.test3 <- function(n = 20)
{
  stopifnot(n >= 1)
  stopifnot(n <= 100)
  
  t  <- seq(1/n, 1, length.out = n)
  x1 <- 1:n
  x2 <- 101:(100 + n)
  y  <- 201:(200 + n)
  
  a <- data.frame(TIME = t, ABP = x1, CO2 = x2, CBFV = y)
  i <- sample(x1)
  a <- a[i, ]
  
  lags <- list(ABP = 2, CO2 = 1, CBFV = 1)
  return(lagSignal(a, lags, "TIME"))
}

.test4 <- function(n = 20)
{
  stopifnot(n >= 1)
  stopifnot(n <= 100)
  
  t  <- seq(1/n, 1, length.out = n)
  x1 <- 1:n
  x2 <- 101:(100 + n)
  y  <- 201:(200 + n)
  
  a <- data.frame(TIME = t, ABP = x1, CO2 = x2, CBFV = y)
  i <- sample(x1)
  a <- a[i, ]
  
  aa <- separateSignalIntoFolds(signal = a,
                                n.folds = 3,
                                time.colname = "TIME")
  
  lags <- list(ABP = 2, CO2 = 1, CBFV = 1)
  return(lagSignalForCrossValidationWithinEachFold(aa, lags, "TIME"))
}

.test5 <- function(n = 20, n.folds = 3)
{
  stopifnot(n >= 1)
  stopifnot(n <= 100)
  
  t  <- seq(1/n, 1, length.out = n)
  x1 <- 1:n
  x2 <- 101:(100 + n)
  y  <- 201:(200 + n)
  
  a <- data.frame(TIME = t, ABP = x1, CO2 = x2, CBFV = y)
  i <- sample(x1)
  a <- a[i, ]
  
  aa <- separateSignalIntoFolds(signal = a,
                                n.folds = n.folds,
                                time.colname = "TIME")
  
  lags <- list(ABP = 2, CO2 = 1, CBFV = 1)
  return(lagSignalForCrossValidationAndRefold(aa, lags, "TIME"))
}

.test6 <- function(n = 20, n.folds = 4)
{
  stopifnot(n >= 1)
  stopifnot(n <= 100)
  
  t  <- seq(1/n, 1, length.out = n)
  x1 <- 1:n
  x2 <- 101:(100 + n)
  y  <- 201:(200 + n)
  
  a <- data.frame(TIME = t, ABP = x1, CO2 = x2, CBFV = y)
  i <- sample(x1)
  a <- a[i, ]
  
  lags <- list(ABP = 2, CO2 = 1, CBFV = 1)
  lagged <- lagSignal(a, lags, "TIME")
  
  folded <- separateSignalIntoFolds(signal = lagged, n.folds = n.folds)
  
  invisible(folded)
}
