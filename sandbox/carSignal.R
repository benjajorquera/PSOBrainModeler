get.normalised.ABP.stimulus <- function(sampling.time = 0.1,
                                        time.until.release = 10,
                                        time.after.release = 20,
                                        smooth.step.stimulus = FALSE,
                                        filter.order = 2,
                                        cutoff.frequency = 0.20,
                                        left.stabilisation.time = ifelse(smooth.step.stimulus, 30, 0),
                                        time.rounding.digits = format.info(sampling.time)[2],
                                        time.tol = sampling.time / 100)
{
  if (!is.divisible(time.until.release, sampling.time, time.tol))
    stop("time until release must be a multiple of the target sampling time")
  if (!is.divisible(time.after.release, sampling.time, time.tol))
    stop("time after release must be a multiple of the target sampling time")
  
  frequency <- 1 / sampling.time
  nsamples.stabilisation.left <-
    round(left.stabilisation.time / sampling.time)
  nsamples.until.release <-
    round(time.until.release / sampling.time) + 1
  nsamples.left <-
    nsamples.stabilisation.left + nsamples.until.release
  nsamples.after.release <-
    round(time.after.release / sampling.time)
  nsamples <- nsamples.until.release + nsamples.after.release
  
  # ABP step stimulus
  P <- c(rep(1, nsamples.left), rep(0, nsamples.after.release))
  
  # Smooths ABP step stimulus if corresponds
  if (smooth.step.stimulus)
  {
    wn <- cutoff.frequency / (frequency / 2)
    b <- butter(filter.order, wn)
    P <- as.numeric(filter(b, P))
  }
  
  if (nsamples.stabilisation.left > 0)
    P <- P[-(1:nsamples.stabilisation.left)]
  
  tini <- -time.until.release
  time <- seq(tini, length.out = nsamples, by = sampling.time)
  
  # Creates the answer
  ans <- list()
  ans[["time.instants"]] <- round(time, time.rounding.digits)
  ans[["ABP.normalised"]] <- P
  ans[["sampling.time"]] <- sampling.time
  ans[["time.release"]] <-
    ans[["time.instants"]][nsamples.until.release]
  
  invisible(ans)
}

is.wholenumber <- function(x, tol, ...)
  UseMethod("is.wholenumber")

is.wholenumber.default <-
  function(x, tol = .Machine$double.eps ^ 0.5, ...)
  {
    abs(x - round(x)) < tol
  }

is.divisible <- function(x, y, tol, ...)
  UseMethod("is.divisible")

is.divisible.default <-
  function(x, y, tol = .Machine$double.eps ^ 0.5, ...)
  {
    is.wholenumber(x / y, tol, ...)
  }

are.tolerably.equal <-
  function(x, y, tol, ...)
    UseMethod("tolerably.equal")

are.tolerably.equal <-
  function(x, y, tol = .Machine$double.eps ^ 0.5, ...)
  {
    abs(x - y) < tol
  }

get.AT.templates.parameters <- function()
{
  K <- c(0.00, 0.20, 0.40, 0.60, 0.80, 0.90, 0.94, 0.96, 0.97, 0.98)
  D <- c(1.60, 1.60, 1.50, 1.15, 0.90, 0.75, 0.65, 0.55, 0.52, 0.50)
  T <- c(2.00, 2.00, 2.00, 2.00, 2.00, 1.90, 1.60, 1.20, 0.87, 0.65)
  ARI <- 0:9
  
  data.frame(T, D, K, ARI)
}

get.AT.decimal.templates.parameters <- function(rounding.digits = 6)
{
  orig <- get.AT.templates.parameters()
  
  ARI.decimal <- round(seq(0, 9, 0.1), 1)
  K.decimal <-
    pracma::interp1(orig[["ARI"]], orig[["K"]], ARI.decimal, 'spline')
  K.decimal <- round(K.decimal, rounding.digits)
  D.decimal <-
    pracma::interp1(orig[["ARI"]], orig[["D"]], ARI.decimal, 'spline')
  D.decimal <- round(D.decimal, rounding.digits)
  T.decimal <-
    pracma::interp1(orig[["ARI"]], orig[["T"]], ARI.decimal, 'spline')
  T.decimal <- round(T.decimal, rounding.digits)
  
  data.frame(T = T.decimal,
             D = D.decimal,
             K = K.decimal,
             ARI = ARI.decimal)
}


get.normalised.ari.templates <- function(time.instants,
                                         normalised.ABP,
                                         sampling.time,
                                         time.release,
                                         stabilisation.time = 30,
                                         at.param.rounding.digits = 6,
                                         time.tol = sampling.time / 100)
{
  at.params <-
    get.AT.decimal.templates.parameters(rounding.digits = at.param.rounding.digits)
  params.indices <- 1:nrow(at.params)
  
  .internal.tmp.fun <- function(i)
    get.theoretical.CBFV.response(
      T = at.params[i, 1],
      D = at.params[i, 2],
      K = at.params[i, 3],
      time.instants = time.instants,
      ABP.normalised = normalised.ABP,
      sampling.time = sampling.time,
      stabilisation.time = stabilisation.time
    )
  templates <- lapply(params.indices, .internal.tmp.fun)
  
  .internal.tmp.fun <-
    function(t)
      .sigexp.internal.normalise(t[["CBFV.theoretical.response"]])
  templates <- lapply(templates, .internal.tmp.fun)
  
  names(templates) <- sprintf("%.1f", at.params[[4]])
  
  templates
}


get.theoretical.CBFV.response <- function(T = 1.9,
                                          D = 0.75,
                                          K = 0.9,
                                          time.instants,
                                          ABP.normalised,
                                          sampling.time = min(round(diff(time.instants), 3)),
                                          stabilisation.time = 1)
{
  # Initialises the answer
  ans <- list()
  ans[["T"]] <- T
  ans[["D"]] <- D
  ans[["K"]] <- K
  ans[["time.instants"]] <- time.instants
  ans[["sampling.time"]] <- sampling.time
  ans[["ABP.normalised"]] <- ABP.normalised
  
  frequency <- 1 / ans[["sampling.time"]]
  nsamples <- length(ans[["time.instants"]])
  nsamples.stabilisation <-
    round(stabilisation.time / ans[["sampling.time"]])
  
  P <- c(rep(ans[["ABP.normalised"]][1], nsamples.stabilisation),
         ans[["ABP.normalised"]],
         rep(ans[["ABP.normalised"]][nsamples], nsamples.stabilisation))
  
  # Gets dP
  dP <- P - 1
  
  # Applies Tiecks' equations to obtain the CBFV signal
  X1 <- vector(mode = "numeric", length = length(P))
  X2 <- vector(mode = "numeric", length = length(P))
  CBFV <- vector(mode = "numeric", length = length(P))
  
  divisor <- frequency * ans[["T"]]
  X1[1] <- 0
  X2[1] <- 0
  CBFV[1] <- 1
  for (t in 2:length(P))
  {
    X1[t] <- X1[t - 1] + (dP[t] - X2[t - 1]) / divisor
    X2[t] <-
      X2[t - 1] + (X1[t] - 2 * ans[["D"]] * X2[t - 1]) / divisor
    CBFV[t] <- 1 + dP[t] - ans[["K"]] * X2[t]
  }
  
  if (nsamples.stabilisation > 0)
    CBFV <- CBFV[-(1:nsamples.stabilisation)]
  CBFV <- CBFV[1:nsamples]
  ans[["CBFV.theoretical.response"]] <- CBFV
  
  invisible(ans)
}

.sigexp.internal.normalise <- function(x)
{
  # Feature scaling
  nx <- (x - min(x)) / (max(x) - min(x))
  if (any(!is.finite(nx)))
    stop("signal contains NAs, NaNs or only zeroes or infinite values")
  nx
}

library(pracma)
library(signal)

x <-
  get.normalised.ABP.stimulus(sampling.time = 0.5, smooth.step.stimulus = TRUE)

y <-
  get.normalised.ari.templates(
    sampling.time = 0.5,
    time.instants = x$time.instants,
    normalised.ABP = x$ABP.normalised,
    time.release = 30
  )

plot(y[["0.0"]])
for (i in y) {
  lines(i, type = "l")
}

# Definir una función que calcule la máxima diferencia entre puntos consecutivos de un vector
max_diff <- function(vec) {
  diffs <-
    diff(vec)  # Calcular las diferencias entre puntos consecutivos
  return(max(abs(diffs), na.rm = TRUE))  # Devolver la máxima diferencia en valor absoluto
}

# Función para calcular la máxima diferencia en una lista de vectores
max_diff_list <- function(vectors_list) {
  # Calcular la máxima diferencia para cada vector en la lista
  max_diffs <- sapply(vectors_list, max_diff)
  # Devolver la máxima diferencia de todas las diferencias calculadas
  return(max(max_diffs, na.rm = TRUE))
}

max_diff_result <- max_diff_list(y)
