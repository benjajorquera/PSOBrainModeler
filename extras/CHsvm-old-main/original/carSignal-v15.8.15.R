
require(ggplot2)
require(gridExtra)
require(signal)
require(pracma)



# This function creates an normalised ABP step stimulus.
# The function receives the following arguments:
# 
# sampling.time: sampling time for the ABP stimulus (in seconds).
#                Default value: 0.1 s
# 
# time.until.release: time included in the resulting signal until the
#                     time of cuff release (in seconds).
#                     Default value: 10 s
# 
# time.after.release: time included in the resulting signals after the
#                     time of cuff release.
#                     Default value: 20 s
#
# smooth.step.stimulus: logical, whether the ABP step stimulus should
#                       be smoothed (filtered).
#                       Default value: FALSE
# 
# filter.order: the order of the low-pass Butterworth filter used to
#               smooth the ABP step stimulus.
#               Default value: 2
# 
# cutoff.frequency: the cutoff frequency for the low-pass Butterworth
#                   filter used to smooth the ABP step stimulus (in Hz).
#                   Default value: 0.20 Hz
# 
# left.stabilisation.time: the time to be used for stabilisation of the
#                          step. This time is included in the step when
#                          a filter is applied, but removed after that.
#                          Default value: 30 s when a filter will be
#                          applied, 0 s otherwise.
#
# time.rounding.digits: the number of decimal when time instants are
#                       used.
#                       Default value: the number of decimals in the
#                       sampling time
#
# The function's answer is a list with the following:
# .$time.instants: the time instants in which the signal is sampled.
# .$ABP.normalised: the normalised ABP stimulus generated
# .$sampling.time: the sampling time used
# .$time.release: the time instant in which the thigh cuffs are
#                 supposedly released. Currently at the second 0.0.
#
get.normalised.ABP.stimulus <- function(
      sampling.time = 0.1, #10 hz
      time.until.release = 10,
      time.after.release = 20,
      smooth.step.stimulus = FALSE, # TRUE
      filter.order = 2,
      cutoff.frequency = 0.20,
      left.stabilisation.time = ifelse(smooth.step.stimulus, 30, 0),
      time.rounding.digits = format.info(sampling.time)[2],
      time.tol = sampling.time / 100
      )
{
  if(!is.divisible(time.until.release, sampling.time, time.tol))
    stop("time until release must be a multiple of the target sampling time")
  if(!is.divisible(time.after.release, sampling.time, time.tol))
    stop("time after release must be a multiple of the target sampling time")
	
	frequency <- 1 / sampling.time
  nsamples.stabilisation.left <-
    round(left.stabilisation.time / sampling.time)
  nsamples.until.release <-
    round(time.until.release / sampling.time) + 1
  nsamples.left <- nsamples.stabilisation.left + nsamples.until.release
  nsamples.after.release <- round(time.after.release / sampling.time)
  nsamples <- nsamples.until.release + nsamples.after.release
  
  # ABP step stimulus
  P <- c(rep(1, nsamples.left), rep(0, nsamples.after.release))
  
  # Smooths ABP step stimulus if corresponds
  if(smooth.step.stimulus)
  {
     wn <- cutoff.frequency / (frequency / 2)
     b <- butter(filter.order, wn)
     P <- as.numeric(filter(b, P))
  }
	
  if(nsamples.stabilisation.left > 0)
    P <- P[-(1:nsamples.stabilisation.left)]
  
  tini <- -time.until.release
  time <- seq(tini, length.out = nsamples, by = sampling.time)
	
  # Creates the answer
  ans <- list()
  ans[["time.instants"]] <- round(time, time.rounding.digits)
  ans[["ABP.normalised"]] <- P
  ans[["sampling.time"]] <- sampling.time
  ans[["time.release"]] <- ans[["time.instants"]][nsamples.until.release]
	
  invisible(ans)
}


normalise.signal <- function(
      signal,
      signal.baseline.value,
      signal.min.value
      )
{
  (signal - signal.min.value) / abs(signal.baseline.value - signal.min.value)
}


get.best.templates <- function(
      time.instants,
      signal,
      templates,
      referential.time.instant = 0,
      delta.time.before.ref = 0,
      delta.time.after.ref = 20 * 0.8,
      comparison.function = get.MSE,
      keep.details = TRUE,
      time.tol = min(diff(time.instants)) / 100,
      ... # Pass over to comparison.function()
      )
{
  print(as.list(match.call()))
  print(time.tol)
  stop()
  
  # Validates lengths
  lengths <- c(length(time.instants), length(signal))
  lengths <- c(lengths, sapply(templates, length))
  lengths <- unique(lengths)
  if(length(lengths) != 1)
    stop("time, signal and templates must have the same length")
  
  # Initialises detailed answer
  ans <- list()
  ans[["time.instants"]] <- time.instants
  ans[["signal"]] <- signal
  ans[["templates"]] <- templates
  ans[["referential.time.instant"]] <- referential.time.instant
  
  # Finds referential sample
  i <- which(are.tolerably.equal(
    time.instants,
    referential.time.instant,
    time.tol
  ))
  if(length(i) != 1)
    stop("a unique referential time instant could not be determined")
  ans[["referential.sample"]] <- i
  
  # Finds initial sample
  ans[["delta.time.before.ref"]] <- delta.time.before.ref
  ans[["initial.time.instant"]] <- referential.time.instant - delta.time.before.ref
  i <- which(are.tolerably.equal(
    time.instants,
    ans[["initial.time.instant"]],
    time.tol
  ))
  if(length(i) != 1)
    stop("initial time instant could not be found in specified time instants")
  ans[["initial.sample"]] <- i
  
  # Finds final sample
  ans[["delta.time.after.ref"]] <- delta.time.after.ref
  ans[["final.time.instant"]] <-
    referential.time.instant + delta.time.after.ref
  i <- which(are.tolerably.equal(
    time.instants,
    ans[["final.time.instant"]],
    time.tol
  ))
  if(length(i) != 1)
    stop("final time instant could not be found in specified time instants")
  ans[["final.sample"]] <- i
  
  # Sets relevant interval
  ans[["relevant.samples"]] <-
    ans[["initial.sample"]]:ans[["final.sample"]]
  
  # Gets relevant segments
  .tmp.fun <- function(s) s[ans[["relevant.samples"]]]
  ans[["relevant.signal.segment"]] <- .tmp.fun(ans[["signal"]])
  ans[["relevant.template.segments"]] <-
    lapply(ans[["templates"]], .tmp.fun)
  
  # Gets fit values
  .tmp.fun <- function(t) 
    comparison.function(ans[["relevant.signal.segment"]], t, ...)
  ans[["fit.values"]] <-
    sapply(ans[["relevant.template.segments"]], .tmp.fun)
  
  # Orders templates and determines the best one
  ans[["ranking"]] <- order(ans[["fit.values"]])
  
  # Deletes details if corresponds
  if(!keep.details)
  {
    i <- ans[["ranking"]][1]
    e <- ans[["fit.values"]][i]
    ans <- list(best.template.index = i, best.fit.value = e)
  }
  
  ans
}



