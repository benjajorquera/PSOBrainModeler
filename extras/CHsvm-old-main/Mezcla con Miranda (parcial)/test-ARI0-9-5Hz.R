require(pracma)

TOP.DIR <- file.path("", "research")
ARI.DIR <- file.path(TOP.DIR, "ARI-R")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")
TEST.DIR <- file.path(SVR.DIR, "Tests")

# [side effect: atARI, carSignal, Metric-utils, Rounding-utils]
MFARI.SCRIPT.BASENAME <- paste("mfARIdev", sep = "-")
MFARI.SCRIPT.BASENAME <- paste(MFARI.SCRIPT.BASENAME, "R", sep = ".")
MFARI.SCRIPT.NAME <- file.path(ARI.DIR, MFARI.SCRIPT.BASENAME)
source(MFARI.SCRIPT.NAME)


test <- function()
{
  abp.step <- get.abp.step()
  abp.data.frame <- data.frame(
    Time = abp.step[["time.instants"]],
    ABP = abp.step[["ABP.normalised"]]
  )
  
  templates <- get.ari.templates(
    time.instants = abp.step[["time.instants"]],
    normalised.ABP = abp.step[["ABP.normalised"]],
    sampling.time = abp.step[["sampling.time"]],
    time.release = abp.step[["time.release"]]
  )
  
  cbf.data.frame <- as.data.frame(do.call(cbind, templates))
  rownames(cbf.data.frame) <- NULL
  col.names <- paste("CBFV", colnames(cbf.data.frame), sep = "-")
  colnames(cbf.data.frame) <- col.names
  
  data.wide <- cbind(abp.data.frame, cbf.data.frame)
  
  #p1 <- get.response.plot(data.wide)
  
  time.ini <- -10
  time.fin <- 20
  time.tol <- 0.2 / 100
  i <- data.wide[["Time"]] > time.ini - time.tol
  j <- data.wide[["Time"]] < time.fin + time.tol
  responses <- data.wide[i & j, ]
  #p2 <- get.response.plot(responses)
  
  tgt.plot.basename <- paste("test", "ARI0", "9", "5Hz",sep = "-")
  tgt.plot.basename <- paste(tgt.plot.basename, "pdf", sep = ".")
  tgt.plot.file <- file.path(TEST.DIR, tgt.plot.basename)
  
  #p <- arrangeGrob(p1, p2, nrow = 2, main = "Test 5Hz")
  #ggsave(
  #  filename = tgt.plot.file,
  #  plot = p,
  #  width = 11,
  #  height = 8,
  #  units = "in"
  #)
  
  for(i in 3:ncol(responses))
  {
    response <- responses[, c(1, 2, i)]
    cat("\n--- ", colnames(response[3]), " ---\n")
#     show.response.info(
#       response = response,
#       time.var.name = "Time",
#       abp.var.name = "ABP",
#       cbf.var.name = colnames(response[3])
#     )
    error <- eval.step.response(
      response = response,
      time.var.name = "Time",
      abp.var.name = "ABP",
      cbf.var.name = colnames(response[3]),
      time.release = abp.step[["time.release"]]
    )
    cat("evaluation: ", error, "\n")
  }
  
  responses
}


get.response.plot <- function(
      data.wide,
      time.col.name = "Time",
      abp.col.name = "ABP",
      sep = "-"
      )
{
  abp.colour = brewer.pal(n = 9, name = "Reds")[5]
  cbf.palette = brewer.pal(n = 9, name = "Blues")
  ann.palette = brewer.pal(n = 9, name = "Greens")
  
  col.names <- colnames(data.wide)
  i <- col.names %in% c(time.col.name, abp.col.name)
  varying <- col.names[!i]
  
  cbf <- reshape(
    data = data.wide,
    drop = abp.col.name,
    idvar = time.col.name,
    varying = varying,
    timevar = "ARI",
    sep = sep,
    direction = "long"
  )
  abp <- data.wide[, col.names[i]]
  
  p <- ggplot(cbf, aes(x = Time, y = CBFV, colour = ARI, group = ARI))
  p <- p + geom_line()
  p <- p + geom_line(
    data = abp,
    aes(x = Time, y = ABP, colour = 0, group = 0),
    colour = abp.colour
  )
  
  p
}


get.ari.templates <- function(
      time.instants,
      normalised.ABP,
      sampling.time,
      time.release,
      baseline.duration = 5,
      stabilisation.time = 0,
      at.param.rounding.digits = 6,
      cbf.rounding.digits = 3,
      time.tol = sampling.time / 100
      )
{
  at.params <- get.AT.decimal.templates.parameters(
    rounding.digits = at.param.rounding.digits
  )
  i <- seq(1, 91, 10)
  at.params <- at.params[i, ]
  nparams <- nrow(at.params)
  
  .internal.tmp.fun <- function(i) get.theoretical.CBFV.response(
    T = at.params[i, 1],
    D = at.params[i, 2],
    K = at.params[i, 3],
    time.instants = time.instants,
    ABP.normalised = normalised.ABP,
    sampling.time = sampling.time,
    stabilisation.time = stabilisation.time
  )
  templates <- lapply(1:nparams, .internal.tmp.fun)
  
  # round and shift
  .internal.tmp.fun <- function(t)
    t[["CBFV.theoretical.response"]] <- round(
      t[["CBFV.theoretical.response"]],
      cbf.rounding.digits
    ) - 0.2
  
  templates <- lapply(templates, .internal.tmp.fun)
  names(templates) <- sprintf("%.1f", at.params[[4]])
  
  templates
}


get.abp.step <- function(
      sampling.time = 0.2,
      time.until.release = 40,
      time.after.release = 20,
      smooth.step.stimulus = TRUE,
      filter.order = 2,
      cutoff.frequency = 0.20,
      left.stabilisation.time = 0,
      abp.rounding.digits = 3,
      time.rounding.digits =  1
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


# Después de revisión Max 6-5-15

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


# Después de revisión Max 5-5-15

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
  min.cbf.info <- cbind(min.cbf.info, matrix(heights, ncol = 1))
  percents <- heights / heights[1] * 100
  min.cbf.info <- cbind(min.cbf.info, matrix(percents, ncol = 1))
  i <- percents > peak.min.relative.height
  min.cbf.info <- min.cbf.info[i, ]
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
    max.cbf.info <- cbind(max.cbf.info, matrix(max.heights, ncol = 1))
    percents <- max.heights / heights[1] * 100
    max.cbf.info <- cbind(max.cbf.info, matrix(percents, ncol = 1))
    i <- percents > peak.min.relative.height
    max.cbf.info <- max.cbf.info[i, ]
    cat("cbf maxima:\n")
    print(max.cbf.info)
    n.maxima <- nrow(max.cbf.info)
  }
  cat("number of cbf maxima found:", n.maxima, "\n", sep = "")
  cat("\n")
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
  
  # Discard if signal does not stabilises
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
  cat("minima found: ", nrow(min.abp.info), "\n")
  cat("min.abp.sample: ", min.abp.info[1, 2], "\n")
  min.abp.time <- response[[time.var.name]][min.abp.info[1, 2]]
  cat("min.abp.time: ", min.abp.time, "\n")
  cat("min.abp.value: ", -min.abp.info[1, 1], "\n")
  
  min.cbf.info <- findpeaks(
    -response[[cbf.var.name]],
    zero = "-",
    threshold = min.cbf.threshold
  )
  cat("minima found: ", nrow(min.cbf.info), "\n")
  cat("1st min.cbf.sample: ", min.cbf.info[1, 2], "\n")
  cat("1st min.cbf.time: ", response[[time.var.name]][min.cbf.info[1, 2]], "\n")
  cat("1st min.cbf.delta.time: ", abs(min.abp.time - response[[time.var.name]][min.cbf.info[1, 2]]), "\n")
  cat("1st min.cbf.value: ", -min.cbf.info[1, 1], "\n")
  cat("vecinity: ", response[[cbf.var.name]][(min.cbf.info[1, 2]-2):(min.cbf.info[1, 2]+2)], "\n")
  cat("vecinity diff: ", abs(diff(response[[cbf.var.name]][(min.cbf.info[1, 2]-2):(min.cbf.info[1, 2]+2)])), "\n")
  cat("max vecinity diff: ", max(abs(diff(response[[cbf.var.name]][(min.cbf.info[1, 2]-2):(min.cbf.info[1, 2]+2)]))), "\n")
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

eval.step.response.old.old.old <- function(
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
