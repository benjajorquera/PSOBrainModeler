TOP.DIR <- file.path("", "research")
ARI.DIR <- file.path(TOP.DIR, "ARI-R")

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
    Signal = abp.step[["ABP.normalised"]]
  )
  
  templates <- get.ari.templates(
    time.instants = abp.step[["time.instants"]],
    normalised.ABP = abp.step[["ABP.normalised"]],
    sampling.time = abp.step[["sampling.time"]],
    time.release = abp.step[["time.release"]]
  )
  
  .internal.tmp.fun <- function(s, n)
    data.frame(
      Time = abp.step[["time.instants"]],
      Signal = s,
      ARI = as.numeric(n)
    )
  cbf.data.frames <- mapply(
    .internal.tmp.fun,
    templates,
    names(templates),
    SIMPLIFY = FALSE
  )
  cbf.data.frame <- do.call(rbind, cbf.data.frames)
  
  p <- get.mvre.plot(abp.data.frame, cbf.data.frame)
  
  tgt.plot.basename <- paste(, "es", sep = "-")
  tgt.stats.basename <- paste(tgt.stats.basename, "csv", sep = ".")
  tgt.stats.file.es <- file.path(tgt.stats.dir, tgt.stats.basename)
  
  ggsave(
    filename = tgt.plot.file,
    plot = p,
    width = 11,
    height = 8,
    units = "in"
  )
  
  
  
  
  
  {
    score <- eval.abp.step.response(
      mvre = mvre,
      sampling.time = sampling.time,
      time.var.name = time.var.name,
      abp.var.name = abp.var.name,
      cbf.var.name = cbf.var.name,
      time.release = mvre.time.release
    )
    score <- round(score, test.cor.rounding.digits)
    cat("score: ", score, "\n")
    
    if(score != best.result[["step.score"]])
      stop("could not rebuild the model - different step response score")
    
    mfari <- get.mfari(
      mvre = mvre,
      time.var.name = time.var.name,
      abp.var.name = abp.var.name,
      cbf.var.name = cbf.var.name,
      sampling.time = sampling.time,
      time.release = mvre.time.release,
      time.tol = time.tol
      )
    cat("mfARI: ", mfari[["mfARI"]], "\n")
           
    # Adds summary table
    t <- NULL
    l <- sprintf("%s lags", abp.var.name)
    s <- sprintf("%d", lags[[abp.var.name]])
    t <- rbind(t, data.frame(Result = l, Value = s))
    l <- sprintf("%s lags", co2.var.name)
    s <- sprintf("%d", lags[[co2.var.name]])
    t <- rbind(t, data.frame(Result = l, Value = s))
    .internal.tmp.fun <- function(p) {
      if(p == "nu")
        s <- format(best.result[[p]])
      else
        s <- sprintf("2^%s", format(log(best.result[[p]], 2)))
      data.frame(Result = p, Value = s)
    }
    d <- do.call(rbind, lapply(svm.param.names, .internal.tmp.fun))
    t <- rbind(t, d)
    s <- sprintf("%.*f", test.cor.rounding.digits, svm.model.cors[["train.cor"]])
    t <- rbind(t, data.frame(Result = "Training correlaton", Value = s))
    s <- sprintf("%.*f", test.cor.rounding.digits, svm.model.cors[["test.cor"]])
    t <- rbind(t, data.frame(Result = "Test correlaton", Value = s))
    s <- sprintf("%.3f", score)
    t <- rbind(t, data.frame(Result = "Response evaluation", Value = s))
    s <- sprintf("%.1f", mfari[["mfARI"]])
    t <- rbind(t, data.frame(Result = "mfARI", Value = s))
    s <- sprintf("%.1f", dari)
    t <- rbind(t, data.frame(Result = "Decimal ARI", Value = s))
    gt <- tableGrob(t, show.rownames = FALSE, #show.colnames = FALSE,
                    gpar.coretext = gpar(cex = 0.5, col = ann.palette[8]),
                    gpar.coltext = gpar(cex = 0.6, col = ann.palette[8], fontface = "bold"),
                    gpar.corefill = gpar(fill = "grey90", col = ann.palette[5]),
                    gpar.colfill = gpar(fill = "grey90", col = ann.palette[5]),
                    padding.v = unit(1.5, "mm"))
    p <- p + annotation_custom(gt, xmin = -mvre.time.until.release,
                                   xmax = mvre.time.release,
                                   ymin = -0.2, ymax = 0.7)
    
    p <- p + ylim(-0.2, 1.2)
    p <- p + theme(
      legend.justification = c(1, 1),
      legend.background = element_rect(fill = alpha('grey90', 0.0)),
      legend.position = c(1, 1),
      legend.key.size =  unit(0.4, "cm"),
      legend.title = element_blank(),
      legend.text = element_text(size = 6),
      legend.margin = unit(0, "cm")
    )
    
    p <- p + ggtitle(cbf.var.name) + xlab("Time") + ylab("Signals")
    
    best.result[["gamma"]] <- log(best.result[["gamma"]], 2)
    best.result[["cost"]] <- log(best.result[["cost"]], 2)
    best.result[["train.time"]] <- as.numeric(round(best.result[["train.time"]], 4))
    best.result[["perf"]] <- round(best.result[["perf"]], 3)
    best.result[["step.score"]] <- round(best.result[["step.score"]], 3)
    
    side.stats <- cbind(
      Instance = src.instance.basename,
      Signal = cbf.var.name,
      best.result,
      dARI = round(dari, 1),
      mfARI = round(mfari[["mfARI"]], 1)
    )
    
    plots <- c(plots, list(p))
    stats <- rbind(stats, side.stats)
  }
  
  main <- paste("\n", "Subject", src.instance.basename)
  params <- c(plots, list(nrow = 2, main = main))
  p <- do.call(arrangeGrob, params)
  
  list(stats = stats, plot = p)
}


get.mvre.plot <- function(abp, cbf)
{
  abp.colour = brewer.pal(n = 9, name = "Reds")[5]
  cbf.palette = brewer.pal(n = 9, name = "Blues")
  ann.palette = brewer.pal(n = 9, name = "Greens")
  
  p <- ggplot(cbf, aes(x = Time, y = Signal, colour = ARI, group = ARI))
  p <- p + geom_line()
  p <- p + geom_line(
    data = abp,
    aes(x = Time, y = Signal, colour = 0, group = 0),
    colour = abp.colour
  )
  print(p)
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


eval.abp.step.response <- function(
      mvre,
      sampling.time,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      time.release,
      min.abp.max.delta.time = 5,  # From release time
      min.cbf.max.delta.time = 3,  # From min ABP
      cbf.drop.max.delta.time = sampling.time, # From min CBF
      cbf.drop.reference = 0.4,
      cbf.lower.bound = -0.2,
      cbf.upper.bound = 1,
      steady.cbf.delta.time = 5,   # From the end of the signal
      steady.cbf.max.variance = 0.002,
      time.tol = sampling.time / 100,
      num.tol = 1 / 10000
      )
{
  mvre <- mvre[, c(time.var.name, abp.var.name, cbf.var.name)]
  
  upto.release <- mvre[[time.var.name]] < time.release + time.tol
  samples.upto.release <- which(upto.release)
  after.release <- mvre[[time.var.name]] > time.release + time.tol
  samples.after.release <- which(after.release)
  
  min.abp <- diff(diff(mvre[[abp.var.name]]) > num.tol) > 0
  if(sum(min.abp) != 1)
    stop("ABP step does not have a unique global minimum")
  sample.min.abp <- which(min.abp) + 1
  time.min.abp <- mvre[[time.var.name]][sample.min.abp]
  
  if(sample.min.abp < samples.after.release[1])
    stop("ABP step has a global minimum before release time")
  
  if(time.min.abp > time.release + min.abp.max.delta.time)
    stop("ABP step has a global minimum after the specified maximum delta time")
  
  # There is one (and only one) local minimum of CBF around
  # the time of min ABP
  i <- mvre[[time.var.name]] > time.min.abp - min.cbf.max.delta.time - time.tol
  j <- mvre[[time.var.name]] < time.min.abp + min.cbf.max.delta.time + time.tol
  samples.min.cbf <- which(i & j)
  min.cbf <- diff(diff(mvre[[cbf.var.name]][samples.min.cbf]) > num.tol) > num.tol
  if(sum(min.cbf) != 1)
    # CBF does not have a unique local minimum in the specified range
    return(0)
  sample.min.cbf <- samples.min.cbf[which(min.cbf)[1] + 1]
  time.min.cbf <- mvre[[time.var.name]][sample.min.cbf]
  
  # The mean CBF signal around its local minimum exhibits a drop 
  cbf.baseline.value <- mean(mvre[[cbf.var.name]][samples.upto.release])
  i <- mvre[[time.var.name]] > time.min.cbf - cbf.drop.max.delta.time - time.tol
  j <- mvre[[time.var.name]] < time.min.cbf + cbf.drop.max.delta.time + time.tol
  samples.cbf.drop <- which(i & j)
  mean.cbf.drop <- mean(mvre[[cbf.var.name]][samples.cbf.drop])
  drop.score <- max(0, mean.cbf.drop - cbf.drop.reference)
  drop.score <- 1 - (min(drop.score, cbf.baseline.value) / cbf.baseline.value)
  
  # The end of the signal is stable
  last.time <- tail(mvre[[time.var.name]], 1)
  i <- mvre[[time.var.name]] > last.time - steady.cbf.delta.time - time.tol
  samples.cbf.steady <- which(i)
  cbf.steady.var <- var(mvre[[cbf.var.name]][samples.cbf.steady])
  steady.score <- 1 / max(1, min(10, cbf.steady.var / steady.cbf.max.variance))
  
  # The signal is lower bounded
  # Assumes bound is negative and > -1
  cbf.minimum <- min(mvre[[cbf.var.name]])
  bound <- 2 * cbf.lower.bound
  lower.bound.score <- 1 - max(bound, min(0, cbf.minimum - cbf.lower.bound)) / bound
  
  # The signal is upper bounded
  cbf.maximum <- max(mvre[[cbf.var.name]])
  bound <- 1.5 * cbf.upper.bound # assumes positive, >= 1
  upper.bound.score <- 1 - min(bound, max(0, cbf.maximum - cbf.upper.bound)) / bound
  
  # The signal does not oscillates much
  # At this point, there is at least one local minimum
  local.mins <- diff(diff(mvre[[cbf.var.name]]) >= num.tol) > 0
  local.maxs <- diff(diff(mvre[[cbf.var.name]]) >= num.tol) < 0
  oscillation.score <- 1 / max(1, sum(local.mins) + sum(local.maxs) - 3)

  min(
    drop.score,
    steady.score,
    lower.bound.score,
    upper.bound.score,
    oscillation.score
  )
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

