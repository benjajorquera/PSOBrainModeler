
script.dir <- dirname(sys.frame(1)$ofile)

CARSIG.SCRIPT.BASENAME <- paste("carSignal", "v15.8.15", sep = "-")
CARSIG.SCRIPT.BASENAME <- paste(CARSIG.SCRIPT.BASENAME, "R", sep = ".")
CARSIG.SCRIPT.NAME <- file.path(script.dir, CARSIG.SCRIPT.BASENAME)
source(CARSIG.SCRIPT.NAME)

LAGMGR.SCRIPT.BASENAME <- paste("Lag", "manager", "v15.5.6", sep = "-")
LAGMGR.SCRIPT.BASENAME <- paste(LAGMGR.SCRIPT.BASENAME, "R", sep = ".")
LAGMGR.SCRIPT.NAME <- file.path(script.dir, LAGMGR.SCRIPT.BASENAME)
source(LAGMGR.SCRIPT.NAME)

RNDUTL.SCRIPT.BASENAME <- paste("Rounding", "utils", "v15.8.15", sep = "-")
RNDUTL.SCRIPT.BASENAME <- paste(RNDUTL.SCRIPT.BASENAME, "R", sep = ".")
RNDUTL.SCRIPT.NAME <- file.path(script.dir, RNDUTL.SCRIPT.BASENAME)
source(RNDUTL.SCRIPT.NAME)

MTRUTL.SCRIPT.BASENAME <- paste("Metric", "utils", "v15.8.15", sep = "-")
MTRUTL.SCRIPT.BASENAME <- paste(MTRUTL.SCRIPT.BASENAME, "R", sep = ".")
MTRUTL.SCRIPT.NAME <- file.path(script.dir, MTRUTL.SCRIPT.BASENAME)
source(MTRUTL.SCRIPT.NAME)

SIGCVL.SCRIPT.BASENAME <- paste("Signal", "cross", "validation", "v15.5.6", sep = "-")
SIGCVL.SCRIPT.BASENAME <- paste(SIGCVL.SCRIPT.BASENAME, "R", sep = ".")
SIGCVL.SCRIPT.NAME <- file.path(script.dir, SIGCVL.SCRIPT.BASENAME)
source(SIGCVL.SCRIPT.NAME)

SIGRID.SCRIPT.BASENAME <- paste("Signal", "grid", "v15.8.25", sep = "-")
SIGRID.SCRIPT.BASENAME <- paste(SIGRID.SCRIPT.BASENAME, "R", sep = ".")
SIGRID.SCRIPT.NAME <- file.path(script.dir, SIGRID.SCRIPT.BASENAME)
source(SIGRID.SCRIPT.NAME)

SIGEXP.SCRIPT.BASENAME <- paste("Signal", "experiment", "v15.8.25", sep = "-")
SIGEXP.SCRIPT.BASENAME <- paste(SIGEXP.SCRIPT.BASENAME, "R", sep = ".")
SIGEXP.SCRIPT.NAME <- file.path(script.dir, SIGEXP.SCRIPT.BASENAME)
source(SIGEXP.SCRIPT.NAME)

SIGLAG.SCRIPT.BASENAME <- paste("Signal", "lagging", "v15.5.6", sep = "-")
SIGLAG.SCRIPT.BASENAME <- paste(SIGLAG.SCRIPT.BASENAME, "R", sep = ".")
SIGLAG.SCRIPT.NAME <- file.path(script.dir, SIGLAG.SCRIPT.BASENAME)
source(SIGLAG.SCRIPT.NAME)

AT.ARI.SCRIPT.BASENAME <- paste("atARI", "v15.8.15", sep = "-")
AT.ARI.SCRIPT.BASENAME <- paste(AT.ARI.SCRIPT.BASENAME, "R", sep = ".")
AT.ARI.SCRIPT.NAME <- file.path(script.dir, AT.ARI.SCRIPT.BASENAME)
source(AT.ARI.SCRIPT.NAME)

MFARI.SCRIPT.BASENAME <- paste("mfARI", "v15.8.15", sep = "-")
MFARI.SCRIPT.BASENAME <- paste(MFARI.SCRIPT.BASENAME, "R", sep = ".")
MFARI.SCRIPT.NAME <- file.path(script.dir, MFARI.SCRIPT.BASENAME)
source(MFARI.SCRIPT.NAME)


run <- function(
      sampling.time = 0.5,
      src.instance.dir = file.path(script.dir, "Data"),
      src.instance.ext = "txt",
      src.result.dir = file.path(script.dir, "Results"),
      src.result.ext = "RData",
      src.basenames = paste0("Sujeto", 1:3),
      tgt.plot.dir = file.path(script.dir, "Plots"),
      time.var.name = "Time",
      abp.var.name = "MABP",
      co2.var.name = "etCO2",
      cbf.var.names = c("CBFVL", "CBFVR"),
      abp.step.time.until.release = 40,
      abp.step.time.after.release =  30,
      abp.step.time.release = 0,
      co2.step.time.until.release = 60,
      co2.step.time.after.release = 100,
      co2.step.time.release = 0,
      mvre.time.until.baseline = 5,
      mvre.baseline.duration = 5,
      mvre.time.after.release = 20,
      mvre.time.release = 0,
      co2.reactivity.time.until.release = 20,
      co2.reactivity.time.after.release = 80,
      co2.reactivity.time.release = 0,
      co2.reactivity.initial.baseline.duration = 20,
      co2.reactivity.final.baseline.duration = 20,
      time.rounding.digits = 1,
      abp.rounding.digits = 4,
      co2.rounding.digits = 3,
      cbf.rounding.digits = 4,
      time.tol = sampling.time / 100
      )
{
  dir.create(tgt.plot.dir, showWarnings = FALSE, recursive = TRUE)
  
  abp.step <- .sigexp.internal.get.abp.step(
    sampling.time = sampling.time,
    time.var.name = time.var.name,
    abp.var.name = abp.var.name,
    co2.var.name = co2.var.name,
    norm.co2.signal = 1,
    abp.step.time.until.release = abp.step.time.until.release,
    abp.step.time.after.release = abp.step.time.after.release,
    time.rounding.digits = time.rounding.digits,
    abp.rounding.digits = abp.rounding.digits,
    co2.rounding.digits = co2.rounding.digits
  )
  
  mvre.time.until.release <-
    mvre.time.until.baseline + mvre.baseline.duration
  time.ini <- mvre.time.release - mvre.time.until.release - time.tol
  time.fin <- mvre.time.release + mvre.time.after.release + time.tol
  i <- abp.step[[time.var.name]] > time.ini
  j <- abp.step[[time.var.name]] < time.fin
  samples <- which(i & j)
  abp.step <- abp.step[samples, ]
  
  normalised.ari.templates <- get.normalised.ari.templates(
    time.instants = abp.step[[time.var.name]],
    normalised.ABP = abp.step[[abp.var.name]],
    sampling.time = sampling.time,
    time.release = mvre.time.release,
    time.tol = time.tol
  )
  
  for(src.basename in src.basenames)
  {
    cat("\n--- Instance: ", src.basename, " ---\n\n")
    
    plot.instance(
      src.instance.dir = src.instance.dir,
      src.instance.ext = src.instance.ext,
      src.result.dir = src.result.dir,
      src.result.ext = src.result.ext,
      src.basename = src.basename,
      tgt.plot.dir = tgt.plot.dir,
      sampling.time = sampling.time,
      time.var.name = time.var.name,
      abp.var.name = abp.var.name,
      co2.var.name = co2.var.name,
      cbf.var.names = cbf.var.names,
      abp.step.time.until.release = abp.step.time.until.release,
      abp.step.time.after.release =  abp.step.time.after.release,
      abp.step.time.release = abp.step.time.release,
      co2.step.time.until.release = co2.step.time.until.release,
      co2.step.time.after.release = co2.step.time.after.release,
      co2.step.time.release = co2.step.time.release,
      mvre.time.until.baseline = mvre.time.until.baseline,
      mvre.baseline.duration = mvre.baseline.duration,
      mvre.time.after.release = mvre.time.after.release,
      mvre.time.release = mvre.time.release,
      co2.reactivity.time.until.release = co2.reactivity.time.until.release,
      co2.reactivity.time.after.release = co2.reactivity.time.after.release,
      co2.reactivity.time.release = co2.reactivity.time.release,
      co2.reactivity.initial.baseline.duration = co2.reactivity.initial.baseline.duration,
      co2.reactivity.final.baseline.duration = co2.reactivity.final.baseline.duration,
      normalised.ari.templates = normalised.ari.templates,
      time.rounding.digits = time.rounding.digits,
      abp.rounding.digits = abp.rounding.digits,
      co2.rounding.digits = co2.rounding.digits,
      cbf.rounding.digits = cbf.rounding.digits,
      time.tol = time.tol
    )
  }
}


get.response.stats <- function(
      responses,
      cbf.var.names,
      cbf.rounding.digits
      )
{
  n <- nchar(cbf.var.names)
  side.codes <- substr(cbf.var.names, n, n)
  stats <- NULL
  for(side.code in side.codes)
  {
    pattern <- paste0(side.code, "$")
    i <- grep(pattern, colnames(responses))
    side.mean <- apply(responses[, i], 1, mean)
    side.mean <- round(side.mean, cbf.rounding.digits)
    side.sd <- apply(responses[, i], 1, sd)
    side.sd <- round(side.sd, cbf.rounding.digits + 1)
    substats <- data.frame(side.mean, side.sd)
    colnames(substats) <- paste0(c("mean", "sd"), side.code)
    
    if(is.null(stats))
      stats <- substats
    else
      stats <- cbind(stats, substats)
  }
  
  all.side.mean <- apply(responses, 1, mean)
  all.side.mean <- round(all.side.mean, cbf.rounding.digits)
  all.side.sd <- apply(responses, 1, sd)
  all.side.sd <- round(all.side.sd, cbf.rounding.digits + 1)
  substats <- data.frame(all.side.mean, all.side.sd)
  colnames(substats) <- c("mean", "sd")
  
  if(is.null(stats))
    stats <- substats
  else
    stats <- cbind(stats, substats)
  
  stats
}


get.stats <- function(
      model.stats,
      cbf.var.names,
      ins.col.name = "Instance",
      cbf.col.name = "Signal",
      col.names = c("train.cor", "test.cor", "dARI", "mfARI", "co2.react")
      )
{
  n <- nchar(cbf.var.names)
  side.codes <- substr(cbf.var.names, n, n)
  new.col.names <- gsub(".", "-", col.names, fixed = TRUE)
  new.col.names <- lapply(side.codes, paste, new.col.names, sep = "-")
  
  stats <- NULL
  instances <- unique(model.stats[[ins.col.name]])
  for(instance in instances)
  {
    i <- model.stats[[ins.col.name]] == instance
    ins.stats <- model.stats[i, ]
    ins.row <- NULL
    for(i in 1:length(cbf.var.names))
    {
      j <- ins.stats[[cbf.col.name]] == cbf.var.names[i]
      substats <- ins.stats[j, col.names]
      colnames(substats) <- new.col.names[[i]]
      if(is.null(ins.row))
        ins.row <- substats
      else
        ins.row <- cbind(ins.row, substats)
    }
    ins.row <- cbind(Instance = instance, ins.row)
    if(is.null(stats))
      stats <- ins.row
    else
      stats <- rbind(stats, ins.row)
  }
  rownames(stats) <- NULL
  
  stats
}


plot.instance <- function(
      src.instance.dir,
      src.instance.ext,
      src.result.dir,
      src.result.ext,
      src.basename,
      tgt.plot.dir,
      sampling.time,
      time.var.name,
      abp.var.name,
      co2.var.name,
      cbf.var.names,
      abp.step.time.until.release,
      abp.step.time.after.release,
      abp.step.time.release,
      co2.step.time.until.release,
      co2.step.time.after.release,
      co2.step.time.release,
      nfolds = 2,
      fold.colname = "Fold",
      svm.type = "nu-regression",
      svm.kernel = "radial",
      svm.param.names = c("gamma", "cost",  "nu"),
      mvre.time.until.baseline,
      mvre.baseline.duration,
      mvre.time.after.release,
      mvre.time.release,
      co2.reactivity.time.until.release,
      co2.reactivity.time.after.release,
      co2.reactivity.time.release,
      co2.reactivity.initial.baseline.duration,
      co2.reactivity.final.baseline.duration,
      normalised.ari.templates,
      time.rounding.digits,
      abp.rounding.digits,
      co2.rounding.digits,
      cbf.rounding.digits,
      train.cor.rounding.digits = 3,
      test.cor.rounding.digits = 3,
      co2.reactivity.rounding.digits = 2,
      time.tol = sampling.time / 100,
      cor.tol = 0.05,
      abp.palette = brewer.pal(n = 9, name = "Reds"),
      cbf.palette = brewer.pal(n = 9, name = "Blues"),
      co2.palette = brewer.pal(n = 9, name = "PuRd"),
      ann.palette = brewer.pal(n = 9, name = "Greens")
      )
{
  # Gests instance
  instance <- get.instance(
    src.dir = src.instance.dir,
    src.basename = src.basename,
    src.ext = src.instance.ext
  )
  
  instance <- instance[, c(time.var.name, abp.var.name, co2.var.name, cbf.var.names)]
  instance[[time.var.name]] <- round(instance[[time.var.name]], time.rounding.digits)
  instance[[abp.var.name]] <- .sigexp.internal.normalise(instance[[abp.var.name]])
  instance[[abp.var.name]] <- round(instance[[abp.var.name]], abp.rounding.digits)
  instance[[co2.var.name]] <- .sigexp.internal.normalise(instance[[co2.var.name]])
  instance[[co2.var.name]] <- round(instance[[co2.var.name]], co2.rounding.digits)
  for(cbf.var.name in cbf.var.names)
  {
    instance[[cbf.var.name]] <- .sigexp.internal.normalise(instance[[cbf.var.name]])
    instance[[cbf.var.name]] <- round(instance[[cbf.var.name]], cbf.rounding.digits)
  }
  
  abp.step <- .sigexp.internal.get.abp.step(
    sampling.time = sampling.time,
    time.var.name = time.var.name,
    abp.var.name = abp.var.name,
    co2.var.name = co2.var.name,
    norm.co2.signal = instance[[co2.var.name]],
    abp.step.time.until.release = abp.step.time.until.release,
    abp.step.time.after.release = abp.step.time.after.release,
    time.rounding.digits = time.rounding.digits,
    abp.rounding.digits = abp.rounding.digits,
    co2.rounding.digits = co2.rounding.digits
  )
  abp.step[[time.var.name]] <- abp.step[[time.var.name]] + abp.step.time.release
  
  co2.step <- get.co2.step(
    sampling.time = sampling.time,
    time.var.name = time.var.name,
    abp.var.name = abp.var.name,
    co2.var.name = co2.var.name,
    norm.abp.signal = instance[[abp.var.name]],
    co2.step.time.until.release = co2.step.time.until.release,
    co2.step.time.after.release = co2.step.time.after.release,
    time.rounding.digits = time.rounding.digits,
    abp.rounding.digits = abp.rounding.digits,
    co2.rounding.digits = co2.rounding.digits
  )
  co2.step[[time.var.name]] <- co2.step[[time.var.name]] + co2.step.time.release
  
  for(cbf.var.name in cbf.var.names)
  {
    cat("\n--- output: ", cbf.var.name, " ---\n\n")
    side.basename <- paste(src.basename, cbf.var.name, sep = "-")
    
    signal <- instance[, c(time.var.name, abp.var.name, co2.var.name, cbf.var.name)]
    
    side.result.basename <- paste(side.basename, src.result.ext, sep = ".")
    side.result.file <- file.path(src.result.dir, side.result.basename)
    side.result <- readRDS(file = side.result.file)
    side.result <- side.result[["stats"]]
    
    plots <- NULL
    for(m in 1:nrow(side.result))
    {
      result <- side.result[m, ]
      #cat("result:\n")
      #print(str(result))
      
      cor.str <- sprintf("cor=%.3f", result[["test.cor"]])
      abp.lag.str <- 
        sprintf("%d %s lags", result[[abp.var.name]], abp.var.name)
      co2.lag.str <- 
        sprintf("%d %s lags", result[[co2.var.name]], co2.var.name)
      model.str <- paste(cor.str, abp.lag.str, co2.lag.str, sep = ", ")
      model.index <- sprintf("Model %03d:", m)
      model.name <- paste(model.index, model.str)
      
      cat(model.name, "\n")
      
      i <- colnames(signal) %in% colnames(result)
      lag.col.names <- colnames(signal)[i]
      lags <- as.list(result[, lag.col.names])
      #cat("lags:\n")
      #print(str(lags))
      
      lagged.signal <- lagSignal(
        signal = signal,
        lags = lags,
        time.colname = time.var.name
      )
      #cat("lagged.signal:\n")
      #print(head(lagged.signal))
      
      folded.signal <- separateSignalIntoFolds(
        signal = lagged.signal,
        n.folds = nfolds,
        fold.colname = fold.colname,
        time.colname = time.var.name
      )
      #cat("folded.signal:\n")
      #print(head(folded.signal))
      
      no.input.var.names <- c(time.var.name, cbf.var.name)
      input.var.names <- colnames(lagged.signal)
      input.var.names <- input.var.names[! input.var.names %in% no.input.var.names]
      
      svm.model <- get.svm.model(
        folded.signal = folded.signal,
        fold = result[["fold"]],
        model.params = as.list(result[, model.param.names]),
        input.var.names = input.var.names,
        cbf.var.name = cbf.var.name,
        svm.type = svm.type,
        svm.kernel = svm.kernel,
        svm.params = as.list(result[, svm.param.names])
      )
      #cat("svm.model:\n")
      #print(str(svm.model, 1))
      
      svm.model.cors <- get.model.cors(
        folded.signal = folded.signal,
        fold = result[["fold"]],
        model = svm.model,
        input.var.names = input.var.names,
        cbf.var.name = cbf.var.name,
        train.cor.rounding.digits = train.cor.rounding.digits,
        test.cor.rounding.digits = test.cor.rounding.digits
      )
      #cat("svm.model.cors:\n")
      #print(svm.model.cors)
      
      if(svm.model.cors[["train.cor"]] != result[["train.cor"]])
      {
        s <- sprintf("%.*f", train.cor.rounding.digits, svm.model.cors[["train.cor"]])
        s <- paste("Warning: different training correlation, now", s)
        cat(s, "\n", sep = "")
        if(abs(svm.model.cors[["train.cor"]] - result[["train.cor"]]) > cor.tol)
          stop("could not rebuild the model - different training correlation")
      }
      
      if(svm.model.cors[["test.cor"]] != result[["test.cor"]])
      {
        s <- sprintf("%.*f", test.cor.rounding.digits, svm.model.cors[["test.cor"]])
        s <- paste("Warning: different test correlation, now", s)
        cat(s, "\n", sep = "")
        if(abs(svm.model.cors[["test.cor"]] - result[["test.cor"]]) > cor.tol)
          stop("could not rebuild the model - different test correlation")
      }
      
      i <- names(lags) == cbf.var.name
      input.lags <- lags[!i]
      
      lagged.abp.step <- lagSignal(
        signal = abp.step,
        lags = input.lags,
        time.colname = time.var.name
      )
      #cat("lagged.abp.step:\n")
      #print(head(lagged.abp.step))
      
      mvre <- get.abp.step.response(
        svm.model = svm.model,
        lagged.abp.step = lagged.abp.step,
        sampling.time = sampling.time,
        time.release = mvre.time.release,
        time.var.name = time.var.name,
        abp.var.name = abp.var.name,
        cbf.var.name = cbf.var.name,
        time.until.baseline = mvre.time.until.baseline,
        baseline.duration = mvre.baseline.duration,
        time.after.release = mvre.time.after.release,
        abp.rounding.digits = abp.rounding.digits,
        cbf.rounding.digits = cbf.rounding.digits,
        time.tol = time.tol
      )
      cat("manoeuvre:\n")
      print(head(mvre))
      stop()
      
      
      mfari <- get.mfari(
        mvre = mvre,
        time.var.name = time.var.name,
        abp.var.name = abp.var.name,
        cbf.var.name = cbf.var.name,
        sampling.time = sampling.time,
        time.release = mvre.time.release,
#         --
      abp.step.time.until.release,
      abp.step.time.after.release,
      abp.step.time.release,
      co2.step.time.until.release,
      co2.step.time.after.release,
      co2.step.time.release,
      mvre.time.until.baseline,
      mvre.baseline.duration,
      mvre.time.after.release,
      mvre.time.release,
      co2.reactivity.time.until.release,
      co2.reactivity.time.after.release,
      co2.reactivity.time.release,
      co2.reactivity.initial.baseline.duration,
      co2.reactivity.final.baseline.duration,
      time.rounding.digits,
      abp.rounding.digits,
      co2.rounding.digits,
      cbf.rounding.digits,
      co2.reactivity.rounding.digits = 2,
      time.tol = sampling.time / 100,
      cor.tol = 0.05,
#         --
    abp.step.time.until.release = abp.step.time.until.release,
    abp.step.time.after.release = abp.step.time.after.release,
    time.rounding.digits = time.rounding.digits,
#     --
        time.var.name = time.var.name,
        abp.var.name = abp.var.name,
        cbf.var.name = cbf.var.name,
        time.until.baseline = mvre.time.until.baseline,
        baseline.duration = mvre.baseline.duration,
        time.after.release = mvre.time.after.release,
        abp.rounding.digits = abp.rounding.digits,
        cbf.rounding.digits = cbf.rounding.digits,
        time.tol = time.tol,
#     --
      baseline.initial.time = time.instants[1],
      baseline.final.time = time.release,
      min.ABP.max.delta.time = 5 * 0.8,
      transient.ABP.duration = 6,
      min.CBFV.max.delta.time = 8 * 0.8, 
      min.transient.CBFV.duration = 1.5,
      max.transient.CBFV.duration = 10,
      steady.CBFV.duration = 6,
      min.Ks = 0.0,
      max.Ks = 1.022095,
      min.ABP.angle = 0.0,
      max.ABP.angle = 90.0,
      min.CBFV.angle = 0.0,
      max.CBFV.angle = 90.0,
      min.Phi = 0.0,
      max.Phi = 35.238932,
      keep.details = TRUE,
      ABP.rounding.digits = 2,
      CBFV.rounding.digits = 2,
      Ks.rounding.digits = 4,
      Phi.rounding.digits = 2,
      time.tol = sampling.time / 100,
      ...
        )
      cat("mfARI: ", mfari[["mfARI"]], "\n")
      
      dari <- get.dari(
        time.instants = mvre[[time.var.name]],
        cbf.response = mvre[[cbf.var.name]],
        normalised.ari.templates = normalised.ari.templates,
        time.release = mvre.time.release
      )
      cat("dARI: ", dari, "\n")
      
      p1 <- get.abp.step.response.plot(
        mvre = mvre,
        time.var.name = time.var.name,
        abp.var.name = abp.var.name,
        cbf.var.name = cbf.var.name,
        abp.colour = abp.palette[5],
        cbf.colour = cbf.palette[5]
      )
      
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
          s <- format(result[[p]])
        else
          s <- sprintf("2^%s", format(log(result[[p]], 2)))
        data.frame(Result = p, Value = s)
      }
      d <- do.call(rbind, lapply(svm.param.names, .internal.tmp.fun))
      t <- rbind(t, d)
      s <- sprintf(
        "%.*f",
        train.cor.rounding.digits,
        svm.model.cors[["train.cor"]]
      )
      t <- rbind(t, data.frame(Result = "Training correlaton", Value = s))
      s <- sprintf(
        "%.*f",
        test.cor.rounding.digits,
        svm.model.cors[["test.cor"]]
      )
      t <- rbind(t, data.frame(Result = "Test correlaton", Value = s))
      s <- sprintf("%.1f", mfari[["mfARI"]])
      t <- rbind(t, data.frame(Result = "mfARI", Value = s))
      s <- sprintf("%.1f", dari)
      t <- rbind(t, data.frame(Result = "Decimal ARI", Value = s))
      gt <- tableGrob(
        d = t,
        show.rownames = FALSE,
        #show.colnames = FALSE,
        gpar.coretext = gpar(cex = 0.5, col = ann.palette[8]),
        gpar.coltext = gpar(cex = 0.6, col = ann.palette[8], fontface = "bold"),
        gpar.corefill = gpar(fill = "grey90", col = ann.palette[5]),
        gpar.colfill = gpar(fill = "grey90", col = ann.palette[5]),
        padding.v = unit(1.5, "mm")
      )
      p1 <- p1 + annotation_custom(
        grob = gt,
        xmin = -(mvre.time.until.baseline + mvre.baseline.duration),
        xmax = mvre.time.release,
        ymin = -0.2,
        ymax = 0.7
      )
      p1 <- p1 + theme(
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha('grey90', 0.0)),
        legend.position = c(1, 1),
        legend.key.size =  unit(0.4, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.margin = unit(0, "cm")
      )      
      
      title <- "ABP step-response"
      p1 <- p1 + ggtitle(title) + xlab("Time") + ylab("Signals")
      
      lagged.co2.step <- lagSignal(
        signal = co2.step,
        lags = input.lags,
        time.colname = time.var.name
      )
      #cat("lagged.co2.step:\n")
      #print(head(lagged.co2.step))
      
      co2.mvre <- get.co2.step.response(
        svm.model = svm.model,
        lagged.co2.step = lagged.co2.step,
        sampling.time = sampling.time,
        time.var.name = time.var.name,
        abp.var.name = abp.var.name,
        cbf.var.name = cbf.var.name,
        time.release = co2.reactivity.time.release,
        time.until.release = co2.reactivity.time.until.release,
        time.after.release = co2.reactivity.time.after.release,
        time.tol = time.tol
      )
      
      #cat("CO2 reactivity:\n")
      ib.ini <- co2.reactivity.time.release - co2.reactivity.initial.baseline.duration
      ib.fin <- co2.reactivity.time.release
      #cat("   initial time initial baseline:", ib.ini, "\n")
      #cat("   final time initial baseline:", ib.fin, "\n")
      
      final.time.instant <- tail(co2.mvre[[time.var.name]], 1)
      fb.ini <- final.time.instant - co2.reactivity.final.baseline.duration
      fb.fin <- final.time.instant
      #cat("   initial time final baseline:", fb.ini, "\n")
      #cat("   final time final baseline:", fb.fin, "\n")
      
      co2.reactivity.data <- get.co2.reactivity(
        mvre = co2.mvre,
        time.var.name = time.var.name,
        co2.var.name = co2.var.name,
        cbf.var.name = cbf.var.name,
        initial.time.initial.baseline = ib.ini,
        final.time.initial.baseline = ib.fin,
        initial.time.final.baseline = fb.ini,
        final.time.final.baseline = fb.fin,
        time.tol = time.tol
      )
      co2.reactivity <- round(
        co2.reactivity.data[["co2.reactivity"]] * 100,
        co2.reactivity.rounding.digits
      )
      
      p2 <- get.co2.reactivity.plot(
        mvre = co2.mvre,
        time.var.name = time.var.name,
        co2.var.name = co2.var.name,
        cbf.var.name = cbf.var.name,
        initial.time.initial.baseline = ib.ini,
        final.time.initial.baseline = ib.fin,
        initial.time.final.baseline = fb.ini,
        final.time.final.baseline = fb.fin,
        time.tol = time.tol,
        co2.colour = co2.palette[5],
        cbf.colour = cbf.palette[5]
      )
      title <- "CO2 step-response"
      p2 <- p2 + ggtitle(title) + xlab("Time") + ylab("Signals")
      p2 <- p2 + coord_cartesian(ylim = c(0, 1)) 
      p2 <- p2 + theme(
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha('grey90', 0.0)),
        legend.position = c(0, 1),
        legend.key.size =  unit(0.4, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.margin = unit(0, "cm")
      )
      
      # Adds summary table
      t <- NULL
      l <- sprintf("%s Initial Level", co2.var.name)
      s <- sprintf("%.*f", co2.rounding.digits, co2.reactivity.data[["co2.initial.level"]])
      t <- rbind(t, data.frame(Result = l, Value = s))
      l <- sprintf("%s Final Level", co2.var.name)
      s <- sprintf("%.*f", co2.rounding.digits, co2.reactivity.data[["co2.final.level"]])
      t <- rbind(t, data.frame(Result = l, Value = s))
      l <- sprintf("Delta %s", co2.var.name)
      s <- sprintf("%.*f", co2.rounding.digits + 1, co2.reactivity.data[["co2.delta"]])
      t <- rbind(t, data.frame(Result = l, Value = s))
      l <- sprintf("%s Initial Level", cbf.var.name)
      s <- sprintf("%.*f", cbf.rounding.digits, co2.reactivity.data[["cbf.initial.level"]])
      t <- rbind(t, data.frame(Result = l, Value = s))
      l <- sprintf("%s Final Level", cbf.var.name)
      s <- sprintf("%.*f", cbf.rounding.digits, co2.reactivity.data[["cbf.final.level"]])
      t <- rbind(t, data.frame(Result = l, Value = s))
      l <- sprintf("Delta %s", cbf.var.name)
      s <- sprintf("%.*f", cbf.rounding.digits + 1, co2.reactivity.data[["cbf.delta"]])
      t <- rbind(t, data.frame(Result = l, Value = s))
      l <- sprintf("%s Reactivity", co2.var.name)
      s <- sprintf("%.*f%%", co2.reactivity.rounding.digits, co2.reactivity)
      t <- rbind(t, data.frame(Result = l, Value = s))
      gt <- tableGrob(
        d = t,
        show.rownames = FALSE, #show.colnames = FALSE,
        gpar.coretext = gpar(cex = 0.5, col = ann.palette[8]),
        gpar.coltext = gpar(cex = 0.6, col = ann.palette[8], fontface = "bold"),
        gpar.corefill = gpar(fill = "grey90", col = ann.palette[5]),
        gpar.colfill = gpar(fill = "grey90", col = ann.palette[5]),
        padding.v = unit(1.5, "mm")
      )
      
      x <- (fb.ini - ib.fin) / 2
      p2 <- p2 + annotation_custom(
        gt,
        xmin = x - 15,
        xmax = x + 15,
        ymin = 0,
        ymax = 0.3
      )
      
      main <- paste0("\n", model.name)
      p <- arrangeGrob(p1, p2, ncol = 2, main = main)
      plots <- c(plots, list(p))
    }
    
    tgt.plot.basename <- paste(side.basename, "step", "responses", sep = "-")
    tgt.plot.basename <- paste(tgt.plot.basename, "pdf", sep = ".")
    tgt.plot.file <- file.path(tgt.plot.dir, tgt.plot.basename)
    
    top <- paste0("\n", side.basename)
    params <- c(plots, list(nrow = 1, ncol = 1, top = top))
    p <- do.call(marrangeGrob, params)
    ggsave(
      filename = tgt.plot.file,
      plot = p,
      width = 11,
      height = 8,
      units = "in"
    )
  }
}


get.abp.step.response.plot <- function(
      mvre,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      abp.colour,
      cbf.colour
      )
{
  t <- mvre[[time.var.name]]
  abp <- mvre[[abp.var.name]]
  cbf <- mvre[[cbf.var.name]]
  colours <- c(abp.colour, cbf.colour)
  names(colours) <- c(abp.var.name, cbf.var.name)
  
  d1 <- data.frame(Time = t, Signal = abp, Type = abp.var.name)
  d2 <- data.frame(Time = t, Signal = cbf, Type = cbf.var.name)
  d <- rbind(d1, d2)
  
  p <- ggplot(d, aes(x = Time, y = Signal, colour = Type))
  p <- p + geom_line() + geom_point()
  p <- p + scale_colour_manual(values = colours)
  
  p
}


get.co2.reactivity <- function(
      mvre,
      time.var.name,
      co2.var.name,
      cbf.var.name,
      initial.time.initial.baseline,
      final.time.initial.baseline,
      initial.time.final.baseline,
      final.time.final.baseline,
      time.tol
      )
{
  i <- mvre[[time.var.name]] > initial.time.initial.baseline - time.tol
  j <- mvre[[time.var.name]] < final.time.initial.baseline + time.tol
  initial.baseline <- mvre[i & j, ]
  #cat("CO2 reactivity initial baseline:\n")
  #print(initial.baseline)
  
  i <- mvre[[time.var.name]] > initial.time.final.baseline - time.tol
  j <- mvre[[time.var.name]] < final.time.final.baseline + time.tol
  final.baseline <- mvre[i & j, ]
  #cat("CO2 reactivity final baseline:\n")
  #print(final.baseline)
  
  ib.co2 <- mean(initial.baseline[[co2.var.name]])
  fb.co2 <- mean(final.baseline[[co2.var.name]])
  delta.co2 <- fb.co2 - ib.co2
  #cat("CO2 reactivity delta CO2:", delta.co2, "\n")
  
  ib.cbf <- mean(initial.baseline[[cbf.var.name]])
  fb.cbf <- mean(final.baseline[[cbf.var.name]])
  delta.cbf <- max(0, fb.cbf - ib.cbf)
  #cat("CO2 reactivity delta CBFV:", delta.cbf, "\n")
  
  co2.reactivity <- delta.cbf / delta.co2
  #cat("CO2 reactivity:", co2.reactivity, "\n")
  
  list(
    co2.initial.level = ib.co2,
    co2.final.level = fb.co2,
    co2.delta = delta.co2,
    cbf.initial.level = ib.cbf,
    cbf.final.level = fb.cbf,
    cbf.delta = delta.cbf,
    co2.reactivity = co2.reactivity
  )
}


get.dari <- function(
      time.instants,
      cbf.response,
      normalised.ari.templates,
      time.release,
      delta.time.before.ref = 0,
      delta.time.after.ref = round(20 * 0.8, 1),
      comparison.function = get.MSE
      )
{
  normalised.cbf.response <- .sigexp.internal.normalise(cbf.response)
    
  atari <- get.best.templates(
    time.instants = time.instants,
    signal = normalised.cbf.response,
    templates = normalised.ari.templates,
    referential.time.instant = time.release,
    delta.time.before.ref = delta.time.before.ref,
    delta.time.after.ref = delta.time.after.ref,
    comparison.function = comparison.function,
    keep.details = FALSE
  )
  best.i <- atari[["best.template.index"]]
  
  #plot(time.instants, normalised.cbf.response)
  #lines(time.instants, normalised.ari.templates[[best.i]])
  #for(i in seq(1, 91, 10))
    #lines(time.instants, normalised.ari.templates[[i]])
  
  as.numeric(names(normalised.ari.templates)[best.i])
}


get.mfari <- function(
      mvre,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      sampling.time,
      time.release,
      time.until.baseline = 5,
      baseline.duration = 5,
      time.after.release = 20,
      abp.rounding.digits = 4,
      cbf.rounding.digits = 4,
      time.tol = sampling.time / 100,
      ...
      )
{
  mfari <- get.mfARI.parameters(
    time.instants = mvre[[time.var.name]],
    ABP.signal = mvre[[abp.var.name]],
    CBFV.signal = mvre[[cbf.var.name]],
    ...
  )
  mfari[["mfARI"]] <- get.mfARI(
    mfari[["Ks"]],
    mfari[["Delta.tau"]],
    mfari[["Phi"]]
  )
  mfari
}


get.normalised.ari.templates <- function(
      time.instants,
      normalised.ABP,
      sampling.time,
      time.release,
      stabilisation.time = 30,
      at.param.rounding.digits = 6,
      time.tol = sampling.time / 100
      )
{
  at.params <- get.AT.decimal.templates.parameters(
                 rounding.digits = at.param.rounding.digits
               )
  params.indices <- 1:nrow(at.params)
  
  .internal.tmp.fun <- function(i) get.theoretical.CBFV.response(
    T = at.params[i, 1],
    D = at.params[i, 2],
    K = at.params[i, 3],
    time.instants = time.instants,
    ABP.normalised = normalised.ABP,
    sampling.time = sampling.time,
    stabilisation.time = stabilisation.time
  )
  templates <- lapply(params.indices, .internal.tmp.fun)
  
  .internal.tmp.fun <- function(t) .sigexp.internal.normalise(t[["CBFV.theoretical.response"]])
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


get.abp.step.response <- function(
      svm.model,
      lagged.abp.step,
      sampling.time,
      time.release,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      time.until.baseline = 5,
      baseline.duration = 5,
      time.after.release = 20,
      abp.rounding.digits = 4,
      cbf.rounding.digits = 4,
      time.tol = sampling.time / 100
      )
{
  step.response <- predict(svm.model, lagged.abp.step)
  step.response <- round(step.response, cbf.rounding.digits)
  lagged.abp.step[[cbf.var.name]] <- step.response
  
  time.until.release <- time.until.baseline + baseline.duration
  time.ini <- time.release - time.until.release - time.tol
  time.fin <- time.release + time.after.release + time.tol
  i <- lagged.abp.step[[time.var.name]] > time.ini
  j <- lagged.abp.step[[time.var.name]] < time.fin
  samples <- which(i & j)
  
  col.names <- c(time.var.name, abp.var.name, cbf.var.name)
  response <- lagged.abp.step[samples, col.names]
  
  time.ini <- time.release - time.until.baseline - time.tol
  time.fin <- time.release + time.tol
  i <- response[[time.var.name]] > time.ini
  j <- response[[time.var.name]] < time.fin
  samples <- which(i & j)
  vshift <- 0.8 - mean(response[[cbf.var.name]][samples])
  response[[cbf.var.name]] <- response[[cbf.var.name]] + vshift
  
  response
}


get.co2.step.response <- function(
      svm.model,
      lagged.co2.step,
      sampling.time,
      time.var.name,
      abp.var.name,
      cbf.var.name,
      time.release = 0,
      time.until.release = 10,
      time.after.release = 60,
      time.tol = sampling.time / 100
      )
{
  step.response <- predict(svm.model, lagged.co2.step)
  
  time.ini <- time.release - time.until.release - time.tol
  time.fin <- time.release + time.after.release + time.tol
  i <- lagged.co2.step[[time.var.name]] > time.ini
  j <- lagged.co2.step[[time.var.name]] < time.fin
  samples <- which(i & j)
  
  segment <- lagged.co2.step[samples, ]
  segment[[cbf.var.name]] <- step.response[samples]
    
  segment
}


get.model.cors <- function(
      folded.signal,
      fold,
      model,
      input.var.names,
      cbf.var.name,
      train.cor.rounding.digits,
      test.cor.rounding.digits
     )
{
  fold.colname <- attr(folded.signal, FOLD.COLUMN.NAME.ATTR)
  nfolds <- attr(folded.signal, NUMBER.OF.FOLDS.ATTR)
  
  i <- folded.signal[[fold.colname]] == fold
  train.signal <- folded.signal[i, ]
  train.cor <- cor(model[["fitted"]], train.signal[[cbf.var.name]])
  train.cor <- round(train.cor, train.cor.rounding.digits)
  
  test.fold <- fold + 1
  if(test.fold > nfolds)
    test.fold <- test.fold - 2
  
  i <- folded.signal[[fold.colname]] == test.fold
  test.signal <- folded.signal[i, ]
  
  x <- test.signal[, input.var.names]
  y <- test.signal[[cbf.var.name]]
  pred <- predict(model, x)
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
  
  do.call(svm, params)
}


get.instance <- function(src.dir, src.basename, src.ext)
{
  src.filename <- paste(src.basename, src.ext, sep = ".")
  src.file <- file.path(src.dir, src.filename)
  ins <- read.table(file = src.file, header = TRUE, check.names = FALSE)
  col.names <- names(ins)
  col.names <- gsub("-", "", col.names, fixed = TRUE)
  names(ins) <- col.names
  ins
}


get.co2.step <- function(
      sampling.time,
      time.var.name,
      abp.var.name,
      co2.var.name,
      norm.abp.signal,
      co2.step.time.until.release,
      co2.step.time.after.release,
      time.rounding.digits,
      abp.rounding.digits,
      co2.rounding.digits
      )
{
  smooth.step.stimulus <- TRUE
  filter.order <- 1
  cutoff.frequency <- 1/60
  left.stabilisation.time <- 0
  
  step <- get.normalised.ABP.stimulus(
    sampling.time = sampling.time,
    time.until.release = co2.step.time.until.release,
    time.after.release = co2.step.time.after.release,
    smooth.step.stimulus = smooth.step.stimulus,
    filter.order = filter.order,
    cutoff.frequency = cutoff.frequency,
    left.stabilisation.time = left.stabilisation.time,
    time.rounding.digits =  time.rounding.digits
  )
  
  abp <- round(mean(norm.abp.signal), abp.rounding.digits)
  co2 <- -.sigexp.internal.normalise(step[["ABP.normalised"]]) + 1
  co2 <- round(co2, co2.rounding.digits)
  
  co2.step <- data.frame(step[["time.instants"]], abp, co2)
  colnames(co2.step) <- c(time.var.name, abp.var.name, co2.var.name)
  
  co2.step
}


test <- function(
      sampling.time = 0.5,
      src.instance.dir = file.path(script.dir, "Data"),
      src.instance.ext = "txt",
      src.result.dir = file.path(script.dir, "Temp"),
      src.result.ext = "RData",
      src.basename = "Sujeto1",
      tgt.plot.dir = file.path(script.dir, "Temp"),
      time.var.name = "Time",
      abp.var.name = "MABP",
      co2.var.name = "etCO2",
      cbf.var.names = "CBFVL",
      abp.step.time.until.release = 40,
      abp.step.time.after.release =  30,
      abp.step.time.release = 0,
      co2.step.time.until.release = 60,
      co2.step.time.after.release = 100,
      co2.step.time.release = 0,
      mvre.time.until.baseline = 5,
      mvre.baseline.duration = 5,
      mvre.time.after.release = 20,
      mvre.time.release = 0,
      co2.reactivity.time.until.release = 20,
      co2.reactivity.time.after.release = 80,
      co2.reactivity.time.release = 0,
      co2.reactivity.initial.baseline.duration = 20,
      co2.reactivity.final.baseline.duration = 20,
      time.rounding.digits = 1,
      abp.rounding.digits = 4,
      co2.rounding.digits = 3,
      cbf.rounding.digits = 4,
      time.tol = sampling.time / 100
      )
{
  dir.create(tgt.plot.dir, showWarnings = FALSE, recursive = TRUE)
  
  abp.step <- .sigexp.internal.get.abp.step(
    sampling.time = sampling.time,
    time.var.name = time.var.name,
    abp.var.name = abp.var.name,
    co2.var.name = co2.var.name,
    norm.co2.signal = 1,
    abp.step.time.until.release = abp.step.time.until.release,
    abp.step.time.after.release = abp.step.time.after.release,
    time.rounding.digits = time.rounding.digits,
    abp.rounding.digits = abp.rounding.digits,
    co2.rounding.digits = co2.rounding.digits
  )
  
  mvre.time.until.release <-
    mvre.time.until.baseline + mvre.baseline.duration
  time.ini <- mvre.time.release - mvre.time.until.release - time.tol
  time.fin <- mvre.time.release + mvre.time.after.release + time.tol
  i <- abp.step[[time.var.name]] > time.ini
  j <- abp.step[[time.var.name]] < time.fin
  samples <- which(i & j)
  abp.step <- abp.step[samples, ]
  
  normalised.ari.templates <- get.normalised.ari.templates(
    time.instants = abp.step[[time.var.name]],
    normalised.ABP = abp.step[[abp.var.name]],
    sampling.time = sampling.time,
    time.release = mvre.time.release,
    time.tol = time.tol
  )
  
  plot.instance(
    src.instance.dir = src.instance.dir,
    src.instance.ext = src.instance.ext,
    src.result.dir = src.result.dir,
    src.result.ext = src.result.ext,
    src.basename = src.basename,
    tgt.plot.dir = tgt.plot.dir,
    sampling.time = sampling.time,
    time.var.name = time.var.name,
    abp.var.name = abp.var.name,
    co2.var.name = co2.var.name,
    cbf.var.names = cbf.var.names,
    abp.step.time.until.release = abp.step.time.until.release,
    abp.step.time.after.release =  abp.step.time.after.release,
    abp.step.time.release = abp.step.time.release,
    co2.step.time.until.release = co2.step.time.until.release,
    co2.step.time.after.release = co2.step.time.after.release,
    co2.step.time.release = co2.step.time.release,
    mvre.time.until.baseline = mvre.time.until.baseline,
    mvre.baseline.duration = mvre.baseline.duration,
    mvre.time.after.release = mvre.time.after.release,
    mvre.time.release = mvre.time.release,
    co2.reactivity.time.until.release = co2.reactivity.time.until.release,
    co2.reactivity.time.after.release = co2.reactivity.time.after.release,
    co2.reactivity.time.release = co2.reactivity.time.release,
    co2.reactivity.initial.baseline.duration = co2.reactivity.initial.baseline.duration,
    co2.reactivity.final.baseline.duration = co2.reactivity.final.baseline.duration,
    normalised.ari.templates = normalised.ari.templates,
    time.rounding.digits = time.rounding.digits,
    abp.rounding.digits = abp.rounding.digits,
    co2.rounding.digits = co2.rounding.digits,
    cbf.rounding.digits = cbf.rounding.digits,
    time.tol = time.tol
  )
}


get.co2.reactivity.plot <- function(
      mvre,
      time.var.name,
      co2.var.name,
      cbf.var.name,
      initial.time.initial.baseline,
      final.time.initial.baseline,
      initial.time.final.baseline,
      final.time.final.baseline,
      time.tol,
      co2.colour,
      cbf.colour
      )
{
  t <- mvre[[time.var.name]]
  co2 <- mvre[[co2.var.name]]
  cbf <- mvre[[cbf.var.name]]
  colours <- c(co2.colour, cbf.colour)
  names(colours) <- c(co2.var.name, cbf.var.name)
  
  d1 <- data.frame(Time = t, Signal = co2, Type = co2.var.name)
  d2 <- data.frame(Time = t, Signal = cbf, Type = cbf.var.name)
  d <- rbind(d1, d2)
  
  p <- ggplot(d, aes(x = Time, y = Signal, colour = Type))
  p <- p + geom_line()
  p <- p + scale_colour_manual(values = colours)

  i <- d[["Time"]] > initial.time.initial.baseline - time.tol
  j <- d[["Time"]] < final.time.initial.baseline + time.tol
  
  k <- d[["Time"]] > initial.time.final.baseline - time.tol
  l <- d[["Time"]] < final.time.final.baseline + time.tol
  
  d <- d[((i & j) | (k & l)), ]
  p <- p + geom_point(data = d)
  
  p
}


get.co2.step.plot <- function(
      co2.step,
      time.var.name,
      co2.var.name,
      co2.colour
      )
{
  t <- co2.step[[time.var.name]]
  co2 <- co2.step[[co2.var.name]]
  colours <- c(co2.colour)
  names(colours) <- c(co2.var.name)
  
  d <- data.frame(Time = t, Signal = co2, Type = co2.var.name)
  
  p <- ggplot(d, aes(x = Time, y = Signal, colour = Type))
  p <- p + geom_line() + geom_point()
  p <- p + scale_colour_manual(values = colours)
  
  p
}
