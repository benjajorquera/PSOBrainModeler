
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

process.instance <- function(
      src.dir,
      src.basename,
      sampling.time,
      src.ext,
      tmp.dir,
      tmp.ext,
      tgt.dir,
      tgt.ext,
      lags,
      parameters,
      nfolds,
      parallel.lagging,
      lag.per.group,
      save.every.ngroups,
      keep.nstats,
      ...
      )
{
  # Gests instance (Time MABP CBFVL CBFVR etCO2)
  instance <- get.instance(
    src.dir = src.dir,
    src.basename = src.basename,
    src.ext = src.ext
  )
  
  for(side in c("CBFVL", "CBFVR"))
  {
    tmp.basename <- paste(src.basename, side, sep = "-")
    tmp.basename <- paste(tmp.basename, tmp.ext, sep = ".")
    tmp.file <- file.path(tmp.dir, tmp.basename)
    
    tgt.basename <- paste(src.basename, side, sep = "-")
    tgt.basename <- paste(tgt.basename, tgt.ext, sep = ".")
    tgt.file <- file.path(tgt.dir, tgt.basename)
    
    signal <- instance[, c("Time", "MABP", side, "etCO2")]
    
    sig.exp <- get.signal.experiment(
      signal = signal,
      signal.sampling.time = sampling.time,
      time.var.name = "Time",
      abp.var.name = "MABP",
      co2.var.name = "etCO2",
      cbf.var.name = side
    )
    
    lag.mgr <- get.lag.manager(
      lag.list = lags,
      tmp.file = tmp.file,
      output.var.name = side,
      lag.per.group = lag.per.group,
      save.every.ngroups = save.every.ngroups,
      keep.nstats = keep.nstats
    )
    
    start.time <- Sys.time()
    ans <- grid.lags.and.parameters(
      sig.exp = sig.exp,
      lag.mgr = lag.mgr,
      parallel.lagging = parallel.lagging,
      parameters = parameters,
      nfolds = nfolds,
      ...
    )
    end.time <-Sys.time()
    
    ans[["start.time"]] <- start.time
    ans[["end.time"]] <- end.time
    ans[["lagging.time"]] <- end.time - start.time
    
    saveRDS(ans, file = tgt.file, compress = FALSE)
    file.remove(tmp.file)
  }
  
  TRUE
}


run <- function(
      sampling.time = 0.5,
      src.basenames = as.character(1:3),
      src.ext = "txt",
      tmp.ext = "tmpRData",
      tgt.ext = "RData",
      nworkers = 2)
{
  src.dir <- file.path(script.dir, "Data")
  tmp.dir <- file.path(script.dir, "Temp")
  dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)
  tgt.dir <- file.path(script.dir, "Results")
  dir.create(tgt.dir, showWarnings = FALSE, recursive = TRUE)
  
  # Lags for CARNet bootstrap on 20-apr-2015
  lags <- list(MABP = 1:8, etCO2 = 0:6)
  
  # Hyper parameters (huge! 25-ago-2015)
  sigma <- 2 ^ seq(-4, 10, 1)
  gamma <- 1 / (2 * sigma ^ 2)
  cost <- 2 ^ seq(-2, 10, 1)
  nu <- seq(0.1, 1, 0.1)
  
  parameters <- list(gamma = gamma,
                     cost = cost,
                     nu = nu)
  
  export.names <- c(
    "grid.parameters",
    "lagSignal",
    "lagDataFrameColumns",
    "LAGGED.DATA.CLASS.NAME",
    "LAG.ORDER.NAME.ATTR",
    "LAGGED.COLUMN.NAMES.ATTR",
    "crossValidate",
    "separateSignalIntoFolds",
    "FOLDED.DATA.CLASS.NAME",
    "FOLD.COLUMN.NAME.ATTR",
    "NUMBER.OF.FOLDS.ATTR",
    "TIME.COLUMN.NAME.ATTR",
    .sigexp.internal.get.export.names()
  )
  export.packages <- c("e1071", "pracma")
  
  cluster <- makeCluster(nworkers)
  registerDoParallel(cluster)
  
  for(src.basename in src.basenames)
  {
    src.basename <- paste0("Sujeto", src.basename)
    cat("instance ", src.basename, "\n")
    
    process.instance(
      src.dir = src.dir,
      src.basename = src.basename,
      sampling.time = sampling.time,
      src.ext = src.ext,
      tmp.dir = tmp.dir,
      tmp.ext = tmp.ext,
      tgt.dir = tgt.dir,
      tgt.ext = tgt.ext,
      lags = lags,
      parameters = parameters,
      nfolds = 2,
      parallel.lagging = "par",
      lag.per.group = nworkers,
      save.every.ngroups = 3,
      keep.nstats = 100,
      export.names = export.names,
      export.packages = export.packages
    )
  }
  
  stopCluster(cluster)
}
