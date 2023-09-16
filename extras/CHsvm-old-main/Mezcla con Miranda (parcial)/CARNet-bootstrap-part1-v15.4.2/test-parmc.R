
script.dir <- dirname(sys.frame(1)$ofile)

CARSIG.SCRIPT.BASENAME <- paste("carSignal", sep = "-")
CARSIG.SCRIPT.BASENAME <- paste(CARSIG.SCRIPT.BASENAME, "R", sep = ".")
CARSIG.SCRIPT.NAME <- file.path(script.dir, CARSIG.SCRIPT.BASENAME)
source(CARSIG.SCRIPT.NAME)

LAGMGR.SCRIPT.BASENAME <- paste("Lag", "manager", "v15.4.2", sep = "-")
LAGMGR.SCRIPT.BASENAME <- paste(LAGMGR.SCRIPT.BASENAME, "R", sep = ".")
LAGMGR.SCRIPT.NAME <- file.path(script.dir, LAGMGR.SCRIPT.BASENAME)
source(LAGMGR.SCRIPT.NAME)

RNDUTL.SCRIPT.BASENAME <- paste("Rounding", "utils", sep = "-")
RNDUTL.SCRIPT.BASENAME <- paste(RNDUTL.SCRIPT.BASENAME, "R", sep = ".")
RNDUTL.SCRIPT.NAME <- file.path(script.dir, RNDUTL.SCRIPT.BASENAME)
source(RNDUTL.SCRIPT.NAME)

MTRUTL.SCRIPT.BASENAME <- paste("Metric", "utils", sep = "-")
MTRUTL.SCRIPT.BASENAME <- paste(MTRUTL.SCRIPT.BASENAME, "R", sep = ".")
MTRUTL.SCRIPT.NAME <- file.path(script.dir, MTRUTL.SCRIPT.BASENAME)
source(MTRUTL.SCRIPT.NAME)

SIGCVL.SCRIPT.BASENAME <- paste("Signal", "cross", "validation", "v15.4.1", sep = "-")
SIGCVL.SCRIPT.BASENAME <- paste(SIGCVL.SCRIPT.BASENAME, "R", sep = ".")
SIGCVL.SCRIPT.NAME <- file.path(script.dir, SIGCVL.SCRIPT.BASENAME)
source(SIGCVL.SCRIPT.NAME)

SIGRID.SCRIPT.BASENAME <- paste("Signal", "grid", "v15.4.1", sep = "-")
SIGRID.SCRIPT.BASENAME <- paste(SIGRID.SCRIPT.BASENAME, "R", sep = ".")
SIGRID.SCRIPT.NAME <- file.path(script.dir, SIGRID.SCRIPT.BASENAME)
source(SIGRID.SCRIPT.NAME)

SIGEXP.SCRIPT.BASENAME <- paste("Signal", "experiment", "v15.4.1", sep = "-")
SIGEXP.SCRIPT.BASENAME <- paste(SIGEXP.SCRIPT.BASENAME, "R", sep = ".")
SIGEXP.SCRIPT.NAME <- file.path(script.dir, SIGEXP.SCRIPT.BASENAME)
source(SIGEXP.SCRIPT.NAME)

SIGLAG.SCRIPT.BASENAME <- paste("Signal", "lagging", sep = "-")
SIGLAG.SCRIPT.BASENAME <- paste(SIGLAG.SCRIPT.BASENAME, "R", sep = ".")
SIGLAG.SCRIPT.NAME <- file.path(script.dir, SIGLAG.SCRIPT.BASENAME)
source(SIGLAG.SCRIPT.NAME)



.get.instance <- function(src.dir,
                          src.basename,
                          src.ext)
{
  src.filename <- paste(src.basename, src.ext, sep = ".")
  src.file <- file.path(src.dir, src.filename)
  ins <- read.table(file = src.file, header = TRUE, check.names = FALSE)
  col.names <- names(ins)
  col.names <- gsub("-", "", col.names, fixed = TRUE)
  names(ins) <- col.names
  ins
}


test <- function(
      sampling.time = 0.5,
      data.version = paste0(round(1/sampling.time), "Hz"),
      src.basename = "1",
      src.ext = "txt",
      tmp.ext = "tmpRData",
      tgt.ext = "RData",
      parallel.lagging = "parmc",
      nworkers = 6)
{
  src.dir <- file.path(script.dir, "2nd-CARNet-bootstrap", data.version)
  tmp.dir <- file.path(script.dir, "Tests")
  dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)
  tgt.dir <- file.path(script.dir, "Tests")
  dir.create(tgt.dir, showWarnings = FALSE, recursive = TRUE)
  
  tmp.basename <- paste("test", src.basename, sep = "-")
  tmp.basename <- paste(tmp.basename, tmp.ext, sep = ".")
  tmp.file <- file.path(tmp.dir, tmp.basename)
  
  tgt.basename <- paste("test", src.basename, sep = "-")
  tgt.basename <- paste(tgt.basename, tgt.ext, sep = ".")
  tgt.file <- file.path(tgt.dir, tgt.basename)
  
  # Gests instance (Time MABP CBFVL CBFVR etCO2)
  signal <- .get.instance(
    src.dir = src.dir,
    src.basename = src.basename,
    src.ext = src.ext
  )
  signal <- signal[, c("Time", "MABP", "CBFVL", "etCO2")]
  
  exp.mgr <- get.signal.experiment(
    signal = signal,
    time.var.name = "Time",
    abp.var.name = "MABP",
    co2.var.name = "etCO2",
    cbf.var.name = "CBFVL",
    sampling.time = sampling.time
  )
  
  # Lags for CARNet bootstrap on 20-apr-2015
  lags <- list(MABP = 1:8, etCO2 = 0:6)
  # Reduces lags for test
  lags <- list(MABP = 1:4, etCO2 = 0:2)
  
  lag.mgr <- get.lag.manager(
    lag.list = lags,
    tmp.file = tmp.file,
    output.var.name = "CBFVL",
    lag.per.group = nworkers,
    save.every.ngroups = 2,
    keep.nstats = 100
  )
  
  # Hyper parameters for CARNet bootstrap on 20-apr-2015
  sigma <- 2 ^ seq(-4, 10, 2)
  gamma <- 1 / (2 * sigma ^ 2)
  cost <- 2 ^ seq(-2, 10, 2)
  nu <- seq(0.2, 0.8, 0.2)
  # Reduces hyper parameters for test
  gamma <- gamma[4:6]
  cost <- cost[4:5]
  nu <- nu[2]
  
  parameters <- list(gamma = gamma,
                     cost = cost,
                     nu = nu)
  
  start.time <- Sys.time()
  ans <- grid.lags.and.parameters(
    exp.mgr = exp.mgr,
    lag.mgr = lag.mgr,
    parallel.lagging = parallel.lagging,
    parameters = parameters,
    nfolds = 2,
    time.rounding.digits = 4,
    train.cor.rounding.digits = 3,
    test.cor.rounding.digits = 3,
    nworkers = nworkers
  )
  end.time <-Sys.time()
  
  ans[["start.time"]] <- start.time
  ans[["end.time"]] <- end.time
  ans[["lagging.time"]] <- end.time - start.time
  
  # Reduces models to 10
  ans[["models"]] <- ans[["models"]][1:10]
  
  saveRDS(ans, file = tgt.file, compress = FALSE)
  file.remove(tmp.file)
  
  invisible(ans)
}
