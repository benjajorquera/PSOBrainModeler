
# TOP.DIR <- "/media/jljara/7443-AD5A/Research-backup"
TOP.DIR <- file.path("", "research")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")
ARI.DIR <- file.path(TOP.DIR, "ARI-R")
DATA.DIR <- file.path(TOP.DIR, "Data")
DATA.NAME <- "2nd-CARNet-bootstrap"

# [side effect: Signal-cross-validation, Signal-lagging]
SIGRID.SCRIPT.BASENAME <- paste("Signal", "grid", "v15.4.1", sep = "-")
SIGRID.SCRIPT.BASENAME <- paste(SIGRID.SCRIPT.BASENAME, "R", sep = ".")
SIGRID.SCRIPT.NAME <- file.path(SVR.DIR, SIGRID.SCRIPT.BASENAME)
source(SIGRID.SCRIPT.NAME)

LAGMGR.SCRIPT.BASENAME <- paste("Lag", "manager", "v15.4.2", sep = "-")
LAGMGR.SCRIPT.BASENAME <- paste(LAGMGR.SCRIPT.BASENAME, "R", sep = ".")
LAGMGR.SCRIPT.NAME <- file.path(SVR.DIR, LAGMGR.SCRIPT.BASENAME)
source(LAGMGR.SCRIPT.NAME)

SIGEXP.SCRIPT.BASENAME <- paste("Signal", "experiment", "v15.4.1", sep = "-")
SIGEXP.SCRIPT.BASENAME <- paste(SIGEXP.SCRIPT.BASENAME, "R", sep = ".")
SIGEXP.SCRIPT.NAME <- file.path(SVR.DIR, SIGEXP.SCRIPT.BASENAME)
source(SIGEXP.SCRIPT.NAME)

CARSIG.SCRIPT.BASENAME <- paste("carSignal", sep = "-")
CARSIG.SCRIPT.BASENAME <- paste(CARSIG.SCRIPT.BASENAME, "R", sep = ".")
CARSIG.SCRIPT.NAME <- file.path(ARI.DIR, CARSIG.SCRIPT.BASENAME)
source(CARSIG.SCRIPT.NAME)



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

# Comparing models with adjusted R2 is not a standard method for comparing
# nonlinear models (it is standard for multiple linear regression).
# If you do compare models by comparing adjusted R2, make sure that
# identical data, weighted identically, are used for all fits.


test <- function(
      sampling.time = 0.5,
      data.version = paste0(round(1/sampling.time), "Hz"),
      src.basename = "100",
      src.ext = "txt",
      tmp.ext = "tmpRData",
      tgt.ext = "RData",
      parallel.lagging = "seq",
      nworkers = 6)
{
  src.dir <- file.path(DATA.DIR, DATA.NAME, data.version)
  tmp.dir <- file.path(SVR.DIR, "Tests")
  tgt.dir <- file.path(SVR.DIR, "Tests")
  
  tmp.basename <- paste("test12", src.basename, sep = "-")
  tmp.basename <- paste(tmp.basename, tmp.ext, sep = ".")
  tmp.file <- file.path(tmp.dir, tmp.basename)
  
  tgt.basename <- paste("test12", src.basename, sep = "-")
  tgt.basename <- paste(tgt.basename, tgt.ext, sep = ".")
  tgt.file <- file.path(tgt.dir, tgt.basename)
  
  # Gests instance (Time MABP CBFVL CBFVR etCO2)
  signal <- .get.instance(
    src.dir = src.dir,
    src.basename = src.basename,
    src.ext = src.ext
  )
  signal <- signal[, c("Time", "MABP", "CBFVL", "etCO2")]
  #cat("length original signal: ", nrow(signal), "\n")
  
  exp.mgr <- get.signal.experiment(
    signal = signal,
    time.var.name = "Time",
    abp.var.name = "MABP",
    co2.var.name = "etCO2",
    cbf.var.name = "CBFVL",
    sampling.time = sampling.time
  )
  
  # Lags - defined with MCh on 15-dic-2014
  lags <- list(MABP = 0:8, etCO2 = 0:8, "CBFVL" = 0:6)
  # Reduces lags for CARNet bootstrap on 20-apr-2015
  #lags <- list(MABP = 1:8, etCO2 = 0:6)
  # Reduces lags for test
  #lags <- list(MABP = 1:2, etCO2 = 1:2, CBFVL = 1:2)
  # Reduces lags for FIR test
  lags <- list(MABP = 1:2, etCO2 = 0:2)
  
  lag.mgr <- get.lag.manager(
    lag.list = lags,
    tmp.file = tmp.file,
    output.var.name = "CBFVL",
    lag.per.group = nworkers,
    save.every.n.groups = 4,
    keep.nstats = 50
  )
  
  # hyper parameters - defined with MCh on 15-dic-2014
  sigma <- 2^(-4:10)
  gamma <- 1 / (2 * sigma^2)
  cost <- 2^(-2:10)
  nu <- seq(0.1, 0.9, 0.1)
  # Reduces hyper parameters for CARNet bootstrap on 20-apr-2015
  sigma <- 2 ^ seq(-4, 10, 2)
  gamma <- 1 / (2 * sigma ^ 2)
  cost <- 2 ^ seq(-2, 10, 2)
  nu <- seq(0.2, 0.8, 0.2)
  # Reduces hyper parameters for testing
  #sigma <- 2^(-1:1)
  #gamma <- 1 / (2 * sigma^2)
  #cost <- 2^(-1:1)
  #nu <- c(0.2, 0.4, 0.6)
  # Reduces hyper parameters for testing
  sigma <- 2^(0:1)
  gamma <- 1 / (2 * sigma^2)
  cost <- 2^(0:1)
  nu <- 0.5
  
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
    test.cor.rounding.digits = 3
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

compare.answers <- function(file1, file2)
{
  ans1 <- readRDS(file = file1)
  ans2 <- readRDS(file = file2)
  
  cat("time 1: ", ans1[["lagging.time"]], "\n")
  cat("time 2: ", ans2[["lagging.time"]], "\n")
  
  is <- all.equal(ans1[["stats"]], ans2[["stats"]])
#   is <- is && all.equal(ans1[["models"]], ans2[["models"]])

  is
}
