

# TOP.DIR <- "/media/jljara/7443-AD5A/Research-backup"
TOP.DIR <- file.path("", "research")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")
DATA.DIR <- file.path(TOP.DIR, "Data")
DATA.NAME <- "2nd-CARNet-bootstrap"

# [side effect: Signal-cross-validation, Signal-lagging]
SIGRID.SCRIPT.BASENAME <- paste("Signal", "grid", sep = "-")
SIGRID.SCRIPT.BASENAME <- paste(SIGRID.SCRIPT.BASENAME, "R", sep = ".")
SIGRID.SCRIPT.NAME <- file.path(SVR.DIR, SIGRID.SCRIPT.BASENAME)
source(SIGRID.SCRIPT.NAME)

SVR.SCRIPT.BASENAME <- paste("Signal", "SVR", "v2", sep = "-")
SVR.SCRIPT.BASENAME <- paste(SVR.SCRIPT.BASENAME, "R", sep = ".")
SVR.SCRIPT.NAME <- file.path(SVR.DIR, SVR.SCRIPT.BASENAME)
source(SVR.SCRIPT.NAME)


.get.instance <- function(data.version = "5Hz",
                          src.basename = "100",
                          src.ext = ".txt")
{
  src.dir <- file.path(DATA.DIR, DATA.NAME, data.version)
  src.filename <- paste0(src.basename, src.ext)
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

# List of 11
#  $ MABP=7, etCO2=7, CBFVL=5:List of 27
#  $ MABP=8, etCO2=7, CBFVL=5:List of 27
#  $ MABP=7, etCO2=8, CBFVL=5:List of 27
#  $ MABP=8, etCO2=8, CBFVL=5:List of 27
#  $ MABP=7, etCO2=7, CBFVL=6:List of 27
#  $ MABP=8, etCO2=7, CBFVL=6:List of 27
#  $ MABP=7, etCO2=8, CBFVL=6:List of 27
#  $ MABP=8, etCO2=8, CBFVL=6:List of 27
#  $ start.time              : POSIXct[1:1], format: "2015-04-16 14:23:41"
#  $ end.time                : POSIXct[1:1], format: "2015-04-16 14:24:13"
#  $ lagging.time            :Class 'difftime'  atomic [1:1] 32.7
#   .. ..- attr(*, "units")= chr "secs"

# 8 * 27 * 2 = 432 models 
# -rw-rw-r-- 1 jljara jljara 19396898 Apr 16 14:28 test10.RData
# -rw-rw-r-- 1 jljara jljara      19M Apr 16 14:28 test10.RData
# = 44900.23 bytes per model
# for 1755 models: 78799898 bytes = 79M

test <- function(data.version = "5Hz", src.basename = "100", src.ext = ".txt",
                 parallel.lagging = "seq", parallel.param.search = "seq")
{
  # Time MABP CBFVL CBFVR etCO2
  signal <- .get.instance(src.basename = src.basename, src.ext = src.ext)
  signal <- signal[, c("Time", "MABP", "CBFVL", "etCO2")]
  
  # Lags - defined with MCh on 15-dic-2014
  lags <- list(MABP = 0:8, etCO2 = 0:8, "CBFVL" = 0:6)
  
  # Reduces lags for testing
  lags <- list(MABP = 7:8, etCO2 = 7:8, "CBFVL" = 5:6)
  
  # hyper parameters - defined with MCh on 15-dic-2014
  sigma <- 2^(-4:10)
  gamma <- 1 / (2 * sigma^2)
  cost <- 2^(-2:10)
  nu <- seq(0.1, 0.9, 0.1)
  
  # Reduces hyper parameters for testing
  sigma <- 2^(-1:1)
  gamma <- 1 / (2 * sigma^2)
  cost <- 2^(-1:1)
  nu <- c(0.1, 0.4, 0.7)
  
  parameters <- list(gamma = gamma,
                     cost = cost,
                     nu = nu)
  
  # Start time
  start.time <- Sys.time()
  
  ans <- grid.lags.and.parameters(
    signal = signal,
    time.colname = "Time",
    # default fold.colname,
    output.colname = "CBFVL",
    input.colnames = c("MABP", "etCO2"),
    process.fold.function = nu.svr.process.fold.v1,
    combine.folds.function = nu.svr.combine.folds.v1,
    parameters = parameters,
    combine.params.function = nu.svr.combine.params.v1,
    n.folds = 2,
    parallel.param.search = parallel.param.search,
    lags = lags,
    combine.lags.function = nu.svr.combine.lags.v1,
    # default lagging.if.folded,
    parallel.lagging = parallel.lagging,
    output.var.name = "CBFVL"
  )
  
  # End time
  end.time <-Sys.time()
  
  ans[["start.time"]] <- start.time
  ans[["end.time"]] <- end.time
  ans[["lagging.time"]] <- end.time - start.time
  
  invisible(ans)
}
