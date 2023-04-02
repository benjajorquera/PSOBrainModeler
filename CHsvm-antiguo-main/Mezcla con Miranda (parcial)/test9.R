

# TOP.DIR <- "/media/jljara/7443-AD5A/Research-backup"
TOP.DIR <- file.path("", "research")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")
DATA.DIR <- file.path(TOP.DIR, "Data")
DATA.NAME <- "2nd-CARNet-bootstrap"



.get.instance <- function(data.version = "5Hz",
                          src.basename = "100",
                          src.ext = ".txt")
{
  src.dir <- file.path(DATA.DIR, DATA.NAME, data.version)
  src.filename <- paste0(src.basename, src.ext)
  src.file <- file.path(src.dir, src.filename)
  read.table(file = src.file, header = TRUE, check.names = FALSE)
}

test <- function(data.version = "5Hz", src.basename = "100", src.ext = ".txt",
                 parallel.lagging = "seq", parallel.param.search = "seq")
{
  # Time MABP CBFV.L CBFV.R etCO2
  signal <- .get.instance(src.basename = src.basename, src.ext = src.ext)
  
  # Lags - defined with MCh on 15-dic-2014
  lags <- list(MABP = 0:8, etCO2 = 0:8, "CBFV-L" = 0:6)
  
  # Reduces lags for testing
  lags <- list(MABP = 0:1, etCO2 = 0:1, "CBFV-L" = 0:1)
  lags <- list(MABP = 0, etCO2 = 0, "CBFV-L" = 0)
  
  # hyper parameters - defined with MCh on 15-dic-2014
  sigma <- 2^(-4:10)
  gamma <- 1 / (2 * sigma^2)
  cost <- 2^(-2:10)
  nu <- seq(0.1, 0.9, 0.1)
  
  # Reduces hyper parameters for testing
  sigma <- 2^(-1:1)
  gamma <- 1 / (2 * sigma^2)
  cost <- 2^(-1:1)
  nu <- seq(0.4, 0.6, 0.1)
  
  sigma <- 1
  gamma <- 1 / (2 * sigma^2)
  cost <- 1
  nu <- 0.5
  
  parameters <- list(gamma = gamma,
                     cost = cost,
                     nu = nu)
  
  # Start time
  start.time <- Sys.time()
  
  ans <- grid.lags.and.parameters(
           signal = signal, time.colname = "Time",
           # default fold.colname,
           output.colname = "CBFV-L",
           input.colnames = c("MABP", "etCO2"),
           process.fold.function = nu.svr.process.fold.v2,
           combine.folds.function = nu.svr.combine.folds.v2,
           lags = lags,
           combine.lags.function = nu.svr.combine.lags.v2,
           # default lagging.if.folded,
           parallel.lagging = parallel.lagging,
           parameters = parameters,
           combine.params.function = nu.svr.combine.params.v2,
           n.folds = n.folds,
           parallel.param.search = parallel.param.search,
           output.var.name = "CBFV",
           n.keep = 100)
  
  # End time
  end.time <-Sys.time()
  
  ans[["start.time"]] <- start.time
  ans[["end.time"]] <- end.time
  ans[["lagging.time"]] <- end.time - start.time
  
  invisible(ans)
}
