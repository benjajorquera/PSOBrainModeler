
library(doParallel)

TOP.DIR <- file.path("", "research")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")
DATA.DIR <- file.path(SVR.DIR, "VEP", "Baseline data")

SIGRID.SCRIPT.BASENAME <- paste("Signal", "grid", sep = "-")
SIGRID.SCRIPT.BASENAME <- paste(SIGRID.SCRIPT.BASENAME, "R", sep = ".")
SIGRID.SCRIPT.NAME <- file.path(SVR.DIR, SIGRID.SCRIPT.BASENAME)
source(SIGRID.SCRIPT.NAME)

SIGLAG.SCRIPT.BASENAME <- paste("Signal", "lagging", sep = "-")
SIGLAG.SCRIPT.BASENAME <- paste(SIGLAG.SCRIPT.BASENAME, "R", sep = ".")
SIGLAG.SCRIPT.NAME <- file.path(SVR.DIR, SIGLAG.SCRIPT.BASENAME)
source(SIGLAG.SCRIPT.NAME)

SIGSIM.SCRIPT.BASENAME <- paste("Signal", "simulation", sep = "-")
SIGSIM.SCRIPT.BASENAME <- paste(SIGSIM.SCRIPT.BASENAME, "R", sep = ".")
SIGSIM.SCRIPT.NAME <- file.path(SVR.DIR, SIGSIM.SCRIPT.BASENAME)
source(SIGSIM.SCRIPT.NAME)

SVR.SCRIPT.BASENAME <- paste("Signal", "SVR", "v1", sep = "-")
SVR.SCRIPT.BASENAME <- paste(SVR.SCRIPT.BASENAME, "R", sep = ".")
SVR.SCRIPT.NAME <- file.path(SVR.DIR, SVR.SCRIPT.BASENAME)
source(SVR.SCRIPT.NAME)



test <- function(sampling.time = 0.2, time.start = 60, duration =  200)
{
  cl <- makeCluster(10)
  registerDoParallel(cl)
  
  # Gajardo (2014) used ABP = 2:10
  lags <- list(ABP = 0:10, CO2 = 0:6, CBFV = 0:6)
  
  # From Gajardo (2014)
  sigma <- 2^(-1:5)
  gamma <- 1 / (2 * sigma^2)
  cost <- 2^(-2:10)
  nu <- seq(0.1, 0.9, 0.1)
  parameters <- list(gamma = gamma, cost = cost, nu = nu)
  
  subject.names <- c("AM", "AN", "DY")
  src.basenames <- paste(subject.names, "Baseline", 3, sep = "_")
  src.basenames <- paste(src.basenames, "PAR", sep = ".")
  src.names <- file.path(DATA.DIR, src.basenames)
  src.format <- c("Time", "CBFV", "ABP", "CO2")
  
  for(subj in 1:1)
  {
    # Start time
    start.time <- Sys.time()
    
    subj.data <- read.table(file = src.names[subj])
    colnames(subj.data) <- src.format
    
    ini <- min(which(subj.data[["Time"]] >= time.start))
    fin <- min(which(subj.data[["Time"]] >= time.start + duration)) - 1
    subj.data <- subj.data[ini:fin, ]
    
    folded.data <- separateSignalIntoFolds(signal = subj.data,
                                           n.folds = 2,
                                           time.colname = "Time")
    ans <- grid.lags.and.parameters(
             signal = folded.data, time.colname = "Time",
             # default fold.colname,
             output.colname = "CBFV", input.colnames = c("ABP", "CO2"),
             process.fold.function = nu.svr.process.fold.v1,
             combine.folds.function = nu.svr.combine.folds.v1,
             lags = lags,
             combine.lags.function = nu.svr.combine.lags.v1,
             lagging.if.folded = "within-fold",
             # default parallel.lagging,
             parameters = parameters,
             combine.params.function = nu.svr.combine.params.v1,
             n.folds = 2,
             # parallel.param.search = parallel.param.search,
             keep.model = FALSE)
    
    # End time
    end.time <-Sys.time()
    
    time <- end.time - start.time
    ans <- list(cv = ans, time = time)
    return(ans)
  }
}
