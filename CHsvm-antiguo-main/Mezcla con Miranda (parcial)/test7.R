require(ggplot2)
require(gridExtra)
require(pracma)

# TOP.DIR <- "/media/jljara/7443-AD5A/Research-backup"
TOP.DIR <- file.path("", "research")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")
DATA.DIR <- file.path(TOP.DIR, "Data")
DATA.NAME <- "2nd-CARNet-bootstrap"
DATA.VERSION <- "Original"
SRC.DATA.DIR <- file.path(DATA.DIR, DATA.NAME, DATA.VERSION)



.downsample.by.averaging <- function(df, factor, from = 1, until = nrow(df), ...)
{ 
  df <- df[from:until, ]
  n <- nrow(df)
  spurious <- n %% factor
  df <- df[1:(n - spurious), ]
  n <- nrow(df)
  
  groups <- rep(1:n, each = factor, len = n)
  df <- aggregate(df, by = list(groups), FUN = mean, ...)
  
  return(df[, 2:ncol(df)])
}

.get.instance <- function(src.basename = "1", src.ext = ".txt")
{
  src.filename <- paste0(src.basename, src.ext)
  src.file <- file.path(SRC.DATA.DIR, src.filename)
  signals <- read.table(file = src.file, header = TRUE, check.names = FALSE)
  signals
}

test <- function(src.basename = "1", src.ext = ".txt")
{
  signals <- .get.instance(src.basename = src.basename,
                          src.ext = src.ext)
  signals <- signals[921:1120, ]
  signals["Time"] <- seq(0, by = 0.1, length.out = nrow(signals))
  
  sampled1 <- .downsample.by.averaging(df = signals, factor = 5)
  sampled1[["Time"]] <- seq(0.5, by = 0.5, length.out = nrow(sampled1))
  
  new.time <- round(seq(0, by = 0.5, length.out = nrow(sampled1)), 1)
  new.abp <- pracma::interp1(signals[["Time"]], signals[["MABP"]], new.time, 'spline')
  new.cbfvl <- pracma::interp1(signals[["Time"]], signals[["CBFV-L"]], new.time, 'spline')
  new.cbfvr <- pracma::interp1(signals[["Time"]], signals[["CBFV-R"]], new.time, 'spline')
  new.etco2 <- pracma::interp1(signals[["Time"]], signals[["etCO2"]], new.time, 'spline')
  sampled2 <- data.frame(new.time, new.abp, new.cbfvl, new.cbfvr, new.etco2)
  colnames(sampled2) <- c("Time", "MABP", "CBFV-L", "CBFV-R", "etCO2")
  
  p <- ggplot(data = signals, aes(x = Time, y = MABP))
  p <- p + geom_line(colour = "red")
  p <- p + geom_point(data = sampled1, aes(x = Time, y = MABP), shape = 10, colour = "blue")
  p <- p + geom_line(data = sampled1, aes(x = Time, y = MABP), linetype = "dashed", colour = "blue")
  p <- p + geom_point(data = sampled2, aes(x = Time, y = MABP), shape = 12, colour = "purple")
  p <- p + geom_line(data = sampled2, aes(x = Time, y = MABP), linetype = "dashed", colour = "purple")
  p
}

