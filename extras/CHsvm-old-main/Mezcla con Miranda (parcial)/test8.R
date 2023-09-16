require(ggplot2)
require(gridExtra)

# TOP.DIR <- "/media/jljara/7443-AD5A/Research-backup"
TOP.DIR <- file.path("", "research")
SVR.DIR <- file.path(TOP.DIR, "SVM-R")
DATA.DIR <- file.path(TOP.DIR, "Data")
DATA.NAME <- "2nd-CARNet-bootstrap"



.get.instance <- function(src.basename = "100", src.ext = ".txt")
{
  src.dir <- file.path(DATA.DIR, DATA.NAME, "Original")
  src.filename <- paste0(src.basename, src.ext)
  src.file <- file.path(src.dir, src.filename)
  original <- read.table(file = src.file, header = TRUE, check.names = FALSE)
  
  src.dir <- file.path(DATA.DIR, DATA.NAME, "5Hz-by-average")
  src.filename <- paste0(src.basename, src.ext)
  src.file <- file.path(src.dir, src.filename)
  average.5Hz <- read.table(file = src.file, header = TRUE, check.names = FALSE)
    
  src.dir <- file.path(DATA.DIR, DATA.NAME, "2Hz-by-average")
  src.filename <- paste0(src.basename, src.ext)
  src.file <- file.path(src.dir, src.filename)
  average.2Hz <- read.table(file = src.file, header = TRUE, check.names = FALSE)
  
  src.dir <- file.path(DATA.DIR, DATA.NAME, "5Hz")
  src.filename <- paste0(src.basename, src.ext)
  src.file <- file.path(src.dir, src.filename)
  interp.5Hz <- read.table(file = src.file, header = TRUE, check.names = FALSE)
    
  src.dir <- file.path(DATA.DIR, DATA.NAME, "2Hz")
  src.filename <- paste0(src.basename, src.ext)
  src.file <- file.path(src.dir, src.filename)
  interp.2Hz <- read.table(file = src.file, header = TRUE, check.names = FALSE)
  
  list(original, average.5Hz, average.2Hz, interp.5Hz, interp.2Hz)
}

test <- function(src.basename = "100", src.ext = ".txt")
{
  signals <- .get.instance(src.basename = src.basename,
                           src.ext = src.ext)
  f <- function(df){
    i <- df[["Time"]] > 92.05 & df[["Time"]] < 112.05
    df[i, ]
  }
  signals <- lapply(signals, f)
  
  df <- data.frame(Time = signals[[1]][["Time"]],
                   MABP = signals[[1]][["MABP"]],
                   Sampling = "10Hz")
  df <- rbind(df, data.frame(Time = signals[[2]][["Time"]],
                             MABP = signals[[2]][["MABP"]],
                             Sampling = "Average.5Hz"))
  df <- rbind(df, data.frame(Time = signals[[3]][["Time"]],
                             MABP = signals[[3]][["MABP"]],
                             Sampling = "Average.2Hz"))
  df <- rbind(df, data.frame(Time = signals[[4]][["Time"]],
                             MABP = signals[[4]][["MABP"]],
                             Sampling = "Interp.5Hz"))
  df <- rbind(df, data.frame(Time = signals[[5]][["Time"]],
                             MABP = signals[[5]][["MABP"]],
                             Sampling = "Interp.2Hz"))
  
  p <- ggplot(data = df, aes(x = Time, y = MABP, colour = Sampling))
  p <- p + geom_line()
  p
}

