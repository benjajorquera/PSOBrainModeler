
script.dir <- dirname(sys.frame(1)$ofile)

CARSIG.SCRIPT.BASENAME <- paste("carSignal", "v15.8.15", sep = "-")
CARSIG.SCRIPT.BASENAME <- paste(CARSIG.SCRIPT.BASENAME, "R", sep = ".")
CARSIG.SCRIPT.NAME <- file.path(script.dir, CARSIG.SCRIPT.BASENAME)
source(CARSIG.SCRIPT.NAME)

RNDUTL.SCRIPT.BASENAME <- paste("Rounding", "utils", "v15.8.15", sep = "-")
RNDUTL.SCRIPT.BASENAME <- paste(RNDUTL.SCRIPT.BASENAME, "R", sep = ".")
RNDUTL.SCRIPT.NAME <- file.path(script.dir, RNDUTL.SCRIPT.BASENAME)
source(RNDUTL.SCRIPT.NAME)



run <- function(
      sampling.time = 0.5,
      time.until.release = 10 - sampling.time,
      time.after.release = 20,
      smooth.step.stimulus = TRUE,
      filter.order = 2,
      cutoff.frequency = 0.20,
      left.stabilisation.time = 30,
      time.rounding.digits =  1,
      abp.rounding.digits =  4,
      tgt.file.sep = "-",
      tgt.file.prefix = paste("abp", "step", sep = tgt.file.sep),
      tgt.file.ext = "csv",
      time.tol = sampling.time / 100
      )
{
  freq <- sprintf("st=%.2f_Hz", 1 / sampling.time)
  pre <- sprintf("pre=%.1f_s", time.until.release)
  post <- sprintf("post=%.1f_s", time.after.release)
  smoothed <- "raw"
  if(smooth.step.stimulus)
    smoothed <- 
      sprintf("butter=%d_%.2f_Hz", filter.order, cutoff.frequency)
  suffix <- paste(freq, pre, post, smoothed, sep = tgt.file.sep)
  tgt.filename <- paste(tgt.file.prefix, suffix, sep = tgt.file.sep)
  tgt.filename <- paste(tgt.filename, tgt.file.ext, sep = ".")
  tgt.filename <- file.path(script.dir, tgt.filename)
  
  abp.step <- get.normalised.ABP.stimulus(
    sampling.time = sampling.time,
    time.until.release = time.until.release,
    time.after.release = time.after.release,
    smooth.step.stimulus = smooth.step.stimulus,
    filter.order = filter.order,
    cutoff.frequency = cutoff.frequency,
    left.stabilisation.time = left.stabilisation.time,
    time.rounding.digits =  time.rounding.digits,
    time.tol = time.tol
  )
  
  abp.step[["ABP.normalised"]] <- 
    round(abp.step[["ABP.normalised"]], abp.rounding.digits)
  
  release <- rep(0, length(abp.step[["ABP.normalised"]]))
  i <- which(are.tolerably.equal(
    x = abp.step[["time.instants"]],
    y = abp.step[["time.release"]],
    tol = time.tol
  ))
  release[i] <- 1
  
  abp.step <- data.frame(
    Time = abp.step[["time.instants"]],
    ABP = abp.step[["ABP.normalised"]],
    Release = release
  )
  
  write.csv2(x = abp.step, file = tgt.filename, row.names = FALSE)
  
  abp.step
}

