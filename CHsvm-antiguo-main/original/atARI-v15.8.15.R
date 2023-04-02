
require(ggplot2)
require(gridExtra)
require(pracma)
require(signal)



# This function obtains the CBFV response to an ABP stimulus
# according to the Aaslid-Tiecks (A-T) model with the specified
# parameters.
# The function receives the following arguments:
# 
# T, D and K: the model parameters.
#             Default value: values that generates a response with
#             A-T ARI = 5.
#
# time.instants: the time instants in which the input ABP stimulus was
#                sampled.
#
# ABP.normalised: the normalised ABP stimulus to be used as input to the
#                 A-T model.
#
# sampling.time: the sampling time used in the input ABP stimulus.
#                Default value: the smallest difference between time
#                instants (rounded to 3 decimal places - max. 1000 Hz).
#
# stabilisation.time: the time to be used for stabilisation of the model,
#                     before and after the input stimulus.
#                     Default value: 1 s
# 
# The function's answer is a list with the following:
# .$T, .$D, .$K: the parameters of the model used
# .$time.instants: a copy of the specified time instants.
# .$sampling.time: a copy of the specified sampling time of the signals.
# .$ABP.normalised: the specified normalised ABP stimulus.
# .$CBFV.theoretical.response: the CBFV response given by the A-T model
#                              with the specified parameters to the given
#                              ABP stimulus.
#
get.theoretical.CBFV.response <- function(
      T = 1.9,
      D = 0.75,
      K = 0.9,
      time.instants,
      ABP.normalised,
      sampling.time = min(round(diff(time.instants), 3)),
      stabilisation.time = 1
      )
{
  # Initialises the answer
  ans <- list()
  ans[["T"]] <- T
  ans[["D"]] <- D
  ans[["K"]] <- K
  ans[["time.instants"]] <- time.instants
	ans[["sampling.time"]] <- sampling.time
  ans[["ABP.normalised"]] <- ABP.normalised
  
	frequency <- 1 / ans[["sampling.time"]]
	nsamples <- length(ans[["time.instants"]])
	nsamples.stabilisation <-
    round(stabilisation.time / ans[["sampling.time"]])
  
  P <- c(
    rep(ans[["ABP.normalised"]][1], nsamples.stabilisation),
	  ans[["ABP.normalised"]],
		rep(ans[["ABP.normalised"]][nsamples], nsamples.stabilisation)
  )
	
  # Gets dP
  dP <- P - 1
  
  # Applies Tiecks' equations to obtain the CBFV signal
  X1 <- vector(mode = "numeric", length = length(P))
  X2 <- vector(mode = "numeric", length = length(P))
  CBFV <- vector(mode = "numeric", length = length(P))
  
  divisor <- frequency * ans[["T"]]
  X1[1] <- 0
  X2[1] <- 0
  CBFV[1] <- 1
  for(t in 2:length(P))
  {
    X1[t] <- X1[t-1] + (dP[t] - X2[t-1]) / divisor
    X2[t] <- X2[t-1] + (X1[t] - 2 * ans[["D"]] * X2[t-1]) / divisor
    CBFV[t] <- 1 + dP[t] - ans[["K"]] * X2[t]
  }
	
  if(nsamples.stabilisation > 0)
    CBFV <- CBFV[-(1:nsamples.stabilisation)]
	CBFV <- CBFV[1:nsamples]
	ans[["CBFV.theoretical.response"]] <- CBFV
  
  invisible(ans)
}


# This function creates a normalised ABP step stimulus and obtains the
# CBFV response according to the Aaslid-Tiecks (A-T) model with a
# specific level of efficiency.
# The function receives the following arguments:
# 
# sampling.time,
# time.until.release,
# time.after.release,
# smooth.step.stimulus,
# filter.order,
# cutoff.frequency,
# left.stabilisation.time,
# time.rounding.digits: the parameters required to generate a normalised
#                       ABP stimulus.
#                       Default values: as in function
#                       get.normalised.ABP.stimulus()
#
# T, D, K, stabilisation.time: the parameters necessary to obtain the
#                              CBFV response to the ABP stimulus
#                              generated.
#                              Default values: as in function
#                              get.theoretical.CBFV.response()
# 
# The function's answer is a list with the following:
# .$T, .$D, .$K: the parameters of the model used
# .$time.instants: the specified time instants.
# .$sampling.time: the sampling time of the signals.
# .$ABP.normalised: the specified normalised ABP stimulus.
# .$CBFV.step.response: the CBFV response given by the model to the
#                       specified ABP stimulus.
# .$time.release: the time instant in which the thigh-cuffs are
#                 supposedly released
#
get.AT.curve <- function(
      sampling.time = 0.1,
      time.until.release = 10,
      time.after.release = 20,
      smooth.step.stimulus = FALSE,
      filter.order = 2,
      cutoff.frequency = 0.20,
      left.stabilisation.time = ifelse(smooth.step.stimulus, 30, 0),
      time.rounding.digits =  format.info(sampling.time)[2],
      T = 1.9,
      D = 0.75,
      K = 0.9,
      stabilisation.time = 1
      )
{
	ans.abp <- get.normalised.ABP.stimulus(
    sampling.time = sampling.time,
    time.until.release = time.until.release,
    time.after.release = time.after.release,
    smooth.step.stimulus = smooth.step.stimulus,
    filter.order = filter.order,
    cutoff.frequency = cutoff.frequency,
    left.stabilisation.time = left.stabilisation.time,
    time.rounding.digits = time.rounding.digits
  )
  
	ans.cbfv <- get.theoretical.CBFV.response(
    T = T,
    D = D,
    K = K,
    time.instants = ans.abp$time.instants,
    ABP.normalised = ans.abp$ABP.normalised,
    sampling.time = ans.abp$sampling.time,
    stabilisation.time = stabilisation.time
  )
  
	ans.cbfv[["CBFV.step.response"]] <-
    ans.cbfv[["CBFV.theoretical.response"]]
	ans.cbfv[["CBFV.theoretical.response"]] <- NULL
	ans.cbfv[["time.release"]] <- ans.abp[["time.release"]]
  
  invisible(ans.cbfv)
}


# This function creates a plot of the signals generated with an
# Aaslid-Tiecks (A-T) model.
# The function receives the following arguments:
# curve: a list with the A-T curve, as returned by the function
#        get.AT.curve()
# title: the title for the plot.
#        Default value: NULL
# Specifically:
# - plots curve$ABP.normalised and curve$CBFV.step.response signals using
#   curve$time.instants as x-axis
# - annotates the plot with the values of curve$T, curve$D, curve$K and
#   curve$sampling.time
# - annotates the plot with a vertical grey line marking the time of
#   thigh-cuff release
# - uses the argument title (if specified) as the plot's title
# 
# The function returns the resulting plot
#
get.AT.curve.plot <- function(curve, title = NULL)
{
  tab <- data.frame(
    Parameters = c(
      curve[["T"]],
      curve[["D"]],
      curve[["K"]],
      curve[["sampling.time"]]
  ))
  tab[["Parameters"]] <- format(tab[["Parameters"]])
  g <- tableGrob(tab, gp = gpar(fontsize = 5))
  xrange <- range(curve[["time.instants"]])
  xrange <- (xrange[2] - xrange[1]) / 5
  yrange <- 0.5
  
  df1 <- data.frame(
    Time = curve[["time.instants"]],
    Value = curve[["ABP.normalised"]],
    Signal = "ABP"
  )
  df2 <- data.frame(
    Time = curve[["time.instants"]],
    Value = curve[["CBFV.step.response"]],
    Signal = "CBFV"
  )
  df <- rbind(df1, df2)
  p <- ggplot(data = df, aes(x = Time, y = Value, colour = Signal))
  p <- p + geom_line() + geom_point()
  p <- p + geom_vline(xintercept = curve[["time.release"]], colour = "grey")  
  
  n <- length(curve[["time.instants"]])
  xmax <- curve[["time.instants"]][n]
  y <- curve[["CBFV.step.response"]][n]
  if(y > yrange)
    y <- c(0, yrange)
  else
    y <- c(yrange, 2 * yrange)
  p <- p + annotation_custom(
    g,
    xmin = xmax - xrange,
    xmax = xmax,
    ymin = y[1],
    ymax = y[2]
  )
  
  if(!is.null(title))
    p <- p + labs(title = title, x = "Time [s]", y = "Signals [a.u.]")
  else
    p <- p + labs(x = "Time [s]", y = "Signals [a.u.]")
  
  p
}


get.AT.templates.parameters <- function()
{
  K <- c(0.00, 0.20, 0.40, 0.60, 0.80, 0.90, 0.94, 0.96, 0.97, 0.98)
  D <- c(1.60, 1.60, 1.50, 1.15, 0.90, 0.75, 0.65, 0.55, 0.52, 0.50)
  T <- c(2.00, 2.00, 2.00, 2.00, 2.00, 1.90, 1.60, 1.20, 0.87, 0.65)
  ARI <- 0:9
  
  data.frame(T, D, K, ARI)
}


get.AT.templates <- function(
      sampling.time = 0.1,
      time.until.release = 10,
      time.after.release = 20,
      smooth.step.stimulus = FALSE,
      filter.order = 2,
      cutoff.frequency = 0.20,
      left.stabilisation.time = ifelse(smooth.step.stimulus, 30, 0),
      time.rounding.digits =  format.info(sampling.time)[2],
      stabilisation.time = 1
      )
{
	ans.abp <- get.normalised.ABP.stimulus(
    sampling.time = sampling.time,
    time.until.release = time.until.release,
    time.after.release = time.after.release,
    smooth.step.stimulus = smooth.step.stimulus,
    filter.order = filter.order,
    cutoff.frequency = cutoff.frequency,
    left.stabilisation.time = left.stabilisation.time,
    time.rounding.digits = time.rounding.digits
  )
  params <- get.AT.templates.parameters()
   
  .tmp.fun <- function(row)
    get.theoretical.CBFV.response(
      T = row[["T"]],
      D = row[["D"]],
      K = row[["K"]],
      time.instants = ans.abp$time.instants,
      ABP.normalised = ans.abp$ABP.normalised,
      sampling.time = ans.abp$sampling.time,
      stabilisation.time = stabilisation.time
    )
  templates <- apply(params, 1, .tmp.fun)
  
  .tmp.fun <- function(ans) {
    ans[["CBFV.step.response"]] <- ans[["CBFV.theoretical.response"]]
    ans[["CBFV.theoretical.response"]] <- NULL
    ans[["time.release"]] <- ans.abp[["time.release"]]
    ans
  }
  templates <- lapply(templates, .tmp.fun)
  
  .tmp.fun <- function(i) {
    templates[[i]][["ARI.value"]] <- params[i, "ARI"]
    templates[[i]]
  }
  templates <- lapply(1:length(templates), .tmp.fun)
  
  names(templates) <- paste("ARI", params[["ARI"]], sep = " = ")
  
  templates
}


get.AT.decimal.templates <- function(
      sampling.time = 0.1,
      time.until.release = 10,
      time.after.release = 20,
      smooth.step.stimulus = FALSE,
      filter.order = 2,
      cutoff.frequency = 0.20,
      left.stabilisation.time = ifelse(smooth.step.stimulus, 30, 0),
      time.rounding.digits =  format.info(sampling.time)[2],
      stabilisation.time = 1,
      rounding.digits = 6
      )
{
	ans.abp <- get.normalised.ABP.stimulus(
    sampling.time = sampling.time,
    time.until.release = time.until.release,
    time.after.release = time.after.release,
    smooth.step.stimulus = smooth.step.stimulus,
    filter.order = filter.order,
    cutoff.frequency = cutoff.frequency,
    left.stabilisation.time = left.stabilisation.time,
    time.rounding.digits = time.rounding.digits
  )
  params <- get.AT.decimal.templates.parameters(
    rounding.digits = rounding.digits
  )
  
  .tmp.fun <- function(row)
    get.theoretical.CBFV.response(
      T = row[["T"]],
      D = row[["D"]],
      K = row[["K"]],
      time.instants = ans.abp$time.instants,
      ABP.normalised = ans.abp$ABP.normalised,
      sampling.time = ans.abp$sampling.time,
      stabilisation.time = stabilisation.time
    )
  templates <- apply(params, 1, .tmp.fun)
  
  .tmp.fun <- function(ans) {
    ans[["CBFV.step.response"]] <- ans[["CBFV.theoretical.response"]]
    ans[["CBFV.theoretical.response"]] <- NULL
    ans[["time.release"]] <- ans.abp[["time.release"]]
    ans
  }
  templates <- lapply(templates, .tmp.fun)
  
  .tmp.fun <- function(i) {
    templates[[i]][["ARI.value"]] <- params[i, "ARI"]
    templates[[i]]
  }
  templates <- lapply(1:length(templates), .tmp.fun)
  
  names(templates) <-
    paste("ARI", sprintf("%.1f", params[["ARI"]]), sep = " = ")
  
  templates
}


get.AT.decimal.templates.parameters <- function(rounding.digits = 6)
{
  orig <- get.AT.templates.parameters()
  
  ARI.decimal <- round(seq(0, 9, 0.1), 1)
  K.decimal <-
    pracma::interp1(orig[["ARI"]], orig[["K"]], ARI.decimal, 'spline')
  K.decimal <- round(K.decimal, rounding.digits)
  D.decimal <- 
    pracma::interp1(orig[["ARI"]], orig[["D"]], ARI.decimal, 'spline')
  D.decimal <- round(D.decimal, rounding.digits)
  T.decimal <-
    pracma::interp1(orig[["ARI"]], orig[["T"]], ARI.decimal, 'spline')
  T.decimal <- round(T.decimal, rounding.digits)
  
  data.frame(
    T = T.decimal,
    D = D.decimal,
    K = K.decimal,
    ARI = ARI.decimal
  )
}


# This function creates a plot with CBFV responses generated by
# Aaslid-Tiecks (A-T) models  (normally for the same ABP stimulus).
# The function receives the following argument:
# curves: a list in which each element is a list with an A-T curve as
#         returned by the function get.AT.curve()
# label.name: the name for the legend of the plot. If NULL, no legend is
#             added and curves are plotted in black.
#             Default value: NULL
# labels: a vector of the same length as curves, containing the label for
#         each curve. If label.name is not NULL, these labels are used as
#         the colour aesthetic and for labelling the curves in the legend.
#         Default value: NULL
# title: the title for the plot.
#        Default value: NULL
#
# The function returns the resulting plot
#
get.AT.curves.plot <- function(
      curves,
      label.name = NULL,
      labels = NULL,
      title = NULL
      )
{
  n <- length(curves)
  if(is.null(labels))
    labels <- 1:n
  group.name <- "Response"
  if(!is.null(label.name))
    group.name <- label.name
  
  times <- c(sapply(1:n, function(i) curves[[i]][["time.instants"]]))
  values <- c(sapply(1:n, function(i) curves[[i]][["CBFV.step.response"]]))
  lengths <- c(sapply(1:n, function(i) length(curves[[i]][["time.instants"]])))
  groups <- rep(labels, times = lengths)
  
  df <- data.frame(Time = times, CBFV = values, groups)
  names(df)[ncol(df)] <- group.name
  
  p <- ggplot(df, aes_string(x = "Time", y = "CBFV", group = group.name))
  if(is.null(label.name))
    p <- p + geom_line()
  else
    p <- p + geom_line(aes_string(colour = group.name))
  
  if(!is.null(title))
    p <- p + labs(title = title, x = "Time [s]", y = "CBFV responses [a.u.]")
  else
    p <- p + labs(x = "Time [s]", y = "CBFV responses [a.u.]")
  
  p
}




